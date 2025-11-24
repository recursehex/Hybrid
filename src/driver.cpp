// This file implements the command-line driver that configures compiler sessions, parses inputs, and compiles source files.

#include "parser.h"
#include "toplevel.h"
#include "ast.h"
#include "compiler_session.h"
#include "optimizer/arc_optimizer.h"
#include "optimizer/escape_analysis.h"

#include <algorithm>
#include <cctype>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>
#include <system_error>
#include <vector>
#include <limits>

#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

namespace {

bool endsWith(const std::string &value, const char *suffix) {
  size_t len = std::strlen(suffix);
  if (value.size() < len)
    return false;
  return value.compare(value.size() - len, len, suffix) == 0;
}

void initializeOperatorPrecedence() {
  auto &precedence = currentParser().binopPrecedence;
  precedence.clear();
  precedence["="] = 2;   // assignment (lowest)
  precedence["+="] = 2;  // compound assignments
  precedence["-="] = 2;
  precedence["*="] = 2;
  precedence["/="] = 2;
  precedence["%="] = 2;
  precedence["\?\?="] = 2;
  precedence["&="] = 2;  // bitwise compound assignments
  precedence["|="] = 2;
  precedence["^="] = 2;
  precedence["<<="] = 2;
  precedence[">>="] = 2;
  precedence["\?\?"] = 3;  // null-coalescing
  precedence["||"] = 5;  // logical OR
  precedence["&&"] = 6;  // logical AND
  precedence["|"] = 7;   // bitwise OR
  precedence["^"] = 8;   // bitwise XOR
  precedence["&"] = 9;   // bitwise AND
  precedence["<"] = 10;  // comparisons
  precedence[">"] = 10;
  precedence["<="] = 10;
  precedence[">="] = 10;
  precedence["=="] = 10;
  precedence["!="] = 10;
  precedence["<<"] = 15; // shift operators
  precedence[">>"] = 15;
  precedence["+"] = 20;  // arithmetic
  precedence["-"] = 20;
  precedence["*"] = 40;  // multiplication/division/modulo
  precedence["/"] = 40;
  precedence["%"] = 40;  // modulo has same precedence as * and /
}

bool loadSourceFile(const char *path, std::string &outBuffer) {
  std::ifstream input(path, std::ios::in | std::ios::binary);
  if (!input)
    return false;

  std::ostringstream contents;
  contents << input.rdbuf();
  outBuffer = contents.str();
  return true;
}

std::string deriveOutputPath(const std::string &sourcePath) {
  std::string stem = sourcePath;
  std::size_t slash = stem.find_last_of("/\\");
  if (slash != std::string::npos)
    stem = stem.substr(slash + 1);
  std::size_t dot = stem.find_last_of('.');
  if (dot != std::string::npos)
    stem = stem.substr(0, dot);
  if (stem.empty())
    stem = "output";
  return stem + ".ll";
}

void printUsage() {
  fprintf(stderr, "Usage: hybrid [options] [files...]\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  --emit-llvm        Emit the generated LLVM IR\n");
  fprintf(stderr, "  -o <file>          Write output to <file> (implies --emit-llvm)\n");
  fprintf(stderr, "  -o -               Write LLVM IR to stdout\n");
  fprintf(stderr, "  --diagnostics generics\n");
  fprintf(stderr, "                     Print generics diagnostics summary\n");
  fprintf(stderr, "  --dump-generic-stack\n");
  fprintf(stderr, "                     Dump the active generic binding stack on overflow\n");
  fprintf(stderr, "  --max-generic-depth <n>\n");
  fprintf(stderr, "                     Override the max generic binding depth (default 128)\n");
  fprintf(stderr, "  --max-generic-instantiations <n>\n");
  fprintf(stderr, "                     Fail when total generic instantiations exceed <n>\n");
  fprintf(stderr, "  --max-nested-generics <n>\n");
  fprintf(stderr, "                     Fail when nested generic depth exceeds <n>\n");
  fprintf(stderr, "  --arc-optimizer    Enable ARC retain/release peephole pass\n");
  fprintf(stderr, "  --arc-trace-retains\n");
  fprintf(stderr, "                     Emit per-function retain/release/autorelease counts\n");
  fprintf(stderr, "  --arc-escape-debug Enable ARC escape analysis logging\n");
}

bool shouldEnableGenericsMetrics() {
  const char *env = std::getenv("HYBRID_GENERICS_METRICS");
  if (!env)
    return false;
  while (std::isspace(static_cast<unsigned char>(*env)))
    ++env;
  if (*env == '\0')
    return true;

  std::string lowered;
  lowered.reserve(std::strlen(env));
  for (const char *ptr = env; *ptr; ++ptr)
    lowered.push_back(
        static_cast<char>(std::tolower(static_cast<unsigned char>(*ptr))));

  if (lowered == "0" || lowered == "false" || lowered == "off")
    return false;
  return true;
}

bool shouldForceGenericsDiagnostics() {
  const char *env = std::getenv("HYBRID_SHOW_GENERIC_METRICS");
  if (!env)
    return false;
  while (std::isspace(static_cast<unsigned char>(*env)))
    ++env;
  if (*env == '\0')
    return true;

  std::string lowered;
  lowered.reserve(std::strlen(env));
  for (const char *ptr = env; *ptr; ++ptr)
    lowered.push_back(
        static_cast<char>(std::tolower(static_cast<unsigned char>(*ptr))));

  if (lowered == "0" || lowered == "false" || lowered == "off")
    return false;
  return true;
}

template <typename T>
bool parseUnsignedValue(const std::string &text, T &out) {
  if (text.empty())
    return false;
  char *end = nullptr;
  errno = 0;
  unsigned long long value = std::strtoull(text.c_str(), &end, 10);
  if (errno != 0 || end == text.c_str() || *end != '\0')
    return false;
  if (value > std::numeric_limits<T>::max())
    return false;
  out = static_cast<T>(value);
  return true;
}

void emitGenericsMetricsSummary(const CodegenContext &context) {
  const auto &metrics = context.genericsMetrics;
  const auto &diag = context.genericsDiagnostics;
  if (!metrics.enabled && !diag.diagnosticsEnabled)
    return;
  if (metrics.enabled) {
    fprintf(stderr,
            "[generics-metrics] functions hits:%llu misses:%llu | types hits:%llu misses:%llu\n",
            static_cast<unsigned long long>(metrics.functionCacheHits),
            static_cast<unsigned long long>(metrics.functionCacheMisses),
            static_cast<unsigned long long>(metrics.typeCacheHits),
            static_cast<unsigned long long>(metrics.typeCacheMisses));
  }
  if (diag.diagnosticsEnabled) {
    fprintf(stderr,
            "[generics-diag] type-specializations:%llu function-specializations:%llu "
            "| module-bytes:%llu | depth:%u/%u\n",
            static_cast<unsigned long long>(diag.uniqueCompositeInstantiations),
            static_cast<unsigned long long>(diag.uniqueFunctionInstantiations),
            static_cast<unsigned long long>(diag.moduleIRBytesAfterPrint),
            diag.peakBindingDepth, diag.maxBindingDepth);
    if (diag.depthLimitHit)
      fprintf(stderr, "  note: generic binding depth limit reached\n");
    if (diag.instantiationBudgetExceeded)
      fprintf(stderr,
              "  note: generic instantiation budget exceeded (limit %llu)\n",
              static_cast<unsigned long long>(diag.instantiationBudget));
    if (diag.nestedBudgetExceeded)
      fprintf(stderr,
              "  note: nested generic depth budget exceeded (limit %u)\n",
              diag.nestedDepthBudget);
  }
}

static void emitArcTraceSummary(const CodegenContext &context) {
  const ArcTraceState &trace = context.arcTrace;
  if (!trace.traceEnabled)
    return;

  auto printEntry = [](const char *phase, const std::string &name,
                       const ArcRetainCounts &counts) {
    fprintf(stderr, "[arc-trace] %s %s retains=%llu releases=%llu autoreleases=%llu\n",
            phase, name.c_str(),
            static_cast<unsigned long long>(counts.retains),
            static_cast<unsigned long long>(counts.releases),
            static_cast<unsigned long long>(counts.autoreleases));
  };

  for (const auto &entry : trace.preOptimizationCounts)
    printEntry("pre", entry.first, entry.second);

  if (!trace.optimizerRan)
    return;

  std::map<std::string, ArcRetainCounts> merged = trace.postOptimizationCounts;
  for (const auto &entry : trace.preOptimizationCounts) {
    merged.emplace(entry.first, ArcRetainCounts{});
  }

  for (const auto &entry : merged) {
    auto it = trace.postOptimizationCounts.find(entry.first);
    const ArcRetainCounts &counts =
        it != trace.postOptimizationCounts.end() ? it->second
                                                 : ArcRetainCounts{};
    printEntry("post", entry.first, counts);
  }
}

}

int main(int argc, char **argv) {
  CompilerSession session;
  session.resetAll();
  pushCompilerSession(session);
  bool dumpGenericStack = false;
  bool diagnosticsFlag = false;
  const bool diagnosticsFromEnv = shouldForceGenericsDiagnostics();
  uint64_t maxGenericInstantiations = 0;
  unsigned maxNestedGenerics = 0;
  unsigned maxGenericDepth = 0;
  bool enableArcOptimizer = false;
  bool enableArcTrace = false;
  bool enableArcEscapeDebug = false;

  session.codegen().genericsMetrics.enabled = shouldEnableGenericsMetrics();

  bool emitLLVM = false;
  std::string outputPath;
  std::vector<std::string> sourceFiles;

  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "--emit-llvm") {
      emitLLVM = true;
    } else if (arg == "--diagnostics") {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --diagnostics requires a value\n");
        printUsage();
        popCompilerSession();
        return 1;
      }
      std::string value = argv[++i];
      if (value == "generics") {
        diagnosticsFlag = true;
      } else {
        fprintf(stderr, "Error: Unknown diagnostics channel '%s'\n", value.c_str());
        printUsage();
        popCompilerSession();
        return 1;
      }
    } else if (arg.rfind("--diagnostics=", 0) == 0) {
      std::string value = arg.substr(std::strlen("--diagnostics="));
      if (value == "generics") {
        diagnosticsFlag = true;
      } else {
        fprintf(stderr, "Error: Unknown diagnostics channel '%s'\n", value.c_str());
        printUsage();
        popCompilerSession();
        return 1;
      }
    } else if (arg == "--dump-generic-stack") {
      dumpGenericStack = true;
    } else if (arg == "--max-generic-depth") {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --max-generic-depth requires a value\n");
        printUsage();
        popCompilerSession();
        return 1;
      }
      if (!parseUnsignedValue(argv[++i], maxGenericDepth)) {
        fprintf(stderr, "Error: Invalid value for --max-generic-depth\n");
        popCompilerSession();
        return 1;
      }
    } else if (arg.rfind("--max-generic-depth=", 0) == 0) {
      std::string value = arg.substr(std::strlen("--max-generic-depth="));
      if (!parseUnsignedValue(value, maxGenericDepth)) {
        fprintf(stderr, "Error: Invalid value for --max-generic-depth\n");
        popCompilerSession();
        return 1;
      }
    } else if (arg == "--max-generic-instantiations") {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --max-generic-instantiations requires a value\n");
        printUsage();
        popCompilerSession();
        return 1;
      }
      if (!parseUnsignedValue(argv[++i], maxGenericInstantiations)) {
        fprintf(stderr, "Error: Invalid value for --max-generic-instantiations\n");
        popCompilerSession();
        return 1;
      }
    } else if (arg.rfind("--max-generic-instantiations=", 0) == 0) {
      std::string value =
          arg.substr(std::strlen("--max-generic-instantiations="));
      if (!parseUnsignedValue(value, maxGenericInstantiations)) {
        fprintf(stderr, "Error: Invalid value for --max-generic-instantiations\n");
        popCompilerSession();
        return 1;
      }
    } else if (arg == "--max-nested-generics") {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --max-nested-generics requires a value\n");
        printUsage();
        popCompilerSession();
        return 1;
      }
      if (!parseUnsignedValue(argv[++i], maxNestedGenerics)) {
        fprintf(stderr, "Error: Invalid value for --max-nested-generics\n");
        popCompilerSession();
        return 1;
      }
    } else if (arg.rfind("--max-nested-generics=", 0) == 0) {
      std::string value =
          arg.substr(std::strlen("--max-nested-generics="));
      if (!parseUnsignedValue(value, maxNestedGenerics)) {
        fprintf(stderr, "Error: Invalid value for --max-nested-generics\n");
        popCompilerSession();
        return 1;
      }
    } else if (arg == "--arc-optimizer") {
      enableArcOptimizer = true;
    } else if (arg == "--arc-trace-retains") {
      enableArcTrace = true;
    } else if (arg == "--arc-escape-debug") {
      enableArcEscapeDebug = true;
    } else if (arg == "-o") {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: -o requires an output path\n");
        printUsage();
        popCompilerSession();
        return 1;
      }
      outputPath = argv[++i];
    } else if (!arg.empty() && arg[0] == '-') {
      fprintf(stderr, "Error: Unknown option '%s'\n", arg.c_str());
      printUsage();
      popCompilerSession();
      return 1;
    } else {
      sourceFiles.push_back(arg);
    }
  }

  diagnosticsFlag = diagnosticsFlag || diagnosticsFromEnv || dumpGenericStack;
  CodegenContext &codegenCtx = session.codegen();
  codegenCtx.genericsMetrics.enabled =
      codegenCtx.genericsMetrics.enabled || diagnosticsFlag;
  auto &diagConfig = codegenCtx.genericsDiagnostics;
  diagConfig.diagnosticsEnabled = diagnosticsFlag;
  if (dumpGenericStack)
    diagConfig.stackDumpEnabled = true;
  if (maxGenericDepth)
    diagConfig.maxBindingDepth = maxGenericDepth;
  if (maxGenericInstantiations)
    diagConfig.instantiationBudget = maxGenericInstantiations;
  if (maxNestedGenerics)
    diagConfig.nestedDepthBudget = maxNestedGenerics;
  if (enableArcTrace)
    codegenCtx.arcTrace.traceEnabled = true;
  if (enableArcOptimizer)
    codegenCtx.arcTrace.optimizerEnabled = true;

  auto runArcPasses = [&](llvm::Module *module) {
    if (!module)
      return;
    const bool arcDebug = std::getenv("HYBRID_ARC_OPT_DEBUG") != nullptr;
    const bool escapeDebug = enableArcEscapeDebug || arcDebug;
    if (codegenCtx.arcTrace.traceEnabled)
      collectArcRetainReleaseCounts(
          *module, codegenCtx.arcTrace.preOptimizationCounts);

    bool arcPassesRan = false;
    if (codegenCtx.arcTrace.optimizerEnabled || escapeDebug) {
      if (arcDebug)
        fprintf(stderr, "[arc-opt] begin escape analysis\n");
      ArcEscapeSummary escapeSummary{};
      bool escapeChanged =
          runARCEscapeAnalysis(*module, escapeDebug, &escapeSummary);
      arcPassesRan = arcPassesRan || escapeChanged || escapeDebug;
      if (arcDebug && escapeSummary.removedCalls == 0 && !escapeChanged)
        fprintf(stderr, "[arc-opt] escape analysis made no changes\n");
    }

    if (codegenCtx.arcTrace.optimizerEnabled) {
      if (arcDebug)
        fprintf(stderr, "[arc-opt] begin optimization pass\n");
      runARCOptimizationPass(*module);
      arcPassesRan = true;
      if (arcDebug) {
        if (llvm::verifyModule(*module, &llvm::errs()))
          fprintf(stderr,
                  "[arc-opt] module verification failed after optimizer\n");
        else
          fprintf(stderr, "[arc-opt] module verification succeeded\n");
      }
    }

    if (arcPassesRan && codegenCtx.arcTrace.traceEnabled) {
      if (arcDebug)
        fprintf(stderr, "[arc-opt] collecting post-optimization counts\n");
      collectArcRetainReleaseCounts(
          *module, codegenCtx.arcTrace.postOptimizationCounts);
    }
    codegenCtx.arcTrace.optimizerRan =
        codegenCtx.arcTrace.optimizerRan || arcPassesRan;
  };

  if (!outputPath.empty())
    emitLLVM = emitLLVM || outputPath == "-" || endsWith(outputPath, ".ll") || endsWith(outputPath, ".bc");

  // Initialize LLVM
  InitializeModule();
  initializeOperatorPrecedence();

  bool hasSources = !sourceFiles.empty();
  int exitCode = 0;

  if (hasSources) {
    SetInteractiveMode(false);

    bool hadFailure = false;

    for (std::size_t idx = 0; idx < sourceFiles.size(); ++idx) {
      std::string source;
      if (std::getenv("HYBRID_ARC_OPT_DEBUG"))
        fprintf(stderr, "[arc-opt] compiling %s\n", sourceFiles[idx].c_str());
      if (!loadSourceFile(sourceFiles[idx].c_str(), source)) {
        fprintf(stderr, "Error: Failed to open source file '%s'\n", sourceFiles[idx].c_str());
        hadFailure = true;
        break;
      }

      session.beginUnit(idx > 0);
      currentLexer().setInputBuffer(source);

      getNextToken();
      MainLoop();

      if (currentParser().hadError) {
        hadFailure = true;
        break;
      }
    }

    if (!hadFailure) {
      FinalizeTopLevelExecution();
      if (std::getenv("HYBRID_ARC_OPT_DEBUG"))
        fprintf(stderr, "[arc-opt] finalized module\n");
      llvm::Module *module = getModule();
      runArcPasses(module);

      std::string targetOutput;
      if (!outputPath.empty())
        targetOutput = outputPath;
      else if (emitLLVM)
        targetOutput = deriveOutputPath(sourceFiles.back());
      else
        targetOutput = "a.out";

      std::string cachedModuleIR;
      bool cachedModuleValid = false;
      auto captureModuleIR = [&]() {
        if (cachedModuleValid)
          return;
        if (!codegenCtx.genericsDiagnostics.diagnosticsEnabled)
          return;
        llvm::raw_string_ostream buffer(cachedModuleIR);
        module->print(buffer, nullptr);
        buffer.flush();
        cachedModuleValid = true;
        codegenCtx.genericsDiagnostics.moduleIRBytesBeforePrint =
            cachedModuleIR.size();
      };

      auto writeModuleIR = [&](llvm::raw_ostream &os) {
        // LLVM's module printer emits type declarations after functions, but LLVM's
        // textual IR parser requires types to be declared before use. We work around
        // this by capturing the IR, extracting type declarations, and re-ordering them.
        std::string moduleIR;
        llvm::raw_string_ostream buffer(moduleIR);

        if (codegenCtx.genericsDiagnostics.diagnosticsEnabled) {
          captureModuleIR();
          moduleIR = cachedModuleIR;
        } else {
          module->print(buffer, nullptr);
          buffer.flush();
        }

        // Extract and reorder: module metadata, then types, then everything else
        std::istringstream iss(moduleIR);
        std::string line;
        std::vector<std::string> moduleMetadata;
        std::vector<std::string> typeDecls;
        std::vector<std::string> otherLines;
        bool inMetadata = true;

        while (std::getline(iss, line)) {
          // Module metadata lines start with ';' or are special directives
          if (inMetadata && (line.empty() || line[0] == ';' ||
              line.find("source_filename") == 0 ||
              line.find("target datalayout") == 0 ||
              line.find("target triple") == 0)) {
            moduleMetadata.push_back(line);
            continue;
          }
          inMetadata = false;

          // Type declarations match pattern: %TypeName = type { ... }
          size_t firstNonSpace = line.find_first_not_of(" \t");
          if (firstNonSpace != std::string::npos && line[firstNonSpace] == '%') {
            size_t typePos = line.find(" = type ");
            if (typePos != std::string::npos) {
              typeDecls.push_back(line);
              continue;
            }
          }
          otherLines.push_back(line);
        }

        // Emit in correct order: metadata, types, then everything else
        for (const auto &meta : moduleMetadata) {
          os << meta << "\n";
        }
        for (const auto &decl : typeDecls) {
          os << decl << "\n";
        }
        for (const auto &other : otherLines) {
          os << other << "\n";
        }
      };

      auto emitTextFile = [&](const std::string &path) -> bool {
        std::error_code ec;
        llvm::raw_fd_ostream out(path, ec, llvm::sys::fs::OF_Text);
        if (ec) {
          fprintf(stderr, "Error: Failed to write LLVM IR to '%s': %s\n",
                  path.c_str(), ec.message().c_str());
          return false;
        }
        writeModuleIR(out);
        return true;
      };

      auto shouldEmitIR = [&](const std::string &path) {
        return emitLLVM || path == "-" || endsWith(path, ".ll") || endsWith(path, ".bc");
      };

      if (shouldEmitIR(targetOutput)) {
        if (targetOutput == "-") {
          writeModuleIR(llvm::outs());
        } else {
          if (!emitTextFile(targetOutput))
            hadFailure = true;
        }
        if (codegenCtx.genericsDiagnostics.diagnosticsEnabled)
          codegenCtx.genericsDiagnostics.moduleIRBytesAfterPrint =
              codegenCtx.genericsDiagnostics.moduleIRBytesBeforePrint;
      } else {
        llvm::SmallString<128> tempPath;
        int tempFD;
        if (auto ec = llvm::sys::fs::createTemporaryFile("hybrid_ir", ".ll", tempFD, tempPath)) {
          fprintf(stderr, "Error: Failed to create temporary file: %s\n", ec.message().c_str());
          hadFailure = true;
        } else {
          std::string tempPathStr = tempPath.str().str();
          {
            llvm::raw_fd_ostream tempStream(tempFD, true);
            writeModuleIR(tempStream);
          }
          if (codegenCtx.genericsDiagnostics.diagnosticsEnabled)
            codegenCtx.genericsDiagnostics.moduleIRBytesAfterPrint =
                codegenCtx.genericsDiagnostics.moduleIRBytesBeforePrint;

#ifdef HYBRID_RUNTIME_SUPPORT_SOURCE
          const char *linker = "clang++";
          std::string command = std::string(linker) + " \"" + tempPathStr + "\" \"" +
                                std::string(HYBRID_RUNTIME_SUPPORT_SOURCE) + "\"";
#ifdef HYBRID_ARC_RUNTIME_SOURCE
          command += " \"" + std::string(HYBRID_ARC_RUNTIME_SOURCE) + "\"";
#endif
#ifdef HYBRID_REFCOUNT_SOURCE
          command += " \"" + std::string(HYBRID_REFCOUNT_SOURCE) + "\"";
#endif
#ifdef HYBRID_WEAK_TABLE_SOURCE
          command += " \"" + std::string(HYBRID_WEAK_TABLE_SOURCE) + "\"";
#endif
#ifdef HYBRID_SOURCE_DIR
          command += " -I\"" + std::string(HYBRID_SOURCE_DIR) + "\"";
#endif
#ifdef HYBRID_RUNTIME_INCLUDE_DIR
          command += " -I\"" + std::string(HYBRID_RUNTIME_INCLUDE_DIR) + "\"";
#else
#ifdef HYBRID_SOURCE_DIR
          command += " -I\"" + std::string(HYBRID_SOURCE_DIR) + "/../runtime/include\"";
#endif
#endif
          command += " -std=c++17 -o \"" + targetOutput + "\"";
#else
          std::string command = "clang \"" + tempPathStr + "\" -o \"" + targetOutput + "\"";
#endif
          int status = std::system(command.c_str());
          if (status != 0) {
            fprintf(stderr, "Error: clang failed when generating '%s' (exit code %d)\n",
                    targetOutput.c_str(), status);
            hadFailure = true;
          }

          llvm::sys::fs::remove(tempPath);
        }
      }

      if (hadFailure)
        exitCode = 1;
    }

    if (hadFailure)
      exitCode = 1;
  } else {
    SetInteractiveMode(true);

    fprintf(stderr, "ready> ");
    getNextToken();
    MainLoop();

    if (!currentParser().hadError) {
      FinalizeTopLevelExecution();
      runArcPasses(getModule());
      fprintf(stderr, "\n=== Final Generated LLVM IR ===\n");
      getModule()->print(llvm::errs(), nullptr);
    }
  }

  emitGenericsMetricsSummary(session.codegen());
  emitArcTraceSummary(session.codegen());
  popCompilerSession();

  return exitCode;
}
