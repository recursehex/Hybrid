#include "parser.h"
#include "toplevel.h"
#include "ast.h"
#include "compiler_session.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>
#include <system_error>
#include <vector>

#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Module.h"
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
}

}

int main(int argc, char **argv) {
  CompilerSession session;
  session.resetAll();
  pushCompilerSession(session);

  bool emitLLVM = false;
  std::string outputPath;
  std::vector<std::string> sourceFiles;

  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "--emit-llvm") {
      emitLLVM = true;
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
      llvm::Module *module = getModule();

      std::string targetOutput;
      if (!outputPath.empty())
        targetOutput = outputPath;
      else if (emitLLVM)
        targetOutput = deriveOutputPath(sourceFiles.back());
      else
        targetOutput = "a.out";

      auto writeModuleIR = [&](llvm::raw_ostream &os) {
        module->print(os, nullptr);
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

          std::string command = "clang \"" + tempPathStr + "\" -o \"" + targetOutput + "\"";
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

    fprintf(stderr, "\n=== Final Generated LLVM IR ===\n");
    getModule()->print(llvm::errs(), nullptr);
  }

  popCompilerSession();

  return exitCode;
}
