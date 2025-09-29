#ifndef CONCEPTS_H
#define CONCEPTS_H

#include <concepts>
#include <type_traits>
#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"

// Forward declarations
class ExprAST;
class StmtAST;

namespace HybridConcepts
{

// Concept for LLVM Value types
template<typename T>
concept LLVMValueType = std::is_base_of_v<llvm::Value, T> ||
                        std::is_same_v<T, llvm::Value*> ||
                        std::is_same_v<T, llvm::Value>;

// Concept for LLVM Type types
template<typename T>
concept LLVMTypeType = std::is_base_of_v<llvm::Type, T> ||
                       std::is_same_v<T, llvm::Type*> ||
                       std::is_same_v<T, llvm::Type>;

// Concept for AST Expression nodes
template<typename T>
concept ASTExpression = std::is_base_of_v<ExprAST, T>;

// Concept for AST Statement nodes
template<typename T>
concept ASTStatement = std::is_base_of_v<StmtAST, T>;

// Concept for any AST node
template<typename T>
concept ASTNode = ASTExpression<T> || ASTStatement<T>;

// Concept for types that can be converted to LLVM values
template<typename T>
concept CodegenCapable = requires(T t) {
    { t.codegen() } -> std::convertible_to<llvm::Value*>;
};

// Concept for types that have a type name
template<typename T>
concept TypeNamed = requires(const T t) {
    { t.getTypeName() } -> std::convertible_to<std::string>;
};

} // namespace HybridConcepts

#endif // CONCEPTS_H