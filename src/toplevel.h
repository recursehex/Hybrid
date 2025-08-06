#ifndef TOPLEVEL_H
#define TOPLEVEL_H

// Top-level parsing handlers
void HandleDefinition();
void HandleExtern();
void HandleTopLevelExpression();
void HandleVariableDeclaration();
void HandleStructDefinition();
void MainLoop();

#endif // TOPLEVEL_H