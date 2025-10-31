#ifndef TOPLEVEL_H
#define TOPLEVEL_H

// Top-level parsing handlers
void HandleDefinition();
void HandleExtern();
void HandleTopLevelExpression();
void HandleVariableDeclaration();
void HandleStructDefinition();
void HandleClassDefinition();
void HandleUnsafe();
void HandleSwitchStatement();
void HandleAssertStatement();
void MainLoop();
void SetInteractiveMode(bool enabled);

#endif // TOPLEVEL_H
