File -> Item* EOF

Item -> FnItem

FnItem -> "fn" IDENTIFIER(FnArg*) ReturnType? Block

ReturnType -> "->" Type

FnArg -> IDENTIFIER ":" Type

Block -> "{" Stmt* Expr? "}" 

Type -> IDENTIFIER