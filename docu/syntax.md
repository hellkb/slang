```
block          -> (let statement| assignment | BlockExpression)* expression?          
letstatement      -> let expression ";"
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
                
               | "(" expression ")" ;
STRING         ->                

fncall         -> lit (  parm,*  )( param,* )
array          -> expression[...]


case Ident =>  
a(expr,..)(expr,...)[expr][expr]()

```