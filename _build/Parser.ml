open AST

let parser int =		 n:''[0-9]+'' 				-> int_of_string n
let parser ident = 		id:''[a-zA-Z][a-zA-Z0-9]*'' -> id
let parser bool = 		b:''(true)|(false)+'' 		-> bool_of_string b
let parser string = 	'"' - s:''[^"]*'' - '"' 	-> s

let parser expr = 
  n:int -> Int n
| name:ident -> Var(name)
| "(" e:expr ")" -> e
| name:ident  "=" e2:expr -> Egal(name,e2)
| e1:expr  "+" e2:expr -> Add(e1,e2)
| e1:expr "<" e2: expr -> Inf(e1,e2)
|  "[" e1:exprList "]" -> List(e1)
| name: ident "[" e2:expr "]" -> Index(name,e2)
| chaine:string -> Chaine(chaine)
| name:ident
  "(" args:exprList ")" -> Call(name,args)
and exprList =	 EMPTY -> []
				|e:expr es:{"," e':expr}* 	-> e::es
				

let _ = Decap.active_debug := false

let parser instr =
  | e:expr -> InstructionToExpression(e)
  | "def" nameId:ident paramsId:{"(" i:ident is:{ "," i':ident -> i'}* ")" -> i::is}?[[]] "<@" defBody:instrList "@>" -> Def({name = nameId;params = paramsId;body = defBody;})
  | "while" DEBUG"after while" e:expr "<@" inbcl:instrList "@>" -> While(e,inbcl)
  | "for" id:ident "in" e:instr "<@" inbcl:instrList "@>" -> For(id,e,inbcl)
  | "if" id:{id:ident "in"}? e:instr "<@"inIf:instrList "@>" inElse:{"else" "<@" instrList "@>"}? ->If(id,e,inIf,inElse)
and instrList =	 es:{e':instr}* -> es

let parser program = instr*

let blank = Decap.blank_regexp "[ \t\n\r]*"
let _ = Decap.handle_exception (fun () ->
       let p = Decap.parse_channel program blank stdin in
       Printf.printf("Ok Parser!");
       Expr_sem.run [] p
       ) ()
