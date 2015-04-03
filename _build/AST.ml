type ident = string
type expr =
			| Int of int
			| Bool of bool
			| Add of expr * expr
			| Inf of expr * expr
			| List of expr list
			| Index of ident * expr
			| Chaine of string
			| Var of ident
			| Call of ident * expr list
			| Egal of ident * expr

type def = { name : ident; params : ident list; body : program }

and instr = 
			| InstructionToExpression of expr 
			| Def of def
(*			| Def of ident * ident list * program*)
			| While of expr * instr list
			| For of ident * instr * instr list
			| If of ident option * instr * instr list * instr list option

and program = instr list 

type result = 
 | RInt of int
 | RBool of bool
 | RChaine of string
 | RList of result list
 
 type fonctions = (ident * def) list
 type variables  = (ident * result) list
