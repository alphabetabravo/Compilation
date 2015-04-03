open AST
let rec eval : fonctions -> variables -> expr -> (result*variables) 
= fun genv lenv -> function
| Int n -> (RInt n, lenv)
| Chaine(n) -> (RChaine(n),lenv)
| Var(n:string) -> (List.assoc n lenv, lenv)
| Bool(n:bool) -> (RBool n, lenv)
   
| Add(n,m) -> (
    let en, lenv = eval genv lenv n in
    let em, lenv = eval genv lenv m in
    match (en, em) with
    | RInt a, RInt b -> (RInt(a+b), lenv)
    | _ -> failwith "Addition de non entier")
    
| Inf(n,m) -> (
	let en, lenv = eval genv lenv n in
    let em, lenv = eval genv lenv m in
    match en, em with
    | RInt a, RInt b -> if a < b then (RBool(true),lenv) else (RBool(false),lenv)
    | _ -> failwith "Problème de variable")

| Index(name,expr) -> (
                      let listVal = List.assoc name lenv in
(*                      let valCpt, lenv = eval genv lenv listVal in*)
                      match listVal with
                      | RList a -> let em, lenv = eval genv lenv expr in
                      				match em with
                      				| RInt b -> List.nth a b,lenv
                      				|_ -> failwith "Le numero d'index n'est pas un entier"
                      |_ -> failwith "Parcour d'une variable NON-Liste")
(*                      let listVal = (try List.assoc name lenv with Not_found -> List.assoc name genv) in*)
(*                      let em, lenv = eval genv lenv expr in*)
(*                      List.nth listVal em,lenv*)
                      
| Egal(name,expr) -> (
    let em, lenv = eval genv lenv expr in
    em, (name,em)::lenv
    )

| List(listExpr) -> (
	let lstRetour, lenv = List.fold_left (fun (l, lenv) e -> let x,lenv = eval genv lenv e in ((x::l), lenv))
								([], lenv) listExpr in
	RList(List.rev lstRetour),lenv
	)
	
	(* List.fold_left (+) 0 l -> somme des éléments de l *)

| Call(id, args) -> let f = List.assoc id genv in
(*					let f = (try List.assoc id lenv with Not_found -> List.assoc id genv) in*)
(*                    let args = List.map (eval genv lenv) args in*)
					let args = List.fold_left (fun (l, lenv) e -> let x,lenv = eval genv lenv e in ((x::l), lenv))
								([], lenv) args in
                    let newLenv = List.combine f.params (List.rev args) in
                    eval genv newLenv f.body,lenv
                    
let rec run : globals -> program -> unit
 = fun genv -> function
   [] -> ()
 | instr::instrs ->
   if instr.params = [] then (
     let n = eval genv [] instr.body in
     Printf.printf "%s=%d\n" instr.name n
   );
   let genv = (instr.name, instr)::genv in
   run genv instrs

