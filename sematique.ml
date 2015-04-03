open AST
let rec eval : fonctions -> variables -> expr -> (result*variables) 
= fun genv lenv -> function
| Int n -> (RInt n, lenv)
| Chaine(n) -> (RChaine(n),lenv)
| Var(n:string) -> (List.assoc n lenv, lenv)
| Bool(n:bool) -> (Rbool n, lenv)
   
| Add(n,m) -> (
    let en, lenv = eval genv lenv n in
    let em, lenv = eval genv lenv m in
    match (en, em) with
    | RInt a, RInt b -> (RInt(a+b), lenv)
    | _ -> failwith "Addition de non entier")
    
| Inf(n,m) -> (
	let en = eval genv lenv n in
    let em = eval genv lenv m in
    match en, em with
    | RInt a, RInt b -> if a < b then (Rbool("True"),lenv) else (Rbool("False"),lenv)
    | _ -> failwith "Problème de variable")

| Index(name,expr) -> 
                      let listVal = (try List.assoc name lenv with Not_found -> List.assoc name genv) in
                      let em = eval genv lenv expr in
                      listVal.nth(em),lenv
                      
| Egal(name,expr) -> (
    let em = eval genv lenv expr in
    em, (name,em)::lenv
    )

| List(listExpr) -> (
	let lstRetour = List.map (eval genv lenv) listExpr in
	lstRetour,lenv
	)

| Call(id, args) -> try List.assoc id lenv with Not_found ->
                    let f = List.assoc id genv in
                    let args = List.map (eval genv lenv) args in
                    let lenv = List.combine f.params args in
                    eval genv lenv f.body,lenv
                    
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

