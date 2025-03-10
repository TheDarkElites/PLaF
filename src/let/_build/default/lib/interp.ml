(*George K Oliynyk*)
(*I pledge my honor that I have abided by the Stevens Honor System*)

open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | EmptyTree(_t) ->
    return (TreeVal Empty)
  | Node(e1, e2, e3) ->
    eval_expr e1 >>= fun data ->
    eval_expr e2 >>=
    tree_of_treeVal >>= fun left ->
    eval_expr e3 >>=
    tree_of_treeVal >>= fun right ->
    return (TreeVal (Node(data, left, right)))
  | IsEmpty(e) ->
    eval_expr e >>=
    tree_of_treeVal >>= fun tree ->
    return (BoolVal (tree = Empty))
  | CaseT(e1,e2,id1,id2,id3,e3) ->
    eval_expr e1 >>=
    tree_of_treeVal >>= fun tree ->
    eval_expr e2 >>= fun exp2 ->
    (match tree with
    | Empty -> return exp2
    | Node(data,left,right) ->
    extend_env id1 data >>+
    extend_env id2 (TreeVal (left)) >>+
    extend_env id3 (TreeVal (right)) >>+
    eval_expr e3 >>= fun exp3 ->
    return exp3)
  | Record(fs) ->
    let es = (List.map (fun x->
    match x with
    | (_,(_,exprVal)) -> exprVal
    )
    fs) in
    eval_exprs es >>= fun evals ->
    List.fold_right2 (fun x y acc->
    acc >>= record_of_recordVal >>= fun recAcc ->
    match x with
    | (str,(boolVal,_)) when List.length (List.filter (
    fun curRec ->
        match curRec with
        | (strCur, (_,_)) when strCur=str -> true
        | _ -> false
    ) recAcc)=0 ->
    return (RecordVal ((str, (boolVal,y)) :: recAcc))
    | _ -> error "Record : duplicate fields"
    ) fs evals (return (RecordVal []))
  | Proj(e,id) ->
    eval_expr e >>=
    record_of_recordVal >>= fun ourRecord ->
    let found = (List.find_map (fun x ->
        match x with
        | (str,(_,value)) when str=id -> Some value
        | _ -> None
        ) ourRecord) in
    (match found with
    | Some value -> return value
    | None -> error "Proj : field does not exist")
  | Debug(_e) ->
      string_of_env >>= fun str ->
      print_endline str;
      error "Debug called"
  | _ -> failwith "Not implemented yet!"
and
   eval_exprs : expr list -> ( exp_val list ) ea_result =
    fun es ->
    match es with
    | [] -> return []
    | h::t -> eval_expr h >>= fun i ->
    eval_exprs t >>= fun l ->
    return (i::l)

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


