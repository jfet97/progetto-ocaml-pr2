(* espressioni *)
type ide = string;;
type tide = string;; (* "int" | "bool" | "string" *)

type exp =
  | CstInt of int
  | CstTrue
  | CstFalse
  | Eq of exp * exp
  | Sum of exp * exp
  | Sub of exp * exp
  | Times of exp * exp
  | Ifthenelse of exp * exp * exp
  | Den of ide
  | Let of ide * exp * exp
  | LetRec of ide * ide * exp * exp
  | Fun of ide * exp
  | Apply of exp * exp
  (* sets *)
  | Empty of tide
  | Singleton of exp * tide
  | Insert of exp * exp
  | Remove of exp * exp
  | IsEmpty of exp
  | Contains of exp * exp
  | IsSubSet of exp * exp
  | MinEl of exp
  | MaxEl of exp
  | Union of exp * exp
  | Intersection of exp * exp
  | Difference of exp * exp
  | For_all of exp * exp
  | Exists of exp * exp
  | Filter of exp * exp
  | Map of exp * exp;;


(* environment e tipi esprimibili *)
type env = ide -> evT
    and evT =
    | Int of int
    | Bool of bool
    | String of string
    | Closure of ide * exp * env
    | RecClosure of ide * ide * exp * env
    | Set of string * evT list
    | Unbound;;

let emptyEnv = fun _ -> Unbound;;

(* bind ritorna una funzione di lookup *)
let bind (a: env) (li:ide) (lv:evT) = (fun i ->
  if li = i then lv
  else a i
);;

(* typechecker dinamico *)
let typecheck (x, y) = match x with
  | "int" ->
        (match y with
          | Int (i) -> true
          | _ -> false)
  | "bool" ->
        (match y with
          | Bool (b) -> true
          | _ -> false)
  | "string" ->
        (match y with
          | String (s) -> true
          | _ -> false)
  | "function" ->
          (match y with
            | Closure (_,_,_) -> true
            | RecClosure (_,_,_,_) -> true
            | _ -> false)
  | "set" ->
            (match y with
              | Set (_,_) -> true
              | _ -> false)
  | _ -> failwith ("not a valid type");;

(* operazioni primitive eseguibili in fase di evaluation di una espressione *)
let int_eq (x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
      | (true, true, Int(i), Int(j)) -> Bool(i = j)
      | (_, _, _, _) -> failwith("run-time error");;

let int_plus (x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
      | (true, true, Int(i), Int(j)) -> Int(i + j)
      | (_, _, _, _) -> failwith("run-time error");;

let int_times (x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
      | (true, true, Int(i), Int(j)) -> Int(i * j)
      | (_, _, _, _) -> failwith("run-time error");;

let int_sub (x,y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
      | (true, true, Int(i), Int(j)) -> Int(i - j)
      | (_, _, _, _) -> failwith("run-time error");;

let set_eq v1 v2 =
  match v1 with
    | Int (i1) ->
        (match v2 with
          | Int (i2) -> i1 = i2
          | _ -> false)
    | Bool (b1) ->
        (match v2 with
          | Bool (b2) -> b1 = b2
          | _ -> false)
    | String (s1) ->
        (match v2 with
          | String (s2) -> s1 = s2
          | _ -> false)
    | _ -> false;;

let rec contains s v =
  match (typecheck("set", s), s) with
      | (true, Set(st, (seth::sett))) ->
          (match (typecheck(st, v), v) with
          | (true, _) -> if (set_eq seth v) then true else contains (Set(st, sett)) v
          | (_, _) -> failwith("contains set type error"))
      | (true, Set(st, [])) ->
          (match (typecheck(st, v), v) with
          | (true, _) -> false
          | (_, _) -> failwith("contains set type error"))
      | (_, _) -> failwith("contains set type error");;

let rec containsAll s1 s2 =
  match (typecheck("set", s1), typecheck("set", s2), s1, s2) with
      | (true, true, Set(st1, (seth1::sett1)), Set(st2, _)) ->
          (if st1 = st2 then (contains s2 seth1) && (containsAll (Set(st1, sett1)) s2) else failwith("containsAll set type error"))
      | (true, true, Set(st1, []), Set(st2, _)) ->
          (if st1 = st2 then true else failwith("containsAll set type error"))
      | (_, _, _, _) -> failwith("containsAll set type error");;

let insert s v =
  match (typecheck("set", s), s) with
      | (true, Set(st, set)) ->
          (match (typecheck(st, v), v) with
          | (true, _) -> if (contains s v) then s else Set(st, v::set)
          | (_, _) -> failwith("insert set type error"))
      | (_, _) -> failwith("insert set type error");;

let rec remove s v =
  match (typecheck("set", s), s) with
      | (true, Set(st, (seth::sett))) ->
          (match (typecheck(st, v), v) with
          | (true, _) -> if (set_eq seth v) then Set(st, sett) else insert (remove (Set(st, sett)) v) seth
          | (_, _) -> failwith("remove set type error"))
      | (true, Set(st, [])) ->
          (match (typecheck(st, v), v) with
          | (true, _) -> s
          | (_, _) -> failwith("remove set type error"))
      | (_, _) -> failwith("remove set type error");;

let is_empty s =
  match (typecheck("set", s), s) with
      | (true, Set(st, set)) -> List.length set = 0
      | (_, _) -> failwith("is_empty set type error");;

let min s =
  match s with
      | [] -> failwith("min set type error")
      | (sh::st) ->
        let rec min_i set m = (match set with
          | [] -> m
          | (seth::sett) -> min_i sett (if seth < m then seth else m)
        ) in min_i st sh;;

let max s =
  match s with
      | [] -> failwith("max set type error")
      | (sh::st) ->
        let rec max_i set m = (match set with
          | [] -> m
          | (seth::sett) -> max_i sett (if seth > m then seth else m)
        ) in max_i st sh;;


let remove_duplicates ls = List.fold_right (fun v l -> if List.mem v l then l else v::l) ls [];;

let union l1 l2 = remove_duplicates (List.append l1 l2);;

let intersection l1 l2 = List.fold_right (fun v l -> if List.mem v l2 then v::l else l) l1 [];;

(* l1 \ l2 *)
let difference l1 l2 = List.fold_right (fun v l -> if List.mem v l2 then l else v::l) l1 [];;

let rec for_all f s = match s with
  | [] -> true
  | (sh::st) -> (let cr = (match f with
    | Closure(fp, b, cev) ->
      let aev = bind cev fp sh in
      eval b aev
    | RecClosure(rn, rfp, rb, rcev) ->
      let raev = bind (bind rcev rfp sh) rn f in
      eval rb raev
    | _ -> failwith("for_all set type error"))
        in (
          let br = (match cr with
          | Bool(b) -> b
          | _ -> failwith("for_all set type error")) in br && (for_all f st)))
and exists f s = match s with
| [] -> false
| (sh::st) -> (let cr = (match f with
  | Closure(fp, b, cev) ->
    let aev = bind cev fp sh in
    eval b aev
  | RecClosure(rn, rfp, rb, rcev) ->
    let raev = bind (bind rcev rfp sh) rn f in
    eval rb raev
  | _ -> failwith("exists set type error"))
      in (
        let br = (match cr with
        | Bool(b) -> b
        | _ -> failwith("exists set type error")) in br || (exists f st)))
and filter f s = match s with
| [] -> []
| (sh::st) -> (let cr = (match f with
  | Closure(fp, b, cev) ->
    let aev = bind cev fp sh in
    eval b aev
  | RecClosure(rn, rfp, rb, rcev) ->
    let raev = bind (bind rcev rfp sh) rn f in
    eval rb raev
  | _ -> failwith("filter set type error"))
      in (
        let br = (match cr with
        | Bool(b) -> b
        | _ -> failwith("filter set type error")) in if br then sh::(filter f st) else (filter f st)))
and map f s = match s with
| [] -> []
| (sh::st) -> (let cr = (match f with
  | Closure(fp, b, cev) ->
    let aev = bind cev fp sh in
    eval b aev
  | RecClosure(rn, rfp, rb, rcev) ->
    let raev = bind (bind rcev rfp sh) rn f in
    eval rb raev
  | _ -> failwith("map set type error"))
      in (
        let _ = (match cr with
        | Bool(b) -> Bool(b)
        | Int(i) -> Int(i)
        | String(s) -> String(s)
        | _ -> failwith("map set type error")) in cr::(map f st)))
(* interprete *)
and eval ex ev =
  match ex with
  | CstInt i -> Int(i)
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | Eq(e1, e2) -> int_eq((eval e1 ev), (eval e2 ev))
  | Times(e1, e2) -> int_times((eval e1 ev), (eval e2 ev))
  | Sum(e1, e2) -> int_plus((eval e1 ev), (eval e2 ev))
  | Sub(e1, e2) -> int_sub((eval e1 ev), (eval e2 ev))
  | Ifthenelse(cond, ife, elsee) ->
      let c = eval cond ev in
        (match (typecheck("bool", c), c) with
          | (true, Bool(true)) -> eval ife ev
          | (true, Bool(false)) -> eval elsee ev
          | (_, _) -> failwith("non boolean guard"))
  | Den(i) -> ev i
  | Let (i, e, b) -> eval b (bind ev i (eval e ev))
  | LetRec (i, fp, b, e) -> eval e (bind ev i (RecClosure(i, fp, b, ev)))
  | Fun(fp, b) -> Closure(fp, b, ev)
  | Apply(ef, ape) ->
      let c = eval ef ev in
        (match (typecheck("function", c), c) with
          | (true, Closure(fp, b, cev)) ->
            let ap = eval ape ev in
            let aev = bind cev fp ap in
            eval b aev
          | (true, RecClosure(rn, rfp, rb, rcev)) ->
            let ap = eval ape ev in
            let raev = bind (bind rcev rfp ap) rn c in
            eval rb raev
          | _ -> failwith("Application: not a functional value")
        )
  | Empty(t) -> (match t with
    | "int" -> Set ("int", [])
    | "bool" -> Set ("bool", [])
    | "string" -> Set ("string", [])
    | _ -> failwith("Empty set type error"))
  | Singleton(e, t) -> (match t with
    | "int" -> insert (Set ("int", [])) (eval e ev)
    | "bool" -> insert (Set ("bool", [])) (eval e ev)
    | "string" -> insert (Set ("string", [])) (eval e ev)
    | _ -> failwith("Singleton set type error"))
  | Insert(se, e) -> insert (eval se ev) (eval e ev)
  | Remove(se, e) -> remove (eval se ev) (eval e ev)
  | IsEmpty(se) -> Bool (is_empty (eval se ev))
  | Contains(se, e) -> Bool (contains (eval se ev) (eval e ev))
  | IsSubSet(se1, se2) -> Bool (containsAll (eval se1 ev) (eval se2 ev))
  | MinEl(se) ->
    let s = eval se ev in
      (match (typecheck("set", s), s) with
        | (true, Set(_, (_::_ as set))) -> min set
        | (_, _) -> failwith("MinEl set type error"))
  | MaxEl(se) ->
    let s = eval se ev in
      (match (typecheck("set", s), s) with
        | (true, Set(_, (_::_ as set))) -> max set
        | (_, _) -> failwith("MaxEl set type error"))
  | Union(se1, se2) ->
    let s1 = eval se1 ev in
    let s2 = eval se2 ev in
      (match (typecheck("set", s1), typecheck("set", s2), s1, s2) with
        | (true, true, Set(st1, set1), Set(st2, set2)) ->
          if st1 = st2 then Set(st1, (union set1 set2)) else failwith("Union set type error")
        | (_,_,_,_) -> failwith("Union set type error"))
  | Intersection(se1, se2) ->
    let s1 = eval se1 ev in
    let s2 = eval se2 ev in
      (match (typecheck("set", s1), typecheck("set", s2), s1, s2) with
        | (true, true, Set(st1, set1), Set(st2, set2)) ->
          if st1 = st2 then Set(st1, (intersection set1 set2)) else failwith("Intersection set type error")
        | (_,_,_,_) -> failwith("Intersection set type error"))
  | Difference(se1, se2) ->
    let s1 = eval se1 ev in
    let s2 = eval se2 ev in
      (match (typecheck("set", s1), typecheck("set", s2), s1, s2) with
        | (true, true, Set(st1, set1), Set(st2, set2)) ->
          if st1 = st2 then Set(st1, (difference set1 set2)) else failwith("Difference set type error")
        | (_,_,_,_) -> failwith("Difference set type error"))
  | For_all(fe, se) ->
    let f = eval fe ev in
    let s = eval se ev in
    (match (typecheck("function", f), typecheck("set", s), f, s) with
      | (true, true, _, Set(st, set)) -> Bool(for_all f set)
      | (_,_,_,_) -> failwith("For_all set type error"))
  | Exists(fe, se) ->
    let f = eval fe ev in
    let s = eval se ev in
      (match (typecheck("function", f), typecheck("set", s), f, s) with
        | (true, true, _, Set(st, set)) -> Bool(exists f set)
        | (_,_,_,_) -> failwith("Exists set type error"))
  | Filter(fe, se) ->
    let f = eval fe ev in
    let s = eval se ev in
      (match (typecheck("function", f), typecheck("set", s), f, s) with
        | (true, true, _, Set(st, set)) -> Set(st, (filter f set))
        | (_,_,_,_) -> failwith("Filter set type error"))
  | Map(fe, se) ->
    let f = eval fe ev in
    let s = eval se ev in
      (match (typecheck("function", f), typecheck("set", s), f, s) with
        | (true, true, _, Set(st, set)) -> Set(st, (map f set))
        | (_,_,_,_) -> failwith("Map set type error"));;