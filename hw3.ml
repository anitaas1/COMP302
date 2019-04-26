exception NotImplemented;;

let rec memberof (n, l) =
  match l with
  | [] -> false
  | x :: xs -> if x = n then true else memberof (n, xs)
;;

let remove (item, lst) =
  List.filter (fun u -> not (u = item)) lst
  
(* Question 1. *)

let rec common twolists =
  match twolists with 
  | ([], []) -> []
  | (_ , []) -> []
  | ([], _) -> []
  | (x::xs, l) -> if memberof(x, l) then x::common(remove(x, xs), l) else common(xs, l)
  
;;


(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let rec split l = 
  match l with
  | [] -> ([], []) 
  | [x] -> ([x], []) 
  | (x0::x1::xs) -> let (l1, l2) = split xs in ((x0::l1), (x1::l2))
    
;;

(* Question 3 Here you implement merge. *)

let rec merge twolists = 
  match twolists with
  | ([], []) -> [] 
  | ([x], []) -> [x]
  | ([], [y]) -> [y] 
  | (x::xs, []) -> x::xs
  | ([], y::ys) -> y::ys
  | (x::xs, y::ys) -> if x < y then x::(merge(xs, y::ys)) else y::merge(x::xs, ys) 
          
;;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let rec mergesort l =
  match l with 
  | [] -> [] 
  | (x::[]) -> x::[]
  | (x::xs) -> let (l1, l2) = split l in merge(mergesort l1, mergesort l2) 
;;
