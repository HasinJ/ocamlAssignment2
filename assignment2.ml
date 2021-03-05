open List


let rec cond_dup l f =
match l with
  | [] -> []
  | (h::t) -> if (f h) then (h::h::(cond_dup t f)) else (cond_dup t f) ;;


let rec n_times (f, n, v) =
  if n=0 then v else n_times(f,n-1,(f v))


let rec zipwith f l1 l2 =
  match l1 with
  | [] -> []
  | (h::t) -> (match l2 with
    | [] -> []
    | (h2::t2) -> (f h h2)::(zipwith f t t2)) ;;


(*
let rec helper p curr lst =
  match lst with
  | [] -> []
  | (h2::t2) -> if (p curr h2) then h2::(helper p curr t2) else helper p curr t2 in

let rec helper2 original first f tail =
  match l with
  | [] -> [] @ original
  | (h2::t2) -> if (f first h2) then h2::(helper2 (remove original h2) first f t2) else helper2 original first f t2 in

let rec helper p curr lst =
  match lst with
  | [] -> []
  | (h2::t2) -> if (p curr h2) then h2::(helper p curr t2) else helper p curr t2 in

let rec helper2 p l =
  match l with
  | [] -> []
  | h::t -> (h::(helper p h t)::[]) @ (helper2 )

 - check for empty, if it is then skip
 - try passing a different thing other than [[]]
 - try rverse technique tail rec
*)

let buckets p l =

  let rec helper p curr lst =
    match lst with
    | [] -> []
    | (h2::t2) -> if (p curr h2) then h2::(helper p curr t2) else helper p curr t2 in

  let rec remove x a =
    match a with
    | [] -> []
    | h::t -> if h = x then t else h::(remove x t) in

  let rec removeAll lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | h::t -> removeAll t (remove h lst2) in

  let rec main p l total =
    match l with
    | [] -> total
    | (h::t2) -> main p (removeAll (h::(helper p h t2)) t2) ((h::(helper p h t2))::total) in

  let rec reverse l a =
    match l with
    | [] -> a
    | (h::t) -> reverse t (h::a) in

  reverse (main p l []) [] ;;


let fib_tailrec n =
  let rec helper curr prev z n =
    if z=n then curr else helper (curr+prev) curr (z+1) n in
  helper 0 1 0 n ;;


let assoc_list lst =

  let counter num lest =
    List.fold_left (fun a x -> if num=x then a+1 else a) 0 lest in

  let duplicates lest =
    List.fold_left (fun a h -> if ((counter h lest)>0) then (if ((counter h a)=0) then h::a else a) else h::a) [] lest in

  let count lest =
    let set = duplicates lest in
    List.map (fun h -> (h,counter h lest)) set in

  count lst;;


let ap fs args =
  let lst = List.map (fun x -> List.map x args) fs in
  List.fold_left (fun a h -> h@a) [] lst ;;

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for cond_dup *)
  let _ =
    try
      assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times((fun x-> x+1), 50, 0) = 50)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for zipwith *)
  let _ =
    try
      assert ([5;7] = (zipwith (+) [1;2;3] [4;5]))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 50 = 12586269025)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 7 programming questions are incorrect.\n") (!error_count)

let _ = main()
