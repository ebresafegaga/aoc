open Prelude

(* solution = last . unfold done (concatMap g . group) 
    where g e 
      | length e > 2 = []
      | otherwise = e 
    done = find (> 2) . map length . group 
    
   SSDDFGHJJKL;
*)

let to_list s = s |> String.to_seq |> List.of_seq
let to_string s = s |> List.to_seq |> String.of_seq

let rec group list = 
  match list with 
  | [] -> [[]]
  | x :: xs -> 
    let almost = group xs in 
    let h, t = List.hd almost, List.tl almost in 
    if List.length h < 1 then [x] :: t 
    else if List.hd h = x then (x :: h) :: t
    else [x] :: h :: t

let predicate l = List.length l > 2
let should_remove ll = List.exists predicate ll

let rec candy l = 
  let result = group l in  
  if should_remove result then 
     result |> List.filter (predicate >> not) |> List.concat |> candy 
  else l

let candy str =
  let str = to_list str in
  to_string (candy str)
  


(* graph algorithms *)

let graph = 
  [ "a", "b"; 
    "a", "c"; 
    "a", "d";
    "b", "e";
    "c", "f"; 
    "d", "e";
    "e", "f";
    "e", "g" ]

let nexts node graph = graph |> List.filter_map (fun (n, elem) -> if n = node then Some elem else None)

let rec dfs stack graph visited = 
  match stack with 
  | [] -> List.rev visited 
  | x :: stack  -> 
    if List.mem x visited then 
      dfs stack graph visited 
    else 
      dfs (nexts x graph @ stack) graph (x :: visited)

let small_graph = 
  [ "a", "b";
     "a", "c"; 
     "c", "d"; 
     "b", "d"]

let rec dfs_path start graph visited = 
    List.fold_left 
      (fun result elem -> result @ List.map (List.cons start) (dfs_path elem graph visited))
      [[]] 
      (nexts start graph)


let parse_line = 
  let digit = Str.regexp "[0-9]" in 
  let rest str = String.sub str 1 (String.length str - 1) in
  let rec help str = 
    if Str.string_match digit str 0 then
      let n = str |> Str.matched_string |> int_of_string in 
      n :: help (rest str)
    else [] 
  in 
  help


type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let rec bfs = function 
  | [] -> []
  | Empty :: rest -> bfs rest 
  | Node (elem, left, right) :: rest -> elem :: bfs (rest @ [left; right])

let rec levels = function 
  | [] -> []
  | Empty :: rest -> levels rest 
  | Node (elem, left, right) :: rest -> elem :: levels (rest @ [left; right]) 

let tree = 
 Node (1, Node (5, Node (3, Empty, Empty), 
                   Empty), 
          Node (7, Empty, 
                   Empty))


let arr a () = 
  for i = 0 to 10 do 
    print_int a.(i)  
  done 

let rec stock_prices ls =
  match ls with 
  | [] -> [[]] 
  | l :: ls -> 
    let almost = stock_prices ls in 
    almost @ List.map (List.cons l) almost 


(* 0,0 -> 2,0 
   0,1 -> 1,1
   0,2 -> 2,1 
   
   2,1 -> 1,2 
   
(a b  (0 
 c d)  1)
 
(-1 2   (0 
  1 0)   2)
 
b = 2 
d = 0 
 
a + b = 1 
c + d = 1 

1 2 3 6 9 8 7 4

0   1     1

1   0     1


1

1

*)

let to_char s = s.[0]

let parse = input_lines >> List.map (Str.split (Str.regexp "") >> List.map to_char)



let minMax = 
  let step (min, max) x =
    if x < min then (x, max) 
    else if x > max then (min, x)
    else (min, max)
  in 
  List.fold_left step (Int.max_int, Int.min_int)

let rec transpose matrix = 
  match matrix with
  | [] -> failwith "no"
  | [xs] -> xs |> List.map (fun x -> [x])
  | xs :: xss -> List.map2 List.cons xs (transpose xss)

let rotate = transpose >> List.map List.rev

let m = 
  [ [1;2;3];
    [4;5;6];
    [7;8;9] ]

let m2 = 
  [ [5;1;9;11]; 
    [2;4;8;10]; 
    [13;3;6;7]; 
    [15;14;12;16] ]

let rec common xs ys = 
  match xs, ys with 
  | x :: xs, y :: ys when x = y -> x :: common xs ys 
  | _ -> []

let common_prefix words = 
  let first, words = List.hd words, List.tl words in
  List.fold_left common first words

let common_prefix = List.map string_to_list >> common_prefix >> list_to_string

let nx graph _visited node =
  let visited_small _key = false in
  let f (key, value) = if node = key && not (visited_small value) then Some value else None in 
  graph |> List.filter_map f

(* let nondet_next graph visited node = 
  if visited_double visited then singleton (next1 graph visited node) 
  else 
    let nxt = nx graph visited node in
    let almost = intersect visited nxt in 
    let rest = nxt // almost in 
    if almost = [] then singleton nxt 
    else List.map (fun a -> a :: rest) almost  *)

(* let rec all graph v next node = 
  let adjss = next graph v node  in
  let v = node :: v in 
  if last node then v, [[node]] else
  (* List.concat_map (all_path graph v >> List.map (List.cons node)) adjs *)
  let f (v, paths) adj = 
    let (_this_v, this_path) = all graph v next adj in
    v, (List.map (List.cons node) this_path) @ paths
  in
  v, List.concat_map (fun adjs -> List.fold_left f (v, []) adjs |> snd) adjss  *)