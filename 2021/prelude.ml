
let string_to_list s = s |> String.to_seq |> List.of_seq
let list_to_string s = s |> List.to_seq |> String.of_seq

let is_lower s = String.lowercase_ascii s = s

let rec min_by f list =
  match list with
  | [] -> failwith "we don't do empy lists"
  | [x] -> x
  | x :: xs -> 
    let min = min_by f xs in 
    if f x min then x else min

let rec remove x = function
  | [] -> []
  | elem :: elems -> if elem = x then elems else elem :: remove x elems 

let rec remove_duplicates = function 
  | [] -> []
  | x :: xs -> x :: remove_duplicates (remove x xs)

let sum xs = List.fold_left (+) 0 xs

let snoc xs x = xs @ [x]

let rec range start stop = if start > stop then [] else start :: range (succ start) stop 

let rec take n list =
  match n, list with 
  | 0, _ | _, [] -> []
  | n, x :: xs -> x :: take (n-1) xs 

let singleton x = [x]

let contains x = List.exists ((=) x)

let intersect ys = List.fold_left (fun result x -> if contains x ys then x :: result else result) [] 

let (//) xs ys = List.fold_right remove ys xs

let (>>) f g a = a |> f |> g

let input_lines file = 
  let rec loop chan = 
    match input_line chan with 
    | line -> line :: loop chan 
    | exception End_of_file -> []
  in
  let chan = open_in file in 
  let result = loop chan in 
  close_in chan;
  result

let input_string = input_lines >> String.concat "\n"
