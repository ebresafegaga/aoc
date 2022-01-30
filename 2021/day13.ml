open Prelude
(* solution = count . filter (= #). fold *)

let rec combineWith f l1 l2  =
  match l1, l2 with
  | [], [] -> []
  | [], _xs -> [] 
  | xs, [] -> xs
  | a1::l1, a2::l2 -> f a1 a2 :: combineWith f l1 l2 

let rec take n list =
  match n, list with 
  | 0, _x :: xs -> [], xs
  | _, [] -> [], []
  | n, x :: xs -> let (l,r) = take (n-1) xs in (x :: l, r) 

let splitY = take
let splitX x = List.map (take x) >> List.split

let merge a b = 
  match a, b with 
  | '#', _ | _, '#' -> '#'
  | _ -> '.'

let merge a b =  combineWith (combineWith merge) a b

let foldY y (matrix : char list list) = 
  let top, bottom = splitY y matrix in 
  let top' = List.rev top in
  let almost = merge top' bottom in 
  List.rev almost

let foldX x matrix = 
  let left, right = splitX x matrix in 
  let left' = List.map List.rev left in 
  let almost = merge left' right in 
  List.map List.rev almost

let fold (ty, am) = 
  match ty with 
  | "x" -> foldX am
  | "y" -> foldY am
  | _ -> failwith "can only fold x or y"


let count = List.map (List.filter ((=) '#') >> List.length) >> List.fold_left (+) 0 
let part1 fold = fold >> count 

let foldAll matrix = List.fold_left (fun matrix f -> fold f matrix) matrix 

let tuple = function [x;y] -> (x, y) | _ -> assert false

let parse str = 
  let result = str |> Str.split (Str.regexp "\n\n") in 
  let ps, fs = List.hd result, List.hd (List.tl result) in 
  let ps = 
    ps 
    |> String.split_on_char '\n' 
    |> List.map (String.split_on_char ',' >> List.map int_of_string >> tuple) in
  let parse_fold = 
    let transform (axis, pos) = List.nth (String.split_on_char ' ' axis) 2, int_of_string pos in 
    String.split_on_char '=' >> tuple >> transform 
  in
  let fs = fs |> String.split_on_char '\n' |> List.map parse_fold in 
  ps, fs

let build_grid (xmax, ymax) =
  let xmax, ymax = xmax+1, ymax+1 in
  Array.init ymax (fun _xindex -> Array.init xmax (fun _yindex -> '.'))

let mark_grid points grid = 
  let update (x, y) = grid.(y).(x) <- '#' in
  points |> List.iter update; 
  grid

let grid_dimension lines =
  let max = List.fold_left max Int.min_int in
  let xs, ys = List.split lines in 
  max xs, max ys

let to_list = Array.map Array.to_list >> Array.to_list  

let () = 
  let file = "./input/day13.txt" in
  let points, folds = file |> input_string |> parse in
  let first_fold = folds |> List.hd |> fold in 
  let grid = points |> grid_dimension  |> build_grid |> mark_grid points |> to_list in 
  Printf.printf "part one: %d \n" (part1 first_fold grid)

let result = 
  [['#'; '#'; '#'; '#'; '.'; '#'; '#'; '#'; '.'; '.'; '#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '#'; '#'; '.'; '.'; '#'; '#'; '#'; '.'; '.';'#'; '#'; '#'; '#'; '.'; '#'; '.'; '.'; '#'; '.'];
   ['#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '.'; '#'; '.';'#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'];
   ['#'; '#'; '#'; '.'; '.'; '#'; '#'; '#'; '.'; '.'; '#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '#'; '#'; '.'; '.'; '#'; '.'; '.'; '#'; '.';'#'; '#'; '#'; '.'; '.'; '#'; '#'; '#'; '#'; '.'];
   ['#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '#'; '#'; '.'; '.';'#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'];
   ['#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '#'; '.'; '.';'#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.'];
   ['#'; '#'; '#'; '#'; '.'; '#'; '#'; '#'; '.'; '.'; '#'; '#'; '#'; '#'; '.'; '.'; '#'; '#'; '.'; '.'; '#'; '#'; '#'; '.'; '.'; '#'; '.'; '.'; '#'; '.';'#'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '#'; '.']]


(* EBLUBRFH *)