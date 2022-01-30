open Prelude

(* solution = sum . simulate step*)

type state = Flashed of int | Unflashed of int 
let map_state f = function Flashed x -> Flashed (f x) | Unflashed x -> Unflashed (f x)
let value = function Flashed x | Unflashed x -> x
let flashed x = Flashed x
let unflashed x = Unflashed x 
let is_flashed = function Flashed _ -> true | _ -> false
let is_unflashed = function Unflashed _ -> true | _ -> false 

let adjacent (x, y) (i, j) = 
  let dimensions = 
    [`up; `down; `left; `right; `top_left; `top_right; `bot_left; `bot_right] in 
  let f adj = 
    let in_bounds i j = if i > x || i < 0 || j > y || j < 0 then adj else (i, j) :: adj in
    function 
    | `up -> in_bounds i (j + 1) | `down -> in_bounds i (j - 1)
    | `left -> in_bounds (i - 1) j | `right -> in_bounds (i + 1) j 
    | `top_left -> in_bounds (i - 1) (j -  1) | `top_right -> in_bounds (i + 1) (j - 1)
    | `bot_left -> in_bounds (i - 1) (j  + 1) | `bot_right -> in_bounds (i + 1) (j  + 1)
  in 
  List.fold_left f [] dimensions

let get_dimension array = 
  let y = Array.length array in 
  let x = Array.length array.(0) in
  x - 1, y - 1

let index matrix (x, y) = matrix.(y).(x)
let update matrix (x, y) elem = matrix.(y).(x) <- elem; matrix


let mapi f matrix = matrix |> Array.mapi (fun y row -> row |> Array.mapi (fun x elem -> f (x, y) elem))
let fold folder = Array.fold_left (Array.fold_left folder)
let filteri f matrix  =
  let folder total array = 
    let result = array |> Array.to_list |> List.filter (fun (index,elem) -> f index elem) in
    total @ result 
  in 
  Array.fold_left folder [] (mapi (fun index elem -> index, elem) matrix)

let can_flash = fold (fun flash elem -> is_unflashed elem && value elem > 9 || flash) false 

let rec step (count, matrix) = 
  let x,y = get_dimension matrix in
  if can_flash matrix then 
    (* do flashing *)
    (* get the indeces that can flash *)
    (* make then flashed *)
    (* increase the adjacents of this indeces that are not already flashed *)
    let f _ix = function Unflashed elem -> elem > 9 | _ -> false in
    let idxs = matrix |> filteri f in
    let contains x  = idxs |> List.exists (fst >> (=)x) in
    let matrix = matrix |> mapi (fun ix elem -> if contains ix then Flashed 0 else elem) in
    let increment matrix ix =
      match index matrix ix with 
      | Unflashed x -> update matrix ix (Unflashed (x + 1))
      | Flashed _x -> matrix
    in
    let adjs = idxs |> List.concat_map (fst >> adjacent (x, y)) in
    let matrix = List.fold_left increment matrix adjs in
    let len = List.length idxs in
    step (count+len, matrix)
  else
    let matrix = matrix |> mapi (fun _ix elem -> if  is_flashed elem then Unflashed 0 else elem) in 
    (count, matrix) 

let step (count, matrix) = step (count, mapi (fun _ -> map_state succ) matrix)

let rec simulate n stepper =
  match n with
  | 0 -> Fun.id
  | n -> stepper >> simulate (pred n) stepper

let part1 = simulate 100 step >> fst

let part2 matrix = 
  let rec help count (flash, matrix) = 
    let (next_flash, matrix) = step (flash, matrix) in 
    if next_flash - flash = 100 then (count + 1) 
    else help (count+1) (next_flash, matrix) 
  in 
  let (flash, matrix) = step (0, matrix) in
  help 1 (flash, matrix)

let () =  
  let file = "./input/day11.txt" in
  let to_matrix list = list |> Array.of_list |> Array.map Array.of_list in
  let parse_line = Str.split (Str.regexp "") >> List.map int_of_string in
  let lines = file |> input_lines |> List.map parse_line |> to_matrix in
  let lines = lines |> mapi (fun _ elem -> unflashed elem) in 
  Printf.printf "part one: %d \n" (part1 (0, lines));
  Printf.printf "part one: %d \n" (part2 lines)