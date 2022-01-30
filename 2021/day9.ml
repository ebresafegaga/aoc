open Prelude

let get_dimension array = 
  let y = Array.length array in 
  let x = Array.length array.(0) in
  x - 1, y - 1

let index array (x, y) = array.(y).(x)
let indexes array = List.map (index array)

let adjacent (x, y) (i, j) = 
  let dimensions = [`up; `down; `left; `right] in 
  let f adj = 
    let in_bounds i j = if i > x || i < 0 || j > y || j < 0 then adj else (i, j) :: adj in
    function 
    | `up -> in_bounds i (j + 1) | `down -> in_bounds i (j - 1)
    | `left -> in_bounds (i - 1) j | `right -> in_bounds (i + 1) j 
  in 
  List.fold_left f [] dimensions

let lowpoint array = 
  let array2_mapi f matrix = matrix |> Array.mapi (fun y row -> row |> Array.mapi (fun x elem -> f (x, y) elem)) in
  let array2_filter f matrix  =
    let folder total array = 
      let result = array |> Array.to_list |> List.filter f in
      total @ result 
    in 
    Array.fold_left folder [] matrix
  in
  let (x, y) = get_dimension array in

  array 
  |> array2_mapi (fun (i, j) elem -> 
      let idxs = adjacent (x, y) (i, j) in
      let adjs = indexes array idxs in
      elem, ((i, j), adjs))
  |> array2_filter (fun (elem, (_, adjs)) -> List.for_all (fun adj -> elem < adj) adjs)
   
let part1 = lowpoint >> List.map (fst >> succ) >> List.fold_left (+) 0

let basin matrix (i, j) = 
  let x,y = get_dimension matrix in
  let rec dfs visited elem = 
    let seen visited elem = visited |> List.find_opt ((=) elem) |> Option.is_some in
    let nexts (i, j) = adjacent (x,y) (i,j) |> List.filter (fun (i, j) -> index matrix (i, j) <> 9) in
    if seen visited elem then visited 
    else 
      let adjs = nexts elem in 
      let visited = elem :: visited in 
      List.fold_left dfs visited adjs
  in 
  dfs [] (i, j)

let part2 matrix = 
  matrix
  |> lowpoint 
  |> List.map (snd >> fst >> basin matrix >> List.length)
  |> List.sort (fun a b -> b - a) 
  |> take 3
  |> List.fold_left ( * ) 1

let () =  
  let file = "./input/day9.txt" in
  let to_matrix list = list |> Array.of_list |> Array.map Array.of_list in
  let parse_line = Str.split (Str.regexp "") >> List.map int_of_string in
  let lines = file |> input_lines |> List.map parse_line |> to_matrix in 
  Printf.printf "part one: %d \n" (part1 lines);
  Printf.printf "part two: %d \n" (part2 lines)