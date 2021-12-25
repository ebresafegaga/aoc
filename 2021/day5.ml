open Prelude 

let covers (x, y) ((x1, y1), (x2, y2)) = 
  let dy, dx = y2 - y1, x2 - x1 in
  let is_horizontal = dy = 0 and is_vertical = dx = 0 in
  let m () = (dy / dx) in
  let in_range a b x =
    let max = max a b and min = min a b in 
    x <= max && x >= min in
  if is_horizontal then y = y1 && in_range x1 x2 x 
  else if is_vertical then x = x1 && in_range y1 y2 y
  else 
    (* return false when solving for part1 *)
    let c = y1 - (m () * x1) in
    y = m () * x + c && (* y = m*x + c *)
    in_range x1 x2 x && in_range y1 y2 y 

let part1 lines grid =
  grid
  |> List.concat_map
      (List.map (fun point -> 
        lines
        |> List.filter (covers point) 
        |> List.length) >> 
      List.filter (fun x -> x >= 2))
  |> List.length

let build_grid (xmax, ymax) =
  let xmax, ymax = xmax+1, ymax+1 in
  List.init xmax (fun xindex -> List.init ymax (fun yindex -> xindex, yindex))

let grid_dimension lines =
  let trans ((x1, y1), (x2, y2)) = max x1 x2, max y1 y2 in 
  let list_max = List.fold_left max Int.min_int in
  let xs, ys = lines |> List.map trans |> List.split in 
  list_max xs, list_max ys

let () =  
  let file = "./input/day5.txt" in
  let tuple = function [x;y] -> (x, y) | _ -> assert false in
  let lines = 
    file 
    |> input_lines 
    |> List.map (Str.split (Str.regexp " -> ") >> List.map (String.split_on_char ',' >> List.map int_of_string >> tuple) >> tuple)
  in
  let grid = lines |> grid_dimension |> build_grid in
  Printf.printf "part one: %d \n" (part1 lines grid)