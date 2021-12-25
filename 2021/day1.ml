open Prelude

let rec take n xs = 
  match n, xs with 
  | 0, _ -> []
  | _n, [] -> failwith "take: not enough elements" 
  | n, x :: xs -> x :: take (n - 1) xs
  
let rec windowed size input = 
  match input with 
  | [] -> [] 
  | _x :: xs -> 
    match take size input with 
    | window -> window :: windowed size xs
    | exception Failure _msg -> []

let part1 input =
  input
  |> windowed 2 
  |> List.map (function [x; y] when y > x -> 1 | _ -> 0) 
  |> sum

let part2 input = 
  input
  |> windowed 3 
  |> List.map sum 
  |> windowed 2 
  |> List.map (function [x; y] when y > x -> 1 | _ -> 0) 
  |> sum

let () =  
  let file = "./input/day1.txt" in
  let lines = file |> input_lines |> List.map int_of_string in 
  Printf.printf "part one: %d \n" (part1 lines);
  Printf.printf "part two: %d \n" (part2 lines)