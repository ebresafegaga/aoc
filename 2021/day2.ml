open Prelude

type command = Forward of int | Down of int | Up of int

let command_of_string scalar = function 
  | "forward" -> Forward scalar 
  | "down" -> Down scalar 
  | "up" -> Up scalar
  | _ -> failwith "unknown command"

let part1 = 
  let rec loop (x, y) input = 
    match input with 
    | [] -> x * y 
    | Forward delta :: rest -> loop (x+delta, y) rest 
    | Down delta :: rest -> loop (x, y+delta) rest 
    | Up delta :: rest -> loop (x, y-delta) rest 
  in 
  loop (0, 0)

let part2 = 
  let rec loop (x, y, aim) input = 
    match input with 
    | [] -> x * y 
    | Forward delta :: rest -> loop (x+delta, y+(aim*delta), aim) rest 
    | Down delta :: rest -> loop (x, y, aim+delta) rest 
    | Up delta :: rest -> loop (x, y, aim-delta) rest 
  in 
  loop (0, 0, 0)

let () =  
  let file = "./input/day2.txt" in
  let parse input = 
    match String.split_on_char ' ' input with 
    | [str; scalar] -> command_of_string (int_of_string scalar) str
    | _ -> failwith "expected two items in the list"
  in 
  let lines = file |> input_lines |> List.map parse in 
  Printf.printf "part one: %d \n" (part1 lines);
  Printf.printf "part one: %d \n" (part2 lines)