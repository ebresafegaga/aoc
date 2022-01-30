open Prelude

(* solutions crabs = min . map (cost crabs) *)
let cost1 crabs position = List.fold_left (fun total crab -> total + abs (crab - position)) 0 crabs

let cost2 crabs position =
  let f total crab =
    let n = abs (crab - position) in 
    total + n*(n+1) / 2
  in 
  List.fold_left f 0 crabs

let part crabs = List.map (cost2 crabs) >> min_by (<)

let () =  
  let file = "./input/day7.txt" in
  let crabs = file |> input_string |> String.split_on_char ',' |> List.map int_of_string in 
  let positions = range 0 (min_by (>) crabs) in
  Printf.printf "part one: %d \n" (part crabs positions)