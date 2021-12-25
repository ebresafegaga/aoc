open Prelude

let rec transpose = function
  | [] -> [] 
  | [xs] -> xs |> List.map (fun elem -> [elem])
  | xs :: xss -> transpose xss |> List.combine xs |> List.map (fun (x, xs) -> x :: xs)

let most_common input = 
  let ones = input |> List.filter (fun bit -> bit = 1) |> List.length in 
  let zeros = input |> List.filter (fun bit -> bit = 0) |> List.length in
  if ones >= zeros then 1 else 0 

let least_common input = 
  let ones = input |> List.filter (fun bit -> bit = 1) |> List.length in 
  let zeros = input |> List.filter (fun bit -> bit = 0) |> List.length in
  if zeros <= ones then 0 else 1

let rec pow x n = if n <= 0 then 1 else x * pow x (n-1)

let decimal_of_binary binary = 
  binary
  |> List.rev 
  |> List.mapi (fun index elem -> index, elem)
  |> List.fold_left (fun result (power, bit) -> (pow 2 power * bit) + result) 0

let part1 input = 
  let gamma, epsilon = 
    input
    |> transpose
    |> List.map (fun number -> most_common number, least_common number) 
    |> List.split
  in
  decimal_of_binary gamma * decimal_of_binary epsilon

let part2 input = 
  let rec filter input predicate = 
    let mc_lc = input |> List.map List.hd |> predicate in 
    let input = input |> List.filter (fun bits -> List.hd bits = mc_lc) in 
    if List.length input = 1 then List.hd input 
    else 
      let input = input |> List.map List.tl in 
      mc_lc :: filter input predicate
  in
  let filter = filter input in 
  let oxy = filter most_common in 
  let c02 = filter least_common in
  decimal_of_binary oxy * decimal_of_binary c02

let () =  
  let file = "./input/day3.txt" in
  let parse = String.to_seq >> List.of_seq >> List.map (Printf.sprintf "%c" >> int_of_string) in
  let lines = file |> input_lines |> List.map parse in 
  Printf.printf "part one: %d \n" (part1 lines);
  Printf.printf "part two: %d \n" (part2 lines)