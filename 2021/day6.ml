open Prelude

(* solution = length . simulate 80 step *)

let step  = List.concat_map (function 0 -> [6;8] | n -> [n-1])

let rec simulate n stepper =
  match n with
  | 0 -> Fun.id
  | n -> stepper >> simulate (pred n) stepper
  
let part1 = simulate 256 step >> List.length

let rec compress alist = 
  let rec assoc key alist = 
    match alist with 
    | [] -> None
    | (k, elem) :: alist when k=key -> Some (elem, alist) 
    | (k, elem) :: alist ->
      match assoc key alist with 
      | None -> None 
      | Some (e, almost) -> Some (e, (k, elem) :: almost)  
  in
  match alist with 
  | [] -> []
  | (key, elem) :: alist -> 
    match assoc key alist with  
    | Some (elem2, alist) -> (key, elem+elem2) :: compress alist
    | None -> (key, elem) :: compress alist  

let step2 fishes = 
  let f = function 
    | (0, n) -> List.concat @@ List.init n (Fun.const [6,1; 8,1;]) 
    | (k,e) -> singleton (k-1,e) 
  in
  fishes |> List.concat_map f |> compress 

let trans input = 
  let array = Array.init 9 (Fun.const 0) in
  let update updater index = Array.set array index (updater array.(index)) in
  List.iter (update succ) input; 
  array |> Array.mapi (fun fish number -> fish, number) |> Array.to_list

let solution = trans >> simulate 256 step2 >> List.map snd >> sum

let () =  
  let file = "./input/test.txt" in
  let lines = file |> input_string |> String.split_on_char ',' |> List.map int_of_string in 
  Printf.printf "part one: %d \n" (solution lines)