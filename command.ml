
type command =
  | Info
  | Hit
  | Stand
  | Split
  | Doubledown

type bet =
  | Legal of int list
  | Negative
  | Zero

exception Empty

exception Malformed

(** [remove_spaces lst] is string list [lst] without empty strings. *)
let rec remove_spaces lst =  match lst with
  | [] -> []
  | h :: t -> if h = "" then remove_spaces t else h :: (remove_spaces t)

(** [spilt_str str] is a string list of the words (delimited by the space
    character) in [str]. The returned list does not have any empty strings. *)
let spilt_str str = str |> String.split_on_char ' ' |> remove_spaces

(** [get_command str_lst] returns head of string list [str_lst].
    Raises: [Empty] if [str_lst] is empty. *)
let get_command (str_lst : string list) = match str_lst with 
  | [] -> raise Empty
  | h :: _ -> h

(** [headless str_lst] is string list [str_lst] without its first element. If
    [str_lst] is empty then an empty list is returned. *)
let headless str_lst = match str_lst with
  | [] -> []
  | h :: t -> t

(** [has_neg lst] is true if int list [lst] has a negative integer, else false. *)
let has_neg lst = List.fold_left (fun acc x -> acc || x < 0) false lst

(** [all_zero lst] is true if int list [lst] has all 0s, else false. *)
let all_zero lst = List.fold_left (fun acc x -> acc && x = 0) true lst

let parse str =
  let str_lst = str |> spilt_str in
  let command = str_lst |> get_command in 
  match String.lowercase_ascii command with
  | "info" -> Info
  | "hit" -> Hit
  | "stand" -> Stand
  | "split" -> Split
  | "doubledown" -> Doubledown
  | _ -> raise Malformed

let parse_bet str =
  let str_lst = str |> spilt_str in
  if List.length str_lst <> 4 then raise Malformed else
    let int_lst = try List.map int_of_string str_lst with
      | Failure _ -> [] in
    match int_lst with
    | [] -> raise Malformed
    | lst ->
      if has_neg lst then Negative else if all_zero lst then Zero else Legal lst