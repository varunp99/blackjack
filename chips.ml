
type chip = Red | Blue | White | Black

type chips = (chip * int) list

exception InsufficientFunds

let create_chips =  [(Red, 0); (Blue, 0); (White, 0); (Black, 0)]

(** [chip_value (c, x)] is the value of [x] number of chip [c]. *)
let chip_value = function
  | Red, x -> 5 * x
  | Blue, x -> 10 * x
  | White, x -> 25 * x
  | Black, x -> 100 * x

(** [string_of_chip (c, x)] is the string representation of [x] number of [c] chips. *)
let string_of_chip = function
  | Red, x -> Emoji.red_circle ^ "($5): " ^ string_of_int x
  | Blue, x -> Emoji.blue_circle ^ "($10): " ^ string_of_int x
  | White, x -> Emoji.white_circle ^ "($25): " ^ string_of_int x
  | Black, x -> Emoji.black_circle ^ "($100): " ^ string_of_int x

let chips_value b =
  List.fold_left (fun acc c -> chip_value c + acc) 0 b

let string_of_chips b =
  List.fold_left (fun acc c -> acc ^ string_of_chip c ^ "\t") "" b ^
  "\n Total = $" ^ string_of_int (chips_value b) ^ ".\n"

let to_list b = b

let inc_chips c n b =
  List.map (fun (x, y) -> if x = c then (x, y + n) else (x, y)) b

let dec_chips c n b = List.map (fun (x, y) ->
    if x = c then if y - n < 0 then raise InsufficientFunds else (x, y - n)
    else (x, y)) b

let compare x y = compare (chips_value x) (chips_value y)