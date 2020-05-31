
(** Initialize Random so it won't use the default seed *)
let _ = Random.self_init()

type suit = Diamonds | Hearts | Clubs | Spades

(** [suits] is a set-like list of all tags of [suit]. *)
let suits = [Diamonds; Hearts; Clubs; Spades]

type ace = One | Eleven

type rank = Ace of ace | Two | Three | Four | Five | Six | Seven | Eight
          | Nine | Ten | Jack | Queen | King

(** [ranks] is a set-like list of all tags of [rank]. *)
let ranks =
  [Ace One; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King]

type card = {
  suit : suit;
  rank : rank;
}

type deck = card list

let suit c = c.suit

let rank c = c.rank

let score c = match c.rank with
  | Ace x -> if x = One then 1 else 11
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | _ -> 10

let is_ace c = match c.rank with Ace _ -> true | _ -> false

let create_card s r = {
  suit = s;
  rank = r;
}

let ace_one c = if is_ace c then create_card c.suit (Ace One) else c

let ace_eleven c = if is_ace c then create_card c.suit (Ace Eleven) else c

(** [create_suit s] is a [card list] with all 13 cards of suit [s]. *)
let create_suit s = List.map (create_card s) ranks

(** [shuffle d] is deck [d] with its cards arranged in random order. *)
let shuffle d = 
  (* tag each list element with a random value, sort based on random value,
     remove random values. *)
  List.map (fun c -> (Random.bits (), c)) d
  |> List.sort compare
  |> List.map snd

let create_unshuffled = List.fold_right (fun s a -> (create_suit s)@a) suits []

let create = List.fold_right (fun s a ->
    (List.map (create_card s) [Four;Five])@a) suits [] |> shuffle

(* let create = List.fold_right (fun s a -> (create_suit s)@a) suits []
             |> shuffle *)

(** [combine_two d1 d2] is deck [d1] will all cards of deck [d2] added after
    all cards in [d1], and in the same order as in [d2]. *)
let combine_two d1 d2 = d1 @ d2

let combine lst = List.fold_right combine_two lst []

let pop d = match d with
  | [] -> d
  | _ :: t -> t

let next d = match d with
  | [] -> None
  | h :: _ -> Some h

let add d c = c :: d

let reshuffle d lst = d @ lst |> shuffle

let count d = List.length d

let string_of_suit s = match s with
  | Diamonds -> "Diamonds"
  | Hearts -> "Hearts"
  | Clubs -> "Clubs"
  | Spades -> "Spades"

let string_of_rank r = match r with
  | Ace _ -> "Ace"
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"

(** [rank_string r] is the string representation to display for rank [r]. *)
let rank_string r = match r with 
  | Ace _ -> "A "
  | Two -> "2 "
  | Three -> "3 "
  | Four -> "4 "
  | Five -> "5 "
  | Six -> "6 "
  | Seven -> "7 "
  | Eight -> "8 "
  | Nine -> "9 "
  | Ten -> "10"
  | Jack -> "J "
  | Queen -> "Q "
  | King -> "K "

(** [suit_string s] is the string representation for the symbol for suit [s]. *)
let suit_string s = match s with 
  | Diamonds -> Emoji.diamond_suit
  | Hearts -> Emoji.heart_suit
  | Clubs -> Emoji.club_suit
  | Spades -> Emoji.spade_suit

(** [h_line c] is the string for the top line for the visual display of card. *)
let h_line c = "-----------"

(** [top_rank c] is the string for the top rank line for the visual display of
    card [c]. *)
let top_rank c = "| " ^ rank_string c.rank ^ "      |"

(** [s_line c] is the string for the side line for the visual display of card. *)
let s_line c = "|         |"

(** [suit_display c] is the string for the suit line for the visual display of
    card [c]. *)
let suit_display c = "|    " ^ suit_string c.suit ^ "    |" 

(** [bottom_rank c] is the string for the top rank line for the visual display
    of card [c]. *)
let bottom_rank c = "|       " ^ rank_string c.rank ^ "|"  

let card_display = [h_line; top_rank; s_line; suit_display; s_line; bottom_rank; h_line]

let string_of_card c = List.fold_right (fun f a -> a ^ (f c) ^ "\n") card_display ""

let display_cards_list lst = 
  let f_lst = card_display in 
  let display_line f a = List.fold_right (fun c a -> a ^ (f c) ^ "\t") lst a in
  List.fold_right (fun f a -> (display_line f a) ^ "\n") f_lst ""
