
type player_id = int

type name = string

type player = {
  id : player_id;
  name : name;
  cards : Card.card list * Card.card list;
  chips : Chips.chips;
  bet : Chips.chips * Chips.chips
}

(** [init_bank] is a bank with the standard chips totalling $1000. *)
let init_bank =
  let inc = Chips.inc_chips in
  Chips.create_chips |> inc Red 10 |> inc Blue 10 |> inc White 10 |> inc Black 6

let create name i =
  { id = i;
    name = name;
    cards = [], [];
    chips = init_bank;
    bet = Chips.create_chips, Chips.create_chips }

let id p = p.id

let name p = p.name

let cards first p = if first then fst p.cards else snd p.cards

let score first p = List.fold_left (fun acc c ->
    Card.score c + acc) 0 (if first then fst p.cards else snd p.cards)

let chips p = p.chips

let bet first p = if first then fst p.bet else snd p.bet

let has_split p = snd p.cards <> []

(** [deck_score d] is the score of all cards in list [d]. *)
let deck_score d = List.fold_left (fun acc c -> Card.score c + acc) 0 d

(** [ace_1 p] is card list [d] with all Aces counted as 1. *)
let ace_1 d = (List.fold_right (fun c acc ->
    (if Card.is_ace c then Card.ace_one c else c) :: acc) d [])

(** [ace_11 p] is card list [d] with one Ace counted as 11, if possible, else
    [d] with all Aces counted as 1. *)
let ace_11 d =
  let rec make_11 lst = function
      [] -> lst
    | h :: t ->
      if Card.is_ace h then lst@[Card.ace_eleven h]@t
      else make_11 (lst@[h]) t in
  let d_1 = ace_1 d in
  if deck_score d_1 < 12 then make_11 [] d else d_1

let add_card first c p =
  { p with
    cards = if first then c :: fst p.cards |> ace_11, snd p.cards
      else fst p.cards, c :: snd p.cards |> ace_11 }

let remove_cards first p =
  { p with
    cards = if first then [], snd p.cards else fst p.cards, [] }

let update_bet first lst p =
  List.fold_left (fun p (c, n) ->
      { p with
        chips = Chips.dec_chips c n p.chips;
        bet = if first then Chips.inc_chips c n (fst p.bet), snd p.bet
          else fst p.bet, Chips.inc_chips c n (snd p.bet) }) p lst

let doubledown first p =
  update_bet first (bet first p |> Chips.to_list) p

(** [split_cards p] is player [p] with their card from the first hand moved to
    their second hand.
    Requires: [p] has only 2 cards and only holds one hand. *)
let split_cards p =
  { p with
    cards = fst p.cards |> List.tl, [fst p.cards |> List.hd] }

let split p =
  update_bet false (fst p.bet |> Chips.to_list) p
  |> split_cards

let clear_bet first p =
  { p with
    bet = if first then Chips.create_chips, snd p.bet
      else fst p.bet, Chips.create_chips }

let bet_to_bank first p =
  let f = List.fold_left (fun b (c, n) -> Chips.inc_chips c (2 * n) b) p.chips in
  let bet_chips = Chips.to_list (if first then fst p.bet else snd p.bet) in
  { p with
    chips = f bet_chips }
  |> clear_bet first

let push_player first p =
  let f = List.fold_left (fun b (c, n) -> Chips.inc_chips c n b) p.chips in
  let bet_chips = Chips.to_list (if first then fst p.bet else snd p.bet) in
  { p with
    chips = f bet_chips }
  |> clear_bet first

let bankrupt p = Chips.chips_value p.chips = 0