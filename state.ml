
type hand = First | Second

(* Invariant: All changes made to player with id [current_player] are 
   accordingly represented in [players]. *)
type st = {
  players : Player.player list;
  deck : Card.deck;
  discard : Card.card list;
  dealer : Player.player;
  current_player : hand * Player.player_id;
}

(** [dealerID] is the dealer's ID. *)
let dealerID = 0

(** [make_decks lst n] is a list of [n] decks.
    Requires: [lst] is an empty list. *)
let rec make_decks lst = function
  | 0 -> lst
  | n -> make_decks (Card.create :: lst) (n - 1)

(** [match_player p lst] is player list [lst] with the player, whose id is the
    same as player [p], replaced by [p]. *)
let match_player p lst =
  List.map (fun x -> if Player.id x = Player.id p then p else x) lst

(** [change_player st p] is state [st] with changed player configuration of
    the player, with the id same as [p], to [p].  *)
let change_player st p = { st with players = match_player p st.players }

(** [get_player st id] is the player in state [st] with id [id].
    Raises: [Failure] if there is no player with id [id]. *)
let get_player st id =
  let rec match_id id = function
      [] -> st.dealer
    | h :: t -> if Player.id h = id then h else match_id id t in
  match_id id st.players

(** [get_current_player st] is the current player in state [st]. *)
let get_current_player st = match st.current_player with
  | _, dID when dID = dealerID -> st.dealer
  | _, pID -> get_player st pID

(** [deal_player st] is state [st] with the next card in the deck dealt to
    the current player.
    Raises: [Failure] if the deck is empty or the player is the dealer. *)
let deal_player st =
  if snd st.current_player = dealerID then failwith "use deal_dealer" else 
    let c = match Card.next st.deck with
      | None -> failwith "No more cards"
      | Some x -> x in
    let first = fst st.current_player = First in
    Player.add_card first c (get_current_player st)
    |> change_player { st with deck = Card.pop st.deck }

(** [deal_dealer st] is state [st] with the next card in the deck dealt to
    the dealer. The current player is unchanged.
    Raises: [Failure] if the deck is empty. *)
let deal_dealer st =
  let c = match Card.next st.deck with
    | None -> failwith "No more cards"
    | Some x -> x in
  { st with
    deck = Card.pop st.deck;
    dealer = Player.add_card true c st.dealer }

(** [deal_all st] is state [st] with each player's first hand dealt a card
    one by one, and the current player returned to the the current player of [st]. *) 
let deal_all st =
  let player_st = List.fold_left (fun acc x ->
      deal_player { acc with current_player = First, Player.id x })
      st st.players in
  let dealer_st = deal_dealer player_st in
  { dealer_st with current_player = st.current_player }

(** [make_players lst] is a list of the [Player.player] representation of each
    name in string list [lst]. *)
let make_players lst =
  let rec make n players = function
    | [] -> players |> List.rev
    | h :: t -> make (n - 1) ((Player.create h n) :: players) t in
  make (List.length lst) [] lst

let init lst decks =
  let ps = make_players lst in
  let st =
    { players = ps;
      deck = make_decks [] decks |> Card.combine;
      discard = [];
      dealer = Player.create "The Dealer" dealerID;
      current_player = First, List.hd ps |> Player.id } in
  deal_all st |> deal_all

let first_player st =
  { st with
    current_player =
      First,
      try List.hd st.players |> Player.id with Failure _ -> dealerID }

(** [remove_bankrupt st] is state [st] with the bankrupt players removed. *)
let remove_bankrupt st =
  { st with
    players = List.fold_right (fun x acc ->
        if Player.bankrupt x then acc else x :: acc) st.players [] }

let init_again st = st |> remove_bankrupt |> first_player |> deal_all |> deal_all

type hit_result = LegalHit of st | BustHit of st * string

type bet_result = LegalBet of st | IllegalBet

type dd_result = LegalDD of st | LateDD | IllegalDD

type split_result = LegalSplit of st | CostlySplit | IllegalSplit

(** [next_player st] is the next player's id in state [st]. If the current player
    is the dealer, then [dealerID] is returned. *)
let next_player st =
  let rec find_next id = function
    | h1 :: h2 :: t -> if Player.id h1 = id then Player.id h2
      else find_next id (h2 :: t)
    | _ -> dealerID in
  find_next (snd st.current_player) st.players

(** [next_hand st] is state [st] with the current hand changed to the next hand
    to be played. *)
let next_hand st =
  if get_current_player st |> Player.has_split && fst st.current_player = First
  then { st with current_player = Second, snd st.current_player }
  else { st with current_player = First, next_player st }

(** [play_info_no_bank p] is a string of all the information of player [p]. The
    player's bank information is not included.  *)
let player_info_no_bank p =
  let info first p =
    Card.display_cards_list (Player.cards first p) ^
    "Their score is " ^ (Player.score first p |> string_of_int) ^ ".\n" ^
    "They have bet $" ^ (Player.bet first p |> Chips.chips_value |> string_of_int) ^
    ".\n" in
  let has_split = Player.has_split p in
  Player.name p ^ " has the following cards:\n" ^ info true p ^
  (if has_split then
     "Their second hand has the following cards:\n" ^ info false p
   else "")

(** [play_info p] is a string of all the information of player [p].  *)
let player_info p =
  player_info_no_bank p ^
  "They have the following chips:\n" ^ (Player.chips p |> Chips.string_of_chips)

(** [play_info_no_chips p] is a string of all the information of player [p]. The
    specific chips are not included. *)
let player_info_no_chips p =
  player_info_no_bank p ^
  "Their bank is worth $" ^ (Player.chips p |> Chips.chips_value |> string_of_int)

let hit st =
  let new_st =
    st |> (if snd st.current_player = dealerID then deal_dealer else deal_player) in
  match Player.score (fst st.current_player = First) (get_current_player new_st) with
  | n when n > 21 ->
    BustHit (next_hand new_st, get_current_player new_st |> player_info_no_chips)
  | _ -> LegalHit new_st

let stand st = next_hand st

(** [same_rank lst] is true if all cards in list [lst] have the same rank, else
    false.
    Raises: [Failure] if [lst] is empty. *)
let same_rank lst =
  let hd_rank = List.hd lst |> Card.rank in
  List.fold_left (fun acc x -> acc && Card.rank x = hd_rank) true lst

let split st =
  let player_cards = get_current_player st |> Player.cards true in
  if
    fst st.current_player = Second ||
    List.length player_cards > 2 ||
    not (same_rank player_cards)
  then IllegalSplit
  else try
      let split_st = get_current_player st |> Player.split |> change_player st in
      let first_card_dealt = deal_player split_st in
      let second_card_dealt =
        deal_player { first_card_dealt with 
                      current_player = Second, snd st.current_player } in
      LegalSplit { second_card_dealt with
                   current_player = First, snd st.current_player }
    with 
      Chips.InsufficientFunds -> CostlySplit

let doubledown st =
  let first = fst st.current_player = First in
  if get_current_player st |> Player.cards first |> List.length > 2 then
    LateDD
  else
    try
      LegalDD (get_current_player st |> Player.doubledown first
               |> change_player st)
    with
    | Chips.InsufficientFunds -> IllegalDD

let bet st lst =
  let curr_player = get_current_player st in
  try
    begin
      let updated_player = Player.update_bet true lst curr_player in
      let new_st = change_player st updated_player in 
      LegalBet { new_st with current_player = First, next_player new_st}
    end
  with
  | Chips.InsufficientFunds -> IllegalBet

(** [use_op op p] is [op true p]. If player [p] has split then it is
    [op true p |> op false p]. *)
let use_op op p =
  let up_pl = op true p in
  if Player.has_split p then op false up_pl else up_pl

let state_dealer_bust st =
  let op first p = if Player.score first p > 21 then Player.clear_bet first p
    else Player.bet_to_bank first p in
  { st with players = List.map (fun p -> use_op op p) st.players }

let state_dealer_stand st =
  let dealer_score = get_current_player st |> Player.score true in
  let op first p = if Player.score first p > 21 then Player.clear_bet first p
    else match compare (Player.score first p) dealer_score with
      | n when n < 0 -> Player.clear_bet first p
      | 0 -> Player.push_player first p
      | _ -> Player.bet_to_bank first p in
  { st with players = List.map (fun p -> use_op op p) st.players }

let state_dealer_blackjack st =
  let op first p = if Player.score first p = 21 then Player.push_player first p
    else Player.clear_bet first p in
  { st with players = List.map (fun p -> use_op op p) st.players }

let discard_cards st =
  { st with 
    players = List.map (use_op Player.remove_cards) st.players;
    discard = List.fold_left (fun acc p ->
        Player.cards true p @ Player.cards false p @ acc) [] (st.dealer :: st.players) @ st.discard;
    dealer = Player.remove_cards true st.dealer }

let check_deck st =
  if Card.count st.deck <= 52 then
    { st with
      deck = Card.reshuffle st.deck st.discard;
      discard = [] }
  else st

let current_player_name st = get_current_player st |> Player.name

let current_player_score st =
  get_current_player st |>
  Player.score (fst st.current_player = First)

let current_player_bank st = get_current_player st |> Player.chips

let current_player_info st = get_current_player st |> player_info_no_bank

let current_dealer_info st =
  let p = st.dealer in
  "\n\n" ^ Player.name p ^ " has\n" ^
  Card.display_cards_list (Player.cards true p) ^ "\n" ^
  "Their score is " ^ (Player.score true p |> string_of_int) ^ "."

let current_player_dealer st = snd st.current_player = dealerID

(** [dealer_info st] is the dealer's information in state [st]. *)
let dealer_info st = let p = st.dealer in
  let rec show_first = function
    | [] -> "no cards."
    | h :: t -> Card.string_of_card h ^ "\nThe second card is face-down" in
  Player.name p ^ " has\n" ^ show_first (Player.cards true p)

let all_info st =
  "\n\n" ^
  (List.fold_left (fun acc p -> acc ^ player_info_no_bank p ^ "\n\n") "" st.players) ^
  dealer_info st

let bank_info st =
  "\n\nGame Status:\n" ^
  (List.fold_left (fun acc p ->
       acc ^ Player.name p ^ " has the following chips:\n"
       ^ (Player.chips p |> Chips.string_of_chips) ^ "\n\n")) "" st.players

let player_name_list st = List.fold_right (fun p acc ->
    Player.name p :: acc) st.players []