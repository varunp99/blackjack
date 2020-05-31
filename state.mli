(** A representation of a gamestate and all associated functions. This module
    handles the meat of the game by changing the gamestate according to the
    player's commands. *)

(** A representation of a player's hand. *)
type hand

(** A representation of a gamestate. *)
type st

(** [init lst decks] is a new state [st] with a dealer, and players with names in
    string list [lst]. Each player gets $1000 worth of chips. [decks] number of
    decks are used. 2 cards are dealt to each player. The first player is the
    first name in [lst].
    Requires: [lst] length = 4. *)
val init : string list -> int -> st

(** [first_player] is state [st] with the current player returned to the first
    player, and their first hand. If all players are bankrupt then the
    dealer is made the current player. *)
val first_player : st -> st

(** [init_again st] is state [st] with a new round of cards dealt and the current
    player returned to the first player. *)
val init_again : st -> st

(** [hit_result] is the type representing the result of an attempted hit. *)
type hit_result = LegalHit of st | BustHit of st * string

(** [bet_result] is the type representing the result of an attempted bet. *)
type bet_result = LegalBet of st | IllegalBet

(** [dd_result] is the type representing the result of an attempted doubledown. *)
type dd_result = LegalDD of st | LateDD | IllegalDD

(** [split_result] is the type representing the result of an attempted split. *)
type split_result = LegalSplit of st | CostlySplit | IllegalSplit

(** [hit st] is state [st] with the current player dealt a card, and returns:
    [BustHit (st', str)] if the current hand goes bust after being hit,
    where [st'] is [st] with the current hand changed to the next hand, and
    [str] is the information of the player whose hand went bust, OR
    [LegalHit st'] if the player can continue to hit, where [st'] is the updated
    state. *)
val hit : st -> hit_result

(** [stand st] is state [st] with the hand changed to the next hand. If the
    current player is the dealer then [st] is returned. *)
val stand : st -> st

(** [split st] is [st] with the current player's cards split, and returns:
    [LegalSplit st'] if the split is legal where [st'] is the new state, OR
    [CostlySplit] if the player doesn't have enough money to place a second bet, OR
    [IllegalSplit] otherwise e.g. the player has more than 2 cards. *)
val split : st -> split_result

(** [doubledown st] is state [st] with the current player's bet doubled, returning:
    [LegalDD st'] if the doubledown in legal, where [st'] is the new state, OR
    [LateDD] if the player tries to doubledown when they have more than 2 cards, OR
    [IllegalDD] if the player does not have enough chips. *)
val doubledown : st -> dd_result

(** [bet st lst] is state [st] with the current player's bet increased by the
    chips indicated in [Chips.chip * int] list [lst], returning:
    [LegalBet st'] if the player has chips in [lst] where [st'] is new state with
    the current player changed to the next player, else
    [IllegalBet]. *)
val bet : st -> (Chips.chip * int) list -> bet_result

(** [state_dealer_bust st] is state [st] with non-bust players earning 2x their
    bet amount and their bet being reset. The bust players lose their bet. *)
val state_dealer_bust : st -> st

(** [state_dealer_stand st] is state [st] with non-bust players earning 2x their
    bet amount if they have a higher score than the dealer, having their original
    bet returned if they had the same score as the dealer, and losing their bet
    otherwise. *)
val state_dealer_stand : st -> st

(** [state_dealer_blackjack st] is state [st] with every player losing their bet
    unless they have a blackjack, in which case their bet is returned. *)
val state_dealer_blackjack : st -> st

(** [discard_cards st] is state [st] with all player cards discarded. *)
val discard_cards : st -> st

(** [check_deck st] is state [st] if the deck has more than 52 cards, else [st]
    with the discard pile added to the deck and the deck shuffled. *)
val check_deck : st -> st

(** [current_player_name st] is the score of the current player in state [st]. *)
val current_player_name : st -> string

(** [current_player_score st] is the score of the current player in state [st]. *)
val current_player_score : st -> int

(** [current_player_bank st] is the bank of the current player in state [st]. *)
val current_player_bank : st -> Chips.chips

(** [current_player_info st] is a string of all information about the current
    player in state [st], except for bank information. *)
val current_player_info : st -> string

(** [current_dealer_info st] is a string of all information about the dealer
    in state [st]. *)
val current_dealer_info : st -> string

(** [current_player_dealer st] is true if the current player is the dealer,
    else false. *)
val current_player_dealer : st -> bool

(** [all_info st] is a string of all information about the players in state [st].
    The dealer's second card is not revealed. *)
val all_info : st -> string

(** [bank_info st] is a string of all bank information about the players in
    state [st]. *)
val bank_info : st -> string

(** [player_name_list st] is a list of all the players' names in state [st]. *)
val player_name_list : st -> string list