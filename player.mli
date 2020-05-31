(** A representation of a player and associated functions. A player has an id,
    a name, cards (which can be split into two hands), a bank and a bet. *)

(** A representation of player's id. *)
type player_id = int

(** A representation of player's name. *)
type name = string

(** A representation of a player. *)
type player

(** [name p] is the name of player [p]. *)
val name : player -> string

(** [create name i] is a [player] with name [name] and id [id].
    They have no cards. *)
val create : string -> int -> player

(** [id p] is the id of player [p]. *)
val id : player -> int

(** [cards first p] is a [card] list of the hand of player [p]. It is the first
    hand if [first] is true, else the second hand. *)
val cards : bool -> player -> Card.card list

(** [score first p] is the score of player [p]. It is the first hand's score
    if [first] is true, else the second hand's score. *)
val score : bool -> player -> int

(** [chips p] is the chips held by player [p]. *)
val chips : player -> Chips.chips

(** [bet first p] is the chips bet by player [p]. It is the first hand's bet
    if [first] is true, else the second hand's bet. *)
val bet : bool -> player -> Chips.chips

(** [has_split p] is true if player [p] has split their cards. *)
val has_split : player -> bool

(** [add_card first c p] is player [p] with card [c] added to their hand. The
    card is added to the first hand if [first] is true, else the second hand.
    An Ace is scored at 11 if possible, else all are scored at 1. *)
val add_card : bool -> Card.card -> player -> player

(** [split p] is player [p] with their first hand's second card put into their
    second hand. The bet for their second hand is made the same as their
    first hand's bet.
    Raises: [InsufficientFunds] if [p] does not have enough money to bet for
    their second hand. *)
val split : player -> player

(** [remove_cards first p] is player [p] with all their cards removed. If [first]
    is true then cards are removed from the first hand, else from the second
    hand. *)
val remove_cards : bool -> player -> player

(** [update_bet first lst p] is player [p] with their bet increased as indicated
    by [lst], which is a [Chips.chips * int] list indicating which chips are bet 
    and how many of each. If [first] is true then the bet is for the first
    hand, else for the second hand.
    Raises: [InsufficientFunds] if [p] does not have enough chips. *)
val update_bet : bool -> (Chips.chip * int) list -> player -> player

(** [doubledown first p] is player [p] with their bet doubled. If [first] is
    true then their first hand's bet is doubled, else their second hand's bet
    is doubled.
    Raises: [InsufficientFunds] if [p] does not have enough chips. *)
val doubledown : bool -> player -> player

(** [clear_bet first p] is player [p] with their bet made 0. If [first] is
    true then the bet for the first hand is cleared, else for the second hand. *)
val clear_bet : bool -> player -> player

(** [bet_to_bank first p] is player [p] with 2x their bet added to their bank
    and their bet cleared. If [first] is true then the bet considered is the
    one for the first hand, else the one for the second hand. *)
val bet_to_bank : bool -> player -> player

(** [push_player first p] is [p] with their bet cleared and added to their bank.
    If [first] is true then the bet considered is the one for the first hand,
    else the one for the second hand. *)
val push_player : bool -> player -> player

(** [bankrupt p] is true if player [p] is bankrupt, else false. *)
val bankrupt : player -> bool