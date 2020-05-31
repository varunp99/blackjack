(** A representation for cards and decks and associated functions. *)

(** A type for the suit of a card. *)
type suit

(** A type for the value of an Ace card, since an Ace can have a value of 1 or 11. *)
type ace

(** A type for the rank of a card. *)
type rank

(** A type for a card. *)
type card

(** A type for a deck of cards. *)
type deck

(** [suit c] is the suit of card [c]. *)
val suit : card -> suit

(** [sank c] is the rank of card [c]. *)
val rank : card -> rank

(** [score c] is the score of card [c]. *)
val score : card -> int

(** [is_ace c] is true if [c] is an Ace, else false. *)
val is_ace : card -> bool

(** [create_card s r] is a [card] of suit [s] and rank [r]. *)
val create_card : suit -> rank -> card

(** [create_unshuffled] is an unshuffled deck of 52 cards. *)
val create_unshuffled : deck

(** [create] is a shuffled deck of 52 cards. *)
val create : deck

(** [ace_one c] is an Ace of value 1 if [c] is an Ace, else [c]. *)
val ace_one : card -> card

(** [ace_eleven c] is an Ace of value 11 if [c] is an Ace, else [c]. *)
val ace_eleven : card -> card

(** [combine lst] is a deck with all cards part of the decks in deck list [lst].
    The cards are in an order as if the first deck was placed on top of the
    second, the second on the third and so forth.
    Requires: [lst] length > 0. *)
val combine : deck list -> deck

(** [pop d] is deck [d] without its top card. If [d] has no cards then [d] is
    returned. *)
val pop : deck -> deck

(** [next d] is [Some c] if deck [d] has a next card [c], else [None]. *)
val next : deck -> card option

(** [add d c] is deck [d] with card [c] on top. *)
val add : deck -> card -> deck

(** [reshuffle d lst] is deck [d] with all cards in list [lst] added, shuffled. *)
val reshuffle : deck -> card list -> deck

(** [count d] is the number of cards in deck [d]. *)
val count : deck -> int

(** [string_of_suit c] is the string representation of the suit of a single card
    [c]. *)
val string_of_suit : suit -> string

(** [string_of_rank c] is the string representation of the rank of a single card
    [c]. *)
val string_of_rank : rank -> string

(** [card_display] is the list of functions that display a card. *)
val card_display : (card -> string) list

(** [string_of_card c] is string representation of a single card [c]. *)
val string_of_card : card -> string

(** [display_cards_list lst] is string representation of a list of card [lst]. *)
val display_cards_list : card list -> string