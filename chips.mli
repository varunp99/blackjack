(** A representation of chips and associated functions. *)

(** A representation of a casino chip. *)
type chip = Red | Blue | White | Black

(** A representation of a set of chips. All types of [chip] are included
    along with how many of each chip the set contains. *)
type chips

(** Raised when there are insufficient funds in the bank. *)
exception InsufficientFunds

(** [create_chips] is a set of chips with no chips. *)
val create_chips : chips

(** [chips_value b] is the total monetary value of chips [b]. *)
val chips_value : chips -> int

(** [string_of_chips b] is the string representation of chips [b]. *)
val string_of_chips : chips -> string

(** [to_list b] is a (chip * int) list representation of the types and number of
    chips in chips [b].
    E.g. [to_list b] is [(Red, 1); (Blue, 2)] if there is 1 red chip and 2 blue
    chips in [b]. *)
val to_list : chips -> (chip * int) list

(** [inc_chips c n b] is chips [b] with [n] more chips of type [c]. *)
val inc_chips : chip -> int -> chips -> chips

(** [dec_chips c n b] is chips [b] with [n] less chips of type [c].
    Raises [InsufficientFunds] if [b] does has less than [n] chips of type [c]. *)
val dec_chips : chip -> int -> chips -> chips

(** [compare x y] is analogus to [Stdlib.compare] for chips [x] and [y]. *)
val compare : chips -> chips -> int