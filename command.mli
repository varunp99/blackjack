(** A representation of commands. This module parses the user's text input into
    a command (in the case of gameplay) and a bet_status (in the case of placing
    bets). *)

(** A representation of a player's input. *)
type command =
  | Info
  | Hit
  | Stand
  | Split
  | Doubledown

(** A representation of a player's intended bet. *)
type bet =
  | Legal of int list
  | Negative
  | Zero

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is parsed. *)
exception Malformed

(** [parse str] parses [str] into a [command]. The following strings are
    acceptable: "info", "hit", "stand", "split" and "doubledown". Examples:
    - [parse "info"] is [Info].
    - [parse "hit"] is [Hit].
    - [parse "stand"] is [Stand].
    - [parse "split"] is [Split].
    - [parse "doubledown"] is [Doubledown].
      Requires: [str] contains only letters (A-Z, a-z), space characters and a
      single word. Other spaces and the character's cases are both irrelevant.
      Raises: [Empty] if [str] is the empty string or contains only spaces. 
      Raises: [Malformed] if the [str] is not acceptable. *)
val parse : string -> command

(** [parse_bet str] parses [str] into a [bet_status]. Examples:
    - [parse_bet "1 1 1 1"] is [Legal lst] where [lst] is [1; 1; 1; 1].
    - [parse_bet "-1 1 1 1"] is [Negative] since >= 1 of the 4 ints are negative.
    - [parse_bet "0 0 0 0"] is [Zero] since all 4 ints are 0.
      Requires: [str] is 4 ints with > 1 space(s) between each.
      Raises: [Malformed] if [str] does not have 4 ints. *)
val parse_bet : string -> bet