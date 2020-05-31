(** This is the test file for this project. Tests for State and Main were
    done by playing the game multiple times and trying out all scenarios. *)

open OUnit2

let deck1 = Card.create_unshuffled
let deck2 = Card.create
let card2 = match Card.next deck1 with 
  | Some c -> c
  | None -> failwith "testfailed"
let card1 = match Card.next (Card.pop deck1) with 
  | Some c -> c
  | None -> failwith "testfailed"
let card3 = match Card.next deck2 with
  | Some c -> c
  | None -> failwith "testfailed"

(** [make_test name exp_out f x] creates an OUnit test named [name]
    that asserts [f x] with [exp_out]. *)
let make_test name exp_out f x =
  name >:: (fun _ -> assert_equal exp_out (f x))

(** [make_2f_test name exp_out f2 f1 x] creates an OUnit test named [name]
    that asserts [x |> f1 |> f2] with [exp_out]. *)
let make_2f_test name exp_out f2 f1 x =
  name >:: (fun _ -> assert_equal exp_out (x |> f1 |> f2))

(** [make_2a_test name exp_out f1 x1 x2] creates an OUnit test named [name]
    that asserts [f1 x1 x2] with [exp_out]. *)
let make_2a_test name exp_out f1 x1 x2 =
  name >:: (fun _ -> assert_equal exp_out (f1 x1 x2))

(** [make_raise_test name exp_err f x] creates an OUnit test named [name]
    that asserts the error raised by [f1 x1 x2] with [exp_err]. *)
let make_raise_test name exp_err f x =
  name >:: (fun _ ->
      assert_raises exp_err (fun () -> f x))

let card_tests = [
  make_2f_test "getSuit" "Diamonds" Card.string_of_suit Card.suit card1;
  make_2f_test "getRank" "2" Card.string_of_rank Card.rank card1;
  make_test "score" 2 Card.score card1;
  make_test "count" 52 Card.count deck1;
  make_2f_test "count and combine" 104 Card.count Card.combine [deck1; deck1];
  make_2f_test "count and pop" 51 Card.count Card.pop deck1;
  make_test "is_ace_true" true Card.is_ace card2;
  make_test "is_ace_false" false Card.is_ace card1;
  make_2f_test "ace_one" 1 Card.score Card.ace_one card2;
  make_2f_test "ace_eleven" 11 Card.score Card.ace_eleven card2;
  "card.create (shuffled)" >:: 
  (fun _ -> assert_equal false ((Card.score card3)=(Card.score card2)));
  "add" >:: (fun _ ->
      let card4 = match Card.add (Card.pop deck1) card1 |> Card.next with 
        | Some c -> c
        | None -> failwith "testfailed" in
      assert_equal 2 (Card.score card4));
]

(* Variables for chip tests *)
let chips1 = Chips.create_chips
let chips2 = Chips.inc_chips Chips.Red 5 chips1
let chips1_rep = 
  [(Chips.Red, 0); (Chips.Blue, 0); (Chips.White, 0); (Chips.Black, 0)]
let chips2_rep = 
  ([(Chips.Red, 5); (Chips.Blue, 0); (Chips.White, 0); (Chips.Black, 0)])

let chips_tests = [
  make_test "chips_value" 0 Chips.chips_value chips1;
  make_test "to_list" chips1_rep Chips.to_list chips1;
  make_test "inc_chips" chips2_rep Chips.to_list
    (Chips.inc_chips Chips.Red 5 chips1);
  make_test "dec_chips" chips1_rep Chips.to_list
    (Chips.dec_chips Chips.Red 5 chips2);
  "dec_chips_failure" >:: (fun _ ->
      assert_raises (Chips.InsufficientFunds) (fun () ->
          Chips.to_list (Chips.dec_chips Chips.Red 5 chips1)));
  make_2a_test "compare_0" 0 Chips.compare chips1 chips1;
  make_2a_test "compare_-1" (-1) Chips.compare chips1 chips2;
  make_2a_test "compare_1" 1 Chips.compare chips2 chips1;
]

(* Variables for player tests. *)
let player1 = Player.create "Player 1" 1
let new_player_chips = 
  ([(Chips.Red, 10); (Chips.Blue, 10); (Chips.White, 10); (Chips.Black, 6)])
let player2 = Player.add_card true card1 player1
let player3 = Player.update_bet true chips2_rep player1
let player_chips = 
  ([(Chips.Red, 5); (Chips.Blue, 10); (Chips.White, 10); (Chips.Black, 6)])

let player_tests = [
  make_test "player name" "Player 1" Player.name player1;
  make_test "player id" 1 Player.id player1;
  make_2a_test "new player cards fst" [] Player.cards true player1;
  make_2a_test "new player cards snd" [] Player.cards false player1;
  make_2a_test "new player score fst" 0 Player.score true player1;
  make_2a_test "new player score snd" 0 Player.score false player1;
  make_2f_test "new player chips" new_player_chips Chips.to_list Player.chips
    player1;
  "new player bet fst" >:: 
  (fun _ -> assert_equal chips1_rep 
      (Chips.to_list((Player.bet true player1))));
  "new player bet snd" >:: 
  (fun _ -> assert_equal chips1_rep 
      (Chips.to_list((Player.bet false player1))));
  make_test "has_spilt_false" false Player.has_split player1;
  "add_card fst" >::
  (fun _ -> assert_equal [card1] 
      (Player.cards true (Player.add_card true card1 player1)));
  make_2a_test "new score" 2 Player.score true player2;
  make_2f_test "update_bet chips" player_chips Chips.to_list Player.chips player3;
  "update_bet chips" >:: 
  (fun _ -> assert_equal chips2_rep (Chips.to_list(Player.bet true player3)));
  "remove cards" >:: 
  (fun _ -> assert_equal [] 
      (Player.cards true (Player.remove_cards true player2)));
]

let command_tests = [
  make_test "hit parse" Command.Hit Command.parse "hit";
  make_test "hit parse caps" Command.Hit Command.parse "HIT";
  make_test "hit parse space" Command.Hit Command.parse "   hit  ";
  make_test "info parse" Command.Info Command.parse "info";
  make_test "stand parse" Command.Stand Command.parse "stand";
  make_test "split parse" Command.Split Command.parse "split";
  make_test  "doubledown" Command.Doubledown Command.parse "doubledown";
  make_raise_test "doubled down malformed" Command.Malformed 
    Command.parse "double down";
  make_raise_test "malformed" Command.Malformed 
    Command.parse "blackjack   ";
  make_raise_test "empty" Command.Empty Command.parse "";
  make_raise_test "only spaces" Command.Empty Command.parse "   ";
  make_test "parse bet legal" Command.(Legal [1; 1; 1; 1]) Command.parse_bet
    "1 1 1 1";
  make_test  "parse bet negative" Command.Negative Command.parse_bet "1 1 1 -1";
  make_test "parse bet zero" Command.Zero Command.parse_bet "0 0 0 0";
  make_raise_test "parse bet malformed" Command.Malformed 
    Command.parse_bet "1 1 1";
  make_raise_test "parse bet malformed no space" Command.Malformed 
    Command.parse_bet "1111";
  make_raise_test "parse bet malformed empty" Command.Malformed 
    Command.parse_bet "";
]

let suite = "search test suite" >::: List.flatten [
    card_tests;
    chips_tests;
    player_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite