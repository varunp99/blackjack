
(** [blackjack_display] is a string of ascii art for "BlackJack" *)
let blackjack_display = 
  "\n\n" ^
  " ____    _                  _           _                  _\n" ^
  "|  _ \\  | |                | |         | |                | |    \n" ^
  "| |_) | | |   __ _    ___  | | __      | |   __ _    ___  | | __ \n" ^
  "|  _ <  | |  / _` |  / __| | |/ /  _   | |  / _` |  / __| | |/ / \n" ^
  "| |_) | | | | (_| | | (__  |   <  | |__| | | (_| | | (__  |   <  \n" ^
  "|____/  |_|  \\__,_|  \\___| |_|\\_\\  \\____/   \\__,_|  \\___| |_|\\_\\ \n\n" 

(** [instructions] is a string of the game's instructions. *)
let instructions =
  "\n\n--------------------" ^
  "\n\nINSTRUCTIONS:" ^
  "\nAll cards are worth their face value except: an Ace can be 1 or 11 and \
   picture cards are worth 10." ^
  "\n\nThe monetary values for the chips are: Red $5;  Blue $10;  Green $25;  \
   Black $100." ^
  "\nEach player starts with $1000 worth of chips." ^
  "\n\nEnter 'hit' to get another card." ^
  "\nEnter 'stand' to end your turn." ^
  "\nEnter 'split' to split your cards. You can only split if you have 2 cards \
   of the same rank and you can only do so once per round." ^
  "\nEnter 'doubledown' to double your bet. You can only doubledown \
   when you have 2 cards in your hand." ^
  "\nEnter 'info' to get information about all the players." ^
  "\nThe aim of the game is to have cards with a score higher than the dealer \
   without exceeding 21. If your inital cards are worth 21, you have Blackjack!\n\n"

(** [bet_str st] is a string of the current player's chips in [st], along with
    the prompt for the player to enter their bet. *)
let bet_str st =
  "\n\n" ^ State.current_player_name st ^ ", you have the following chips:\n" ^ 
  (State.current_player_bank st |> Chips.string_of_chips) ^
  "\nPlease enter exactly 4 numbers, indicating how many chips of Red, Blue, \
   Green and Black you want to bet." ^
  "\nE.g. '1 1 1 0' would bet 1 chip of every denomination except Black." ^
  "\n> "

(** [game_mode st] coordinates all parts of the game for state [st] and interacts
     with the user's inputs. *)
let rec game_mode st =
  if State.current_player_dealer st then game_mode_dealer st
  else
    begin
      print_string "\n\n--------------------";
      ANSITerminal.(print_string [green]
                      ("\n\nIt is "^State.current_player_name st^"'s turn:"));
      print_string ("\n\n" ^ State.current_player_info st);
      print_string ("\n\nPlay the next move " ^ (State.current_player_name st));
      print_string "\n> ";
      match Command.parse (read_line ()) with
      | Command.Info ->
        begin
          State.all_info st |> print_string;
          game_mode st
        end
      | Command.Hit -> game_mode_hit st
      | Command.Stand -> game_mode_stand st
      | Command.Split -> game_mode_split st
      | Command.Doubledown -> game_mode_doubledown st
      | exception Command.Empty -> game_mode_empty st
      | exception Command.Malformed -> game_mode_malformed st
    end

(** [game_mode_hit st] handles a hit command from the user and returns to
    [game_mode] with a new state for the next play. *)
and game_mode_hit st =
  match State.hit st with
  | State.LegalHit new_st -> game_mode new_st
  | State.BustHit (new_st, str) ->
    begin
      ANSITerminal.(print_string [red] "\nYou went bust!");
      print_string ("\n\n" ^ str);
      game_mode new_st
    end

(** [game_mode_stand st] handles a stand command from the user and returns to
    [game_mode] with a new state for the next play. *)
and game_mode_stand st =
  ANSITerminal.(print_string [yellow] "\nYou have ended your turn as \
                                       follows:\n\n");
  print_string (State.current_player_info st);
  let new_st = State.stand st in
  game_mode new_st

(** [game_mode_split st] handles a split command from the user and returns to
    [game_mode] with a new state for the next play. *)
and game_mode_split st =
  match State.split st with
  | State.LegalSplit new_st ->
    begin
      ANSITerminal.(print_string [yellow] "\n\nYour cards have been split.");
      game_mode new_st
    end
  | State.CostlySplit ->
    begin
      ANSITerminal.(print_string [red] "\n\nYou do not have enough money to \
                                        split :(");
      game_mode st
    end
  | State.IllegalSplit ->
    begin
      ANSITerminal.(print_string [red] "\n\nYou cannot split with that hand!");
      game_mode st
    end

(** [game_mode_doubledown st] handles a doubledown command from the user and
    returns to [game_mode] with a new state for the next play. *)
and game_mode_doubledown st =
  match State.doubledown st with
  | LegalDD new_st -> 
    begin
      ANSITerminal.(print_string [yellow] "\n\nYour bet has been doubled.");
      game_mode new_st
    end
  | LateDD ->
    begin
      ANSITerminal.(print_string [red] "\n\nYou cannot doubledown now!");
      game_mode st
    end
  | IllegalDD ->
    begin
      ANSITerminal.(print_string [red] "\n\nYou do not have enough money to \
                                        doubledown :(");
      game_mode st
    end

(** [game_mode_empty st] handles an empty command from the user and returns to
    [game_mode] with a new state for the next play. *)
and game_mode_empty st =
  ANSITerminal.(print_string [red] "\n\nYou entered nothing. Try again!");
  print_string instructions;
  game_mode st

(** [game_mode_malformed st] handles a malformed command from the user and
    returns to [game_mode] with a new state for the next play. *)
and game_mode_malformed st =
  ANSITerminal.(print_string [red] "\n\nYour entry was illegal. Try again!");
  print_string instructions;
  game_mode st

(** [game_mode_dealer st] simulates the dealer's play for state [st] and thus
    ends the round. It returns to [game_mode] if the user desires to play another
    round, else exits the program. *)
and game_mode_dealer st =
  print_string "\n\n--------------------";
  ANSITerminal.(print_string [green] "\n\nEveryone has played their turn. \
                                      It is now the dealer's chance.");
  print_string (State.current_dealer_info st);
  let new_st = bot_mode st in
  ANSITerminal.(print_string [red] "\n\nLet the game begin!");
  print_string (State.all_info new_st);
  game_mode new_st

(** [bot_mode st] deals cards for the dealer and ends the round in state [st]. *)
and bot_mode st = match State.current_player_score st with
  | n when n > 16 ->
    begin
      print_string (State.current_dealer_info st);
      ANSITerminal.(print_string [green]
                      "\n\nThe dealer's score exceeds 16 and so will not hit \
                       further.");
      end_state st
    end
  | n ->
    begin
      ANSITerminal.(print_string [yellow] "\n\nThe Dealer hit.");
      match State.hit st with
      | State.LegalHit new_st ->
        begin
          bot_mode new_st
        end
      | State.BustHit (new_st, _) ->
        begin
          print_string (State.current_dealer_info new_st);
          end_state new_st
        end
    end

(** [end_state st] decides winners and losers in state [st] and distributes money
    accordingly. The discarded cards are added to play if there are less than
    52 cards in play. All the cards are reshuffled. Then, a new round is begun
    if the user desires.
    Requires: current player in [st] is the dealer. *)
and end_state st =
  let new_st = match State.current_player_score st with
    | n when n > 21 -> 
      begin
        ANSITerminal.(print_string [red] "\n\nThe dealer went bust!");
        print_string "\nEveryone who did not go bust wins!";
        State.state_dealer_bust st |> State.discard_cards |> State.check_deck
      end
    | n when n < 21 ->
      begin
        ANSITerminal.(print_string [green]
                        "\n\nPlayers with a score higher than \
                         the dealer's win an amount equal to their bet!");
        ANSITerminal.(print_string [yellow]
                        "\nPlayers with a score equal to the dealer's are \
                         returned their bet.");
        ANSITerminal.(print_string [red]
                        "\nPlayers with a score lower than the dealer's lose \
                         their best :(");
        State.state_dealer_stand st |> State.discard_cards |> State.check_deck
      end
    | _ ->
      begin
        ANSITerminal.(print_string [red] "\n\nThe dealer has 21!");
        print_string "\nEveryone who has 21 is returned their bet, while others \
                      lose.";
        State.state_dealer_blackjack st |> State.discard_cards |> State.check_deck
      end in
  next_round new_st

(** [next_round st] prompts the players for another round in state [st]. If the
    user wishes to do so then they are asked for new bets and a new state
    is returned. Else, the program is exited. *)
and next_round st =
  print_string (State.bank_info st);
  print_string "\n\nWould you like to continue with another round?";
  print_string "\nEnter 'Y' to play another round or anything else to quit the \
                game";
  print_string "\n> ";
  match read_line () with
  | "Y" ->
    begin
      let new_st = State.init_again st in
      let player_lst = State.player_name_list new_st in
      if List.length player_lst = 0 then
        begin
          ANSITerminal.(print_string [red] "\n\nEveryone went bankrupt!");
          print_string "\n\n\nThank you for playing! We hope to see you in our \
                        casino again\n\nGOODBYE!\n\n\n";
          exit 0
        end
      else
        begin
          ANSITerminal.(print_string [green] "\n\nIt is time to place your bets!");
          bet new_st (List.length player_lst)
        end
    end
  | _ ->
    begin
      print_string "\n\nAre you sure you quit? Enter 'Y' to confirm or anything \
                    else to go back";
      print_string "\n> ";
      match read_line () with
      | "Y" ->
        begin
          print_string "\n\n\nThank you for playing! We hope to see you in our \
                        casino again\n\nGOODBYE!\n\n\n";
          exit 0
        end
      | _ -> next_round st
    end

(** [bet st n] prompts all players to bet and returns an altered state [st]
     after all players have done so.
    Requires: [n] is the number of users playing (current version allows
    strictly 4). *)
and bet st n = match n with
  | 0 -> st |> State.first_player
  | n -> bet (take_bet st) (n - 1)

(** [take_bet st] prompts the current player to bet and returns an altered state
     [st] after they have done so. *)
and take_bet st =
  bet_str st |> print_string;
  let bet_lst = List.combine Chips.([Red; Blue; White; Black]) (chip_list st) in
  match State.bet st bet_lst with
  | State.LegalBet new_st -> new_st
  | State.IllegalBet ->
    begin
      ANSITerminal.(print_string [red]
                      "\n\nYou don't have so many chips :( Please try again!");
      take_bet st
    end

(** [chip_list st] reads the user's input and returns a list of ints that
    indicates how many chips the current player wants to bet for each
    denomination. *)
and chip_list st = match read_line () |> Command.parse_bet with
  | Legal lst -> lst
  | Negative
  | Zero ->
    begin
      ANSITerminal.(print_string [red] "\n\nPlease enter a positive amount!");
      bet_str st |> print_string ;
      chip_list st
    end
  | exception Command.Malformed ->
    begin
      ANSITerminal.(print_string [red]
                      "\n\nThat was an invalid input. Please try again!");
      bet_str st |> print_string;
      chip_list st
    end

(** [main ()] prompts for the game to start and then starts it.
    Raises: [Failure] if the names are not entered as prompted. *)
let main () =
  ANSITerminal.resize 150 80;
  ANSITerminal.(print_string [red] blackjack_display);
  print_string "Enter the names of the 4 players with a single space between \
                each. Only 4 one word names!";
  print_string "\n> ";
  let names = read_line () |> String.split_on_char ' ' in
  if List.length names <> 4 || List.mem "" names
  then
    begin
      print_string "\n\nNames not entered corretly. Start the game again!\n\n";
      exit 0
    end
  else let st = (State.init names 4) in
    ANSITerminal.erase Screen;
    print_string " __      __        __                               ";
    print_string "\n/  \\    /  \\ ____ |  |   ____  _____   _____   ____  ";
    print_string "\n\\   \\/\\/   // __ \\|  | _/ ___\\/  _  \\ /     \\_/ __ \\";
    print_string "\n \\        /\\ ____/|  |_\\  \\__(  (_)  )  Y Y  \\  ___/ ";
    print_string "\n  \\__/\\__/  \\_____|____/\\_____\\_____/|__|_|__/\\_____";
    ANSITerminal.(print_string [yellow] (instructions ^ "--------------------"));
    print_string "\n\nIt is time to place your bets!";
    let bet_st = bet st (List.length names) in
    ANSITerminal.erase Screen;
    ANSITerminal.(print_string [red] "\n\nLet the game begin!");
    print_string (State.all_info bet_st);
    game_mode bet_st

let () = main ()