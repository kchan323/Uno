open State
open Command
open Cards

(* Raised when user does not enter a valid (2-4) number of players *)
exception InvalidPlayers

(* Raised when user does not enter a valid (Normal or Special) version of the 
   game to play *)
exception InvalidGameVersion

(** [get_next_line in_channel] is a string option describing if there is
    another line to read from [in_channel]. *)
let get_next_line in_channel =
  match Stdlib.input_line in_channel with 
  | s -> Some s
  | exception End_of_file -> None

(** [parse_file in_channel color] parses the opened file. *)
let rec parse_file in_channel color =
  let next_line = get_next_line in_channel in
  match next_line with
  | Some s -> ANSITerminal.(print_string [Foreground color; Bold] s); 
    print_newline();
    parse_file in_channel color
  | None -> Stdlib.close_in in_channel

(** [print_file file color] opens the [file] in the color [color] *)
let print_file file color = 
  let in_channel = Stdlib.open_in file in
  parse_file in_channel color

(** [generate_card is_normal] is a random Normal or Special card if 
    [is_normal] is true. Otherwise, it is a random Normal,Special, or 
    extraSpecial card. *)
let generate_card is_normal = 
  if is_normal then
    List.hd (generate_cards 1 [] false) else List.hd (generate_cards 1 [] true)

(** [generate_hand is_normal] is the generated cards according to the 
    specifications of the chosen game version, [is_normal]. *)
let generate_hand is_normal  = 
  if is_normal then
    generate_cards 7 [] false else generate_cards 7 [] true

(** [generate_hand_test_uno is_normal] is two generated cards according to the 
    specifications of the chosen game version [is_normal]. Used to test the
    win condition and that calling uno is properly working. *)
let generate_hand_test_uno is_normal = 
  if is_normal then
    generate_cards 2 [] false else generate_cards 2 [] true

(** [is_normal] is true if the user chose the normal version of the 
    game and false if the user chose the special version. *)
let rec is_normal = 
  ANSITerminal.erase(Above); 
  ANSITerminal.resize 156 60;
  print_file "welcome.txt" Black;
  print_file "uno1.txt" Blue;
  print_file "start.txt" Red;
  match read_line () with
  | _ ->  print_file "normal_special.txt" Black; print_string  "> ";
    let input = read_line () in 
    let input_lower = String.lowercase_ascii input in
    if input_lower = "n" then true else if input_lower = "s" then false 
    else raise InvalidGameVersion

(** [print_list list] displays the elements in the list [list]. Used for testing
    purposes. *)
let rec print_list = function 
    [] -> ()
  | e::l -> print_string e ; print_string " " ; print_list l

(** [print_players list] is true and displays the information in [list]. Used
    for testing purposes. *)
let rec print_players list = 
  match list with
  | [] -> true
  | h :: t -> print_endline (h.name);  
    let random_cards_lst = List.map (fun card -> string_card card) h.cards in
    let string_random_cards = String.concat ", " random_cards_lst in 
    let _ = print_endline string_random_cards in print_players t

(** [print_state state] is true and displays the information in [state]. Used
    for testing purposes. *)
let print_state state = 
  print_endline ("Players and cards: "); let _ = print_players state.players in 
  print_endline ("Top card: " ^ string_card state.top_card);
  print_endline ("Number of drawn cards: " ^ string_of_int 
                   state.num_of_drawn_cards);
  print_endline ("Index: " ^ string_of_int state.index);
  print_endline ("Uno called players and cards: ");
  print_players state.uno_called 

(** [insert_players state lst is_normal] is the state after the players in 
    [lst] have been inserted. *)
let rec insert_players state lst is_normal= 
  match lst with 
  | [] -> state
  | h::t -> let hand = generate_hand is_normal in 
    let player = {name = h; cards = hand} in 
    insert_players {state with players = player::state.players} t is_normal

(** [make_player_list num list] is list of usernames. *)
let rec make_player_list num list =
  if num > 0 then let _ = print_endline("Enter player" ^ 
                                        (string_of_int ((List.length list)+ 1))^
                                        " name (no spaces & must be unique): ") 
    in let name = read_line () in make_player_list (num-1) (name::list) else 
    list

(** [main_helper num_players is_normal] is the state after initializing the 
    game with [num_players]. *)
let main_helper num_players is_normal = 
  let num = int_of_string num_players in 
  if num > 4 || num < 2 then raise InvalidPlayers else 
    let list = make_player_list num [] in 
    let empty_state = {players = []; top_card = generate_first_card;
                       num_of_drawn_cards = 0; index = 0; 
                       uno_called = []; notes = ""; messages = []} in 
    insert_players empty_state list is_normal

(** [print_card_list list] displays the cards in the [list]. *)
let rec print_card_list = function 
  | [] -> ()
  | Normal (color, num) :: t  -> 
    print_endline ((string_color color) ^ (string_of_int num)); 
    print_card_list t 
  | Special (color, special) :: t -> 
    print_endline ((string_color color) ^ (string_special special)); 
    print_card_list t 
  | ExtraSpecial (color, extra_special) :: t -> 
    print_endline ((string_color color) ^ (string_extra_special extra_special));
    print_card_list t

(** [descrip_of_players_cards players_lst] displays the names and number of 
    cards of [players_lst]. *)
let rec descrip_of_players_cards players_lst = 
  match players_lst with 
  |[] -> ()
  |h::t -> print_endline(h.name ^ " : " ^ string_of_int (List.length h.cards));
    descrip_of_players_cards t

(** [players_names_to_list uno_player] displays the name of [uno_player]. *)
let rec players_names_to_list uno_player = 
  match uno_player with 
  |[] -> ()
  |h::t -> print_string(h.name ^ " "); players_names_to_list t

(** [match_ready_finish  player is_ready] is true to be ready (if [is_ready] is
    true and finished otherwise) if [player] inputs "y" and false if "n". *)
let rec match_ready_finish player is_ready= 
  match String.lowercase_ascii (read_line ()) with 
  | "y" ->  true 
  | "n" -> ready_finish player is_ready
  | _ -> let msg = "Invalid input. Try again" in 
    ANSITerminal.(print_string [Foreground Red; Bold] msg); false

(** [ready_finish player is_ready] is true if [player] inputs that they are 
    ready to start their turn if [is_ready] is true or done with their turn 
    if [is_ready] is false. *)
and ready_finish player is_ready = 
  if is_ready then 
    let ready = player.name ^ ", are you ready? Enter (y/n)" in 
    ANSITerminal.(print_string [Foreground Black; Bold] ready);
    print_newline();
    print_string(">");
    match_ready_finish  player is_ready 
  else 
    let finished = player.name ^ ", are you done? Enter (y/n)" in 
    ANSITerminal.(print_string [Foreground Black; Bold] finished); 
    print_newline();
    print_string(">");
    match_ready_finish  player is_ready

(** [string_to_ansi color string] displays the [string] in the color [color]. *)
let rec string_to_ansi color string =
  match color with 
  | Red -> ANSITerminal.(print_string [Foreground Red; Bold] string)
  | Yellow -> ANSITerminal.(print_string [Foreground Yellow; Bold] string)
  | Green -> ANSITerminal.(print_string [Foreground Green; Bold] string)
  | Blue -> ANSITerminal.(print_string [Foreground Blue; Bold] string)
  | Undefined -> let new_color = random_color (Random.int 4) in 
    string_to_ansi new_color string

(** [print_hand_top list] displays the top of the cards in [list] in a picture 
    format. *)
let rec print_hand_top = function
  | [] -> print_newline();
  | Normal (color, _) :: t
  | Special (color, _) :: t
  | ExtraSpecial (color, _) :: t -> string_to_ansi color " _______  "; 
    print_hand_top t

(** [print_hand_edges list] displays the edges of the cards in [list] in a 
    picture format. *)
let rec print_hand_edges = function
  | [] -> print_newline();
  | Normal (color, _) :: t
  | Special (color, _) :: t 
  | ExtraSpecial (color, _) :: t -> string_to_ansi color "|       | "; 
    print_hand_edges t

(** [special_abbrv special] is the string abbreviation of [special]. *)
let rec special_abbrv = function
  | Skip -> "  SKP  "
  | Reverse -> "  REV  "
  | Draw2 -> "  D2   "
  | Wildcard -> " WILD  "
  | WildcardDraw4 -> "WILD-D4"

(** [extra_special_abbrv extra_special] is the string abbreviation of 
    [extra_special]. *)
let rec extra_special_abbrv = function
  | Swap -> " SWAP  "
  | DiscardAll -> "DISCALL"
  | SkipEveryone -> "SKPEVRY"
  | Draw1 -> "  D1   "

(** [print_hand_value list] displays the value of each card in [list] in a 
    picture format. *)
let rec print_hand_value = function
  | [] -> print_newline();
  | Normal (color, int) :: t -> 
    string_to_ansi color ("|   " ^ (string_of_int int) ^ "   |" ^ " "); 
    print_hand_value t
  | Special (color, special) :: t -> 
    let _ = if (color = Undefined) then 
        ANSITerminal.(print_string [Foreground Black; Bold] 
                        ("|" ^ (special_abbrv special) ^ "|" ^ " "))
      else string_to_ansi color ("|" ^ (special_abbrv special) ^ "|" ^ " ") in
    print_hand_value t
  | ExtraSpecial (color, extra_special) :: t -> 
    let _ = if (color = Undefined) then 
        ANSITerminal.(print_string [Foreground Black; Bold] 
                        ("|" ^ (extra_special_abbrv extra_special) ^ "|" ^ " "))
      else string_to_ansi color ("|" ^ (extra_special_abbrv extra_special) 
                                 ^ "|" ^ " ") in print_hand_value t

(** [print_hand_bottom list] displays the bottom of the cards in [list] in a 
    picture format. *)
let rec print_hand_bottom = function
  | [] -> print_newline();
  | Normal (color, _) :: t
  | Special (color, _) :: t 
  | ExtraSpecial (color, _) :: t -> string_to_ansi color "|_______| "; 
    print_hand_bottom t

(** [split_list list final acc count] divides list [list] into a list of 
    several lists with a maximum of ten elements in each list, [final] is the
    list list accumulator, [acc] is the list accumulator, and [count] keeps
    track of a maximum of ten elements in list [acc]. *)
let rec split_list list final acc count = 
  match list with
  | [] -> if count = 0 then final else (List.rev acc) :: final
  | h :: t -> if count < 10 then split_list t final (h :: acc) (count + 1) 
    else split_list t ((List.rev acc) :: final) (h :: []) 1

(** [print_hand_list card_list] displays the cards in [card_list] in a picture 
    format. *)
let rec print_hand_list card_list = 
  print_hand_top card_list;
  print_hand_edges card_list;
  print_hand_value card_list;
  print_hand_edges card_list;
  print_hand_bottom card_list

(** [print_hands list] displays the cards in [list] in a picture format. *)
let rec print_hands list = 
  match list with
  | [] -> ()
  | h :: t -> if h = [] then print_hands t else
      (print_hand_list h; print_newline(); print_hands t)

(** [display_commands _] displays information about the commands. *)
let display_commands _ = 
  ANSITerminal.(print_string [Bold; Foreground Magenta] "COMMANDS:");
  print_newline(); 
  print_endline("drop [card] | draw | dropuno [card] | uno_on [player] | quit");
  print_endline("--------------------------------------------------------------\
                 --------------------------------------------------------------\
                 --------------------------------");
  ANSITerminal.(print_string [Bold; Foreground Blue] "DESCRIPTION:");
  print_newline()

(** [display_player_info state] displays information about the players in 
    [state]. *)
let display_player_info state =     
  ANSITerminal.(print_string [Bold; Foreground Black] 
                  "PLAYER ORDER + CARD COUNT");
  print_newline(); descrip_of_players_cards state.players;
  print_newline(); ANSITerminal.(print_string [Bold; Foreground Black] 
                                   "Players that have called UNO!: ");
  players_names_to_list state.uno_called; print_newline();
  ANSITerminal.(print_string [Bold; Foreground Black] 
                  "Number of drawn cards: ");
  print_string(string_of_int state.num_of_drawn_cards); print_newline();
  print_endline("------------------------------------------------------------\
                 ------------------------------------------------------------\
                 ------------------------------------")

(** [display_top_card state] displays information about the top card in 
    [state]. *)
let display_top_card state =    
  ANSITerminal.(print_string [Bold; Foreground Black] "TOP CARD:");
  print_newline(); print_hand_top (state.top_card :: []);
  print_hand_edges (state.top_card :: []); 
  print_hand_value (state.top_card :: []);
  print_hand_edges (state.top_card :: []);
  print_hand_bottom (state.top_card :: []); print_newline();
  print_endline(string_card (state.top_card)); print_newline()

(** [display_hand curr_player] displays the hand of the [curr_player]. *)
let display_hand curr_player =     
  ANSITerminal.(print_string [Bold; Foreground Cyan] "YOUR HAND:");
  print_newline(); let card_list = List.sort compare curr_player.cards in
  let split_card_list = List.rev (split_list card_list [[]] [] 0) in
  print_hands split_card_list; print_card_list (card_list);
  print_endline("------------------------------------------------------------\
                 ------------------------------------------------------------\
                 ------------------------------------")

(** [display_description state curr_player] displays information about the 
    state like the top card, # of drawn cards, player order and their card 
    counts and prompts the [curr_player] for their next move. *)
let display_description state curr_player =     
  display_commands ();
  display_top_card state;
  display_player_info state;
  display_hand curr_player;
  let move = curr_player.name ^ ", enter your move" in 
  ANSITerminal.(print_string [Bold; Foreground Black] move);
  print_newline();
  print_string (">")

(** [update_uno_lst uno_lst state] is the updated uno_on user list that has 
    removed users if they no longer called uno. *)
let rec update_uno_lst player_lst uno_lst acc =
  match player_lst with 
  | [] -> acc
  | h :: t -> 
    if (List.length h.cards > 1 && List.mem h uno_lst) then 
      update_uno_lst t uno_lst acc  
    else if (List.mem h uno_lst) then update_uno_lst t uno_lst (h::acc)
    else  update_uno_lst t uno_lst acc

(** [win_condition player] is true if [player] has won the game, else false *)
let win_condition player =
  if (List.length player.cards = 0) then
    let congrats = "🌟👏CONGRATULATIONS!!" ^ player.name ^ " has won 🥇!!! 🌟"
    in ANSITerminal.(print_string [Foreground Magenta; Bold; Blink] congrats);
    true else false

(** [find_index_of_player name lst] is the index of the player[name]. *)
let rec find_index_of_player name lst=
  match lst with 
  | [] -> raise (Failure "Not Found") 
  | h::t -> if h = name then 0 else 1 + find_index_of_player name t

(** [print_user_messages list curr_player acc] prints the messages in list 
    [list] that are directed to the current player [curr_player] and is a new 
    list of messages where the messages printed are removed. *)
let rec print_user_messages list curr_player acc = 
  match list with
  | [] -> acc
  | (user, message) :: t -> if user.name = curr_player.name then
      let _ = print_endline message in print_user_messages t curr_player acc 
    else print_user_messages t curr_player ((user, message) :: acc)

(** [drop_legal_state is_special state drop_card is_normal] is the state after 
    [drop_card] has been dropped. *)
let drop_legal_state is_special state drop_card is_normal = 
  if is_special then special_card_action drop_card state.index state is_normal
  else let new_state = drop (List.nth state.players state.index) 
           drop_card state in 
    {new_state with index = update_index state.index state} 

(** [play_until_win state descrip is_normal] is the state after the [state]'s
    current player inputs a command. *)
let rec play_until_win state descrip is_normal = 
  let curr_player = List.nth state.players state.index in
  ANSITerminal.(print_string [Bold; Foreground Black] state.notes);
  print_newline();
  let new_messages = print_user_messages state.messages curr_player [] in 
  let new_state = {state with notes = ""; messages = new_messages} in
  let updated_uno_list = update_uno_lst state.players state.uno_called [] in
  let final_state = {new_state with uno_called = updated_uno_list} in
  if ready_finish curr_player true then 
    let _ = description_check descrip curr_player final_state in  
    let command = match read_line () with 
      | exception End_of_file -> exit 0
      | str -> str
    in let updated_state = updated_state_helper command final_state curr_player 
           is_normal in let is_finished = ready_finish curr_player false in 
    finish_check updated_state is_normal is_finished state 
  else play_until_win final_state true is_normal

(** [description_check descrip curr_player state] displays or doesn't display
    the description of the state depending on [descrip]. *)
and description_check descrip curr_player state =  
  if descrip then display_description state curr_player
  else let msg = "Please enter a valid command" in 
    ANSITerminal.(print_string [Foreground Black; Bold] msg); print_newline();
    print_string(">")

(** [finish_check updated_state is_normal is_finished state] is the state 
            that clears the screen if [is_finished]. *)
and finish_check updated_state is_normal is_finished state =  
  if is_finished then let _ = ANSITerminal.erase(Above) in 
    play_until_win updated_state true is_normal else 
    play_until_win state true is_normal

(** [updated_state_helper command state curr_player is_normal] is the state 
    [command] is called. *)
and updated_state_helper command state curr_player is_normal =
  match parse command with 
  | exception InvalidCommand -> let msg = "Invalid command. Try again" in 
    ANSITerminal.(print_string [Foreground Red; Bold] msg);
    play_until_win state false is_normal
  | Drop card -> drop_helper card state false state is_normal
  | Draw -> draw_helper state curr_player is_normal
  | Drop_uno card -> drop_uno_helper curr_player card state is_normal
  | Uno_on player -> uno_on_helper player curr_player state is_normal
  | Quit -> print_endline("Quitting game..."); exit 0

(** [drop_uno_helper curr_player card state is_normal] is the state after
    drop_uno is called. The [card] is dropped from the [curr_player]'s card and 
    they have declared UNO!.*)
and drop_uno_helper curr_player card state is_normal = 
  if List.length (curr_player.cards) = 2 then 
    let state_drop = drop_helper card state false state is_normal in 
    let new_curr_player = List.nth state_drop.players state.index in
    let uno_lst_updated = new_curr_player :: state_drop.uno_called in
    let msg = "You declared UNO!" in 
    ANSITerminal.(print_string [Foreground Cyan; Bold] msg); print_newline();
    {state_drop with uno_called = uno_lst_updated} 
  else let no_call_msg = "You can't call UNO! because you have more than 2 card\
                          s. Try again" in 
    ANSITerminal.(print_string [Foreground Red; Bold] 
                    no_call_msg); play_until_win state false is_normal

(** [check_drawn_cards state draw_card curr_player is_normal] is either the 
    exited state or state after draw is called depending on whether the 
    [state]'s number of drawn card limit is reached. *)
and check_drawn_cards state draw_card curr_player is_normal = 
  if state.num_of_drawn_cards < 49 then
    let msg = "You successfully drew " ^ string_card draw_card in
    ANSITerminal.(print_string [Foreground Green; Bold] msg); print_newline();
    drop_after_draw draw_card curr_player state is_normal
  else let _ = print_endline ("50 cards have been drawn. No one wins") in 
    exit 0 

(** [draw_helper state curr_player is_normal] is the state a random card is
    drawn and either placed into the [curr_player]'s deck or dropped as the new 
    top card. *)
and draw_helper state curr_player is_normal = 
  let draw_card = generate_card is_normal in 
  check_drawn_cards state draw_card curr_player is_normal

(** [drop_after_draw draw_card curr_player state is_normal] is the state after 
    [draw_card] is dropped if it matches the [state]'s topcard 
    and the [curr_player] inputs "y". *)
and drop_after_draw draw_card curr_player state is_normal =  
  let after_draw_state = draw state curr_player draw_card false in
  if (match_card draw_card state.top_card) then 
    let msg = "Would you like to drop " ^ string_card draw_card ^ 
              "? Enter (y/n)" in 
    ANSITerminal.(print_string [Foreground Black; Bold] msg); print_newline();
    print_string(">");
    let input = read_line () in 
    match String.lowercase_ascii input with 
    | "y" -> let new_state = draw state curr_player draw_card true in 
      let card_list = card_to_object_phrase draw_card in 
      drop_helper card_list new_state true after_draw_state is_normal
    | "n" -> after_draw_state
    | _ -> failwith("Won't be reached")
  else after_draw_state

(** [update_legal_drop state drop_card after_draw draw_state is_normal] is the 
    state after a legal [drop_card] matches with the [state]'s top card and 
    is dropped. *)
and update_legal_drop state drop_card after_draw draw_state is_normal = 
  let fin_state = drop_legal_state (is_special drop_card) state drop_card 
      is_normal in if (match_card drop_card state.top_card) then 
    let new_curr_user = List.nth fin_state.players state.index in
    if (win_condition new_curr_user = false) then 
      let drop_msg = "You successfully dropped it!" in
      ANSITerminal.(print_string [Foreground Green; Bold] drop_msg);
      print_newline(); fin_state
    else Stdlib.exit 0
  else 
  if after_draw then let _ = print_endline ("Can't drop this card. 
         Your turn is over") in draw_state else
    let msg = "😓 Your card does not match the top card, please try again " in 
    ANSITerminal.(print_string [Foreground Red; Bold] msg);
    play_until_win state false is_normal

(** [drop_legal_helper state drop_card is_normal after_draw draw_state] is the 
    state after [drop_card] is dropped. *)
and drop_legal_helper state drop_card is_normal after_draw draw_state=  
  let curr_user = List.nth state.players state.index in 
  let curr_hand = curr_user.cards in 
  if (List.mem drop_card curr_hand) then update_legal_drop
      state drop_card after_draw draw_state is_normal
  else 
    (let msg = "You do not have this card, please choose another card to drop "
     in ANSITerminal.(print_string [Foreground Red; Bold] msg);
     play_until_win state false is_normal)

(** [drop_helper card state after_draw draw_state is_norma] is the state after 
    [card] is dropped. The [card] becomes the [state]'s topcard and is also 
    removed from the [state]'s current player's hand. *)
and drop_helper card state after_draw draw_state is_normal= 
  (match (get_card_from_list card) with 
   | Legal drop_card -> drop_legal_helper state drop_card is_normal after_draw 
                          draw_state
   | Illegal -> let msg = "😓Invalid card dropped. Try again " in
     ANSITerminal.(print_string [Foreground Red; Bold] msg);
     play_until_win state false is_normal)

(** [uno_on_state_helper curr_player index state player is_normal] is the state 
    after a card is added to [player] and the index is updated to the next 
    player. *)
and uno_on_state_helper curr_player index state player is_normal= 
  let random_card = generate_card is_normal in
  let upated_player = {player with cards = random_card :: player.cards } in
  let updated_player_list = List.mapi (fun i player -> 
      if i = index then upated_player else player) state.players in
  {state with players = updated_player_list; 
              messages = (upated_player,"Uno_on has been called on you") 
                         :: state.messages}

(** [uno_on_helper on_player curr_player state is_normal] is the state after 
    uno_on is called on [on_player]. *)
and uno_on_helper on_player curr_player state is_normal= 
  let names = List.map (fun user -> 
      String.lowercase_ascii user.name) state.players in
  let uno_on_player = String.lowercase_ascii (List.hd on_player) in 
  let uno_on_player_index = find_index_of_player uno_on_player names in 
  let player = List.nth state.players uno_on_player_index in 
  let player_cards = List.length player.cards in 
  if player_cards = 1 && (List.mem player state.uno_called = false) then 
    let _ = print_endline("Called UNO! on " ^ uno_on_player ^ 
                          ". Make your next move") in 
    let updated_state = uno_on_state_helper curr_player uno_on_player_index
        state player is_normal in 
    play_until_win updated_state false is_normal
  else let msg = "Can't call UNO! on " ^ uno_on_player in
    ANSITerminal.(print_string [Foreground Red; Bold] msg);
    play_until_win state false is_normal

(** [generate_hand_discard is_normal] is the generated cards that also always 
    contains a Blue and Red DiscardAll according to the specifications of the 
    chosen game version, [is_normal]. Used for testing purposes. *)
let generate_hand_discard is_normal  = 
  if is_normal then
    let cards = generate_cards 7 [] false in 
    let updated_cards = ExtraSpecial(Red, DiscardAll) :: cards in
    ExtraSpecial(Blue, DiscardAll) :: updated_cards
  else 
    let cards = generate_cards 7 [] true in
    let updated_cards = ExtraSpecial(Red, DiscardAll) :: cards in
    ExtraSpecial(Blue, DiscardAll) :: updated_cards

(** [generate_hand_swap is_normal] is the generated cards that also always 
    contains a Blue and Red Swap according to the specifications of the chosen 
    game version, [is_normal]. Used for testing purposes. *)
let generate_hand_swap is_normal  = 
  if is_normal then
    let cards = generate_cards 7 [] false in 
    let updated_cards = ExtraSpecial(Red, Swap) :: cards in
    ExtraSpecial(Blue, Swap) :: updated_cards
  else 
    let cards = generate_cards 7 [] true in 
    let updated_cards = ExtraSpecial(Red, Swap) :: cards in
    ExtraSpecial(Blue, Swap) :: updated_cards

(** [generate_hand_skipEveryone is_normal] is the generated cards that also 
    always contains a Blue and Red SkipEveryone according to the specifications 
    of the chosen game version, [is_normal]. Used for testing purposes. *)
let generate_hand_skipEveryone is_normal  = 
  if is_normal then
    let cards = generate_cards 7 [] false in 
    let updated_cards = ExtraSpecial(Red, SkipEveryone) :: cards in
    ExtraSpecial(Blue, SkipEveryone) :: updated_cards
  else 
    let cards = generate_cards 7 [] true in 
    let updated_cards = ExtraSpecial(Red, SkipEveryone) :: cards in
    ExtraSpecial(Blue, SkipEveryone) :: updated_cards

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "Enter # of players (2-4)";
  print_string  "> ";
  Random.self_init ();
  match read_line () with
  | exception End_of_file -> Stdlib.exit 0
  | number_of_players -> 
    let true_if_normal = is_normal in 
    if true_if_normal then 
      let state = main_helper (number_of_players) true in 
      let _ = print_file "normal_rules.txt" Black in
      let _ = play_until_win state true true in ()
    else 
      let state = main_helper (number_of_players) false in 
      let _ = print_file "special_rules.txt" Black in
      let _ = play_until_win state true false in ()

(* Execute the game engine. *)
let () = main ()