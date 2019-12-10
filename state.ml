open Cards

type user = {
  name: string;
  cards: card list;
}

type state = {
  players: user list;
  top_card: card;
  num_of_drawn_cards: int;
  index: int;
  uno_called: user list;
  notes: string;
  messages: (user * string) list;
}

let string_card card = 
  match card with 
  | Normal (color, num) -> (string_color color) ^ (string_of_int num);
  | Special (color, special) -> (string_color color) ^ (string_special special)
  | ExtraSpecial (color, extra_special) -> 
    (string_color color) ^ (string_extra_special extra_special)

let random_color num = 
  match num with
  | 0 -> Red
  | 1 -> Yellow
  | 2 -> Green
  | 3 -> Blue
  | _ -> failwith "should be number 1 - 4"

(** [random_special num] is a random special based on the random number 
    [num]. *)
let random_special num = 
  match num with
  | 0 -> Skip
  | 1 -> Reverse
  | 2 -> Draw2
  | 3 -> Wildcard
  | 4 -> WildcardDraw4
  | _ -> failwith "should be a special card"

(** [random_extra_special num] is a random extra_special based on the random 
    number [num]. *)
let random_extra_special num = 
  match num with
  | 0 -> Swap
  | 1 -> DiscardAll
  | 2 -> SkipEveryone
  | 3 -> Draw1
  | _ -> failwith "should be an extra special card"

let rec generate_first_card = 
  Random.self_init ();
  let color = random_color (Random.int 4) in
  let random_int = Random.int 10 in
  Normal (color, random_int)

(** [match_color color card] is true or false depending on whether the the color
    of [card] matches [color]. *)
let match_color color card = 
  match card with 
  | Normal (c, _) -> if c = color then true else false 
  | Special (c,_) -> if (c = color ) then true else false 
  | ExtraSpecial (c,_) -> if (c = color ) then true else false 

(** [discard_all color hand acc state user] is the updated state based on 
    [state] with the new hand [acc] updated. All of the cards that have the same 
    color as [color] in the user's [user] hand [hand] will be removed. *)
let rec discard_all color hand acc state user = 
  match hand with 
  | [] -> let updated_player=  { user with cards = (List.sort compare acc)} in 
    let updated_player_list = List.mapi (fun i player -> if i = state.index then
                                            updated_player else player ) 
        state.players in
    let player_hand_updated = {state with players = updated_player_list} in
    {player_hand_updated with top_card =  ExtraSpecial (color,DiscardAll)}
  | h :: t -> if (match_color color h) then discard_all color t acc state user 
    else discard_all color t (h::acc) state user

(** [generate_cards count list is_extra_special] is a list [list] of randomly 
    generated normal and special cards if [is_extra_special] is false, otherwise
    is a list of randomly generated normal, special, and extra special cards
    if [is_extra_special] is true, the number of cards randomly generated 
    is [count]. *)
let rec generate_cards count list is_extra_special = 
  Random.self_init ();
  if count > 0 then
    let color = random_color (Random.int 4) in
    let special = (Random.int 5) in
    if (is_extra_special) then
      gen_cards_ex_spec count list is_extra_special special color
    else
      gen_spec count list is_extra_special special color 
  else list

(** [gen_cards_ex_spec count list is_extra_special] is a list [list] of randomly 
    generated extra_special cards if [is_extra_special] is true, otherwise
    is a list of randomly generated normal, special, and extra special cards
    of length [count]. *)
and gen_cards_ex_spec count list is_extra_special special color = 
  if (special = 0) then let random_special = random_special (Random.int 5) in
    if (random_special = Wildcard || random_special = WildcardDraw4) then 
      generate_cards (count - 1) (Special (Undefined, random_special) :: list) 
        is_extra_special
    else generate_cards (count - 1) (Special (color, random_special) :: list) 
        is_extra_special
  else if (special = 1) then gen_ex_spec color count is_extra_special 
      list
  else gen_normal color count is_extra_special list

(** [gen_normal color count is_extra_special list] is a list [list] 
    of randomly generated normal cards of length [count].[is_extra_special] 
    is false, indicating that the cards generated are normal. *)
and gen_normal color count is_extra_special list = 
  let random_int = Random.int 10 in generate_cards (count - 1) 
    (Normal (color, random_int) :: list) is_extra_special

(** [gen_ex_spec color count is_extra_special list] is a list [list] 
    of randomly generated extra_special cards of length [count].
    [is_extra_special] is true, indicating that the cards generated are 
    extra special. *)
and gen_ex_spec color count is_extra_special list = 
  let random_extra_special = random_extra_special (Random.int 4) in 
  generate_cards (count - 1) (ExtraSpecial (color, random_extra_special) 
                              :: list) is_extra_special
(** [generate_spec count list is_extra_special] is a list [list] of 
    randomly generated normal and special cards of length [count] 
    if [is_extra_special] is false, otherwise is a list of randomly 
    generated special cards with color [color]. *)
and gen_spec count list is_extra_special special color = 
  if (special = 0) then let random_special = random_special (Random.int 5) in
    if (random_special = Wildcard || random_special = WildcardDraw4) then 
      generate_cards (count - 1) (Special (Undefined, random_special) :: list)
        is_extra_special
    else generate_cards (count - 1) (Special (color, random_special) :: list) 
        is_extra_special
  else
    let random_int = Random.int 10 in
    generate_cards (count - 1) (Normal (color, random_int) :: list) 
      is_extra_special

(* [draw state user card update_index] is the updated state with the card to add
    [card] to a player's [user] hand in the current state [state] with an 
    updated index if [update_index] is true. *)
let draw state user card update_index= 
  let updated_user = { user with cards = card :: user.cards} in 
  let updated_player_list = List.mapi ( fun i player -> 
      if(i = state.index) then updated_user else player) state.players in 
  if update_index then 
    {state with players = updated_player_list; num_of_drawn_cards = 
                                                 state.num_of_drawn_cards + 1}  
  else let new_index = if (state.index + 1 = List.length (state.players)) then 0 
         else state.index + 1 in 
    {state with players = updated_player_list; index = new_index; 
                num_of_drawn_cards = state.num_of_drawn_cards + 1}

(* [find_and_remove card_to_drop hand head_acc] is the updated hand 
   [head_acc] with the card [card_to_drop] removed in the current [hand]. *)
let rec find_and_remove card_to_drop hand head_acc  = 
  match hand with
  |[] -> head_acc 
  |h::t -> if h = card_to_drop then head_acc @ t else
      find_and_remove card_to_drop t (h::head_acc)

let drop curr_player card_to_drop state =
  let updated_player = 
    {curr_player with cards = find_and_remove card_to_drop 
                          curr_player.cards []} in 
  let updated_player_list = List.mapi (fun i player -> if i = state.index then 
                                          updated_player else player )
      state.players in
  let player_hand_updated = {state with players = updated_player_list} in 
  {player_hand_updated with top_card = card_to_drop}

let update_index curr_index state = 
  if (curr_index + 1 = List.length state.players) then 0 
  else curr_index + 1 

(** [update_index_skip curr_index state] is updated index based on the 
    current index [curr_index] in the current state [state]. *)
let update_index_skip curr_index state = 
  (curr_index + 2) mod (List.length state.players)

(** [update_state card state curr_index] is the updated state based on the 
    current state [state], the passed in card [card] to drop, and the index 
    [curr_index] to change the current state index to. *)
let update_state card state curr_index = 
  let new_state = drop (List.nth state.players state.index) card state in
  {new_state with index = curr_index}

(** [index_player player lst counter] is the index [counter] of the passed 
    in player [player] in the passed in list [lst]. *)
let rec index_player player lst counter= 
  match lst with 
  | [] -> -1
  | h::t -> if h = player then counter else index_player player t (counter+1)

(** [reverse_helper player lst counter] is the updated state. I the player 
    put down a special reverse card. The updated state is changed based on 
    [state], the card to be dropped [card], and the current index 
    [curr_index]. *)
let reverse_helper curr_index state card = 
  let curr_player = List.nth state.players curr_index in 
  let reverse_list = List.rev state.players in 
  let curr_player_index_reverse = index_player curr_player reverse_list 0 in 
  let new_state = {state with players = reverse_list; 
                              index = curr_player_index_reverse;
                              notes = "The order has been reversed"}
  in update_state card new_state (update_index curr_player_index_reverse 
                                    new_state)

(** [draw_state_helper curr_index state card num] is the updated state with a 
    randomly generated number [num] of cards added to the next player's hand 
    [curr_index] in the current state [state]. [card] is the current card. *)
let draw_state_helper state random_cards card next_player_index=  
  let next_player = List.nth state.players next_player_index in 
  let updated_player = {next_player with cards = random_cards @ 
                                                 next_player.cards} in 
  let updated_player_list = List.mapi (fun i player -> if i = next_player_index 
                                        then updated_player else player ) 
      state.players in
  let string_random_cards_list = List.map (fun card -> string_card card) 
      random_cards in
  let string_random_cards = String.concat ", " string_random_cards_list in
  let player_hand_updated = 
    {state with players = updated_player_list; 
                notes = "These cards were added to your hand: " ^ 
                        string_random_cards} in 
  update_state card player_hand_updated next_player_index 

(** [special_draw_helper] is the updated state based on the current [state] with
    a number [num] of randomly generated cards added to the current player's 
    [curr_index] hand. If [is_normal] is true, then normal cards are generated 
    otherwise special cards are generated. *)
let special_draw_helper curr_index state card num is_normal= 
  let next_player_index = update_index curr_index state in 
  if is_normal then let random_cards = generate_cards num [] false in 
    draw_state_helper state random_cards card next_player_index
  else let random_cards = generate_cards num [] true in 
    draw_state_helper state random_cards card next_player_index

(** [get_color color] is the type color version of the string [color]. *)
let get_color color = 
  match color with 
  | "red" -> Red
  | "yellow" -> Yellow
  | "green" -> Green
  | "blue" -> Blue 
  | _ ->   failwith "should be a special card"

(** [is_valid_color color] is true or false depending on whether the passed in 
    string [color] is a valid string version of one of the colors specified 
    in type color. *)
let is_valid_color color =
  if(color = "red" || color = "yellow" || color = "green" || color = "blue") 
  then true else false

(** [wildcard_helper state card curr_index] is the updated state with the top 
    card updated as the color entered by the user, the passed in [card] is
    removed from the current player's [curr_index] hand in the current state
    [state]. *)
let rec wildcard_helper state card curr_index= 
  let ask = "Enter what color you would like to change the card to" in 
  ANSITerminal.(print_string [Foreground Black; Bold] ask); print_newline();
  print_string  "> ";
  let choice = String.lowercase_ascii (read_line()) in 
  if (is_valid_color choice) then 
    let next_player_index = update_index curr_index state in 
    let past_state = update_state card state next_player_index in 
    let color = get_color choice in 
    {past_state with top_card = Special (color, Wildcard)}
  else 
    (print_endline ("😓Please enter a valid color "); 
     wildcard_helper state card curr_index)

(** [wildcard_4_helper state card curr_index] is the updated state with 
    the top card updated as the color entered by the user, the passed in [card] 
    is removed from the current player's [curr_index] hand, and
    4 randomly generated cards are added to the next player's hand
    in the current state [state].*)
let rec wildcard_4_helper state card curr_index= 
  let ask = "Enter what color you would like to change the card to" in 
  ANSITerminal.(print_string [Foreground Black; Bold] ask); print_newline();
  print_string  "> ";
  Random.self_init ();
  let choice = String.lowercase_ascii (read_line()) in 
  if (is_valid_color choice) then 
    let color = get_color choice in
    {state with top_card = Special (color, WildcardDraw4)}
  else 
    (print_endline ("😓Please enter a valid color "); 
     wildcard_helper state card curr_index)

(** [is_valid_user lst user] is true if all the users [user] in the 
    list [lst] exist and false otherwise. *)
let rec is_valid_user lst user =
  match lst with
  | [] -> false
  | h :: t -> if h.name = user then true else is_valid_user t user

(** [get_user_hand lst user] is the hand of the passed in user [user] in 
    the current player list [lst].*)
let rec get_user_hand lst user =
  match lst with
  | [] -> failwith "SHOULDNT REACH"
  | h :: t -> if h.name = user then h.cards else get_user_hand t user

(** [find_index name lst] is the index of the user with the name [name] in the 
    player list [lst].*)
let rec find_index name lst=
  match lst with 
  | [] -> raise (Failure "Not Found") 
  | h::t -> if h = name then 0 else 1 + find_index name t

(** [valid_user_state state input user card] is the updated state based on the old 
    state [state] with the current card [card] removed for the current user's [user] 
    hand. The current user's hand is switched the the user read from the user 
    input [input]. *)
let valid_user_state state input user card = 
  let names = List.map (fun user -> 
      String.lowercase_ascii user.name) state.players in
  let user2_index = find_index (String.lowercase_ascii input) names in
  let user2 = List.nth state.players user2_index in
  let old_hand = find_and_remove card user.cards []  in
  let new_hand = (List.nth state.players user2_index).cards in
  let updated_user1 = {user with cards = new_hand} in 
  let updated_player_list = List.mapi (fun i player -> 
      if i = state.index then updated_user1 else player) state.players in
  let updated_state1 = {state with players = updated_player_list} in
  let updated_user2 = {user2 with cards = old_hand} in 
  let updated_player_list2 = List.mapi (fun i player -> 
      if i = user2_index then updated_user2 else player) 
      updated_state1.players in
  {updated_state1 with index = update_index updated_state1.index
                           updated_state1; players = updated_player_list2; 
                       messages = (user2, user.name ^ 
                                          " swapped hands with you") :: 
                                  state.messages;top_card = card}

(** [swap state user] is the updated state based on the current state [state] 
    with the passed in user's [user] hand switched with another player. *)
let rec swap state user card= 
  print_endline "Enter which user you would like to swap hands with";
  print_string  "> ";
  let input = match read_line () with 
    | exception End_of_file -> exit 0
    | str -> str
  in 
  if is_valid_user state.players input then
    valid_user_state state input user card
  else let _ = print_endline("Invalid user to swap with. Try again") in 
    swap state user card

(** [extra_special_match card index state] is the updated state based 
    on the extra_special card [card] dropped and the index of the current player
    [index] in the current state [state]. [is_normal] is true if the card 
    is special. *)
let extra_special_match card index state is_normal = 
  match card with 
  | ExtraSpecial (_, Swap) ->
    let curr_player = List.nth state.players state.index in swap state
      curr_player card
  | ExtraSpecial (color , DiscardAll) -> let curr_player = List.nth 
                                             state.players state.index in 
    let new_state = discard_all color curr_player.cards [] state curr_player in 
    update_state card new_state ((index + 1 )mod (List.length state.players))
  | ExtraSpecial (_, Draw1) -> special_draw_helper index state card 1 is_normal 
  | ExtraSpecial (_, SkipEveryone) -> 
    let new_state = update_state card state index in 
    {new_state with notes = "Everyone was skipped" } 
  | _ -> let curr_index = update_index_skip index state in 
    update_state card state curr_index

let special_card_action card index state is_normal= 
  match card with 
  |  Special  (_, Skip) -> let curr_index_reg = update_index index state in
    let curr_index_skip = update_index_skip index state in 
    let new_state = update_state card state curr_index_skip in 
    let player = List.nth state.players curr_index_reg in 
    {new_state with notes = player.name ^ " was skipped" } 
  | Special  (_, Reverse) -> if (List.length state.players = 2) then 
      let curr_index_skip = update_index_skip index state in 
      let new_state = update_state card state curr_index_skip in 
      {new_state with notes = "The order has been reversed"} 
    else reverse_helper index state card 
  | Special  (_, Draw2) ->  special_draw_helper index state card 2 is_normal 
  | Special  (_, Wildcard) -> wildcard_helper state card index
  | Special  (_, WildcardDraw4) -> let draw4_state = special_draw_helper 
                                       index state card 4 is_normal in 
    wildcard_4_helper draw4_state card index
  | ExtraSpecial (_,_) -> extra_special_match card index state is_normal
  |  _ -> let curr_index = update_index_skip index state in 
    update_state card state curr_index