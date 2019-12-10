(** Updates attributes of the game state. *)

open Cards 

(** The type of user that has a name and their hand (list of cards). *)
type user = {
  name: string;
  cards: card list;
}

(** The type of the state of the game. *)
type state = {
  players: user list;
  top_card: card;
  num_of_drawn_cards: int;
  index: int;
  uno_called: user list;
  notes: string;
  messages: (user * string) list;
}

(* [string_card card] is the string version of the card [card. *)
val string_card: card -> string

(** [random_color num] is a random color based on the random number [num]. *)
val random_color: int -> Cards.color

(** [random_extra_special num] is a random extra_special based on the random 
    number [num]. *)
(* val random_extra_special: int -> Cards.extraSpecial *)

(** [generate_first_card] is a randomly generated card. *)
val generate_first_card: card

(** [generate_cards count list is_extra_special] is a list [list] of randomly 
    generated normal and special cards if [is_extra_special] is false, otherwise
    is a list of randomly generated normal, special, and extra special cards
    if [is_extra_special] is true, the number of cards randomly generated 
    is [count]. *)
val generate_cards: int -> card list -> bool-> card list

(* [draw state user card] is the updated state with the card to add
   [card] to a player's [user] hand in the current state [state]. *)
val draw: state -> user -> card -> bool -> state

(** [drop curr_player card_to_drop state] is the updated state with the card to 
    remove [card_to_drop] from a player's [curr_player] hand in the current 
    state [state]. *)
val drop: user -> card -> state -> state

(** [update_index curr_index state] updates the current index [curr_index]
    in the current state [state]. *)
val update_index: int -> state -> int

(** [special_card_action card index state] is the updated state based 
    on the card [card] dropped and the index of the current player [index] 
    in the current state [state]. [is_normal] is true if the card is special. *)
val special_card_action: card -> int -> state -> bool -> state


