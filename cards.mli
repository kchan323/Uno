(** Manages information about normal, special, and extra special cards. *)

(** The type of the color of the card. *)
type color = Red | Yellow | Green | Blue | Undefined

(** The type of the version of the special card. *)
type special = Skip | Reverse | Draw2 | Wildcard | WildcardDraw4

(** The type of the version of the extra special card *)
type extraSpecial = Swap | DiscardAll | SkipEveryone | Draw1

(** The type of card. Can either be Normal, Special, or ExtraSpecial *)
type card = Normal of color * int | Special of color * special 
          | ExtraSpecial of color * extraSpecial

(** The type of result of playing a card. Can either be Legal or Illegal *)
type lCard = Legal of card | Illegal

(** [string_color color] is the string version of the color [color]. *)
val string_color: color -> string 

(** [string_special special] is the string version of the special [special]. *)
val string_special: special -> string

(** [string_extra_special extra_special] is the string version of the 
    extra_special [extra_special]. *)
val string_extra_special: extraSpecial -> string

(** [card_to_object_phrase card] is the string list version of the 
    card [card]. *)
val card_to_object_phrase: card -> string list

(** [is_special special] is true if the passed in card [special] is one 
    of the defined special cards and false otherwise *)
val is_special: card -> bool 

(** [match_card player_card top_card] is true if [player_card] matches with
    [top_card] by color, number, or is the same type of special card. *)
val match_card: card -> card -> bool 

(** [get_card_from_list card] is a lCard which is legal if the card [card] 
    is one of the defind types and illegal otherwise. *)
val get_card_from_list: string list -> lCard