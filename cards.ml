type color = Red | Yellow | Green | Blue | Undefined

type special = Skip | Reverse | Draw2 | Wildcard | WildcardDraw4

type extraSpecial = Swap | DiscardAll | SkipEveryone | Draw1

type card = Normal of color * int | Special of color * special 
          | ExtraSpecial of color * extraSpecial

type lCard = Legal of card | Illegal 

let string_color color = 
  match color with
  | Red -> "Red "
  | Yellow -> "Yellow "
  | Green -> "Green "
  | Blue -> "Blue "
  | Undefined -> ""

let string_special special =
  match special with 
  | Skip -> "Skip"
  | Reverse -> "Reverse"
  | Draw2 -> "Draw2"
  | Wildcard -> "Wildcard"
  | WildcardDraw4 -> "WildcardDraw4"

let string_extra_special extra_special = 
  match extra_special with
  | Swap -> "Swap"
  | DiscardAll -> "DiscardAll"
  | SkipEveryone -> "SkipEveryone"
  | Draw1 -> "Draw1"

let card_to_object_phrase card =
  match card with 
  | Normal (color, int) -> [string_color color; string_of_int int]
  | Special (color, special) -> if special = Wildcard then ["Wildcard"] 
    else if special = WildcardDraw4 then ["WildcardDraw4"] 
    else [string_color color; string_special special]
  | ExtraSpecial (color, extra_special) -> 
    [string_color color; string_extra_special extra_special]

(** [variant_color_string color] is the corresponding color based on [color]. *)
let variant_color_string color = 
  match color with
  | "red" -> Red
  | "yellow" -> Yellow
  | "green" -> Green
  | "blue"-> Blue
  | "Undefined" -> Undefined
  | _ -> failwith "invalid color"

(** [variant_special_string special] is the corresponding special card
    based on [special]. *)
let variant_special_string special =
  match special with 
  | "skip" -> Skip
  | "reverse" ->  Reverse 
  | "draw2" -> Draw2 
  | "wildcard" -> Wildcard 
  | "wildcarddraw4" -> WildcardDraw4 
  | _ -> failwith "invalid special card"

(** [variant_extra_pecial_string special] is the corresponding extra_special 
    card based on [special]. *)
let variant_extra_special_string special =
  match special with 
  | "swap" -> Swap
  | "discardall" ->  DiscardAll
  | "skipeveryone" -> SkipEveryone
  | "draw1" -> Draw1
  | _ -> failwith "invalid extra special card"

let is_special special = 
  match special with 
  | Special (_, Skip) -> true
  | Special (_, Reverse) -> true
  | Special (_, Draw2) -> true
  | Special (_, Wildcard) -> true
  | Special (_, WildcardDraw4) -> true
  | ExtraSpecial (_, Swap) -> true 
  | ExtraSpecial (_, DiscardAll) -> true 
  | ExtraSpecial (_, SkipEveryone ) -> true 
  | ExtraSpecial (_, Draw1) -> true 
  | _ -> false

(** [color_lower_case color] is the lowercase version of the passed in color
    [color] *)
let color_lower_case color = 
  let string_color2 = string_color color in 
  String.lowercase_ascii string_color2

(** [special_lower_case special] is the lowercase version of the passed in 
    special card [special] *)
let special_lower_case special = 
  let string_special2 = string_special special in 
  String.lowercase_ascii string_special2

(** [extra_special_lower_case extra_special] is the lowercase version of the
    passed in extra_special card [extra_special]  *)
let extra_special_lower_case extra_special = 
  let string_extra_special = string_extra_special extra_special in
  String.lowercase_ascii string_extra_special

let match_card player_card top_card = 
  match player_card, top_card with 
  | Normal (player_color,player_num), Normal (top_color,top_num) -> 
    if (color_lower_case player_color = color_lower_case top_color 
        || player_num = top_num) then true else false 
  | Special (Undefined, Wildcard), _ -> true
  | Special (Undefined, WildcardDraw4), _ -> true
  | Special (player_color, _), Special (top_color, Wildcard)
  | Normal (player_color, _), Special (top_color, Wildcard) ->   
    if (color_lower_case player_color = color_lower_case top_color) 
    then true else false
  | Special (player_color, player_special), Special (top_color, top_special) -> 
    if(special_lower_case player_special = special_lower_case top_special 
       || color_lower_case player_color =color_lower_case top_color) 
    then true else false
  | Normal (player_color,_), Special (top_color,_) 
  | Special (player_color, _), Normal (top_color, _) -> 
    if(color_lower_case player_color = color_lower_case top_color) 
    then true else false
  | Normal (player_color, _), ExtraSpecial (top_color,_) 
  | ExtraSpecial (top_color,_), Normal (player_color, _) -> 
    if(color_lower_case player_color =color_lower_case top_color) 
    then true else false
  | ExtraSpecial (player_color, player_card), ExtraSpecial (top_color, top_card) 
    -> if (extra_special_lower_case player_card  = 
           extra_special_lower_case top_card || 
           color_lower_case player_color = color_lower_case top_color) 
    then true else false
  | ExtraSpecial (top_color,_) , Special(player_color,_) 
  | Special(player_color,_), ExtraSpecial (top_color,_) -> 
    if(color_lower_case player_color = color_lower_case top_color) 
    then true else false

(** [get_num num] is the number in the option [num]. If [num] is None, then 
    -1 is returned. *)
let get_num num = 
  match num with 
  | None -> -1 
  | Some x -> x

(** [special_card_color color] is true if [color] is a valid color. *)
let special_card_color color = 
  if (color = "red" || color = "yellow" || color = "green" || color = "blue") 
  then true else false 

(** [special_card_color color] is true if [name_of_card] is a valid special card
    . *)
let special_card_specials name_of_card = 
  if name_of_card  = "skip" || name_of_card  = "reverse" || 
     name_of_card  = "draw2" then true else false 

(** [special_card_extra_specials]is true if [name_of_card] is a valid extra
    special card. *)
let special_card_extra_specials name_of_card = 
  if (name_of_card = "swap" || name_of_card  = "discardall" || 
      name_of_card = "skipeveryone" || name_of_card = "draw1") 
  then true else false 

(** [special_card color num name_of_card] is a lCard which is legal if the
    color of the card [color], the number of the card [num], and the name of 
    the card (if it is a extra_special_card)[name_of_card] is one of the 
    defined cards otherwise, the card is illegal. *)
let special_card color num name_of_card = 
  let color = String.lowercase_ascii color in 
  if special_card_specials name_of_card && special_card_color color
  then Legal (Special (variant_color_string color,
                       variant_special_string name_of_card))
  else if (get_num (int_of_string_opt num) >= 0 
           && get_num (int_of_string_opt num) <= 9 && special_card_color color)
  then Legal (Normal (variant_color_string color, int_of_string num))
  else if special_card_extra_specials name_of_card && special_card_color color 
  then Legal (ExtraSpecial ( variant_color_string color, 
                             variant_extra_special_string name_of_card ))
  else Illegal

let get_card_from_list card = 
  match card with 
  | [] -> Illegal
  | wild :: [] ->  let lower_wild = String.lowercase_ascii wild in 
    if (lower_wild = "wildcard" || lower_wild = "wildcarddraw4") then
      Legal(Special (Undefined, variant_special_string lower_wild)) 
    else Illegal
  | color :: t -> let color = String.trim(String.lowercase_ascii color) in 
    let num = List.hd t in 
    let name_of_card = String.trim(String.lowercase_ascii num )in
    special_card color num name_of_card