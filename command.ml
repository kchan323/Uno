type object_phrase = string list

type command = 
  | Drop of object_phrase
  | Draw
  | Quit
  | Uno_on of object_phrase
  | Drop_uno of object_phrase

exception InvalidCommand

(** [match_draw_quit draw_quit] is the true if [draw_quit] matches the 
    specificiation for the Draw or Quit command. *)
let match_draw_quit draw_quit = let draw_quit_lower = 
                                  String.lowercase_ascii draw_quit in 
  if draw_quit_lower = "draw" then Draw 
  else if draw_quit_lower = "quit" then Quit else raise InvalidCommand

(** [match_drop_wildcard d_u_lower t] is the true if [d_u_lower] matches the 
    specificiation for the Drop command if [t] is a wildcard. *)
let match_drop_wildcard d_u_lower t = 
  if List.length t = 1 && (String.lowercase_ascii (List.hd t) = "wildcard" 
                           || String.lowercase_ascii (List.hd t) 
                              = "wildcarddraw4") && (d_u_lower = "drop")
  then true else false

(** [match_dropuno_wildcard d_u_lower t] is the true if [d_u_lower] matches the 
    specificiation for the Drop_uno command if [t] is a wildcard. *)
let match_dropuno_wildcard d_u_lower t = 
  if List.length t = 1 && (String.lowercase_ascii (List.hd t) = "wildcard" || 
                           String.lowercase_ascii 
                             (List.hd t) = "wildcarddraw4") && 
     (d_u_lower = "dropuno") 
  then true else false

(** [match_drop d_u_lower t] is the true if [d_u_lower] matches the 
    specificiation for the Drop command and if [t] is not a wildcard. *)
let match_drop d_u_lower t = 
  if List.length t = 2 && d_u_lower = "drop" && 
     (String.lowercase_ascii (List.hd t) <> "wildcard" &&
      String.lowercase_ascii (List.hd t) <> "wildcarddraw4") 
  then true else false

(** [match_dropuno d_u_lower t] is the true if [d_u_lower] matches the 
    specificiation for the Drop_uno command and if [t] is not a wildcard. *)
let match_dropuno d_u_lower t = 
  if List.length t = 2 && d_u_lower = "dropuno" && 
     (String.lowercase_ascii (List.hd t) <> "wildcard" &&
      String.lowercase_ascii (List.hd t) <> "wildcarddraw4") 
  then true else false

let parse str =
  if str = "" then raise InvalidCommand
  else let lst_with_spaces = String.split_on_char ' ' str in 
    let lst_without_spaces = List.filter(fun x -> x <> "") lst_with_spaces in 
    match lst_without_spaces with 
    | draw_quit::[] -> match_draw_quit draw_quit
    | drop_uno::t -> let d_u_lower = String.lowercase_ascii drop_uno in 
      if match_drop_wildcard d_u_lower t then Drop t 
      else if match_dropuno_wildcard d_u_lower t then Drop_uno t
      else if match_drop d_u_lower t then Drop t 
      else if match_dropuno d_u_lower t then Drop_uno t 
      else if d_u_lower = "uno_on" && List.length t = 1 then Uno_on t 
      else raise InvalidCommand
    | _ -> raise InvalidCommand