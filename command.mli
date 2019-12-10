(** Parses commands inputted by users. *)

(** Similar to A2: The type [object_phrase] represents the object phrase 
    that can be part of a player command. Each element of the list 
    represents a word of the object phrase, where a {i word} is defined as a 
    consecutive sequence of non-space 
    characters. Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["drop Red 7"], then the object phrase is 
      [["drop"; "Red"; "7"]].
      An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** Similar to A2: The type [command] represents a player command that is 
    decomposed into a verb and possibly an object phrase. *)
type command = 
  | Drop of object_phrase
  | Draw
  | Quit
  | Uno_on of object_phrase
  | Drop_uno of object_phrase

(* Raised when user enters an invalid command *)
exception InvalidCommand

(** Similar to A2: [parse str] parses a player's input into a [command], as 
    follows. The first word of [str] becomes the verb. The rest of the words, 
    if any, become the object phrase.
    Examples: 
    - [parse "    drop   Red   7   "] is [Drop ["Red"; "7"]]
    - [parse " dropuno Green 3"] is [Drop_uno ["Green"; "3"]]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [InvalidCommand] if the verb is not "draw", "quit", "uno" or 
    "drop [color number/specialcard]" or "uno_on [playername]". *)
val parse: string -> command 