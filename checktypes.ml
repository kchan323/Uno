open Cards 
module type StateSig = sig
  (* type color *)
  (* type special  *)
  (* type card *)
  type user 
  type state 
  val generate_cards: int -> card list -> bool-> card list
end

module StateCheck : StateSig = State

module type AuthorSig = sig
  val hours_worked : int list
end

module AuthorCheck : AuthorSig = Author
