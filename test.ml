open OUnit2
open Command
open Cards
open State

(* APPROACH TO TESTING: We did a lot of our testing interactively by 
   playing the game because we didn't want to expose functions just for the sake
   of testing them. We manually tested all of our commands (Draw, Drop, etc) and 
   their corresponding actions, win condition, and REPL by playing the game 
   multiple times. To make sure we tested certain functionalities 
   (ex: swapping cards), we created helper functions that gave us certain 
   varying amounts of cards and displayed the information of the state after
   each turn. A lot of testing had to be done interactively because they
   required user input (like changing colors). We also had our friends test the
   game to see how it could be improved for real users!

   In this test suite, we focused on the functions we did expose and especially
   invalid inputs that might not neccesarily come up in interactive testing
   (if the users follow the rules at least). The OUnit tests the modules 
   Command, Cards, and State. For example, it tests matching (valid/
   invalid) cards, parsing commands, and state. 

   For our state tests, we made sure that the state had the right attributes 
   when a user did something to change their state like dropping or drawing
   a card. To do this, we tested the length of the cards in the current 
   player's hand, made sure the state was updated with the right index, and also
   checked that it had the right set of notes to display. We did this for each 
   type of card - special, extraspecial, and normal.

   We used the bisect tool for OCaml to help us see which expressions in our
   program were tested and which were not tested. Bisect is a form of glass-box
   testing to help us see how much of our code was exercised by our test suite.

   Our bisect scores for each class were very high. For state, our bisect score
   was 46%, for cards 92% , and for command 98%. Our bisect score for 
   state was low was because many of the functions in state were not 
   exposed in our mli, and as a result we couldn't test those functions.

   We believe that our test suite demonstrates the correctness of our system 
   because we tested a majority of our functions and many edge cases. *)

let user1 = {name = "I"; cards = [Normal(Red, 7); Normal(Blue , 8); 
                                  Normal(Green , 9);  Normal(Yellow, 0); 
                                  Special(Blue, Skip);Special(Yellow, Reverse);
                                  Special(Red, Draw2 );
                                  Special(Undefined, Wildcard);
                                  Special(Undefined, WildcardDraw4);] } 
let updated_user1 = {name = "I"; cards = [ Normal(Blue , 8); 
                                           Normal(Green , 9);  
                                           Normal(Yellow, 0); 
                                           Special(Blue, Skip);
                                           Special(Yellow, Reverse);
                                           Special(Red, Draw2 );
                                           Special(Undefined, Wildcard);
                                           Special(Undefined, WildcardDraw4);]}

let user2 = {name = "K"; cards = [Normal(Red, 0); Normal(Blue , 8); 
                                  Normal(Green , 9);  Normal(Yellow, 0); 
                                  Special(Blue, Skip);Special(Yellow, Reverse);
                                  Special(Undefined, Wildcard);
                                  Special(Undefined, WildcardDraw4);] } 
let user1_skip = {name = "I"; cards = [Normal(Red, 7); Normal(Blue , 8); 
                                       Normal(Green , 9);  Normal(Yellow, 0); 
                                       Special(Yellow, Reverse);
                                       Special(Red, Draw2 );
                                       Special(Undefined, Wildcard);
                                       Special(Undefined, WildcardDraw4);]} 
let user3 = {name = "E"; cards = 
                           [ExtraSpecial(Red, Swap);ExtraSpecial 
                              (Green, DiscardAll); ExtraSpecial
                              (Red, SkipEveryone);
                            ExtraSpecial(Yellow, Draw1);]} 
let random_cards = [Normal(Red, 0); Normal(Blue , 8); 
                    Normal(Green , 9); ]
let state = {players = [user1;user2]; top_card = Normal (Red, 5); 
             num_of_drawn_cards = 0; index = 0; uno_called =  []; notes = ""; 
             messages =  [(user1, "")]} 
let state_extra_special = {players = [user3;user2]; top_card = Normal (Red, 5); 
                           num_of_drawn_cards = 0; index = 0; uno_called = []; 
                           notes = ""; messages = [(user2, "")]} 
let updated_state = {players = [updated_user1;user2]; top_card = Normal(Red, 7);
                     num_of_drawn_cards = 0; index = 0; uno_called =  []; 
                     notes = ""; messages = [(user1, "")]} 
let updated_state_without_skip = {players = [user1_skip;user2]; 
                                  top_card = Special(Blue, Skip);  
                                  num_of_drawn_cards = 0; index = 0;
                                  uno_called =  []; notes = "K was skipped"; 
                                  messages = [(user2, "")]} 

let command_tests =
  [
    (* Testing case sensitivity-----------------------------------------------*)
    "parse: Draw" >:: (fun _ -> 
        assert_equal (Draw) (parse "Draw"));
    "parse: draw" >:: (fun _ -> 
        assert_equal (Draw) (parse "draw"));
    "parse: DrAw" >:: (fun _ -> 
        assert_equal (Draw) (parse "DrAw"));
    "parse: drop Red 61" >:: (fun _ -> 
        assert_equal (Drop ["Red"; "6"]) (parse "drOP Red 6"));
    "parse: uno_on" >:: (fun _ -> 
        assert_equal (Uno_on ["Ein"]) (parse "unO_oN Ein"));
    "parse: uno_on" >:: (fun _ -> 
        assert_equal (Uno_on ["EIN"]) (parse "UNO_ON EIN"));
    "parse: dropuno1" >:: (fun _ -> 
        assert_equal (Drop_uno ["Red"; "3"]) (parse "droPUno Red 3")); 
    "parse: dropuno2" >:: (fun _ -> 
        assert_equal (Drop_uno ["RED"; "3"]) (parse "DROPUNO RED 3")); 

    (* Testing valid commands-------------------------------------------------*)
    "parse: drop1" >:: (fun _ -> 
        assert_equal (Drop ["Red"; "7"]) (parse "drop Red 7"));
    "parse: drop2" >:: (fun _ -> 
        assert_equal (Drop ["Wildcard"]) (parse "drop Wildcard"));
    "parse: drop3" >:: (fun _ -> 
        assert_equal (Drop ["WildcardDraw4"]) (parse "drop WildcardDraw4"));
    "parse: drop4" >:: (fun _ -> 
        assert_equal (Drop ["wildcard"]) (parse "drop wildcard"));
    "parse: dropuno1" >:: (fun _ -> 
        assert_equal (Drop_uno ["Red"; "4"]) (parse "dropuno Red 4"));
    "parse: dropuno2" >:: (fun _ -> 
        assert_equal (Drop_uno ["Wildcard"]) (parse "dropuno Wildcard"));
    "parse: dropuno3" >:: (fun _ -> 
        assert_equal (Drop_uno ["WildcardDraw4"]) 
          (parse "dropuno WildcardDraw4"));
    "parse: uno_on4" >:: (fun _ -> 
        assert_equal (Uno_on ["Ein"]) (parse "uno_on Ein"));
    "parse: draw" >:: (fun _ -> 
        assert_equal (Draw) (parse "draw"));
    "parse: quit" >:: (fun _ -> 
        assert_equal (Quit) (parse "quit")); 

    (* Testing invalid commands-----------------------------------------------*)
    "parse: InvalidCommand" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "invalid command"));
    "parse: InvalidCommand2" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "UNO_0"));
    "parse: InvalidCommand3" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse ""));
    "parse: InvalidCommand3" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "1 2 3 4 5 6"));
    "parse: InvalidCommand Draw" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "draw blue 4"));
    "parse: InvalidCommand Draw2" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "draw blue4"));
    "parse: InvalidCommand Draw3" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "draw blue4 234"));
    "parse: InvalidCommand Draw3" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "draw blue4 234 23"));
    "parse: InvalidCommand dropuno" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "dropuno"));
    "parse: InvalidCommand dropuno2" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "dropuno red"));
    "parse: InvalidCommand dropuno3" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "dropuno red 4 3"));
    "parse: InvalidCommand uno_on1" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "uno_on"));
    "parse: InvalidCommand uno_on2" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "uno_on play1 play2"));
    "parse: InvalidCommand uno_on3" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> 
            parse "uno_on person1 person2"));
    "parse: InvalidCommand drop1" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "drop blue 2 3")); 
    "parse: InvalidCommand drop2" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "drop wildcard 2")); 
    "parse: InvalidCommand drop3" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "drop wildcard 2 3")); 
    "parse: InvalidCommand drop4" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> 
            parse "drop wildcarddraw4 2 ")); 
    "parse: InvalidCommand drop5" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> 
            parse "drop wildcarddraw4 2")); 
    "parse: InvalidCommand drop6" >:: (fun _ ->
        assert_raises (InvalidCommand) (fun () -> parse "drop wildcarddraw4
         2 3")); 
  ]


let state_tests =
  [
    "state: drop special skip card top card " >:: (fun _ ->
        assert_equal updated_state_without_skip.top_card  
          ((special_card_action  (Special(Blue, Skip)) 0 state true).top_card));
    "state: drop special skip card index" >:: (fun _ ->
        assert_equal updated_state_without_skip.index  
          ((special_card_action (Special(Blue, Skip)) 0 state true).index));
    "state: drop special reverse card notes" >:: (fun _ ->
        assert_equal "The order has been reversed"  
          ((special_card_action (Special(Yellow, Reverse)) 0 state true).notes));

    "state: drop special reverse card top_card" >:: (fun _ ->
        assert_equal (Special(Yellow, Reverse)) 
          ((special_card_action 
              (Special(Yellow, Reverse)) 0 state true).top_card));
    "state: drop special reverse card top_card" >:: (fun _ ->
        assert_equal 0 
          ((special_card_action (Special(Yellow, Reverse)) 0 state true).index)
      );

    "state: drop special draw2 card top card" >:: (fun _ ->
        assert_equal (Special(Red, Draw2 ))  
          ((special_card_action (Special(Red, Draw2 )) 0 state true).top_card));
    "state: drop special draw2 card notes" >:: (fun _ ->
        assert_equal 1 
          ((special_card_action (Special(Red, Draw2 )) 0 state true).index));

    (* ExtraSpecial card - DiscardAll *)
    "state: drop special draw2 card top card" >:: (fun _ ->
        assert_equal (ExtraSpecial(Green, DiscardAll)) 
          ((special_card_action (ExtraSpecial(Green, DiscardAll)) 0 
              state_extra_special true).top_card));
    "state: drop special draw2 card index" >:: (fun _ ->
        assert_equal 1 
          ((special_card_action (ExtraSpecial(Green, DiscardAll)) 0 
              state_extra_special true).index));

    (* ExtraSpecial card - Draw1 *)
    "state: drop special draw1 card top card" >:: (fun _ ->
        assert_equal (ExtraSpecial(Yellow, Draw1)) 
          ((special_card_action (ExtraSpecial(Yellow, Draw1)) 0 
              state_extra_special true).top_card));
    "state: drop special draw1 card index" >:: (fun _ ->
        assert_equal  1 
          ((special_card_action (ExtraSpecial(Yellow, Draw1)) 0 
              state_extra_special true).index));

    (* ExtraSpecial card - SkipEveryone *)
    "state: drop special draw1 card top card" >:: (fun _ ->
        assert_equal (ExtraSpecial(Red, SkipEveryone)) 
          ((special_card_action (ExtraSpecial(Red, SkipEveryone)) 0 
              state_extra_special true).top_card));
    "state: drop special skipeveryone card index" >:: (fun _ ->
        assert_equal 0 
          ((special_card_action (ExtraSpecial(Red, SkipEveryone)) 0 
              state_extra_special true).index));
    "state: drop special draw1 card index2" >:: (fun _ ->
        assert_equal "Everyone was skipped"((special_card_action 
                                               ( ExtraSpecial
                                                   (Red, SkipEveryone)) 0
                                               state_extra_special true).notes));

    "state: normal card top red card" >:: (fun _ ->
        assert_equal (Normal(Red, 7)) 
          ((special_card_action (Normal(Red, 7)) 0 
              state_extra_special true).top_card));
    "state: normal card top green card" >:: (fun _ ->
        assert_equal (Normal(Green, 0)) 
          ((special_card_action (Normal(Green, 0)) 0 
              state_extra_special true).top_card));

    (* generate_cards*)
    "generate_cards list length" >:: (fun _ ->
        assert_equal 5 (List.length(generate_cards 5 [] true))); 
    "generate_cards all elements are extra_special" >:: (fun _ ->
        assert_equal false (List.for_all (fun elt -> 
            match elt with 
            | ExtraSpecial (_,_) -> true 
            | _ -> false) ((generate_cards 5 [] true)))); 

    "string_card red swap" >:: (fun _ ->
        assert_equal "Red Swap" (string_card (ExtraSpecial(Red, Swap))));
    "string_card yellow discardall" >:: (fun _ ->
        assert_equal "Yellow DiscardAll" 
          (string_card (ExtraSpecial(Yellow, DiscardAll))));
    "string_card green skipeveryone" >:: (fun _ ->
        assert_equal "Green SkipEveryone" 
          (string_card (ExtraSpecial(Green, SkipEveryone))));
    "string_card green skipeveryone" >:: (fun _ ->
        assert_equal "Blue Draw1" 
          (string_card (ExtraSpecial(Blue, Draw1 ))));

    "string_card red 7" >:: (fun _ ->
        assert_equal "Red 7" (string_card (Normal(Red, 7))));
    "string_card blue 9" >:: (fun _ ->
        assert_equal "Blue 9" (string_card (Normal(Blue, 9))));
    "string_card green 0" >:: (fun _ ->
        assert_equal "Green 0" (string_card (Normal(Green, 0))));
    "string_card yellow 5" >:: (fun _ ->
        assert_equal "Yellow 5" (string_card (Normal(Yellow, 5))));

    "string_card blue skip" >:: (fun _ ->
        assert_equal "Blue Skip" 
          (string_card (Special(Blue, Skip ))));
    "string_card red reverse" >:: (fun _ ->
        assert_equal "Red Reverse" 
          (string_card (Special(Red, Reverse ))));
    "string_card yellow draw2" >:: (fun _ ->
        assert_equal "Yellow Draw2" 
          (string_card (Special(Yellow, Draw2))));
    "string_card green wildcard" >:: (fun _ ->
        assert_equal "Green Wildcard" 
          (string_card (Special(Green, Wildcard))));
    "string_card red wildcarddraw4" >:: (fun _ ->
        assert_equal "Red WildcardDraw4" 
          (string_card (Special(Red, WildcardDraw4))));

    (*update_index*)
    "state: update+index 2nd to last user" >:: (fun _ ->
        assert_equal 1 
          ((update_index 0 state)));
    "state: drop special draw2 card index" >:: (fun _ ->
        assert_equal 0 
          ((update_index 1 state)));

    (*random color*)
    "state: random color 0" >:: (fun _ ->
        assert_equal Red 
          ((random_color 0 )));
    "state: random color 1" >:: (fun _ ->
        assert_equal Yellow
          ((random_color 1 )));
    "state: random color 2" >:: (fun _ ->
        assert_equal Green
          ((random_color 2 )));
    "state: random color 3" >:: (fun _ ->
        assert_equal Blue
          ((random_color 3 )));
    "state: failwith" >:: (fun _ ->
        assert_raises (Failure "should be number 1 - 4") 
          (fun () -> random_color 99));
  ]

let card_tests =
  [
    (* Valid Match Testing----------------------------------------------------*)
    (* Testing valid match_cards *)
    "match_card: Normal and both same cards" >:: (fun _ -> 
        assert_equal (true) (match_card (Normal(Red,3)) (Normal (Red,3))));
    "match_card: Normal and same color but different number " >:: (fun _ -> 
        assert_equal (true) (match_card (Normal(Blue,4)) (Normal (Blue,0))));
    "match_card: Normal and different color but same number " >:: (fun _ -> 
        assert_equal (true) (match_card (Normal(Blue,4)) (Normal (Green,4))));
    "match_card: Normal and different color and different number" >::(fun _ -> 
        assert_equal (false) (match_card (Normal(Yellow,6))(Normal (Green,4))));
    "match_card: Wildcard" >:: (fun _ -> 
        assert_equal (true) (match_card  (Special (Undefined,Wildcard)) 
                               (Normal(Yellow,6))));
    "match_card: WildcardDraw4" >:: (fun _ -> 
        assert_equal (true) (match_card  (Special (Undefined,WildcardDraw4)) 
                               (Normal(Yellow,6))));
    "match_card: special and special - skip" >:: (fun _ -> 
        assert_equal (true) (match_card  (Special (Blue,Skip)) 
                               (Special (Red,Skip))));
    "match_card: special and special - reverse" >:: (fun _ -> 
        assert_equal (true) (match_card  (Special (Blue,Reverse)) 
                               (Special (Red,Reverse))));
    "match_card: special and special - draw2" >:: (fun _ -> 
        assert_equal (true) (match_card  (Special (Blue,Draw2)) 
                               (Special (Red,Draw2))));
    "match_card: special and special - wildcard" >:: (fun _ -> 
        assert_equal (true) (match_card  (Special (Blue,Draw2)) 
                               (Special (Blue,Wildcard))));
    "match_card: Normal and Special (Wildcard)" >::(fun _ -> 
        assert_equal (true) (match_card (Normal(Yellow,6))
                               (Special (Yellow,Wildcard))));
    "match_card: Normal and Special same color" >::(fun _ -> 
        assert_equal (true) (match_card (Normal(Yellow,6))
                               (Special (Yellow,Draw2))));

    (* DiscardAll Match Testing-----------------------------------------------*)
    (* ---Testing valid DiscardAll_card---------------------------------------*)
    "match_card: Same color DiscardAll" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Blue,DiscardAll)) 
                               (ExtraSpecial (Blue,DiscardAll))));
    "match_card: Yellow and Green color DiscardAll" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Yellow,DiscardAll)) 
                               (ExtraSpecial (Green,DiscardAll))));
    "match_card: Different color DiscardAll" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Red,DiscardAll)) 
                               (ExtraSpecial (Blue,DiscardAll))));
    "match_card: Different color DiscardAll" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Yellow,DiscardAll)) 
                               (ExtraSpecial (Blue,DiscardAll))));
    "match_card: Normal and  DiscardAll" >:: (fun _ -> 
        assert_equal (true) (match_card (Normal (Blue,5)) 
                               (ExtraSpecial (Blue,DiscardAll))));
    "match_card: Normal and DiscardAll2" >:: (fun _ -> 
        assert_equal (true) (match_card (Normal (Red,3)) 
                               (ExtraSpecial (Red,DiscardAll))));
    "match_card: Special and DiscardAll" >:: (fun _ -> 
        assert_equal (true) (match_card (Special (Blue,Draw2)) 
                               (ExtraSpecial (Blue,DiscardAll))));
    "match_card: Special and DiscardAll2" >:: (fun _ -> 
        assert_equal (true) (match_card (Special (Red,Reverse)) 
                               (ExtraSpecial (Red,DiscardAll))));
    "match_card: Special and DiscardAll3" >:: (fun _ -> 
        assert_equal (true) (match_card (Special (Green,Skip)) 
                               (ExtraSpecial (Green,DiscardAll))));
    "match_card: Special and DiscardAll4" >:: (fun _ -> 
        assert_equal (true) (match_card (Special (Undefined,Wildcard)) 
                               (ExtraSpecial (Green,DiscardAll))));
    "match_card: Special and DiscardAll5" >:: (fun _ -> 
        assert_equal (true) (match_card (Special (Undefined,WildcardDraw4)) 
                               (ExtraSpecial (Green,DiscardAll))));
    "match_card: extraSpecial and DiscardAll" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Green,Swap)) 
                               (ExtraSpecial (Green,DiscardAll))));
    "match_card: extraSpecial and DiscardAll2" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Yellow,SkipEveryone)) 
                               (ExtraSpecial (Yellow,DiscardAll))));
    "match_card: extraSpecial and DiscardAll3" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Yellow,Draw1)) 
                               (ExtraSpecial (Yellow,DiscardAll))));
    (* ---Testing invalid DiscardAll_card-------------------------------------*)
    "match_card: invalid1" >:: (fun _ -> 
        assert_equal (false) (match_card (ExtraSpecial (Blue,DiscardAll)) 
                                (ExtraSpecial (Red,SkipEveryone))));
    "match_card: invalid2" >:: (fun _ -> 
        assert_equal (false) (match_card (ExtraSpecial (Blue,DiscardAll)) 
                                (Normal (Red,2))));
    "match_card: invalid3" >:: (fun _ -> 
        assert_equal (false) (match_card (ExtraSpecial (Blue,DiscardAll)) 
                                (Special (Red,Draw2))));

    (* Swap Match Testing-----------------------------------------------------*)
    (* ---Testing valid Swaps-------------------------------------------------*)
    "match_card: Same color Swaps" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Blue,Swap)) 
                               (ExtraSpecial (Blue,Swap))));
    "match_card: Different color Swaps" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Red,Swap)) 
                               (ExtraSpecial (Blue,Swap))));
    "match_card: Normal and Swap" >:: (fun _ -> 
        assert_equal (true) (match_card (Normal (Blue,9)) 
                               (ExtraSpecial (Blue,Swap))));
    "match_card: Special and Swap" >:: (fun _ -> 
        assert_equal (true) (match_card (Special (Blue,Draw2)) 
                               (ExtraSpecial (Blue,Swap))));
    "match_card: Special and Swap" >:: (fun _ -> 
        assert_equal (true) (match_card (Special (Red,Reverse)) 
                               (ExtraSpecial (Red,Swap))));
    "match_card: Wildcard and Swap" >:: (fun _ -> 
        assert_equal (true) (match_card (Special (Undefined,Wildcard)) 
                               (ExtraSpecial (Green,Swap))));
    "match_card: Special and Swap" >:: (fun _ -> 
        assert_equal (true) (match_card (Special (Undefined,WildcardDraw4)) 
                               (ExtraSpecial (Green,Swap))));
    "match_card: extraSpecial and Swap" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Red,Swap)) 
                               (ExtraSpecial (Red,DiscardAll))));
    "match_card: extraSpecial and Swap" >:: (fun _ -> 
        assert_equal (true) (match_card (ExtraSpecial (Yellow,Swap)) 
                               (ExtraSpecial (Yellow,Draw1))));
    (* ---Testing invalid Swap------------------------------------------------*)
    "match_card: invalid1" >:: (fun _ -> 
        assert_equal (false) (match_card (ExtraSpecial (Blue,Swap)) 
                                (ExtraSpecial (Red,SkipEveryone))));
    "match_card: invalid2" >:: (fun _ -> 
        assert_equal (false) (match_card (ExtraSpecial (Blue,Swap)) 
                                (Normal (Red,2))));
    "match_card: invalid3" >:: (fun _ -> 
        assert_equal (false) (match_card (ExtraSpecial (Blue,Swap)) 
                                (Special (Red,Draw2))));

    "string_color: red" >:: (fun _ -> assert_equal ("Red ") (string_color Red));
    "string_color: yellow" >:: (fun _ -> 
        assert_equal ("Yellow ") (string_color Yellow));
    "string_color: green" >:: (fun _ -> 
        assert_equal ("Green ") (string_color Green));
    "string_color: blue" >:: (fun _ -> 
        assert_equal ("Blue ") (string_color Blue));
    "string_color: undefined" >:: (fun _ -> 
        assert_equal ("") (string_color Undefined));

    "string_special: skip" >:: (fun _ -> 
        assert_equal ("Skip") (string_special Skip));
    "string_special: reverse" >:: (fun _ -> 
        assert_equal ("Reverse") (string_special Reverse));
    "string_special: draw2" >:: (fun _ -> 
        assert_equal ("Draw2") (string_special Draw2));
    "string_special: wildcard" >:: (fun _ -> 
        assert_equal ("Wildcard") (string_special Wildcard));
    "string_special: wildcarddraw4" >:: (fun _ -> 
        assert_equal ("WildcardDraw4") (string_special WildcardDraw4));

    "extra_special: swap" >:: (fun _ -> 
        assert_equal ("Swap") (string_extra_special Swap));
    "extra_special: swap" >:: (fun _ -> 
        assert_equal ("DiscardAll") (string_extra_special DiscardAll));
    "extra_special: swap" >:: (fun _ -> 
        assert_equal ("SkipEveryone") (string_extra_special SkipEveryone));
    "extra_special: swap" >:: (fun _ -> 
        assert_equal ("Draw1") (string_extra_special Draw1));

    "card_to_object_phrase: normal" >:: (fun _ -> 
        assert_equal ["Red "; "1"] (card_to_object_phrase (Normal (Red, 1))));
    "card_to_object_phrase: normal" >:: (fun _ -> 
        assert_equal ["Yellow "; "2"] 
          (card_to_object_phrase (Normal (Yellow, 2))));
    "card_to_object_phrase: normal" >:: (fun _ -> 
        assert_equal ["Green "; "3"] 
          (card_to_object_phrase (Normal (Green, 3))));
    "card_to_object_phrase: normal" >:: (fun _ -> 
        assert_equal ["Blue "; "4"] 
          (card_to_object_phrase (Normal (Blue, 4))));
    "card_to_object_phrase: special" >:: (fun _ -> 
        assert_equal ["Red "; "Skip"] 
          (card_to_object_phrase (Special (Red, Skip))));
    "card_to_object_phrase: special" >:: (fun _ -> 
        assert_equal ["Yellow "; "Reverse"] 
          (card_to_object_phrase (Special (Yellow, Reverse))));
    "card_to_object_phrase: special" >:: (fun _ -> 
        assert_equal ["Green "; "Draw2"] 
          (card_to_object_phrase (Special (Green, Draw2))));
    "card_to_object_phrase: special" >:: (fun _ -> 
        assert_equal ["Blue "; "Skip"] 
          (card_to_object_phrase (Special (Blue, Skip))));
    "card_to_object_phrase: special wildcard" >:: (fun _ -> 
        assert_equal ["Wildcard"] 
          (card_to_object_phrase (Special (Undefined, Wildcard))));
    "card_to_object_phrase: special wildcarddraw4" >:: (fun _ -> 
        assert_equal ["WildcardDraw4"] 
          (card_to_object_phrase (Special (Undefined, WildcardDraw4))));
    "card_to_object_phrase: extra special" >:: (fun _ -> 
        assert_equal ["Red "; "Swap"] 
          (card_to_object_phrase (ExtraSpecial (Red, Swap))));
    "card_to_object_phrase: extra special" >:: (fun _ -> 
        assert_equal ["Yellow "; "DiscardAll"] 
          (card_to_object_phrase (ExtraSpecial (Yellow, DiscardAll))));
    "card_to_object_phrase: extra special" >:: (fun _ -> 
        assert_equal ["Green "; "SkipEveryone"] 
          (card_to_object_phrase (ExtraSpecial (Green, SkipEveryone))));
    "card_to_object_phrase: extra special" >:: (fun _ -> 
        assert_equal ["Blue "; "Draw1"] 
          (card_to_object_phrase (ExtraSpecial (Blue, Draw1))));

    "is_special: skip" >:: (fun _ -> 
        assert_equal true (is_special (Special (Red, Skip))));
    "is_special: reverse" >:: (fun _ -> 
        assert_equal true (is_special (Special (Red, Reverse))));
    "is_special: draw2" >:: (fun _ -> 
        assert_equal true (is_special (Special (Red, Draw2))));
    "is_special: wildcard" >:: (fun _ -> 
        assert_equal true (is_special (Special (Undefined, Wildcard))));
    "is_special: wildcarddraw4" >:: (fun _ -> 
        assert_equal true (is_special (Special (Undefined, WildcardDraw4))));
    "is_special: swap" >:: (fun _ -> 
        assert_equal true (is_special (ExtraSpecial (Red, Swap))));
    "is_special: discardall" >:: (fun _ -> 
        assert_equal true (is_special (ExtraSpecial (Red, DiscardAll))));
    "is_special: skipeveryone" >:: (fun _ -> 
        assert_equal true (is_special (ExtraSpecial (Red, SkipEveryone))));
    "is_special: draw1" >:: (fun _ -> 
        assert_equal true (is_special (ExtraSpecial (Red, Draw1))));
    "is_special: false" >:: (fun _ -> 
        assert_equal false (is_special (Normal (Red, 1))));

    "get_card_from_list: illegal" >:: (fun _ -> 
        assert_equal Illegal (get_card_from_list []));
    "get_card_from_list: wildcard" >:: (fun _ -> 
        assert_equal (Legal (Special (Undefined, Wildcard))) 
          (get_card_from_list ["wildcard"]));
    "get_card_from_list: wildcarddraw4" >:: (fun _ -> 
        assert_equal (Legal (Special (Undefined, WildcardDraw4))) 
          (get_card_from_list ["wildcarddraw4"]));
    "get_card_from_list: illegal" >:: (fun _ -> 
        assert_equal (Illegal) (get_card_from_list ["skip"]));
    "get_card_from_list: normal" >:: (fun _ -> 
        assert_equal (Legal (Normal (Red, 1))) 
          (get_card_from_list ["Red "; "1"]));
    "get_card_from_list: normal" >:: (fun _ -> 
        assert_equal (Legal (Normal (Yellow, 2))) 
          (get_card_from_list ["Yellow "; "2"]));
    "get_card_from_list: normal" >:: (fun _ -> 
        assert_equal (Legal (Normal (Green, 3))) 
          (get_card_from_list ["Green "; "3"]));
    "get_card_from_list: normal" >:: (fun _ -> 
        assert_equal (Legal (Normal (Blue, 4))) 
          (get_card_from_list ["Blue "; "4"]));
    "get_card_from_list: special" >:: (fun _ -> 
        assert_equal (Legal (Special (Red, Skip))) 
          (get_card_from_list ["Red "; "Skip"]));
    "get_card_from_list: special" >:: (fun _ -> 
        assert_equal (Legal (Special (Yellow, Reverse))) 
          (get_card_from_list ["Yellow "; "Reverse"]));
    "get_card_from_list: special" >:: (fun _ -> 
        assert_equal (Legal (Special (Green, Draw2))) 
          (get_card_from_list ["Green "; "Draw2"]));
    "get_card_from_list: extra special" >:: (fun _ -> 
        assert_equal (Legal (ExtraSpecial (Red, Swap))) 
          (get_card_from_list ["Red "; "Swap"]));
    "get_card_from_list: extra special" >:: (fun _ -> 
        assert_equal (Legal (ExtraSpecial (Yellow, DiscardAll))) 
          (get_card_from_list ["Yellow "; "DiscardAll"]));
    "get_card_from_list: extra special" >:: (fun _ -> 
        assert_equal (Legal (ExtraSpecial (Green, SkipEveryone))) 
          (get_card_from_list ["Green "; "SkipEveryone"]));
    "get_card_from_list: extra special" >:: (fun _ -> 
        assert_equal (Legal (ExtraSpecial (Blue, Draw1))) 
          (get_card_from_list ["Blue "; "Draw1"]));
  ]

let suite =
  "test suite for UNO"  >::: List.flatten [
    command_tests;
    state_tests;
    card_tests;
  ]

let _ = run_test_tt_main suite