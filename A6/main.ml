open Board
open Parser

(** [pp_string] will provide the format for the pretty-printing *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [print bd] prints the possible moves of the current board*)
let print bd = print_endline(pp_list (fun ((x1, x2), (y1, y2), z) -> Printf.sprintf "((%d, %d), (%d, %d), %s)" x1 x2 y1 y2 (string_of_bool z) ) (possible_moves bd))


(** Define the characters that will be used later in the game *)
let intro =  "Welcome to OcamlCheckers!"
let prompt_char = ">"

(** [refresher]: When restart the game, refresh will deperate the old game and
    the new game by a line *)
let refresher() = 
  for i=0 to 100 do print_endline ""; done

(** [print_move_instructions] will be called at the beginning of the
    game or 'help' is inputed *)
let print_move_instructions () = 
  let _ = print_endline("--- INSTRUCTIONS ---") in 
  let _ = print_endline("Moves are entered in the following format:") in
  let _ = print_endline("move <source_row> <source_column> to <destination_row> <destination_column>") in 
  let _ = print_endline("for example, 'move f 0 to e 1', which will move the piece from location f 0 to location e 1") in
  let _ = print_endline("type 'quit' to exit") in
  print_endline("type 'help' to see the instructions again")

(** [invalid_move_msg] will output the warning when the move is invalid *)
let invalid_move_msg () = 
  print_endline("Invalid Move! Please try again!")

(** [other_player] is the function that helps to switch the players *)
let other_player g = 
  if g.curr_player = Red then White else Red

(* 
let rec possible_moves_filter possible_moves_list = 
  match possible_moves_list with 
  | [] -> None
  | (srt, dst, true)::tl -> Some(srt, dst)
  | _ -> possible_moves_filter tl *)

(** [game_input] takes a game and return a game, it keeps the game running *)
let rec game_input g = 
  let _ = print_board g in
  if check_win g then let _  = print_string((string_of_color (other_player g))^" wins!\n") in exit 0
  else 
    let _ = print_string((string_of_color g.curr_player)^"'s turn: Enter move\n") in
    let _ = print_string prompt_char in 
    let cmd_type = parse_cmd(read_line()) in 
    match cmd_type with
    | Quit -> exit 0
    | Help -> print_move_instructions(); game_input g
    | Move(src, dst) -> 
      (* if square_empty src g then let _ = invalid_move_msg() in game_input g *)
      begin
        match (enforced_captures g src dst) with 
        | Valid -> game_input (move g src dst)
        | Invalid -> let _ = invalid_move_msg () in game_input g
      end
    |_ -> print_endline("Please enter a game command"); game_input g 

(** [game_input_ai] takes in the game and run the Person vs. AI mode, which will 
    consider the White as AI, Red as the player*)
let rec game_input_ai g = 
  let _ = print_board g in
  if check_win g then let _ = print_string((string_of_color (other_player g))^" wins!\n") in exit 0
  else 
  if g.curr_player = Red
  then 
    begin
      let _ = print_string((string_of_color g.curr_player)^"'s turn: Enter move\n") in
      let _ = print_string prompt_char in 
      let cmd_type = parse_cmd(read_line()) in 
      match cmd_type with
      | Quit -> exit 0
      | Help -> print_move_instructions(); game_input_ai g
      | Move(src, dst) -> 
        begin
          match (enforced_captures g src dst) with 
          | Valid -> game_input_ai (move g src dst)
          | Invalid -> let _ = invalid_move_msg () in game_input_ai g 
        end
      |_ -> print_endline("Please enter a game command"); game_input_ai g
    end
  else 
    let _ = print_endline("AI's turn") in
    let _ = print_endline("AI is thinking...") in 
    Unix.sleepf 1.5;
    let potential_ai_moves = possible_moves g in 
    if potential_ai_moves = [] then 
      let _ = print_endline("Red wins!") in exit 0
    else 
      let capturelist = get_captures potential_ai_moves in 
      if capturelist= [] then 
        let (src,dst) = pick_move potential_ai_moves g 5 in
        game_input_ai (move g src dst)
      else let (src,dst,bool) = List.hd capturelist in
        game_input_ai (move g src dst)


(** [game_loop] takes in the game and keeps runnig it by inputing a new game status *)
let game_loop g is_ai = 
  if is_ai then
    game_input_ai g
  else 
    game_input g

(** [main_input] asks for commands and passes it to the parser *)
let rec main_input () = 
  let _ = print_string prompt_char in 
  let i = parse_cmd(read_line()) in 
  match i with 
  | PvP -> print_endline("");
    print_endline("Starting PvP Mode");
    print_endline("");
    print_move_instructions();
    game_loop make_game false
  | PvAI -> print_endline("");
    print_endline("Starting PvAI Mode");
    print_endline("");
    print_move_instructions();
    game_loop make_game true
  | Quit -> exit 0
  | Help -> print_endline("Please enter a valid command");
    main_input()
  | Move(a,b) -> print_endline("Please enter a valid command");
    main_input()
  | InvalidCmd -> print_endline("Please enter a valid command");
    main_input()

let main () = 
  print_endline intro;
  print_endline "";
  print_endline "There are two modes for this game, one is Person vs. Person, the other is Person vs. AI. 
  Please input 'PvP' to start the mode Person vs. Person, input 'PvAI' to start the mode Person vs. AI!";
  print_endline "Type 'quit' to exit";
  main_input()

let _ = main()


