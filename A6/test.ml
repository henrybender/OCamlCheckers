open OUnit2
open Board
open Parser

(********************************************************************
   Helper FUnctions
 ********************************************************************)
(** [pp_string s] pretty-prints string [s]. *)
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

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2


(** [make_generic_test name fn expect] constructs an OUnit
    test named [name] that asserts the quality of [expect]
    with [fn]. *)
let make_generic_test name fn expect =
  name >:: (fun _ ->
      print_endline name; assert_equal expect (fn))

let make_gen1_exception_test name fn input1 input2 e = 
  name >:: (fun _ -> 
      print_string name; assert_raises e (fun () -> fn input1 input2))

let make_gen2_exception_test name fn input1 input2 input3 e = 
  name >:: (fun _ -> 
      print_string name; assert_raises e (fun () -> fn input1 input2 input3))


let switchPlayer color = match color with 
  | White -> Red 
  | Red -> White

(********************************************************************
   End helper functions.
 ********************************************************************)

(********************************************************************
   Begin Test Values
 ********************************************************************)


let startingbd = Board.make_game
let emptybd = {Board.make_game with curr_board = Array.make_matrix 8 8 None}
let whitewinbd = Array.make_matrix 8 8 None
let _ = whitewinbd.(0).(1)<-Some(White,Pawn)
let redwinbd = Array.make_matrix 8 8 None
let _ = redwinbd.(4).(0)<-Some(Red, Pawn) 
let whiteWin = {curr_board = whitewinbd;curr_player = Red}
let redWin = {curr_board = redwinbd; curr_player = White}

let demo_win_board = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;Some (Red,Pawn);None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;Some (White, Pawn);None;None;None;None;None;None;|];
  [|None;None;Some (Red, Pawn);None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];|] 

(********************************************************************
   End values. 
 ********************************************************************)

(********************************************************************
   Begin Test Vectors
 ********************************************************************)

let valid_move_tests = [

  make_generic_test "check switches to White" (move startingbd (5, 0) (4, 1)).curr_player (switchPlayer startingbd.curr_player);
  make_generic_test "check switches to Red" (move startingbd (2, 1) (3, 2)).curr_player (switchPlayer startingbd.curr_player);
  make_generic_test "check move 3" (valid_move startingbd (4, 1) (2, 3)) Invalid;
  make_generic_test "Invalid Move Red to Occupied Spot" (move startingbd (4, 1) (2, 3)).curr_board startingbd.curr_board; 
  make_generic_test "check move 4" (valid_move startingbd (3, 2) (5, 0)) Invalid;
  make_generic_test "Invalid Move Red attempt move enemy piece" (move startingbd (3, 2) (5, 0)).curr_board startingbd.curr_board;
  make_generic_test "Legal Red Move" (move startingbd (5, 4) (4, 5)).curr_board startingbd.curr_board;
  make_generic_test "test white capture red" (move startingbd (3, 2) (5, 0)).curr_board startingbd.curr_board;
  make_generic_test "check move 7" (valid_move startingbd (4, 5) (5, 4)) Invalid;
  make_generic_test "Illegal Red Move backwards" (move startingbd (4, 5) (5, 4)).curr_board startingbd.curr_board;
  make_generic_test "Legal Red Move" (move startingbd (4, 5) (3, 6)).curr_board startingbd.curr_board;
  make_generic_test "Legal White Move" (move startingbd (2, 3) (3, 2)).curr_board startingbd.curr_board;
  make_generic_test "Legal Red Move" (move startingbd (5, 2) (4, 1)).curr_board startingbd.curr_board;

  make_generic_test "check move 11" (valid_move startingbd (5, 2) (4, 1)) Invalid;
  make_generic_test "Illegal White Move Back" (move startingbd (5, 2) (4, 1)).curr_board startingbd.curr_board;
  make_generic_test "check move 12" (valid_move startingbd (1, 6) (3, 4)) Invalid;
  make_generic_test "Illegal White Capture Self" (move startingbd (1, 6) (3, 4)).curr_board startingbd.curr_board;
  make_generic_test "Legal White Move" (move startingbd (2, 5) (3, 4)).curr_board startingbd.curr_board;
  make_generic_test "Red Capture White" (move startingbd (4, 1) (2, 3)).curr_board startingbd.curr_board; 
] 


let check_win_tests = [
  make_generic_test "check red no win" (check_win startingbd ) false;
  make_generic_test "check white no win" (check_win startingbd ) false;
  make_generic_test "check white win" (check_win whiteWin) true;
  make_generic_test "check red win" (check_win redWin ) true;
]

let deadlock_board = [| 
  [|None;None;None;None;Some(White,Pawn);None;Some(White,Pawn);None;|];
  [|None;None;None;None;None;Some (Red,Pawn);None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];|] 

let deadlock_board2 = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;Some(White,Pawn);None;None;None;|];
  [|None;None;None;Some(Red,Pawn);None;Some(Red,Pawn);None;None;|];|] 

let nopossmove_win_tests = 
  [
    make_generic_test "deadlock board" (check_win 
                                          {curr_board = deadlock_board; 
                                           curr_player = Red}) true;
    make_generic_test "deadlock board2" (check_win 
                                           {curr_board = deadlock_board2; 
                                            curr_player = White}) true;

  ]

let getcapture_tests = 
  [
    make_generic_test "get_captures empty list" (get_captures []) [];
    make_generic_test "get_captures 1" (get_captures [((1,1),(2,2),true);
                                                      ((1,3),(4,2),false)]) 
      [((1,1),(2,2),true)];
    make_generic_test "get_captures 2" (get_captures 
                                          (possible_moves 
                                             {curr_board = deadlock_board2;
                                              curr_player = Red}))
      [((7,3),(5,5),true);((7,5),(5,3),true)];
    make_generic_test "get_captures empty matrix" (get_captures 
                                                     (possible_moves 
                                                        {curr_board = Array.make_matrix 8 8 None;
                                                         curr_player = Red})) [];
    make_generic_test "get_captures no available captures" (get_captures 
                                                              (possible_moves 
                                                                 {curr_board = deadlock_board;
                                                                  curr_player = Red})) [];

  ]

let teststartbd = [| 
  [|None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn) |]; 
  [|Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn); None |]; 
  [|None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn) |]; 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None |]; 
  [|None; Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn) |]; 
  [|Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None |]; 
|] 
let possible_moves_test = [
  make_generic_test "no possible moves" (possible_moves emptybd) [];
  make_generic_test "demo win board red moves" 
    (cmp_set_like_lists 
       (possible_moves {Board.make_game with curr_board = demo_win_board })
       [
         ((1, 5), (0, 4), false);
         ((1, 5), (0, 6), false);
         ((5, 2), (3, 0), true);
         ((5, 2), (4, 3), false);

       ]) true;
  make_generic_test "demo win board with curr_player = white" 
    (cmp_set_like_lists 
       (possible_moves {curr_board = demo_win_board; curr_player = White })
       [
         ((4, 1), (6, 3), true);
         ((4, 1), (5, 0), false);
       ]) true;

  make_generic_test "starting board" 
    (cmp_set_like_lists 
       (possible_moves {curr_board = teststartbd;curr_player = Red;})
       [
         ((5, 6), (4, 5), false);
         ((5, 6), (4, 7), false);
         ((5, 4), (4, 3), false);
         ((5, 4), (4, 5), false);
         ((5, 2), (4, 3), false);
         ((5, 2), (4, 1), false);
         ((5, 0), (4, 1), false);
       ]) true;

]

let one_piece_board = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;Some (Red, Pawn);None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];|] 

let one_jump_board = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;Some (White, Pawn);None;None;None;None;None;None;|];
  [|None;None;Some (Red, Pawn);None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];|] 

let post_one_jump_board = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|Some (Red, Pawn);None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];|] 


let two_jump_board = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;Some (White, Pawn);None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None; Some (White, Pawn);None;None;None;None;None;|];
  [|None;None;None; Some (Red, Pawn);None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];|] 

let post_two_jump_board = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;Some (Red, Pawn);None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];|] 

let post_two_jump_promotion_board = [| 
  [|None;None;None;Some(Red, King);None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;Some (White, King);None;None;None;None;None;None;|];|] 

let two_jump_promotion_board = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;Some (White, Pawn);None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None; Some (White, Pawn);None;None;None;None;None;|];
  [|None;None;None; Some (Red, Pawn);None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;Some (White, King);None;None;None;None;None;None;|];|] 

let one_pc_bd = {curr_board = one_piece_board;
                 curr_player = Red}
let post_one_pc_bd = {curr_board = one_piece_board;
                      curr_player = White}
let one_jump_bd = {curr_board = one_jump_board;
                   curr_player = Red}
let post_one_jump_bd = {curr_board = post_one_jump_board; 
                        curr_player = White}
let two_jump_bd = {curr_board = two_jump_board;
                   curr_player = Red}
let post_two_jump_bd = {curr_board = post_two_jump_board; 
                        curr_player = White}
let two_jump_promotion_bd = {curr_board = two_jump_promotion_board; curr_player = Red}
let post_two_jump_promotion_bd = {curr_board = post_two_jump_promotion_board; curr_player = White}


let multiple_jump_test = 
  [
    make_generic_test "no jumps" (multiple_jump (5,2) one_pc_bd) post_one_pc_bd; 
    make_generic_test "one jump" (multiple_jump (5,2) one_jump_bd) post_one_jump_bd; 
    make_generic_test "2 jumps" (multiple_jump (5,3) two_jump_bd) post_two_jump_bd;
    make_generic_test "2 jumps with promotion" (multiple_jump (4,3) two_jump_promotion_bd) post_two_jump_promotion_bd
  ]

let board_tests =
  List.flatten [
    valid_move_tests;
    check_win_tests; 
    possible_moves_test;
    multiple_jump_test;
    nopossmove_win_tests;
    getcapture_tests;
  ]





let suite =
  "test suite for A6"  >::: List.flatten [
    board_tests;
  ]

let _ = run_test_tt_main suite

