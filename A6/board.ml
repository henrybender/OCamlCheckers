type color = Red|White
type rank = King|Pawn
type piece = color*rank

(** The abstract type which represents the current state of the board*)
type t = {
  curr_board: ((piece option) array) array;
  curr_player:color;
}
(** Validity of a move*)
type validity = Valid|Invalid
(** Position of a piece*)
type position = int*int

(** [string_of_color c] returns string representation of color [c]*)
let string_of_color c = 
  match c with
  |Red -> "Red"
  |White -> "White"

let two_jump_promotion_board = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;Some (White, Pawn);None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None; Some (White, Pawn);None;None;None;None;None;|];
  [|None;None;None; Some (Red, Pawn);None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;Some (White, King);None;None;None;None;None;None;|];|] 


let demo_win_board = [| 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;Some (Red,Pawn);None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;Some (White, Pawn);None;None;None;None;None;None;|];
  [|None;None;Some (Red, Pawn);None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];|] 

let starting_board = [| 
  [|None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn) |]; 
  [|Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn); None |]; 
  [|None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn); None; Some (White, Pawn) |]; 
  [|None;None;None;None;None;None;None;None;|];
  [|None;None;None;None;None;None;None;None;|];
  [|Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None |]; 
  [|None; Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn) |]; 
  [|Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None; Some (Red, Pawn); None |]; 
|] 


(** [make_game] intializes the board *)
let make_game = 
  {curr_board = starting_board;
   curr_player = Red;
  }

(** [rank_at bd (x,y)] returns rank option at board position [(x,y)] *)
let rank_at bd (x, y) : rank option = 
  match bd.curr_board.(x).(y) with
  | None -> None
  | Some (_, r) -> Some r

(** [color_at bd (x,y)] returns color option at board position [(x,y)] *)
let color_at bd (x, y) : color option = 
  match bd.curr_board.(x).(y) with
  | None -> None
  | Some (c, _) -> Some c



(**[cmp bd (sx,sy) (dx,dy)] is 
   - Red, White  -> 1
   - Red, Red || White, White -> 0
   - White, Red -> -1
   - _, None -> 0
     = None, _ -> 0
*)
let cmp bd src dst =
  let dst_c = color_at bd dst in 
  match (color_at bd src) with
  | Some Red when (dst_c = Some White) -> 1
  | Some White when (dst_c = Some Red) -> -1
  | _ -> 0

(** [valid_dir bd (sx, sy) (dx, dy) ] is true if the piece at src is 
    allowed to move in the direction of dst
    Red can only move up
    White can only move down
    King can move either
*)
let valid_dir bd (sx, sy) ((dx:int), dy) = let cond = 
                                             match bd.curr_player with 
                                             | Red -> (dx - sx) < 0 
                                             | White -> (dx - sx) > 0 
  in cond || (rank_at bd (sx, sy) = Some King)

(**[valid_jump bd (sx,sy) (dx,dy)] is [true] if the jump from (sx, sx) to 
   (dx, dy) is valid*)
let valid_jump (bd:t) src dst = 
  let (sx,sy),(dx,dy) = src,dst in
  let fx, fy = dx - sx, dy - sy in
  let x = sx + fx/2 in let y = sy + fy/2 in
  (abs fx = 2 && abs fy = 2) && 
  cmp bd (sx, sy) (x, y)  <> 0 && (* if there is a piece at the capture position 
                                     with color != curr_player*)
  valid_dir bd (sx, sy) (dx, dy) && (* if we are capturing in a valid direction*)
  color_at bd (dx, dy) = None (* if dst is empty *)

(** [valid_move bd src dst] returns validity of the move from 
    [src] to [dst] on the board [bd].*)
(* 1. check that src, dst are valid position on board
   2.  check that piece on src is the current player's piece (check color 
   i.e. Red player cannot move White piece )
   3. if pawn, check that the move is diagonal forward (except for capture)
   4. if king, check that move is diagonal forward or backward 
   5. check if there is already a piece on dst 
   6. check that if it's a jump it is a valid jump *)
let valid_move (bd:t) (src:position) (dst:position) = 
  match src,dst with
  (*1.*) 
  (* (sx,sy) is source row, source col position and (dx,dy) is dest row, 
     dest col position*)
  |(sx,sy),(dx,dy)-> if sx < 0 || sx > 7 || sy < 0 || sy > 7 || dx < 0 
                        || dx > 7 || dy < 0 || dy > 7 
    then Invalid
    (*2.*)
    else match bd.curr_board.(sx).(sy) with
      |None -> Invalid
      |Some (c,r) -> if bd.curr_player <> c 
        then Invalid 
        (*3.4.*)
        else if abs (dx-sx) = 1 && abs (dy-sy) = (1) && valid_dir bd src dst
        (*5.*)
        then match bd.curr_board.(dx).(dy) with
          |None->Valid
          |Some _ -> Invalid
          (*6.*)
        else let validjump = valid_jump bd src dst in if validjump 
          then Valid 
          else Invalid

(** [possible_move_help bd src mag] checks whether the piece at src can move
    [mag] distance in any of the 4 diagonal directions. 
    requires: [src] is a valid piece owned by [bd.curr_player]*)
let possible_move_help bd src mag : ((position*position*bool)list)=
  let (x, y) = src in 
  let dx_ = [-mag; -mag; mag; mag] in 
  let dy_ = [-mag; mag; -mag; mag] in
  let rec test_diags ls dx dy = match dx, dy with
    | dx'::dxs, dy'::dys -> 
      begin
        if ((valid_move bd src (x+dx', y+dy')) = Valid) 
        then test_diags ((src, (x+dx', y + dy'), mag = 2)::ls) dxs dys 
        else test_diags ls dxs dys 
      end
    | _ -> ls
  in test_diags [] dx_ dy_

(** [check_row bd row j ls] returns a list of possible moves for a single 
    row of the board *)
let rec check_row bd row j ls =  
  if (j < 0) then ls else
    match (color_at bd (row,j)) with 
    | Some c when c = bd.curr_player -> 
      check_row bd row (j -1) (ls@(possible_move_help bd (row, j) 1)@
                               (possible_move_help bd (row, j) 2))
    | _ -> check_row bd row (j-1) ls 

(**[possible_move bd] is the list of all possible moves the current player 
   can make. *)
let possible_moves bd : ((position*position*bool) list) =
  (check_row bd 7 7 []) |> (check_row bd 6 7) |>  (check_row bd 5 7 ) |> 
  (check_row bd 4 7) 
  |> (check_row bd 3 7) |> (check_row bd 2 7) |> (check_row bd 1 7) |> 
  (check_row bd 0 7)
(* TODO: maybe make this nicer. *)


(** [remove currbd pos] returns the board after removing piece in position [pos] *)
let remove (currbd:((piece option) array) array) pos =
  match pos with
  |(x,y)-> let row = currbd.(x) in
    row.(y)<- None;
    currbd.(x)<- row;
    currbd

(** [add currbd p pos] returns the board after adding piece [p] in position [pos] *)
let add (currbd:((piece option) array) array) p pos = 
  match pos with
  |(x,y)-> let row = currbd.(x) in
    row.(y)<- Some p;
    currbd.(x)<- row;
    currbd

(** [change_king currplayer dx] checks if [currplayer]'s piece is moving to a row 
    closest to the opponent, and thus the piece needs to be changed to a king*)
let change_king currplayer dx = 
  match currplayer with
  |Red->dx=0
  |White->dx=7


(** [checkNoReds] takes a piece option and returns true if it is not Red and 
    false otherwise *)
let checkNoReds = function
    (Some(e, _):piece option) -> e <> Red
  | _ -> true
(** [checkNoReds] takes a piece option and returns true if it is not White and 
    false otherwise *)
let checkNoWhites = function
  | (Some(e, _):piece option) ->  e <> White 
  | _ -> true


(** [check_win b] returns if the opposite player is the winner*)
let check_win bd = 
  match bd.curr_player with
  |White-> possible_moves bd = []|| Array.for_all (fun row -> Array.for_all (checkNoWhites) row) bd.curr_board
  |Red-> possible_moves bd = []|| Array.for_all  (fun row -> Array.for_all (checkNoReds) row) bd.curr_board

(** [string_of_pieceop po] returns a string representation of piece option [po] *)
let string_of_pieceop po = 
  match po with
  |Some (Red,King) -> "❤️"
  |Some (Red,Pawn) -> "🔴"
  |Some (White,King) -> "💟"
  |Some (White,Pawn) -> "⚪"
  |None -> " "




(**[print_board bd] prints the current board *)
let print_board bd = 
  let int_to_alphabet = function
    |0->"a"
    |1->"b"
    |2->"c"
    |3->"d"
    |4->"e"
    |5->"f"
    |6->"g"
    |7 ->"h"
    |_-> "" in
  let _ = print_endline "    0    1    2    3    4    5    6    7 " in
  let print_element = Array.fold_left(fun acc e -> acc^"  | "^string_of_pieceop e) 
      ("  -----------------------------------------\n") in 
  for i = 0 to  7 do 
    print_endline((print_element (bd.curr_board.(i))^"  | ")^" "^int_to_alphabet i); 
  done;
  print_string("  -----------------------------------------\n")

(** [get_captures movelist] returns the list of each move that is a capture 
    in [movelist] *)
let get_captures movelist = 
  let rec get_truelst acc lst = 
    match lst with 
    |[]->acc
    |(pos1,pos2,true)::t-> get_truelst ((pos1,pos2,true)::acc) t
    |(_,_,false)::t-> get_truelst acc t in 
  get_truelst [] movelist

(** possible_capture lst src returns the destination of any captures that can
    be made by the piece at src in the capture list lst.*)
let rec possible_capture lst src = 
  match lst with
  |[] -> None
  |(s, d, true)::xs -> if s = src then Some d else possible_capture xs src
  |_-> failwith "not a capture list"

(** [enforced_captures bd src dst] returns the checks that the move is valid
    and that it captures an opponent if it is possible *)
let enforced_captures bd src dst=
  let poss_moves = possible_moves bd in 
  let poss_cap = get_captures poss_moves in 
  if List.length poss_cap = 0 then  
    if List.mem (src, dst, false) poss_moves then Valid else  Invalid
  else 
  if List.mem(src, dst, true) poss_cap then Valid else 
    let _ = print_endline("You \"must\" jump over the opponent. There is a possible capture!") in Invalid

(**[multiple_jump src bd] returns the board after multiple mandatory jumps
   (if any) have been completed by the piece located at src*)
let rec multiple_jump src bd = 
  match rank_at bd src with
  |Some _ -> begin 
      let caps = get_captures (possible_moves bd) in 
      match possible_capture caps src with
      |None -> {bd with curr_player = if bd.curr_player = Red then White else Red}
      |Some dst -> let p = bd.curr_player in 
        let newbd = move bd src dst in 
        multiple_jump dst {newbd with curr_player = p}
    end
  |None -> {bd with curr_player = if bd.curr_player = Red then White else Red}

(** [move bd src dst] returns the board after the move from [src] to [dst], and
    handles all captures and king promotions. *)
and move bd src dst = 
  let (sx,sy),(dx,dy) = src, dst in 
  match bd.curr_board.(sx).(sy) with
  | None -> bd
  | Some piecetomove -> 
    let fx, fy = dx - sx, dy - sy in
    if abs fx = 2 then
      (* let _ = print_endline("You captured a piece! Keep going!") in *)
      let x = sx + fx/2 in let y = sy + fy/2 in
      let oppremoved = remove bd.curr_board (x,y) in
      let removedbd = remove (oppremoved) (sx,sy) in 
      if change_king bd.curr_player dx then   
        let kingpiece = (bd.curr_player,King) in     
        let updatedbd = add removedbd kingpiece (dx,dy) in
        multiple_jump dst {bd with curr_board = updatedbd}
      else
        let updatedbd = add removedbd piecetomove (dx,dy) in
        multiple_jump dst {bd with curr_board = updatedbd}

    else if change_king bd.curr_player dx then
      let kingpiece = (bd.curr_player,King) in 
      let removedbd = remove (bd.curr_board) (sx,sy) in 
      let updatedbd = add removedbd kingpiece (dx,dy) in
      {curr_board = updatedbd;
       curr_player = if bd.curr_player = Red then White else Red;
      } 
    else
      let removedbd = remove (bd.curr_board) (sx,sy) in 
      let updatedbd = add removedbd piecetomove (dx,dy) in
      {curr_board = updatedbd;
       curr_player = if bd.curr_player = Red then White else Red;
      } 


(** pick_move_helper bd x y returns true if the potential square to move to is
    not exposed to capturing by a red piece, and false if it is exposed to 
    capture by a red piece.*)
let pick_move_helper bd x y = 
  if x < 7 && y < 7 && x > 0 && y > 0 then 
    if bd.curr_board.(x+1).(y+1) = Some(Red, Pawn) || 
       bd.curr_board.(x-1).(y+1) = Some(Red, Pawn) || 
       bd.curr_board.(x-1).(y-1) = Some(Red, King) ||
       bd.curr_board.(x+1).(y-1) = Some(Red, King) then  true
    else false
  else if x < 7 && x > 0 && y > 0 then 
    if bd.curr_board.(x-1).(y-1) = Some(Red, King) ||
       bd.curr_board.(x+1).(y-1) = Some(Red, King) then true
    else false
  else if y < 7 && y > 0 && x > 0 then 
    if bd.curr_board.(x-1).(y+1) = Some(Red, Pawn) || 
       bd.curr_board.(x-1).(y-1) = Some(Red, King) then true
    else false
  else false

(**[pick_move mv_lst bd n] will return the optimal move for the AI given current 
   moves and the next possible move by the human player. pick_move checks for n 
   random moves to see if any of these result in not further exposing pieces to be
   captured by the human player, after n calls, it will just return the first move 
   possible in the mv_lst. *)
let rec pick_move mv_lst bd n = 
  if n = 0 then let (src, dst, b) = List.hd mv_lst in (src, dst)
  else
    let arr = Array.copy bd.curr_board in 
    let copy = {bd with curr_board = arr} in 
    let rand = Random.int (List.length mv_lst) in

    let (src, dst, b) = List.nth mv_lst (rand) in 
    let x = fst dst in 
    let y = snd dst in 
    if pick_move_helper bd x y then
      pick_move mv_lst copy (n-1)
    else (src,dst)