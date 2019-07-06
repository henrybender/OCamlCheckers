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

(** [make_game names] intializes the board *)
val make_game: t

(** [valid_move bd src dst] returns validity of the move from 
    [src] to [dst] is a valid move *)
val valid_move: t->position->position->validity

(** [remove bd pos] returns the board after removing piece in position [pos] *)
val remove: ((piece option) array) array->position->((piece option) array) array

(** [add bd p pos] returns the board after adding piece [p] in position [pos] *)
val add: ((piece option) array) array->piece->position->((piece option) array) array

(** [move bd src dst] returns the board after the move from [src] to [dst] *)
val move: t->position->position->t

(** [check_win b] returns if the opposite player is the winner*)
val check_win: t->bool

(** [color_at bd (x,y)] returns color option at board position [(x,y)] *)
val color_at: t->int*int->color option

(** [string_of_color c] returns string representation of color [c]*)
val string_of_color: color->string

(**[print_board bd] prints the current board *)
val print_board: t -> unit

(**[possible_move bd] is the list of all possible moves the current player can make. *)
val possible_moves: t-> (position*position*bool) list

(** [get_captures movelist] returns the list of each move that is a capture 
    in [movelist]*)
val get_captures: (position*position*bool) list->(position*position*bool) list

(** [enforced_captures bd src dst] returns the checks that the move is valid
    and that it captures an opponent if it is possible *)
val enforced_captures: t->position->position->validity

(**[multiple_jump src bd] returns the board after multiple mandatory jumps
   (if any) have been completed by the piece located at src*)
val multiple_jump: position->t->t

(**[pick_move mv_lst bd n] will return the optimal move for the AI given current 
   moves and the next possible move by the human player. pick_move checks for n 
   random moves to see if any of these result in not further exposing pieces to be
   captured by the human player, after n calls, it will just return the first move 
   possible in the mv_lst. *)
val pick_move: (position*position*bool) list -> t -> int -> (position*position)

