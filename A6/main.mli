(** [pp_string] will provide the format for the pretty-printing *)
val pp_string : string -> string

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
val pp_list : ('a -> string) -> 'a list -> string

val print : Board.t -> unit

val intro : string

val prompt_char : string

(** When restart the game, refresh will deperate the old game and
    the new game by a line *)
val refresher : unit -> unit

(** [print_move_instructions] will be called at the beginning of the
    game or 'help' is inputed *)
val print_move_instructions : unit -> unit

(** [invalid_move_msg] will output the warning when the move is invalid *)
val invalid_move_msg : unit -> unit

(** [other_player] is the function that helps to switch the players *)
val other_player : Board.t -> Board.color

(** [game_input] takes a game and return a game, it keeps the game running *)
val game_input : Board.t -> 'a

(** [game_input_ai] takes in the game and run the Person vs. AI mode, which will 
    consider the White as AI, Red as the player*)
val game_input_ai : Board.t -> 'a

(** [game_loop] takes in the game and keeps runnig it by inputing a new game status *)
val game_loop : Board.t -> bool -> 'a

val main : unit -> 'a