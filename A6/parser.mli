open Board
type command = 
  |PvP
  |PvAI
  |Quit
  |Help
  |Move of position*position
  |InvalidCmd

(** [parse_cmd] takes in the commands and turns them into the types that the game
    will recognize *)
val parse_cmd: string->command