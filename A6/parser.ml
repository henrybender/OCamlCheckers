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
let parse_cmd s = try begin
  let cmd_list = String.split_on_char ' ' s in
  if List.length cmd_list < 1 then InvalidCmd
  else if String.lowercase_ascii (List.nth cmd_list 0) = "pvp" then PvP 
  else if String.lowercase_ascii (List.nth cmd_list 0) = "pvai" then PvAI
  else if String.lowercase_ascii (List.nth cmd_list 0) = "quit" then Quit
  else if String.lowercase_ascii (List.nth cmd_list 0) = "help" then Help 
  else if String.lowercase_ascii (List.nth cmd_list 0) = "move" then if List.length cmd_list < 6 then InvalidCmd

    else
      let alphabet_to_int = function
        |"a"->0
        |"b"->1
        |"c"->2
        |"d"->3
        |"e"->4
        |"f"->5
        |"g"->6
        |"h"->7 
        |_-> -1 in
      let sx = alphabet_to_int (List.nth cmd_list 1) in 
      let sy = int_of_string (List.nth cmd_list 2) in
      let dx = alphabet_to_int (List.nth cmd_list 4) in 
      let dy = int_of_string (List.nth cmd_list 5) in 
      if sx < 0 || sx > 7|| sy < 0 || sy > 7 || dx < 0 || dx > 7 || dy < 0 || dy > 7 
      then InvalidCmd
      else Move ((sx, sy), (dx, dy))
  else InvalidCmd
end
  with int_of_string->InvalidCmd
