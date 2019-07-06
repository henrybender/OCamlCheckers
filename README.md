# OCamlCheckers
Shell Checkers game written in OCaml. PvP and PvAI modes available.

To run use 'Make Build' in the directory. Also attached is an exec file to directly run in the terminal.

Input 'PvP' to play against another person on your machine or 'PvAI' to play against my AI!

Standard rules of checkers apply, jumps and double jumps are mandatory. King pieces are awarded when a standard piece reaches the end of the board.

The AI component involves a depth 2 game tree. All moves 2 steps in the future are checked for the least amount of pieces exposed for capture for the AI and all possible captures of the opponent's pieces. The best move, considering possible captures and least exposure to captures, is then chosen. 
