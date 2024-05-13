library(nonogram)
library(purrr)
library(ggplot2)

#21x21
puzzle_string <- "7,3,7:1,1,3,1,1,1:1,3,1,3,1,3,1:1,3,1,2,1,1,3,1:1,3,1,1,1,1,3,1:1,1,1,1,1,1:7,1,1,1,7:1:5,3,3,1,1,1:2,1,4,1,2,1:1,1,4,1,1,3:4,1,3,1,2:1,1,2,5,1,2,1:1,1,3,3:7,1,1,1,1,1:1,1,1,7:1,3,1,2,1,4,1:1,3,1,1,1,1,3:1,3,1,1,1,1,1:1,1,1,1,1,2:7,2,1,2,3-7,1,2,7:1,1,2,2,1,1:1,3,1,2,1,1,3,1:1,3,1,1,3,1,3,1:1,3,1,1,1,3,1:1,1,2,1,1,1:7,1,1,1,7:3:1,1,2,7,5:1,2,1,1,1,2,1:3,1,1,4,1:1,1,1,1,2,1,2:5,1,5,1:1,1,1:7,5,3:1,1,1,2,1,2:1,3,1,2,3:1,3,1,5,4,2:1,3,1,3,1,1,4:1,1,1,1,5,1:7,1,1,1"
puzzle <- convert_puzzle_string_to_puzzle(puzzle_string)

create_puzzle_plot(puzzle, show_clues=TRUE)
temp_solution_matrix <- solve_puzzle(puzzle, hint = NULL, verbose = FALSE)

hint_string <- generate_hint(temp_solution_matrix)
#hint_string <- "7,3,4,2:1,1,3,1,1,1:3,1,3,3:3,2,1,1,3,1:1,2,1,1,1,1,3,1:1,1,1,1,1,1:7,1,1,1,7:1:4,3,2,1,1,1:2,1,4,1,2,1:1,1,4,1,1,2:2,1,3,2:1,2,5,1,1,1:1,1,3,3:7,1,1,1,1,1:1,1,1,7:1,3,1,2,1,3,1:1,3,1,1,1,1,3:1,3,1,1,1,1,1:1,1,1,1,1,1:2,3,2,1,2,3-2,3,1,1,6:1,1,2,1,1,1:1,2,1,2,1,1,3,1:1,3,1,1,2,1,3:1,3,1,1,3,1:1,1,2,1,1,1:3,3,1,1,1,7:3:1,1,2,7,5:1,2,1,1,1,2,1:3,1,1,4,1:1,1,1,1,2,1,2:5,1,5,1:1,1:2,4,5,3:1,1,1,1,1,2:1,3,1,2,3:1,3,1,2,1,2,1,1:3,1,3,1,1,4:1,1,1,1,5,1:2,4,1,1,1"
print("Creating new random hint...")
print(hint_string$hint)
#hint <- convert_puzzle_string_to_puzzle(hint_string$hint)

#solution_matrix <- solve_puzzle(hint, hint=NULL, verbose=TRUE)

create_puzzle_plot(puzzle, hint_string$sm)

solution_matrix <- solve_puzzle(puzzle, hint = hint_string$sm, verbose = TRUE)

create_puzzle_plot(puzzle, solution_matrix)

