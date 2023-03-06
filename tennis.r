# Define a function to generate a random serve outcome
serve <- function() {
  sample(c("ace", "fault", "in"), 1, prob = c(0.1, 0.1, 0.8))
}

# Define a function to simulate a tennis match
play_game <- function(player_name) {
  # Set up the game
  player_score <- 0
  opponent_score <- 0
  serving <- player_name
  cat("Welcome to the Tennis Game!\n")
  cat("You are playing against the computer.\n")
  
  # Play the game
  repeat {
    # Display the current score
    cat("Score: ", player_name, " ", player_score, " - ", opponent_score, " Computer\n")
    
    # Serve the ball
    outcome <- serve()
    if (outcome == "ace") {
      cat("Ace! ", serving, " wins the point.\n")
      if (serving == player_name) {
        player_score <- player_score + 1
      } else {
        opponent_score <- opponent_score + 1
      }
    } else if (outcome == "fault") {
      cat("Fault! ", serving, " loses the point.\n")
      if (serving == player_name) {
        opponent_score <- opponent_score + 1
      } else {
        player_score <- player_score + 1
      }
    } else {
      cat("In! ")
      # Prompt the player to choose their shot
      repeat {
        cat("Choose your shot:\n1. Forehand\n2. Backhand\n")
        choice <- as.integer(readline(prompt = "Enter your choice: "))
        if (choice == 1 | choice == 2) {
          break
        } else {
          cat("Invalid choice. Try again.\n")
        }
      }
      # Determine the winner of the point
      if (serving == player_name) {
        if (choice == 1) {
          cat("You hit a forehand winner!\n")
          player_score <- player_score + 1
        } else {
          cat("You hit a backhand winner!\n")
          opponent_score <- opponent_score + 1
        }
      } else {
        if (choice == 1) {
          cat("Computer hits a backhand error.\n")
          player_score <- player_score + 1
        } else {
          cat("Computer hits a forehand error.\n")
          opponent_score <- opponent_score + 1
        }
      }
    }
    
    # Switch serving
    if (serving == player_name) {
      serving <- "Computer"
    } else {
      serving <- player_name
    }
    
    # Check for game end
    if (player_score >= 4 & player_score - opponent_score >= 2) {
      cat("Game over. ", player_name, " wins!\n")
      break
    } else if (opponent_score >= 4 & opponent_score - player_score >= 2) {
      cat("Game over. Computer wins!\n")
      break
    }
  }
}

# Start the game
play_game("Player")
