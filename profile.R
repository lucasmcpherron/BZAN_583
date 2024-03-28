Rprof()

par_course <- numeric(10000)
total_strokes_round <- numeric(10000)
total_score_round <- numeric(10000)

# Simulate the golf round 10,000 times
for (sim in 1:10000) {
  total_strokes <- 0
  total_score <- 0
  
  # Initialize a dataframe to store par and strokes for each hole
  scorecard <- data.frame(Hole = 1:18, Par = integer(18), Strokes = 
integer(18))
  
  # Simulate each hole
  for (hole in 1:18) {
    # Choose par for the hole
    par <- sample(c(3, 4, 5), 1, prob = c(0.4, 0.4, 0.2))
    
    # Simulate strokes for the hole
    strokes_to_par <- sample(c(-2, -1, 0, 1, 2, 3), 1, prob = c(0.03, 
0.17, 0.55, 0.18, 0.05, 0.02)) + par - 4
    
    # Store par and strokes for the hole in the scorecard dataframe
    scorecard$Par[hole] <- par
    scorecard$Strokes[hole] <- strokes_to_par
    
    # Update total strokes and total score relative to par
    total_strokes <- total_strokes + strokes_to_par + par
    total_score <- total_score + (strokes_to_par + par)
  }
  
  # Store results for the current iteration
  par_course[sim] <- sum(scorecard$Par)
  total_strokes_round[sim] <- total_strokes
  total_score_round[sim] <- total_strokes - sum(scorecard$Par)
}

# Calculate averages
avg_par_course <- mean(par_course)
avg_total_strokes <- mean(total_strokes_round)
avg_total_score <- mean(total_score_round)

# Print the results
cat("Average par for the course:", avg_par_course, "\n")
cat("Average total strokes for the round:", avg_total_strokes, "\n")
cat("Average total score relative to par for the round:", avg_total_score, 
"\n")

Rprof(NULL)
summaryRprof()
