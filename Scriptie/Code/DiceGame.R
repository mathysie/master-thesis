N = 100 # Amount of dice throws
M = 10  # Step size of probabilities is 1/M

DiceGame <- function(throws, q1, q2) {
  for (i in 1:N) {
    if (throws[i,1] == 3) {
      throws[i,2] <- sample.int(2, 1, prob = c(q1, 1-q1))
    } else {
      throws[i,2] <- sample.int(2, 1, prob = c(q2, 1-q2))
    }
  }
  
  return(mean(throws[,1] != 3))
}

main <- function() {
  throws <- matrix(nrow = N, ncol = 2)
  throws[,1] <- sample.int(6, N, replace = TRUE)
  for (i in 1:N) {
    if (throws[i,1] <= 2) {
      throws[i,2] <- 1
    } else if (throws[i,1] >= 5) {
      throws[i,2] <- 2
    }
  }
  
  results = matrix(nrow = M+1, ncol = M+1)
  for (i in 1:(M+1)) {
    for (j in 1:(M+1)) {
      results[i,j] <- DiceGame(throws, (i-1)/M, (j-1)/M)
    }
  }
  
  return(results)
}

results = main() # Do not save local variables as global