rm(list=ls())

SleepingBeauty <- function(p) {
  a <- 0
  
  heads <- sample(c(0,1), c1, TRUE, c(1-p,p))
  a <- a + sum(heads == 1)
  
  heads <- sample(c(0,1), 2*c2, TRUE, c(1-p,p))
  a <- a + sum(heads == 0)

  return(a)
}

N <- 10000 # Aantal spellen
M <- 100   # Aantal te testen kansen

res <- vector(length=M)
coins <- sort(sample(c(0,1), N, TRUE))
c1 <- sum(coins == 1)
c2 <- sum(coins == 0)

for (i in 1:(M+1)) {
  res[i] <- SleepingBeauty((i-1)/M)
}

res <- res / (c1 + 2*c2)

plot(seq(0,1,1/M), res)
abline(2/3,-1/3, col='red')
