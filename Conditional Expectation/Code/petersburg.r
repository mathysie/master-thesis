N <- 10000 # Games per run
M <- 10    # Runs

rewards <- matrix(nrow=M, ncol=N)
fracs1 <- matrix(nrow=M, ncol=N)
fracs2 <- matrix(nrow=M, ncol=N)
for (j in 1:M) {
  for (i in 1:N) {
    coin <- 1
    coins <- vector()
    
    # Play the St. Petersburg game
    while(coin) {
      coin <- sample(c(0, 1), 1)
      coins <- c(coins, coin)
    }
    
    rewards[j,i] <- 2**(sum(coins)+1)
    if (i == 1) {
      fracs1[j,i] = 0
      fracs2[j,i] = 0
    } else {
      fracs1[j,i] =
        sum(rewards[j,1:i])/(i*log2(i))
      fracs2[j,i] =
        sum(rewards[j,1:i])/(i*log2(i*log2(i)))
    }
  }
}

# Plot the results
svg(filename="Figures/petersburgslow.svg")
plot(1:N, fracs1[1,1:N], type='l',
     ylim=c(min(fracs1[,1000:N]), max(fracs1[,1000:N])),
     xlab='Trials (n)', ylab='S_n/(n*log_2(n))')
for (j in 2:M) {
  lines(1:N, fracs1[j,1:N])
}
lines(1:N, rep(1,N), col='red')
dev.off()
svg(filename="Figures/petersburgfast.svg")
plot(1:N, fracs2[1,1:N], type='l',
     ylim=c(min(fracs1[,1000:N]), max(fracs1[,1000:N])),
     xlab='Trials (n)', ylab='S_n/(n*log_2(n*log_2(n)))')
for (j in 2:M) {
  lines(1:N, fracs2[j,1:N])
}
lines(1:N, rep(1,N), col='red')
dev.off()