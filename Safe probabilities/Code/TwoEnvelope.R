rm(list=ls())

TwoEnvelopeB <- function () {
  sum(Y[,2])/sum(X)
}

TwoEnvelope1 <- function() {
  choices = sample(2, N, TRUE)
  
  win = 0
  for (i in c(1:N)) {
    win = win + Y[i, choices[i]]
  }
  
  win/sum(X)
}

TwoEnvelopeQ <- function(q) {
  choices = sample(2, N, TRUE, c(1-q,q))
  
  win = 0
  for (i in c(1:N)) {
    win = win + Y[i, choices[i]]
  }
  
  win/sum(X)
}

N = 10000
M = 100

X = rexp(N)
flip = rbinom(N, 1, 1/2)
Y = matrix(nrow = N, ncol = 2)
Y[,1] = (1+flip)*X
Y[,2] = (2-flip)*X

winB = TwoEnvelopeB()
win1 = TwoEnvelope1()
winq = vector(length=M)

for (i in c(1:M)) {
  winq[i] = TwoEnvelopeQ((i-1)/(M-1))
}

plot(seq(0, 1, length.out=100), winq, xlab='q', ylab='E[W]/E[X]')
points(1,winB, col='red', pch=2)
points(0.5,win1, col='blue', pch=3)