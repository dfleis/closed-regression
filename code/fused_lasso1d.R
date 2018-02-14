library(genlasso)
set.seed(124)
n <- 250
sigma <- 0.25
njumps <- 5
jumprate <- 7


x <- sort(runif(n, 0, 2 * pi))
jumps <- rpois(njumps + 1, lambda = jumprate)
jumpidx <- c(1, sort(sample(2:(n - 1), size = njumps, replace = F)), n)

X <- sapply(1:(length(jumpidx) - 1), function(k) {
  nb_z_before <- jumpidx[k] - 1
  nb_z_after <- n - jumpidx[k + 1]
  
  xnew <- x[(jumpidx[k] + 1):(jumpidx[k + 1])]
  
  c(rep(0, nb_z_before), xnew, rep(0, nb_z_after))
})
X <- rbind(c(x[1], rep(0, njumps)), X)

y <- rowSums(sapply(1:ncol(X), function(k) 
  (jumps[k] + rnorm(n, 0, sigma)) * (X[,k] > 0)))


m <- fusedlasso1d(y, x)
yhat <- predict(m, lambda = 0)
plot(NA, xlim = range(x), ylim = range(y), xlab = 'x', ylab = 'y')
points(y ~ x, pch = 21, cex = 0.75, bg = 'white')
lines(yhat$fit ~ x, type = 'l', col = 'red', lwd = 1.5)






