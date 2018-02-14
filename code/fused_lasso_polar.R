s#===== libraries =====#
library(genlasso)

#===== parameters =====#
n <- 150
r0 <- 5
epsmin <- -1
epsmax <- -epsmin

#===== generate data =====#
theta <- sort(runif(n, 0, 2 * pi))
eps <- runif(n, epsmin, epsmax)

rtrue <- function(theta) { 
  5 + theta * (theta - 2 * pi) * 
    (0.23 * sin(03 * theta) + 
       0.21 * sin(04 * theta) + 
       0.07 * sin(07 * theta) + 
       0.05 * sin(11 * theta))
}
r <- rtrue(theta) + eps


#===== fit models =====#
m <- fusedlasso1d(r, theta)
rhat <- predict(m, lambda = 1)

#===== figures =====#
plot(r ~ theta, pch = 21, cex = 0.75, bg = 'white',
     xlab = expression(theta))
lines(rhat$fit ~ theta, type = 'l', col = 'red', lwd = 1.5)

xhat <- rhat * cos(theta)
yhat <- rhat * sin(theta)





