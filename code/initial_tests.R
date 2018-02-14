

f <- function(theta, rs) {
  nrs <- length(rs)
  
  for (i in 1:nrs) {
    if (theta >= ((i - 1) * (2 * pi) / nrs) & theta < (i * 2 * pi / nrs)) {
      return (rs[i])
    } else if (theta == 2 * pi) {
      return (rs[nrs])
    }
  }
}
fv <- function(theta, rs) {
  sapply(theta, FUN = function(x) f(x, rs))
}

n <- 500
rs <- runif(5, 1, 1.25)

theta <- sort(runif(n, 0, 2 * pi))
r <- fv(theta, rs)

x <- r * cos(theta)
y <- r * sin(theta)

plot(r ~ theta, pch = 21, bg = 'white', cex = 0.75, type = 'l')
plot(y ~ x, pch = 21, bg = 'white', cex = 0.75, type = 'l')

plot(f(theta, 1, 2) ~ theta, type = 'l')

n <- 100
theta <- sort(runif(n, 0, 2 * pi))
r <- 4 + (0.21 * sin(5 * theta) +  0.11 * sin(3 * theta)) * theta * (theta - 2 * pi) * cos(3 * (sqrt(theta) - 5 * pi / 4))  + runif(n, -0.5, 0.5)

x <- r * cos(theta)
y <- r * sin(theta)

plot(r ~ theta, type = 'o', pch = 21, bg = 'white', cex = 0.5)
plot(y ~ x, type = 'p')


