n <- 150
r0 <- 5
epsmin <- -1
epsmax <- -epsmin

theta <- sort(runif(n, 0, 2 * pi))
eps <- runif(n, epsmin, epsmax)

r <- r <- r0 + theta * (theta - 2 * pi) * 
  (0.23 * sin(03 * theta) + 
     0.21 * sin(04 * theta) + 
     0.07 * sin(07 * theta) + 
     0.05 * sin(11 * theta)) + eps

x <- r * cos(theta)
y <- r * sin(theta)

plot(NA, xlim = range(c(x, y)), ylim = range(c(x, y)),
     xlab = 'x', ylab = 'y')
abline(h = 0, v = 0, col = 'gray50')
points(y ~ x, pch = 21, bg = 'white', cex = 0.75)
m2 <- lm(y ~ x)

plot(NA, xlim = range(c(x, y)), ylim = range(c(x, y)),
     xlab = 'x', ylab = 'y')
abline(h = 0, v = 0, col = 'gray50')
points(y ~ x, pch = 21, bg = 'white', cex = 0.75)
points(m2$fitted.values ~ x, pch = 15, cex = 0.75, col = 'red')
abline(m2, col = 'red')


plot(m2$residuals, pch = 21, bg = 'white')



plot(y ~ x)



r2 <- sqrt(x^2 + y^2)
theta2 <- ifelse(x > 0 & y > 0, atan(y/x),
                 ifelse(x < 0 & y > 0, atan(y/x) + pi,
                        ifelse(x < 0 & y < 0, atan(y/x) + pi, atan(y/x) + 2 * pi)))

plot(r ~ theta, type = 'l')
lines(r2 ~ theta2, col = 'red')




