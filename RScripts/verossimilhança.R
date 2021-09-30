curve(dnorm(x, mean = 10),
      from = 5, to = 20,
      ylim = c(0, 0.4),
      lwd = 2,
      col = "steelblue",
      xlab = "x",
      ylab = "Densidade")
curve(dnorm(x, mean = 15),
      from = 5, to = 20,
      ylim = c(0, 0.4),
      lwd = 2,
      col = "lightsalmon",
      add = TRUE)

set.seed(10)
x <- rnorm(n = 10, mean = 10)

points(x = x, y = rep(0, 10), pch = 16)

legend("topright",
       legend = c("N(10, 1)", "N(15, 1)"),
       col = c("steelblue", "lightsalmon"),
       lty = 1,
       lwd = 2,
       bty = "n")



set.seed(10)
x <- rnorm(n = 10, mean = 10)
theta <- seq(from = 8, to = 11, by = 0.01)

L.fun <- function(theta, x){
  L <- prod(dnorm(x = x, mean = theta))
  L
}

L.theta <- rep(0, length(theta))
for (i in 1:length(theta)){
  L.theta[i] <- L.fun(theta = theta[i], x = x)
}

plot(theta,
     L.theta,
     type = "l",
     lwd = 2,
     xlab = expression(theta),
     ylab = expression(L*(theta)), axes = F)

segments(x0 = theta[which.max(L.theta)],
         y0 = 0,
         x1 = theta[which.max(L.theta)],
         y1 = max(L.theta),
         col = "red", lty = 2)

points(x = theta[which.max(L.theta)], y = max(L.theta), pch = 8, col = "red")

axis(1, at = c(8, 9, 10, 11), labels = c(8, 9, 10, 11))
axis(1, at = theta[which.max(L.theta)], labels =expression(hat(theta)), col.axis = "red")
axis(2)

legend("topright", legend = "n = 10", bty = "n")



lambda <- function(alpha, theta, t){
  (alpha/theta)*(t/theta)^(alpha - 1)
}

alpha <- 1
theta <- 10
t <- seq(0, 15, by = 0.01)
beta <- 1.5

plot(t,
     lambda(alpha = alpha, theta = theta, t = t),
     ylim = c(0, 0.65),
     type = "l",
     lwd = 2,
     main = "Taxa de falha cosntante",
     xlab = "t",
     ylab = expression(lambda*(t)))
lines(t,
     lambda(alpha = alpha, theta = theta, t = t)*exp(beta),
     lty = 2,
     lwd = 2)
legend("topright",
       legend = c(expression(lambda[0]*(t)),
                  expression(lambda[0]*(t)*exp*(beta))),
       lty = c(1, 2),
       lwd = 2,
       bty = "n"
       )     


alpha <- 1.2
theta <- 10
t <- seq(0, 15, by = 0.01)
beta <- 1.5

par(mfrow = c(1,2))

plot(t,
     lambda(alpha = alpha, theta = theta, t = t),
     ylim = c(0, 0.65),
     type = "l",
     lwd = 2,
     main = "Taxa de falha crescente",
     xlab = "t",
     ylab = expression(lambda*(t)))
lines(t,
      lambda(alpha = alpha, theta = theta, t = t)*exp(beta),
      lty = 2,
      lwd = 2)
legend("topleft",
       legend = c(expression(lambda[0]*(t)),
                  expression(lambda[0]*(t)*exp*(beta))),
       lty = c(1, 2),
       lwd = 2,
       bty = "n"
)     

plot(t,
     log(lambda(alpha = alpha, theta = theta, t = t)),
     ylim = c(-3.5, 0.65),
     type = "l",
     lwd = 2,
     main = "Taxa de falha crescente (escala log)",
     xlab = "t",
     ylab = expression(log*(lambda*(t))))
lines(t,
      log(lambda(alpha = alpha, theta = theta, t = t)*exp(beta)),
      lty = 2,
      lwd = 2)
legend("topleft",
       legend = c(expression(lambda[0]*(t)),
                  expression(lambda[0]*(t)*exp*(beta))),
       lty = c(1, 2),
       lwd = 2,
       bty = "n"
)     


alpha <- 0.8
theta <- 10
t <- seq(0, 15, by = 0.01)
beta <- 1.5

par(mfrow = c(1,2))

plot(t,
     lambda(alpha = alpha, theta = theta, t = t),
     ylim = c(0, 0.65),
     type = "l",
     lwd = 2,
     main = "Taxa de falha decrescente",
     xlab = "t",
     ylab = expression(lambda*(t)))
lines(t,
      lambda(alpha = alpha, theta = theta, t = t)*exp(beta),
      lty = 2,
      lwd = 2)
legend("topright",
       legend = c(expression(lambda[0]*(t)),
                  expression(lambda[0]*(t)*exp*(beta))),
       lty = c(1, 2),
       lwd = 2,
       bty = "n"
)     

plot(t,
     log(lambda(alpha = alpha, theta = theta, t = t)),
     ylim = c(-3.5, 0.65),
     type = "l",
     lwd = 2,
     main = "Taxa de falha decrescente (escala log)",
     xlab = "t",
     ylab = expression(log*(lambda*(t))))
lines(t,
      log(lambda(alpha = alpha, theta = theta, t = t)*exp(beta)),
      lty = 2,
      lwd = 2)
legend("topright",
       legend = c(expression(lambda[0]*(t)),
                  expression(lambda[0]*(t)*exp*(beta))),
       lty = c(1, 2),
       lwd = 2,
       bty = "n"
)     


alpha <- 0.8
theta <- 10
t <- seq(0, 15, by = 0.01)
beta <- 1.5

par(mfrow = c(1,2))

plot(t,
     lambda(alpha = alpha, theta = theta, t = t),
     ylim = c(0, 0.65),
     type = "l",
     lwd = 2,
     main = "Taxa de falha decrescente",
     xlab = "t",
     ylab = expression(lambda*(t)))
lines(t,
      lambda(alpha = alpha, theta = theta, t = t)*exp(beta/t^2),
      lty = 2,
      lwd = 2)
legend("topright",
       legend = c(expression(lambda[0]*(t)),
                  expression(lambda[0]*(t)*exp*(beta))),
       lty = c(1, 2),
       lwd = 2,
       bty = "n"
)     

plot(t,
     log(lambda(alpha = alpha, theta = theta, t = t)),
     ylim = c(-3.5, 0.65),
     type = "l",
     lwd = 2,
     main = "Taxa de falha decrescente (escala log)",
     xlab = "t",
     ylab = expression(log*(lambda*(t))))
lines(t,
      log(lambda(alpha = alpha, theta = theta, t = t)*exp(beta/t^2)),
      lty = 2,
      lwd = 2)
legend("topright",
       legend = c(expression(lambda[0]*(t)),
                  expression(lambda[0]*(t)*exp*(beta))),
       lty = c(1, 2),
       lwd = 2,
       bty = "n"
)     
