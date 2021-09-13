## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', out.height='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'censoring-example2.png'))



## ----censura_mecanismo, fig.align='center', out.width='100%', echo=FALSE, message=FALSE, warning=FALSE----

set.seed(2357)
par(mfrow = c(2, 2))

# ---------------
# Dados completos
# ---------------
i <- 1:5
t <- rexp(n = 5, rate = 1/10)
d <- rbinom(n = 5, size = 1, prob = 1)
plot(i ~ t, pch = 16, xlab = "Tempo", ylab = "Paciente", main = "Dados completos", ylim = c(0,6))
segments(x0 = 0, y0 = i, x1 = t, y1 = i, lty = 2)

# ---------------
# Dados com censura tipo I
# ---------------
t <- rexp(n = 5, rate = 1/25)
d <- ifelse(t <= 20, 1, 0)
t[t >= 20] <- 20
plot(i ~ t, xlab = "Tempo", ylab = "Paciente", main = "Dados com censura tipo I", ylim = c(0,6))
points(i[d == 1] ~ t[d == 1], pch = 16)
segments(x0 = 0, y0 = i, x1 = t, y1 = i, lty = 2)
segments(x0 = 20, y0 = 1, x1 = 20, y1 = 5, lty = 4, col = "red")

# ---------------
# Dados com censura tipo II
# ---------------
t <- rexp(n = 5, rate = 1/25)
t.median <- median(t)
d <- ifelse(t <= t.median, 1, 0)
t[t >= t.median] <- t.median
plot(i ~ t, xlab = "Tempo", ylab = "Paciente", main = "Dados com censura tipo II", ylim = c(0,6))
points(i[d == 1] ~ t[d == 1], pch = 16)
segments(x0 = 0, y0 = i, x1 = t, y1 = i, lty = 2)
segments(x0 = t.median, y0 = 1, x1 = t.median, y1 = 5, lty = 4, col = "red")

# ---------------
# Dados com censura aleatória
# ---------------
t <- rexp(n = 5, rate = 1/25)
d <- ifelse(t <= 20, 1, 0)
t.min <- min(t)
d[which(t == t.min)] <- 0
t[t >= 20] <- 20
plot(i ~ t, xlab = "Tempo", ylab = "Indivíduo", main = "", ylim = c(0,6))
points(i[d == 1] ~ t[d == 1], pch = 16)
segments(x0 = 0, y0 = i, x1 = t, y1 = i, lty = 2)
segments(x0 = 20, y0 = 1, x1 = 20, y1 = 5, lty = 4, col = "red")

par(mfrow = c(1, 1))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'funcao_sobrevida.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'funcao_taxa.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'curva_banheira.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'relacao_funcoes.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-bolha.jpg'))


