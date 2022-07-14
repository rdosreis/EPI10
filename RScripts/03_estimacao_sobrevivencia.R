## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'histograma_sem_censura.png'))









## ----dados, echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------

tempo <-  c(1, 2, 3, 3, 3, 5, 5, 16, 16, 16, 16, 16,
            16, 16, 16, 1, 1, 1, 1, 4, 5, 7, 8, 10,
            10, 12, 16, 16, 16)

cens <- c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1,
          0, 0, 0, 0, 0)

grupo <- factor(c(rep("Controle", 15), rep("Esteroide", 14)))

df.hep <- data.frame(tempo, cens, grupo)



## ----dados2, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------

# install.packages("survival")

# Carregando pacote
library(survival)

# Dados do exemplo

head(df.hep)



## ----km.plot, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'---------------

ekm <- survfit(Surv(time = tempo, event = cens) ~ 1,
               data = df.hep,
               subset = grupo == "Esteroide",
               conf.type = "log-log")

plot(ekm, conf.int = FALSE, 
     lwd = 2, xlab = "Tempo (semanas)", 
     ylab = "Sobrevivência estimada")



## ----km, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------

summary(ekm)



## ----km.plot.ic, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

plot(ekm, conf.int = TRUE, 
     lwd = 2, xlab = "Tempo (semanas)", 
     ylab = "Sobrevivência estimada")
abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 16, by = 4),
       col = "lightgrey", lty = 3)



## ----km.grupo, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------

ekm <- survfit(Surv(time = tempo, event = cens) ~ grupo,
               data = df.hep,
               conf.type = "log-log")

summary(ekm)



## ----km.grupo.plot, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'---------

plot(ekm, conf.int = TRUE, 
     col = c("black", "red"), 
     lwd = 2, xlab = "Tempo (semanas)", 
     ylab = "Sobrevivência estimada")

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 16, by = 4),
       col = "lightgrey", lty = 3)

legend("bottomleft",
       c("Controle", "Esteroide"), 
       col = c("black", "red"), 
       lwd = 2, bty = "n")


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-ff.jpg'))


