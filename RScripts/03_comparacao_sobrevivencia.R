## ----dados, echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------

tempo <-  c(1, 2, 3, 3, 3, 5, 5, 16, 16, 16, 16, 16,
            16, 16, 16, 1, 1, 1, 1, 4, 5, 7, 8, 10,
            10, 12, 16, 16, 16)

cens <- c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1,
          0, 0, 0, 0, 0)

grupo <- factor(c(rep("Controle", 15), rep("Esteroide", 14)))

df.hep <- data.frame(tempo, cens, grupo)

# install.packages("survival")

# Carregando pacote
library(survival)

# Dados do exemplo

# head(df.hep)

ekm <- survfit(Surv(time = tempo, event = cens) ~ grupo,
               data = df.hep,
               conf.type = "log-log")



## ----km.grupo, echo=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------

ekm



## ----km.grupo.plot, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------

plot(ekm, conf.int = TRUE, 
     mark.time = TRUE,
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







## ----logrank, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------

survdiff(Surv(time = tempo, event = cens) ~ grupo,
         data = df.hep)



## ----dados.malaria, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------

df.mala <- read.table("../dados/malaria.csv",
                      sep = ";",
                      header = TRUE)
head(df.mala)



## ----km.mala, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'------------------

ekm <- survfit(Surv(time = tempo, event = cens) ~ grupo,
               data = df.mala,
               conf.type = "log-log")

plot(ekm, conf.int = FALSE, 
     mark.time = TRUE,
     col = c("orange", "blue", "red"), 
     lwd = 2, xlab = "Tempo (dias)", 
     ylab = "Sobrevivência estimada")

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 30, by = 5),
       col = "lightgrey", lty = 3)

legend("bottomleft",
       c("Grupo 1", "Grupo 2", "Grupo 3"), 
       col = c("orange", "blue", "red"), 
       lwd = 2, bty = "n")



## ----logrank2, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------

survdiff(Surv(time = tempo, event = cens) ~ grupo,
         data = df.mala)



## ----logrank3, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------

# grupo 1 vs grupo 2
survdiff(Surv(time = tempo, event = cens) ~ grupo,
         data = df.mala,
         subset = grupo != 3)



## ----logrank4, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------

# grupo 1 vs grupo 3
survdiff(Surv(time = tempo, event = cens) ~ grupo,
         data = df.mala,
         subset = grupo != 2)



## ----logrank5, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------

# grupo 2 vs grupo 3
survdiff(Surv(time = tempo, event = cens) ~ grupo,
         data = df.mala,
         subset = grupo != 1)


