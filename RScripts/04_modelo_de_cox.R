## ----dados, echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------

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



## ----km.grupo, echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------

ekm



## ----km.grupo.plot, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

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



## ----logrank, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------

survdiff(Surv(time = tempo, event = cens) ~ grupo,
         data = df.hep)

















## ----laringe, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------

df.laringe <- read.table(file = "../dados/laringe.txt",
                         header = TRUE)


head(df.laringe)



## ----laringe.conhece.dados, echo=TRUE, message=FALSE, warning=FALSE--------------------------------

str(df.laringe)

summary(df.laringe)



## ----laringe.transforma.dados, echo=TRUE, message=FALSE, warning=FALSE-----------------------------

df.laringe$estagio <- factor(x = df.laringe$estagio,
                             levels = 1:4,
                             labels = c("I", "II", "III", "IV"))

str(df.laringe)

summary(df.laringe)



## ----laringe.km, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------

ekm <- survfit(Surv(time = tempos, event = cens) ~ estagio,
               data = df.laringe,
               conf.type = "log-log")

ekm



## ----laringe.km.plot, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

plot(ekm, conf.int = FALSE, 
     mark.time = TRUE,
     col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"), 
     lwd = 2, xlab = "Tempo (meses)", 
     ylab = "Sobrevivência estimada")

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 16, by = 4),
       col = "lightgrey", lty = 3)

legend("topright",
       c("I", "II", "III", "IV"), 
       col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"), 
       lwd = 2, bty = "n")



## ----laringe.lr, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------

survdiff(Surv(time = tempos, event = cens) ~ estagio,
               data = df.laringe)



## ----laringe.cox, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------

mod1 <- coxph(Surv(time = tempos, event = cens) ~ estagio,
               data = df.laringe, method = "breslow")

summary(mod1)



## ----laringe.cox2, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------

mod2 <- coxph(Surv(time = tempos, event = cens) ~ idade,
               data = df.laringe, method = "breslow")

summary(mod2)



## ----laringe.cox3, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------

mod3 <- coxph(Surv(time = tempos, event = cens) ~ estagio + idade,
               data = df.laringe, method = "breslow")

summary(mod3)


