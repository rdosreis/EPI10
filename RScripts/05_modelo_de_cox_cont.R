## ----laringe, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------

df.laringe <- read.table(file = "../dados/laringe.txt",
                         header = TRUE)


head(df.laringe)



## ----laringe.conhece.dados, echo=TRUE, message=FALSE, warning=FALSE-----------------------

str(df.laringe)

summary(df.laringe)



## ----laringe.transforma.dados, echo=TRUE, message=FALSE, warning=FALSE--------------------

df.laringe$estagio <- factor(x = df.laringe$estagio,
                             levels = 1:4,
                             labels = c("I", "II", "III", "IV"))

str(df.laringe)

summary(df.laringe)



## ----laringe.km, echo=TRUE, message=FALSE, warning=FALSE----------------------------------
library(survival)

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
       v = seq(0, 10, by = 2),
       col = "lightgrey", lty = 3)

legend("topright",
       c("I", "II", "III", "IV"), 
       col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"), 
       lwd = 2, bty = "n")



## ----laringe.lr, echo=TRUE, message=FALSE, warning=FALSE----------------------------------

survdiff(Surv(time = tempos, event = cens) ~ estagio,
               data = df.laringe)



## ----laringe.cox, echo=TRUE, message=FALSE, warning=FALSE---------------------------------

mod1 <- coxph(Surv(time = tempos, event = cens) ~ estagio,
               data = df.laringe, method = "breslow")

summary(mod1)



## ----laringe.cox2, echo=TRUE, message=FALSE, warning=FALSE--------------------------------

mod2 <- coxph(Surv(time = tempos, event = cens) ~ idade,
               data = df.laringe, method = "breslow")

summary(mod2)



## ----laringe.cox3, echo=TRUE, message=FALSE, warning=FALSE--------------------------------

mod3 <- coxph(Surv(time = tempos, event = cens) ~ estagio + idade,
               data = df.laringe, method = "breslow")

summary(mod3)



## ----laringe.Lambda0, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

plot(survfit(mod1),
     cumhaz = TRUE,
     conf.int = FALSE,
     lwd = 2, xlab = "t (meses)", 
     ylab = expression(hat(Lambda)[0](t)))

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 10, by = 2),
       col = "lightgrey", lty = 3)



## ----laringe.S0, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

plot(survfit(mod1),
     conf.int = FALSE,
     lwd = 2, xlab = "t (meses)", 
     ylab = expression(hat(S)[0](t)))

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 10, by = 2),
       col = "lightgrey", lty = 3)



## ----laringe.surv.cox, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

df.novo <- data.frame(
  estagio = levels(df.laringe$estagio))

plot(survfit(mod1, newdata = df.novo),
     col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"), 
     lwd = 2, xlab = "Tempo (meses)", 
     ylab = "Sobrevivência estimada")

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 10, by = 2),
       col = "lightgrey", lty = 3)

legend("topright",
       c("I", "II", "III", "IV"),
       title = "Estágio",
       col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"), 
       lwd = 2, bty = "n")



## ----laringe.surv.cox2, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

df.novo <- data.frame(
  idade = c(57, 65, 72))

plot(survfit(mod2, newdata = df.novo),
     col = c("#D95F02",
             "#7570B3", "#E7298A"), 
     lwd = 2, xlab = "Tempo (meses)", 
     ylab = "Sobrevivência estimada")

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 10, by = 2),
       col = "lightgrey", lty = 3)

legend("topright",
       legend = c(57, 65, 72), 
       col = c("#D95F02",
             "#7570B3", "#E7298A"),
       title = "Idade (anos)",
       lwd = 2, bty = "n")



## ----laringe.surv.cox3, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

df.novo <- data.frame(
  idade = 65,
  estagio = levels(df.laringe$estagio))

plot(survfit(mod3, newdata = df.novo),
     col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"), 
     lwd = 2, xlab = "Tempo (meses)", 
     ylab = "Sobrevivência estimada (idade = 65 anos)")

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 10, by = 2),
       col = "lightgrey", lty = 3)

legend("topright",
       c("I", "II", "III", "IV"),
       title = "Estágio",
       col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"), 
       lwd = 2, bty = "n")




## ----laringe.mgd, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

plot(ekm,
     fun = "cloglog",
     conf.int = FALSE,
     col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"),
     lwd = 2, xlab = expression(log*(t)),
     ylab = expression(log*(hat(Lambda)[0]*(t))))

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 10, by = 2),
       col = "lightgrey", lty = 3)

legend("topleft",
       c("I", "II", "III", "IV"),
       title = "Estágio",
       col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"), 
       lwd = 2, bty = "n")




## ----laringe.resid0, echo=FALSE, message=FALSE, warning=FALSE-----------------------------

par(mfrow = c(1,2))



## ----laringe.resid1, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

plot(cox.zph(mod3),
     col = "red",
     lwd = 2)



## ----laringe.resid2, echo=FALSE, message=FALSE, warning=FALSE-----------------------------

par(mfrow = c(1,1))



## ----laringe.resid3, echo=TRUE, message=FALSE, warning=FALSE------------------------------

cox.zph(mod3)


