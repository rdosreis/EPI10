## ----laringe, echo=FALSE, results='hide', message=FALSE, warning=FALSE------------------------

df.laringe <- read.table(file = "../dados/laringe.txt",
                         header = TRUE)


head(df.laringe)



## ----laringe.conhece.dados, echo=FALSE, results='hide', message=FALSE, warning=FALSE----------

str(df.laringe)

summary(df.laringe)



## ----laringe.transforma.dados, echo=FALSE, results='hide', message=FALSE, warning=FALSE-------

df.laringe$estagio <- factor(x = df.laringe$estagio,
                             levels = 1:4,
                             labels = c("I", "II", "III", "IV"))

str(df.laringe)

summary(df.laringe)



## ----laringe.km, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------
library(survival)

ekm <- survfit(Surv(time = tempos, event = cens) ~ estagio,
               data = df.laringe,
               conf.type = "log-log")

ekm



## ----laringe.mgd, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

plot(ekm,
     fun = "cloglog",
     conf.int = FALSE,
     col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"),
     lwd = 2, xlab = expression(log*(t)),
     ylab = expression(log*(hat(Lambda)[0]*(t))))

legend("topleft",
       c("I", "II", "III", "IV"),
       title = "Estágio",
       col = c("#1B9E77", "#D95F02",
             "#7570B3", "#E7298A"), 
       lwd = 2, bty = "n")




## ----laringe.resid, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

mod1 <- coxph(Surv(time = tempos, event = cens) ~ estagio,
               data = df.laringe, method = "breslow")

mod2 <- coxph(Surv(time = tempos, event = cens) ~ idade,
               data = df.laringe, method = "breslow")

mod3 <- coxph(Surv(time = tempos, event = cens) ~ estagio + idade,
               data = df.laringe, method = "breslow")

par(mfrow = c(1,2))
plot(cox.zph(mod3),
     col = "red",
     lwd = 2)



## ----laringe.resid3, echo=TRUE, message=FALSE, warning=FALSE----------------------------------

cox.zph(mod3)



## ----laringe.trv, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------

anova(mod1, mod3)



## ----laringe.trv2, echo=TRUE, message=FALSE, warning=FALSE------------------------------------

mod4 <- coxph(Surv(time = tempos, event = cens) ~ estagio*idade,
              data = df.laringe, method = "breslow")

summary(mod4)

anova(mod3, mod4)



## ----laringe.publish, echo=TRUE, message=FALSE, warning=FALSE---------------------------------

# install.packages(Publish)
library(Publish)

# publish(mod4)

plot(publish(mod4))



## ----laringe.aic, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------

AIC(mod1)
AIC(mod2)

BIC(mod1)
BIC(mod2)



## ----laringe.sp, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------

mod5 <- coxph(Surv(time = tempos, event = cens) ~ pspline(idade),
              data = df.laringe, method = "breslow")

summary(mod5)



## ----laringe.sp2, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='90%'----

termplot(model = mod5, terms = "pspline(idade)",
         se = TRUE,
         xlabs = "idade",
         ylabs = "log-HR",
         data = df.laringe)



## ----laringe.sp3, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='90%'----

library(splines)

mod6 <- coxph(Surv(time = tempos, event = cens) ~ ns(idade, df = 3),
              data = df.laringe, method = "breslow")

summary(mod6)



## ----laringe.sp4, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='90%'----

termplot(model = mod6, terms = "ns(idade, df = 3)",
         se = TRUE,
         xlabs = "idade",
         ylabs = "log-HR",
         data = df.laringe)



## ----laringe.sp5, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='90%'----

library(Greg)

plotHR(mod6,
       term = "idade",
       plot.bty = "o",
       xlim = c(41, 86),
       xlab = "Idade")

