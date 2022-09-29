## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'ex_leucemia.png'))



## ----leucemia, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------

df.leucemia <- read.table(file = "../dados/leucemia.txt",
                         header = TRUE)


head(df.leucemia)



## ----leucemia2, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------

df.leucemia$idadec <- ifelse(df.leucemia$idade > 96,1,0)
df.leucemia$leuinic <- ifelse(df.leucemia$leuini > 75,1,0)
df.leucemia$zpesoc <- ifelse(df.leucemia$zpeso > -2,1,0)
df.leucemia$zestc <- ifelse(df.leucemia$zest > -2,1,0)
df.leucemia$pasc <- ifelse(df.leucemia$pas > 0.05,1,0)
df.leucemia$vacc <- ifelse(df.leucemia$vac > 15,1,0)
df.leucemia$pasc <- ifelse(df.leucemia$pas > 5,1,0)
df.leucemia$riskc <- ifelse(df.leucemia$risk > 1.7,1,0)



## ----leucemia.cox, echo=TRUE, message=FALSE, warning=FALSE------------------------------------

library(survival)

mod1 <- coxph(Surv(time = tempos, event = cens) ~ idadec +
                zpesoc + pasc + vacc + strata(leuinic),
               data = df.leucemia, method = "breslow")

summary(mod1)



## ----leucemia.cox2, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------

# TRV
mod20 <- coxph(Surv(time = tempos, event = cens) ~ idadec +
                zpesoc + pasc + vacc,
               data = df.leucemia, subset = leuinic == 0,
               method = "breslow")

mod21 <- coxph(Surv(time = tempos, event = cens) ~ idadec +
                zpesoc + pasc + vacc,
               data = df.leucemia, subset = leuinic == 1,
               method = "breslow")

TRV <- as.numeric(-2 * (logLik(mod1) - (logLik(mod20) + logLik(mod21))))
TRV
1 - pchisq(TRV, 4)


cox.zph(mod1, transform = "identity")
# plot(cox.zph(mod1))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='60%', out.height='60%'----

knitr::include_graphics(here::here('images', 'var_td_01.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', out.height='60%'----

knitr::include_graphics(here::here('images', 'var_td_02.png'))



## ----tmo, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------

df.tmo <- read.table(file = here::here("dados","tmopc.csv"),
                         header = TRUE, sep = ";")


head(df.tmo, 14)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', out.height='60%'----

knitr::include_graphics(here::here('images', 'var_td_03.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='70%', out.height='70%'----

knitr::include_graphics(here::here('images', 'var_td_04.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'cov-td-01.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'cov-td-02.png'))



## ----tmo2, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------

df.tmo$sexo <- factor(df.tmo$sexo,
                      labels = c("masculino", "feminino"))
df.tmo$deag <- factor(df.tmo$deag,
                      labels = c("não", "sim"))
df.tmo$decr <- factor(df.tmo$decr,
                      labels = c("não", "sim"))
df.tmo$recplaq <- factor(df.tmo$recplaq,
                         labels = c("nao rec", "rec"))
df.tmo$recplaq <- relevel(df.tmo$recplaq, ref = "rec")



## ----tmo.cox, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------

library(survival)

mod1 <- coxph(
  Surv(time = inicio, time2 = fim , event = status) ~ idade +
    sexo + fasegr + deag + decr + recplaq,
  data = df.tmo, method = "breslow")

summary(mod1)


