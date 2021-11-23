## ----echo=TRUE, purl=TRUE-----------------------------------------------------------------------------

library(survival)

head(mgus1[c("id","sex","start","stop","status","event")])



## ----echo=TRUE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', purl=TRUE----------

fitKM <- survfit(Surv(time = stop, event == 'pcm') ~ 1,
                 data = mgus1, subset = (start == 0))

fitCI <- survfit(Surv(time = stop, event = status*as.numeric(event),
                      type = "mstate") ~ 1,
                    data = mgus1, subset = (start == 0))

plot(fitCI, xscale = 365.25, xmax = 7300,
     mark.time = FALSE, col = c("red", "grey"),
     xlab = "Anos desde o diagnóstico de MGUS")
lines(fitKM, fun = 'event', xscale = 365.25,
      xmax = 7300, mark.time = FALSE, conf.int = FALSE)
text(x = 10*365.25, y = .4, "Risco competitivo: óbito",
     col = "grey")
text(x = 16*365.25, .15,"Risco competitivo: progressão",
     col = "red")
text(x = 15*365.25, .30, "KM: prog")



## ----echo=TRUE, message=FALSE, warning=FALSE, purl=TRUE-----------------------------------------------

library(cmprsk)

mgus1 <- subset(mgus1, start == 0)
mgus1$evtype <- mgus1$status * as.numeric(mgus1$event)

fitCI <- cuminc(ftime = mgus1$stop,
                fstatus = mgus1$evtype,
                group = mgus1$sex, cencode = 0)



## ----echo=TRUE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', purl=TRUE----------

fitCI$Tests



## ----echo=TRUE, fig.align='center', message=FALSE, warning=FALSE, out.width='70%', purl=TRUE----------

plot(fitCI,
     col = c("dodgerblue3", "blue",
             "lightsalmon1", "red"),
     lwd = 2, lty = 1,
     xlab = "tempo (dias)", ylab = "Probabilidade")



## ----echo=TRUE, message=FALSE, warning=FALSE, purl=TRUE-----------------------------------------------

fitcph1 <- coxph(Surv(time = stop,
                      event = evtype == 2) ~ sex + age,
                 data=mgus1)
summary(fitcph1)$coef



## ----echo=TRUE, message=FALSE, warning=FALSE, purl=TRUE-----------------------------------------------

fitcph2 <- coxph(Surv(time = stop, event = evtype == 3) ~ sex + age,
                 data = mgus1)
summary(fitcph2)$coef



## ----echo=TRUE, message=FALSE, warning=FALSE, purl=TRUE-----------------------------------------------

library(riskRegression)

fitcox <- CSC(Hist(stop, evtype) ~ sex + age,
              data = mgus1)


## ----echo=TRUE, message=FALSE, warning=FALSE, purl=TRUE-----------------------------------------------

print(fitcox)



## ----echo=TRUE, message=FALSE, warning=FALSE, purl=TRUE-----------------------------------------------

fitfg <- FGR(prodlim::Hist(stop, evtype) ~ sex + age,
             data = mgus1, cause = 2)


## ----echo=TRUE, message=FALSE, warning=FALSE, purl=TRUE-----------------------------------------------

print(fitfg)


