## ----dados, echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------

tempo <-  c(1, 2, 3, 3, 3, 5, 5, 16, 16, 16, 16, 16,
            16, 16, 16, 1, 1, 1, 1, 4, 5, 7, 8, 10,
            10, 12, 16, 16, 16)

cens <- c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1,
          0, 0, 0, 0, 0)

grupo <- factor(c(rep("Controle", 15), rep("Esteroide", 14)))

df.hep <- data.frame(tempo, cens, grupo)



## ----km.plot, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'-------

# Carregando pacote

library(survival)

ekm <- survfit(Surv(time = tempo, event = cens) ~ 1,
               data = df.hep,
               subset = grupo == "Esteroide",
               conf.type = "log-log")

plot(ekm, conf.int = TRUE, 
     mark.time = TRUE,
     lwd = 2, xlab = "Tempo (semanas)", 
     ylab = "Sobrevivência estimada")

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 16, by = 4),
       col = "lightgrey", lty = 3)

segments(x0 = 10, y0 = 0, x1 = 10, y1 = summary(ekm, times = 10)$surv, col = "red", lwd = 2)
arrows(x0 = 10, y0 = summary(ekm, times = 10)$surv, x1 = 0,
       y1 = summary(ekm, times = 10)$surv, col = "red", lwd = 2)
axis(side = 1, at = 10, labels = 10, tick = T, col = "red", col.axis = "red")
axis(side = 2, at = summary(ekm, times = 10)$surv,
     labels = round(summary(ekm, times = 10)$surv, 2), tick = T, col = "red", col.axis = "red")



## ----km.summary, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------

summary(ekm, times = 4)

summary(ekm, times = c(4, 6, 7, 12))



## ----km.mediana, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

plot(ekm, conf.int = TRUE, 
     mark.time = TRUE,
     lwd = 2, xlab = "Tempo (semanas)", 
     ylab = "Sobrevivência estimada")

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 16, by = 4),
       col = "lightgrey", lty = 3)

segments(x0 = 0, y0 = 0.5, x1 = 10, y1 = 0.5, col = "red", lwd = 2)
arrows(x0 = 10, y0 = 0.5, x1 = 10, y1 = 0, col = "red", lwd = 2)
axis(side = 1, at = 10, labels = 10, tick = T, col = "red", col.axis = "red")
axis(side = 2, at = 0.5, labels = 0.5, tick = T, col = "red", col.axis = "red")



## ----km.mediana.ic, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width='80%'----

plot(ekm, conf.int = TRUE, 
     mark.time = TRUE,
     lwd = 2, xlab = "Tempo (semanas)", 
     ylab = "Sobrevivência estimada")

abline(h = seq(0, 1, by = 0.2),
       v = seq(0, 16, by = 4),
       col = "lightgrey", lty = 3)

segments(x0 = 0, y0 = 0.5, x1 = 10, y1 = 0.5, col = "red", lwd = 2)
arrows(x0 = 10, y0 = 0.5, x1 = 10, y1 = 0, col = "red", lwd = 2)
segments(x0 = 0, y0 = 0.5, x1 = 16, y1 = 0.5, col = "red", lwd = 2, lty = 2)
arrows(x0 = 1, y0 = 0.5, x1 = 1, y1 = 0, col = "red", lwd = 2, lty = 2)
axis(side = 1, at = c(1, 10), labels = c(1, 10), tick = T, col = "red", col.axis = "red")
axis(side = 2, at = 0.5, labels = 0.5, tick = T, col = "red", col.axis = "red")



## ----km.mediana.estimativa, echo=TRUE, message=FALSE, warning=FALSE--------------------------------

ekm



## ----survival, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------

library(survival)

ekm <- survfit(Surv(time = tempo, event = cens) ~ grupo,
               data = df.hep,
               conf.type = "log-log")



## ----ggsurv, echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='70%'----------

# install.packages("survminer")

library(survminer)

ggsurvplot(fit = ekm, data = df.hep)



## ----ggsurvsintax, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE-----------------------------
## 
## ggsurvplot(
##   fit = ekm,
##   data = df.hep,
##   size = 1,                 # change line size
##   palette =
##     c("#E7B800", "#2E9FDF"),# custom color palettes
##   conf.int = TRUE,          # Add confidence interval
##   pval = TRUE,              # Add p-value
##   risk.table = TRUE,        # Add risk table
##   risk.table.col = "strata",# Risk table color by groups
##   legend.labs =
##     c("Controle", "Esteroide"),    # Change legend labels
##   risk.table.height = 0.25, # Useful to change when you have multiple groups
##   ggtheme = theme_bw()      # Change ggplot2 theme
## )
## 


## ----ggsurvplot, echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width='100%'----

ggsurvplot(
  fit = ekm,
  data = df.hep,
  size = 1,                 # change line size
  palette =
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs =
    c("Controle", "Esteroide"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-ff.jpg'))


