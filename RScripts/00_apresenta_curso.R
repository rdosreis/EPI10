## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', out.height='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'hi_my_name_is.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images','covid-recomendacoes.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='10%', paged.print=FALSE----

knitr::include_graphics(here::here('images','Rlogo.png'))



## ---- echo=TRUE, eval=TRUE--------------------------------------------------------------------

library(survival)
fit1 <- survfit(Surv(futime,fustat) ~ resid.ds,
                data = ovarian)



## ---- echo=FALSE, eval=TRUE, fig.align='center', out.width='60%'------------------------------

plot(fit1, col = 1:2, xscale = 365.25,
     lwd = 2, mark.time = TRUE,
     xlab = "Years since study entry",
     ylab = "Survival")
legend(750, .9,
       c("No residual disease", "Residual disease"),
       col = 1:2, lwd = 2, bty = 'n')



## ----echo=FALSE, fig.align='right', message=FALSE, warning=FALSE, out.width='15%', paged.print=FALSE----
knitr::include_graphics(here('images','ctanlion.png'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'arvore-processo-envelhecimento.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'processo-envelhecimento.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'ecr-01.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'ecr-02.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-8.jpg'))


