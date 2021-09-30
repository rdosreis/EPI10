
estagioII estagioIII estagioIV

newdat <- with(df.laringe, 
               data.frame(
                 estagio = levels(df.laringe$estagio)
               )
)

newdat


S0 <- survfit(mod1)

coef(mod1)

lines(S0$time, S0$surv^exp(coef(mod1)[1]), type = "s", lty = 3, add = T)
lines(S0$time, S0$surv^exp(coef(mod1)[2]), type = "s", lty = 4, add = T)
lines(S0$time, S0$surv^exp(coef(mod1)[3]), type = "s", lty = 5, add = T)


plot(ekm, fun = "cloglog", conf.int = F,col = c("#1B9E77", "#D95F02","#7570B3", "#E7298A"), lwd = 2)
> plot(cox.zph(mod3), col = "red", lwd = 2)
> cox.zph(mod3, transform = "identity")
