
#***********************************************************************************
# The Effect of Anticipated and Unanticipated Government Spending Shocks in Peru
# Aguilar and Lahura (2025)
#***********************************************************************************


rm(list=ls())


library("readxl")
library("vars")
library("svars")
library("dplyr")
library("seasonal")
library("foreign")
library("ggplot2")
library("psych")
library("rsample")
library("boot")

data <- read_excel("Data_Aguilar_Lahura_2025.xlsx", sheet = "robust3")

gov.dif <- data$fisco
gdp.dif <- data$pbi
tax.dif <- data$tax
pred.dif <- data$pred
ti.dif <- data$ti
cons.dif <- data$cons

#------------------------------------------------
#  BASE MODEL
#------------------------------------------------

rep = 1000

sb = 0.9   

sv_base <- cbind(ti.dif, gov.dif, gdp.dif, cons.dif, tax.dif)

library(openxlsx)

colnames(sv_base) <- cbind("ti", "fisco", "pbi","cons", "taxes")

# Model Checks:

# Lag selection:

lagselect_base <- VARselect(sv_base, lag.max = 5, type = "cons")

lagselect_base$selection

lag = 1

# Stability:

Model_base <- VAR(sv_base, p = lag , season = NULL, exog = NULL, type = "const")

roots(Model_base)

# Autocorrelation

pt12 <- serial.test(Model_base, type = "BG", lags.pt = 1)

pt12

# IRFs


# PBI

irf_base_pbi_fisco <- irf(Model_base, impulse = "fisco", response = "pbi",
                          n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345., ci=sb)


irf_base_pbi_fisco_2 <- irf(Model_base, impulse = "fisco", response = "pbi",
                            n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345., ci=0.68)


lower = irf_base_pbi_fisco[["Lower"]][["fisco"]]

upper = irf_base_pbi_fisco[["Upper"]][["fisco"]]

lower2 = irf_base_pbi_fisco_2[["Lower"]][["fisco"]]

upper2 = irf_base_pbi_fisco_2[["Upper"]][["fisco"]]


irf = irf_base_pbi_fisco_2[["irf"]][["fisco"]]

periodo = data.frame(periodo=seq(0,20,1))

single_varirf_grouped <- data.frame(irf=irf, lower=lower, upper=upper, lower2=lower2, upper2=upper2)

single_varirf_grouped <- as.data.frame(append(periodo,single_varirf_grouped))


pbi_fisco_BASE <- single_varirf_grouped %>% 
  ggplot(aes(x=periodo, y=pbi))+
  geom_hline(yintercept = 0, color="red", size=1) +
  geom_ribbon(aes(x = periodo,ymin = pbi.1, ymax = pbi.2), inherit.aes = FALSE,fill="grey", alpha=0.35)+
  geom_ribbon(aes(x = periodo,ymin = pbi.3, ymax = pbi.4), inherit.aes = FALSE,fill="grey", alpha=0.9)+
  geom_point(size=2)+
  geom_line(size=1.3) +
  theme_light() +
  ggtitle("GDP")+
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 11, hjust=0.25),axis.title.y = element_text(size=11))



# CONSUMPTION

irf_base_cons_fisco <- irf(Model_base, impulse = "fisco", response = "cons",
                           n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345., ci=sb)


irf_base_cons_fisco_2 <- irf(Model_base, impulse = "fisco", response = "cons",
                             n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345., ci=0.68)


lower = irf_base_cons_fisco[["Lower"]][["fisco"]]

upper = irf_base_cons_fisco[["Upper"]][["fisco"]]

lower2 = irf_base_cons_fisco_2[["Lower"]][["fisco"]]

upper2 = irf_base_cons_fisco_2[["Upper"]][["fisco"]]


irf = irf_base_cons_fisco_2[["irf"]][["fisco"]]

periodo = data.frame(periodo=seq(0,20,1))

single_varirf_grouped <- data.frame(irf=irf, lower=lower, upper=upper, lower2=lower2, upper2=upper2)

single_varirf_grouped <- as.data.frame(append(periodo,single_varirf_grouped))


cons_fisco_BASE <- single_varirf_grouped %>% 
  ggplot(aes(x=periodo, y=cons))+
  geom_hline(yintercept = 0, color="red", size=1) +
  geom_ribbon(aes(x = periodo,ymin = cons.1, ymax = cons.2), inherit.aes = FALSE,fill="grey", alpha=0.35)+
  geom_ribbon(aes(x = periodo,ymin = cons.3, ymax = cons.4), inherit.aes = FALSE,fill="grey", alpha=0.9)+
  geom_point(size=2)+
  geom_line(size=1.3) +
  theme_light() +
  ggtitle("Consumption")+
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 11, hjust=0.25),axis.title.y = element_text(size=11))


library('cowplot')
plot_grid( pbi_fisco_BASE, cons_fisco_BASE)


#---------------------------
# EXTENDED VAR 
#---------------------------

sv_pred <- cbind(ti.dif, pred.dif, gov.dif, gdp.dif, cons.dif, tax.dif)

colnames(sv_pred) <- cbind("ti","pred", "fisco","pbi","cons", "tax")


lagselect_base_pred <- VARselect(sv_pred, lag.max = 5, type = "const")

lagselect_base_pred$selection

Model_pred <- VAR(sv_pred, p = 1, season = NULL, exog = NULL, type = "const")


irf_pred_pbi_pred <- irf(Model_pred, impulse = "pred", response = "pbi",
                         n.ahead = 20, ortho = TRUE, runs = 1000, seed = 12345, ci=sb)

irf_pred_pbi_fisco <- irf(Model_pred, impulse = "fisco", response = "pbi",
                          n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=sb)

irf_pred_pbi_fisco_2 <- irf(Model_pred, impulse = "fisco", response = "pbi",
                            n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=0.68)

irf_pred_pbi_pred2 <- irf(Model_pred, impulse = "pred", response = "pbi",
                          n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=0.68)


irf_pred_cons_pred <- irf(Model_pred, impulse = "pred", response = "cons",
                          n.ahead = 20, ortho = TRUE, runs = 1000, seed = 12345, ci=sb)

irf_pred_cons_fisco <- irf(Model_pred, impulse = "fisco", response = "cons",
                           n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=sb)

irf_pred_cons_fisco_2 <- irf(Model_pred, impulse = "fisco", response = "cons",
                             n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=0.68)

irf_pred_cons_pred2 <- irf(Model_pred, impulse = "pred", response = "cons",
                           n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=0.68)

# Gráficos:

# EVAR: choques de gasto público:

# PBI

lower = irf_pred_pbi_fisco[["Lower"]][["fisco"]]
upper = irf_pred_pbi_fisco[["Upper"]][["fisco"]]
lower2 = irf_pred_pbi_fisco_2[["Lower"]][["fisco"]]
upper2 = irf_pred_pbi_fisco_2[["Upper"]][["fisco"]]


irf = irf_pred_pbi_fisco[["irf"]][["fisco"]]

periodo = data.frame(periodo=seq(0,20,1))

single_varirf_grouped <- data.frame(irf=irf, lower=lower, upper=upper, lower2=lower2, upper2=upper2)

single_varirf_grouped <- as.data.frame(append(periodo,single_varirf_grouped))


pbi_fisco_pred <- single_varirf_grouped %>% 
  ggplot(aes(x=periodo, y=pbi))+
  geom_hline(yintercept = 0, color="red", size=1) +
  geom_ribbon(aes(x = periodo,ymin = pbi.1, ymax = pbi.2), inherit.aes = FALSE,fill="grey", alpha=0.35)+
  geom_ribbon(aes(x = periodo,ymin = pbi.3, ymax = pbi.4), inherit.aes = FALSE,fill="grey", alpha=0.9)+
  geom_point(size=2)+
  geom_line(size=1.3) +
  theme_light() +
  ggtitle("GDP")+
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 11, hjust=0.25),axis.title.y = element_text(size=11))


# CONSUMPTION

lower = irf_pred_cons_fisco[["Lower"]][["fisco"]]
upper = irf_pred_cons_fisco[["Upper"]][["fisco"]]
lower2 = irf_pred_cons_fisco_2[["Lower"]][["fisco"]]
upper2 = irf_pred_cons_fisco_2[["Upper"]][["fisco"]]


irf = irf_pred_cons_fisco[["irf"]][["fisco"]]

periodo = data.frame(periodo=seq(0,20,1))

single_varirf_grouped <- data.frame(irf=irf, lower=lower, upper=upper, lower2=lower2, upper2=upper2)

single_varirf_grouped <- as.data.frame(append(periodo,single_varirf_grouped))


cons_fisco_pred <- single_varirf_grouped %>% 
  ggplot(aes(x=periodo, y=cons))+
  geom_hline(yintercept = 0, color="red", size=1) +
  geom_ribbon(aes(x = periodo,ymin = cons.1, ymax = cons.2), inherit.aes = FALSE,fill="grey", alpha=0.35)+
  geom_ribbon(aes(x = periodo,ymin = cons.3, ymax = cons.4), inherit.aes = FALSE,fill="grey", alpha=0.9)+
  geom_point(size=2)+
  geom_line(size=1.3) +
  theme_light() +
  ggtitle("Consumption")+
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 11, hjust=0.25),axis.title.y = element_text(size=11))


library('cowplot')
plot_grid( pbi_fisco_pred, cons_fisco_pred)



# EVAR: Choques de expectativas

# PBI

lower = irf_pred_pbi_pred[["Lower"]][["pred"]]
upper = irf_pred_pbi_pred[["Upper"]][["pred"]]
lower2 = irf_pred_pbi_pred2[["Lower"]][["pred"]]
upper2 = irf_pred_pbi_pred2[["Upper"]][["pred"]]
irf = irf_pred_pbi_pred2[["irf"]][["pred"]]
periodo = data.frame(periodo=seq(0,20,1))

single_varirf_grouped <- data.frame(irf=irf, lower=lower, upper=upper, lower2=lower2, upper2=upper2)
single_varirf_grouped <- as.data.frame(append(periodo,single_varirf_grouped))


pbi_pred <- single_varirf_grouped %>% 
  ggplot(aes(x=periodo, y=pbi))+
  geom_hline(yintercept = 0, color="red", size=1) +
  geom_ribbon(aes(x = periodo,ymin = pbi.1, ymax = pbi.2), inherit.aes = FALSE,fill="grey", alpha=0.35)+
  geom_ribbon(aes(x = periodo,ymin = pbi.3, ymax = pbi.4), inherit.aes = FALSE,fill="grey", alpha=0.9)+
  geom_point(size=2)+
  geom_line(size=1.3) +
  theme_light() +
  ggtitle("GDP")+
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 11, hjust=0.25),axis.title.y = element_text(size=11))



# CONSUMPTION

lower = irf_pred_cons_pred[["Lower"]][["pred"]]
upper = irf_pred_cons_pred[["Upper"]][["pred"]]
lower2 = irf_pred_cons_pred2[["Lower"]][["pred"]]
upper2 = irf_pred_cons_pred2[["Upper"]][["pred"]]
irf = irf_pred_cons_pred2[["irf"]][["pred"]]
periodo = data.frame(periodo=seq(0,20,1))

single_varirf_grouped <- data.frame(irf=irf, lower=lower, upper=upper, lower2=lower2, upper2=upper2)
single_varirf_grouped <- as.data.frame(append(periodo,single_varirf_grouped))


cons_pred <- single_varirf_grouped %>% 
  ggplot(aes(x=periodo, y=cons))+
  geom_hline(yintercept = 0, color="red", size=1) +
  geom_ribbon(aes(x = periodo,ymin = cons.1, ymax = cons.2), inherit.aes = FALSE,fill="grey", alpha=0.35)+
  geom_ribbon(aes(x = periodo,ymin = cons.3, ymax = cons.4), inherit.aes = FALSE,fill="grey", alpha=0.9)+
  geom_point(size=2)+
  geom_line(size=1.3) +
  theme_light() +
  ggtitle("Consumption")+
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 11, hjust=0.25),axis.title.y = element_text(size=11))

library('cowplot')
plot_grid( pbi_pred, cons_pred)


#----------------------------------
# Comparing Models
#----------------------------------

pbi_VAR=ts(data.frame(irf_base_pbi_fisco[["irf"]][["fisco"]]))

pbi_EVAR=ts(data.frame(irf_pred_pbi_fisco[["irf"]][["fisco"]]))

cons_VAR=ts(data.frame(irf_base_cons_fisco[["irf"]][["fisco"]]))

cons_EVAR=ts(data.frame(irf_pred_cons_fisco[["irf"]][["fisco"]]))


d1 <- cons_VAR - cons_EVAR
print(d1)
sum(d1[1:13,])

par(mfrow=c(1,2))

ts.plot(pbi_VAR, lwd=2, col='black', ylim=c(-0.5, 1.2), xlab ='', ylab='', main='GDP')
lines(pbi_EVAR, lwd=2, col='red' )
abline(h=0, col='gray', lwd=2)
legend('topright', col=c('black', 'red'), lwd=2, 
       legend=c("Base VAR", "Extended VAR"), bg='white')  


ts.plot(cons_VAR, lwd=2, col='black', ylim=c(-0.5, 1.2), xlab ='', ylab='', main='Consumption')
lines(cons_EVAR, lwd=2, col='red' )
abline(h=0, col='gray', lwd=2)
legend('topright', col=c('black', 'red'), lwd=2, 
       legend=c("Base VAR", "Extended VAR"), bg='white') 

#-------------------------------------- ---------------------------
# Predictability (Following Gorodnichenko)
#------------------------------------------------------------------

par(mfrow=c(1,1))

e_pred=ts(Model_pred[["varresult"]][["pred"]][["residuals"]])

e_gob=ts(Model_pred[["varresult"]][["fisco"]][["residuals"]])

e_pred=as.vector(e_pred, mode="numeric")

e_gob=as.vector(e_gob, mode="numeric")

corr=cor(e_gob,e_pred)

corr = round(corr,2)

regresion_d=lm(e_gob~e_pred)

summary(regresion_d)

b=regresion_d[["coefficients"]][["e_pred"]]

b = round(b,2)


plot(e_pred,e_gob, col='blue', ylab='Residuals of G in a VAR model',
     xlab='Residuals of G forecasts in a VAR model', lwd=3)

abline(regresion_d, col="red",lwd=2)
text(-4, 15, paste0("corr = ",corr))
text(-4.1, 13, paste0("b = ",b))
text(-4, 11, paste0("se = ",0.42))


#----------------------------------------
# Granger Test folowing Valerie Ramey
#---------------------------------------

# Choques del gasto pÃºblico en VAR

E <- residuals(Model_base)              
Sigma <- cov(E)                        
L <- t(chol(Sigma))                     
U <- E %*% solve(t(L))                  
colnames(U) <- colnames(E)
shock_fisco <- U[, "fisco"]


# Choques del gasto pÃºblico en EVAR

E <- residuals(Model_pred)              
Sigma <- cov(E)                        
L <- t(chol(Sigma))                     
U <- E %*% solve(t(L))                  
colnames(U) <- colnames(E)
shock_fisco_pred <- U[, "fisco"]

plot(ts(shock_fisco, frequency=4, start=c(2007,1)), type='l', col="black", ylab="fiscal shocks", xlab="")
lines(ts(shock_fisco_pred, frequency=4, start=c(2007,1)), type='l', main="EVAR", col="red")
legend('topright', col=c('black', 'red'), lwd=2, 
       legend=c("Base VAR", "Extended VAR"), bg='white')  

# Tests de causalidad a la granger

pred.dif = pred.dif3 

proy = as.data.frame(pred.dif)

proy = proy[2:52,] 

grangertest(shock_fisco ~ proy, order = 1)

grangertest(proy ~ shock_fisco, order = 1)

