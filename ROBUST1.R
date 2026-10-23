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

data <- read_excel("Data_Aguilar_Lahura_2025.xlsx", sheet = "robust1")

gov.dif <- data$fisco
gdp.dif <- data$pbi
tax.dif <- data$tax
pred.dif <- data$pred

#------------------------------------------------
#  MODELO BASE
#------------------------------------------------

rep = 1000

sb = 0.9   #Nivel de significancia

sv_base <- cbind(gov.dif, gdp.dif, tax.dif)

library(openxlsx)

colnames(sv_base) <- cbind("fisco", "pbi", "taxes")

# Model Checks:

# Lag selection:

lagselect_base <- VARselect(sv_base, lag.max = 4, type = "cons")

lagselect_base$selection

lag = 1

# Stability:

library(vars)

Model_base <- VAR(sv_base, p = lag , season = NULL, exog = NULL, type = "const")

roots(Model_base)

# Autocorrelation

pt12 <- serial.test(Model_base, type = "BG", lags.pt = 1)

pt12


Sigma_u <- summary(Model_base)$covres

P <- t(chol(Sigma_u))

shock_1pct <- 1.0

Pjj <- P["fisco", "fisco"]   

factor_escala <- shock_1pct / Pjj

factor_escala

# IRFs

irf_base_pbi_fisco <- irf(Model_base, impulse = "fisco", response = "pbi",
                          n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345., ci=sb)


irf_base_fisco_fisco <- irf(Model_base, impulse = "fisco", response = "fisco",
                            n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345., ci=sb)


irf_base_pbi_fisco_2 <- irf(Model_base, impulse = "fisco", response = "pbi",
                            n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345., ci=0.68)

factor_escala <- as.data.frame(irf_base_fisco_fisco[[1]])[1,1]


lower = irf_base_pbi_fisco[["Lower"]][["fisco"]]

upper = irf_base_pbi_fisco[["Upper"]][["fisco"]]

lower2 = irf_base_pbi_fisco_2[["Lower"]][["fisco"]]

upper2 = irf_base_pbi_fisco_2[["Upper"]][["fisco"]]


irf = irf_base_pbi_fisco[["irf"]][["fisco"]]

periodo = data.frame(periodo=seq(0,20,1))

single_varirf_grouped <- data.frame(irf=irf, lower=lower, upper=upper, lower2=lower2, upper2=upper2)

single_varirf_grouped <- as.data.frame(append(periodo,single_varirf_grouped))


#-----------------------------------
impulse_gdp_fisco_base <- irf
#---------------------------------

pbi_fisco_BASE <- single_varirf_grouped %>% 
  ggplot(aes(x=periodo, y=pbi))+
  geom_hline(yintercept = 0, color="red", size=1) +
  geom_ribbon(aes(x = periodo,ymin = pbi.1, ymax = pbi.2), inherit.aes = FALSE,fill="grey", alpha=0.35)+
  geom_ribbon(aes(x = periodo,ymin = pbi.3, ymax = pbi.4), inherit.aes = FALSE,fill="grey", alpha=0.9)+
  geom_point(size=2)+
  geom_line(size=1.3) +
  theme_light() +
  ggtitle("Base VAR")+
  ylab("")+
  xlab("") +
  scale_y_continuous(limits = c(-0.4, 1.5))+
  theme(plot.title = element_text(size = 11, hjust=0.3),axis.title.y = element_text(size=11))

pbi_fisco_BASE 

#---------------------------
# VAR EXTENDIDO
#---------------------------

sv_pred <- cbind(pred.dif, gov.dif, gdp.dif, tax.dif)

colnames(sv_pred) <- cbind("pred","fisco","pbi", "tax")


lagselect_base_pred <- VARselect(sv_pred, lag.max = 5, type = "both")

lagselect_base_pred$selection


Model_pred <- VAR(sv_pred, p = 1, season = NULL, exog = NULL, type = "const")
roots(Model_pred)
model_summary_pred <- summary(Model_pred)



irf_pred_pbi_pred <- irf(Model_pred, impulse = "pred", response = "pbi",
                         n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=sb)

irf_pred_pbi_fisco <- irf(Model_pred, impulse = "fisco", response = "pbi",
                          n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=sb)

irf_pred_pbi_fisco_2 <- irf(Model_pred, impulse = "fisco", response = "pbi",
                            n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=0.68)

irf_pred_pbi_pred2 <- irf(Model_pred, impulse = "pred", response = "pbi",
                          n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345, ci=0.68)


irf_xxx <- irf(Model_pred, impulse = "fisco", response = "fisco",
               n.ahead = 20, ortho = TRUE, runs = 100, seed = 12345, ci=sb)

irf_yyy <- irf(Model_pred, impulse = "pred", response = "pred",
               n.ahead = 20, ortho = TRUE, runs = 100, seed = 12345, ci=sb)


factor_escala_fisco <- as.data.frame(irf_xxx[[1]])[1,1]


# Graphs


lower = irf_pred_pbi_fisco[["Lower"]][["fisco"]]
upper = irf_pred_pbi_fisco[["Upper"]][["fisco"]]
lower2 = irf_pred_pbi_fisco_2[["Lower"]][["fisco"]]
upper2 = irf_pred_pbi_fisco_2[["Upper"]][["fisco"]]


irf = irf_pred_pbi_fisco[["irf"]][["fisco"]]

#----------------------------------
impulse_gdp_fisco_evar <- irf
#----------------------------------

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
  ggtitle("Extended VAR")+
  ylab("")+
  xlab("") +
  scale_y_continuous(limits = c(-0.4, 1.5))+
  theme(plot.title = element_text(size = 11, hjust=0.25),axis.title.y = element_text(size=11))


library(patchwork)
G1 <- (pbi_fisco_BASE + pbi_fisco_pred ) + 
  plot_layout(ncol = 2, nrow = 1)

G1
#----------------------------------
# Comparing Both Models
#----------------------------------

pbi_VAR=ts(data.frame(irf_base_pbi_fisco[["irf"]][["fisco"]]))

pbi_EVAR=ts(data.frame(irf_pred_pbi_fisco[["irf"]][["fisco"]]))


# Paquetes
library(ggplot2)
library(tidyr)
library(dplyr)

make_time <- function(x) {
  if (inherits(x, "ts")) {
    return(time(x))  # usa el índice de la serie temporal
  } else {
    return(seq_along(x))  # si es vector, usa 1,2,3,...
  }
}

time_index <- make_time(pbi_VAR)


df_long <- tibble(
  time = time_index,
  Base_VAR = as.numeric(pbi_VAR),
  Extended_VAR = as.numeric(pbi_EVAR)
) %>%
  pivot_longer(cols = c(Base_VAR, Extended_VAR),
               names_to = "Modelo",
               values_to = "Valor")

length(pbi_VAR); length(pbi_EVAR)
sum(is.na(pbi_VAR)); sum(is.na(pbi_EVAR))


G2 <- ggplot(df_long, aes(x = time, y = Valor, color = Modelo)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.5) +
  scale_color_manual(values = c("Base_VAR" = "black", "Extended_VAR" = "red")) +  # <-- nombres correctos
  labs(title = "GDP", x = NULL, y = NULL) +
  coord_cartesian(ylim = c(-0.2, 1.5)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

print(G2)


lower = irf_pred_pbi_pred[["Lower"]][["pred"]]
upper = irf_pred_pbi_pred[["Upper"]][["pred"]]
lower2 = irf_pred_pbi_pred2[["Lower"]][["pred"]]
upper2 = irf_pred_pbi_pred2[["Upper"]][["pred"]]
irf = irf_pred_pbi_pred[["irf"]][["pred"]]
periodo = data.frame(periodo=seq(0,20,1))

single_varirf_grouped <- data.frame(irf=irf, lower=lower, upper=upper, lower2=lower2, upper2=upper2)
single_varirf_grouped <- as.data.frame(append(periodo,single_varirf_grouped))


G3 <- single_varirf_grouped %>% 
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


G1
G2
G3

pbi_var <- as.data.frame(irf_base_pbi_fisco[[1]])
pbi_evar <<- as.data.frame(irf_pred_pbi_fisco[[1]])

D_pbi = pbi_var - pbi_evar
D_acum_pbi <- sum(D_pbi[1:12,1])
D_tab_pbi <- c(D_pbi[1,],D_pbi[2,],D_pbi[3,],D_pbi[4,],D_acum_pbi )
D_tab_pbi <- round(D_tab_pbi,2)

#-------------------------------------- ---------------------------
# Predictability (Following Gorodnichenko)
#------------------------------------------------------------------

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


plot(e_pred,e_gob, col='blue', ylab='Residuals of g in the Extended VAR model',
     xlab='Residuals of g forecasts in the Base VAR model', lwd=3)

abline(regresion_d, col="red",lwd=2)
text(-4, 15, paste0("corr = ",corr))
text(-4.1, 13, paste0("b = ",b))
text(-4, 11, paste0("se = ",0.40))


#----------------------------------------
# Granger Test folowing Valerie Ramey
#---------------------------------------

# Choques del gasto pÃÂºblico en VAR

E <- residuals(Model_base)              
Sigma <- cov(E)                        
L <- t(chol(Sigma))                     
U <- E %*% solve(t(L))                  
colnames(U) <- colnames(E)
shock_fisco <- U[, "fisco"]


# Choques del gasto pÃÂºblico en EVAR

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

pred.dif = pred.dif3 + pred.dif2

proy = as.data.frame(pred.dif)

proy = proy[2:52,] 

grangertest(shock_fisco ~ proy, order = 1)

grangertest(proy ~ shock_fisco, order = 1)


