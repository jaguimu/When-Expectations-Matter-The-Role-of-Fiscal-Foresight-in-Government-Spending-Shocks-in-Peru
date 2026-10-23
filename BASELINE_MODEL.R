
#**********************************************************************
# When Expectations Matter: 
# The Role of Fiscal Foresight in Government Spending Shocks in Peru
# Aguilar and Lahura (2025)
#*********************************************************************

# BASELINE MODEL 

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

script.path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script.path)

data <- read_excel("Data_Aguilar_Lahura_2025.xlsx", sheet = "baseline")

gov.dif <- data$fisco
gdp.dif <- data$pbi
tax.dif <- data$tax

#------------------------------------------------
#  BASE MODEL
#------------------------------------------------

rep = 1000

sb = 0.9   

sv_base <- cbind(gov.dif, gdp.dif, tax.dif)

library(openxlsx)

colnames(sv_base) <- cbind("fisco", "pbi", "taxes")

# Model Checks:

# Lag selection:

lagselect_base <- VARselect(sv_base, lag.max = 4, type = "cons")

lagselect_base$selection

# Stability:

library(vars)

lag =1

Model_base <- VAR(sv_base, p = lag , season = NULL, exog = NULL, type = "const")

roots(Model_base)


# IRFs

irf_base_pbi_fisco <- irf(Model_base, impulse = "fisco", response = "pbi",
                          n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345., ci=sb)


irf_base_pbi_fisco_2 <- irf(Model_base, impulse = "fisco", response = "pbi",
                            n.ahead = 20, ortho = TRUE, runs = rep, seed = 12345., ci=0.68)


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

#--------------------------
# Extended VAR
#--------------------------

pred.dif = data$pred

sv_pred <- cbind(pred.dif, gov.dif, gdp.dif, tax.dif)

colnames(sv_pred) <- cbind("pred","fisco","pbi", "tax")

write.xlsx(sv_pred, "baseline_model_data.xlsx")


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

# EVAR: Goverment shocks

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



library(ggplot2)
library(tidyr)
library(dplyr)

make_time <- function(x) {
  if (inherits(x, "ts")) {
    return(time(x))  
  } else {
    return(seq_along(x))  
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
# Predictability of G. Spending (Following Gorodnichenko)
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

# G shocks in VAR

E <- residuals(Model_base)              
Sigma <- cov(E)                        
L <- t(chol(Sigma))                     
U <- E %*% solve(t(L))                  
colnames(U) <- colnames(E)
shock_fisco <- U[, "fisco"]


# G shocks in EVAR

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

# Granger Causality Test

pred.dif = pred.dif3 + pred.dif2

proy = as.data.frame(pred.dif)

proy = proy[2:52,] 

grangertest(shock_fisco ~ proy, order = 1)

grangertest(proy ~ shock_fisco, order = 1)


## Residuals ans Predictability Analysis

s_base <- ts(shock_fisco,      frequency = 4, start = c(2007,2))
s_evar <- ts(shock_fisco_pred, frequency = 4, start = c(2007,2))

dd <- shock_fisco - shock_fisco_pred
dd <- ts(dd, frequency = 4, start = c(2007,2))


library(ggplot2)
library(tidyr)
library(dplyr)


qtime <- function(year, q) year + (q - 1)/4

shade_ranges <- data.frame(
  y1 = c(2007, 2013, 2017, 2011),
  q1 = c(1,     2,    2,    2),
  y2 = c(2007, 2013, 2017,  2012),
  q2 = c(3,     3,    3,    3)
)


time_idx <- as.numeric(time(s_base))
df_long <- tibble(
  time = time_idx,
  `Base VAR`     = as.numeric(s_base),
  `Extended VAR` = as.numeric(s_evar)
) %>%
  pivot_longer(-time, names_to = "Modelo", values_to = "Shock")


df_shade <- tibble(
  xmin = qtime(shade_ranges$y1, shade_ranges$q1),
  xmax = qtime(shade_ranges$y2, shade_ranges$q2) + 0.25  # incluye trimestre final
)


yl <- range(c(s_base, s_evar), na.rm = TRUE)

G5 <- ggplot() +
  #
  geom_rect(data = df_shade,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "gold", alpha = 0.12, inherit.aes = FALSE) +
 
  
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.5) +
 
  geom_line(data = df_long, aes(x = time, y = Shock, color = Modelo),
            linewidth = 1) +
  scale_color_manual(values = c("Base VAR" = "black", "Extended VAR" = "red")) +
  coord_cartesian(ylim = yl) +
  
  scale_x_continuous(breaks = seq(2007, 2020, by = 1)) +
  labs(x = "", y = "Fiscal shocks", title = "(A) Fiscal shocks in the Base and Extended VAR") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

print(G)

library(ggplot2)

pred.dif = pred

pred.dif <- as.data.frame(pred.dif) 

pred.dif <- pred.dif[2:52,1]

dd <- as.data.frame(dd)

dd <- dd[1:51,1]

df <- data.frame(diff = dd, pred = pred.dif)

df <- subset(df, is.finite(diff) & is.finite(pred))

ggplot(df, aes(x = diff, y = pred)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5) +
  labs(title = "",
       x = "Difference of fiscal shocks (Base VAR ??? Extended VAR)",
       y = "Goverment forecast") +
  theme_minimal(base_size = 13)


fit   <- lm(pred ~ diff, data = df)         
s     <- summary(fit)

slope <- coef(s)["diff","Estimate"]           
se_ols<- coef(s)["diff","Std. Error"]         
pval  <- coef(s)["diff","Pr(>|t|)"]         
r_pearson <- cor(df$diff, df$pred, use = "complete.obs")  




lab <- sprintf("Slope = %.3f (se = %.3f) corr = %.3f",
               slope, se_ols, r_pearson)

xr <- range(df$diff, na.rm = TRUE)
yr <- range(df$pred, na.rm = TRUE)
x_anno <- xr[1] + 0.02 * diff(xr)           
y_anno <- yr[2] - 0.02 * diff(yr)           


library(ggplot2)

G6 <- ggplot(df, aes(x = diff, y = pred)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5) +
  annotate("text", x = x_anno, y = y_anno, hjust = 0, vjust = 1,
           label = lab, size = 3.8) +
  labs(title = "(B) Government forecast and Difference of fiscal shocks",
       x = "Difference of fiscal shocks (Base VAR ??? Extended VAR)",
       y = "Government forecast") +
  theme_minimal(base_size = 13)
G6








