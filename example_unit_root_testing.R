# CLEAR THE CONSOLE
cat("\014"); rm(list = ls()); gc()
# SET DEFAULTS: DISPLAY OPTIONS, FONT AND Y AXIS LABEL ROTATION
options(digits = 8); options(scipen = 9999); options(max.print=10000); par(las = 1, family = "serif")
# INSTALL PACMAN PACKAGE MANAGER IF NOT INSTALLED 
if (!"pacman" %in% installed.packages()){install.packages("pacman")}
# SET WORKING DIRECTORY 
# setwd('D:/_teaching/_current.teaching/_SU.TSEF/code-TSEF')
# LOAD HELPER FUNCTIONS STORED IN LOCAL DIRECTORY CALLED: ./local.Functions/
functions_path = c("./local.Functions/"); if (dir.exists(functions_path)){
invisible( lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ) ) }
# UNCOMMENT TO LOAD R BASELINE R_utility_functions.R FROM D:/matlab.tools/db.toolbox
# source("D:/matlab.tools/db.toolbox/R_utility_functions.R")
source("./R_utility_functions.R")
# LOAD REQUIRED PACKAGES
pacman::p_load(tictoc,matlab,tsibble,zoo,tidyverse,readxl,sandwich,car,quantmod); 
tic(); set.seed(1234)

## SCRIPT STARTS HERE ----
# 1) read data as tsibble
NP = read_xlsx("./data/Nelson_Plosser_data.xlsx")
# NP$Year = ymd(NP$Year, truncated = 2L)
# NP = as_tsibble(NP, index = Year)
# print(NP, n = 150)

# select the variable to work with for the ADF tests ---- 
variable_selected = "PCRGNP"        
y = log(get(variable_selected, NP))           # y = log(NP$"PCRGNP")
# convert -inf due to log(0) to NA which R handles by removing them 
y[is.infinite(y)] = NA
Date = NP$Year
dy = y-lag(y)
# dy = c(NA, diff(y))
trend = 1:length(dy)

# plot the series to see if trending or not
plot( Date, y, type = 'l', lwd = 1.5, 
      cex.lab  = 1.5, cex.axis = 1.5, cex.main = 1.5 )

# plot ACF
pacf = plot.acf(dy)
head(pacf)

# Do F-test (manually) to test if unit-root with drift --> 𝛾 = a₂ = 0 (Joint F-test ϕ₃)
# joint.1 = linearHypothesis( adf.1, c("trend=0", "lag(dy, 1) =0"), test = c("F") )
# print(joint.1)
cat(" Nelson-Plosser Series analyzed is: ", variable_selected, "\n")
df.UR = print.results( lm(dy ~ trend + lag(y) + lag(dy,1) ) , -2, Hide = 0)
df.R  = print.results( lm(dy ~                  lag(dy,1) ) , -2, Hide = 1)
plot.acf(df.UR$uhat)

No.restrictions = df.UR$K - df.R$K
Fstat = 1/No.restrictions *(df.R$SSE - df.UR$SSE)/(df.UR$SSE) * df.UR$DF
cat(" F-test of join joint null-hypotheis (H₀: 𝛾 = a₂ = 0):\n")
cat(" F-stat:", round(Fstat, digits = 8), "\n")
# 𝜙3: 6.49 and 8.73 (95% and 99%)
# if Ftest > 6.49  --> Reject H₀: 𝛾 = a₂ = 0.
if (Fstat < 6.49) { cat( " Do NOT Reject H₀: 𝛾 = a₂ = 0 --> Series has a unit-root! \n " )} else
   { cat(" Reject H₀: 𝛾 = a₂ = 0 --> Series stationary around a deterministic time trend!") } 
cat("\n")

# select the variable to work with for the ADF tests -------------------------------------------------
variable_selected = "Unemployment"        
y = log(get(variable_selected, NP))           # y = log(NP$"Unemployment")
# convert -inf due to log(0) to NA which R handles by removing them
y[is.infinite(y)] = NA
Date = NP$Year
dy = y-lag(y)
trend = 1:length(dy)

# plot the series to see if trending or not
plot( Date, y, type = 'l', lwd = 1.5,  # ylim=c(4, 10),
      cex.lab  = 1.5, cex.axis = 1.5, cex.main = 1.5 )

# plot ACF
pacf = plot.acf(y)
head(pacf)

# Do F-test (manually) to test if unit-root without drift --> a₀ = 𝛾 = 0 (Joint F-test ϕ₃)
# joint.1 = linearHypothesis( adf.1, c("trend=0", "lag(dy, 1) =0"), test = c("F") )
# print(joint.1)
cat(" Nelson-Plosser Series analyzed is: ", variable_selected, "\n")
df.UR = print.results( lm(dy ~ lag(y) + lag(dy, 1) ) , -2, Hide = 0)
df.R  = print.results( lm(dy ~ 0      + lag(dy, 1) ) , -2, Hide = 1)
plot.acf(df.UR$uhat)

No.restrictions = df.UR$K - df.R$K
Ftest = 1/No.restrictions *(df.R$SSE - df.UR$SSE)/(df.UR$SSE) * df.UR$DF
cat(" F-test of join joint null-hypotheis (H₀: a₀ = 𝛾 = 0):\n")
cat(" F-stat:", round(Ftest, digits = 4), "\n")
# 𝜙1: 4.71 and 6.70 (95% and 99%)
# if Ftest > 4.71  --> Reject H₀: a₀ = 𝛾 = 0.
if (Ftest < 4.72) { cat( " Do NOT Reject H₀: a₀ = 𝛾 = 0 --> Series has a Unit-root without drift! " )} else
{ cat(" Reject H₀: a₀ = 𝛾 = 0 --> Series is stationary!") }

plot.acf(dy)
