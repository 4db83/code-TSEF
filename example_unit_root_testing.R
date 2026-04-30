# %%  CLEAR THE CONSOLE
cat("\014"); rm(list = ls()); gc()
# SET DEFAULTS: DISPLAY OPTIONS, FONT AND Y AXIS LABEL ROTATION
options(digits = 8); options(scipen = 999);  options(max.print=10000)
windowsFonts("Palatino" = windowsFont("Palatino Linotype")); par(las = 1, family = "Palatino")
# INSTALL PACMAN PACKAGE MANAGER IF NOT INSTALLED
if (!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD HELPER FUNCTIONS STORED IN LOCAL DIRECTORY CALLED: ./local.Functions/
functions_path = c("./local.functions/"); if (dir.exists(functions_path)){
invisible( lapply( paste0(functions_path, list.files(functions_path, "*.R")), source ) ) }
# LOAD REQUIRED PACKAGES
pacman::p_load( tictoc,matlab,tsibble,zoo,tidyverse,readxl,sandwich,car,quantmod ) 
# CHECKS if R_utility_functions.R at D:/matlab.tools/db.toolbox exist, if not, (down)loads online GoogleDrive version
utility.functions = "file:///D:/matlab.tools/db.toolbox/R_utility_functions.R";  # use file before to avoid Positron issues not opening files
local.Rfunction   = "R_utility_functions.R"; 
if (file.exists(local.Rfunction)){source(local.Rfunction) 
} else if(file.exists(substr(utility.functions,9,nchar(utility.functions)))){source(utility.functions)
} else {download.file("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc", destfile = local.Rfunction, mode = "wb")
  source(url("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc"))}
# define some printing to pdf parameters and graphics directory
print2pdf = 0
path2graphics = "./graphics/"
# fix seed if needed for reproducibility of results
set.seed(1234)

# %% SCRIPT STARTS HERE 
# 1) read data as tsibble
NP = read_xlsx("./data/Nelson_Plosser_data.xlsx")
# NP$Year = ymd(NP$Year, truncated = 2L)
# NP = as_tsibble(NP, index = Year)
# print(NP, n = 150)

# select the variable to work with for the ADF tests 
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
pacf = plot_acf(dy)
head(pacf)

# Do F-test (manually) to test if unit-root with drift --> 𝛾 = a₂ = 0 (Joint F-test ϕ₃)
# joint.1 = linearHypothesis( adf.1, c("trend=0", "lag(dy, 1) =0"), test = c("F") )
# print(joint.1)
cat(" Nelson-Plosser Series analyzed is: ", variable_selected, "\n")
df.UR = print_results( lm(dy ~ trend + lag(y) + lag(dy,1) ) , -2, Hide = 0)
df.R  = print_results( lm(dy ~                  lag(dy,1) ) , -2, Hide = 1)
plot_acf(df.UR$uhat)

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
pacf = plot_acf(y)
head(pacf)

# Do F-test (manually) to test if unit-root without drift --> a₀ = 𝛾 = 0 (Joint F-test ϕ₃)
# joint.1 = linearHypothesis( adf.1, c("trend=0", "lag(dy, 1) =0"), test = c("F") )
# print(joint.1)
cat(" Nelson-Plosser Series analyzed is: ", variable_selected, "\n")
df.UR = print_results( lm(dy ~ lag(y) + lag(dy, 1) ) , -2, Hide = 0)
df.R  = print_results( lm(dy ~ 0      + lag(dy, 1) ) , -2, Hide = 1)
plot_acf(df.UR$uhat)

No.restrictions = df.UR$K - df.R$K
Ftest = 1/No.restrictions *(df.R$SSE - df.UR$SSE)/(df.UR$SSE) * df.UR$DF
cat(" F-test of join joint null-hypotheis (H₀: a₀ = 𝛾 = 0):\n")
cat(" F-stat:", round(Ftest, digits = 4), "\n")
# 𝜙1: 4.71 and 6.70 (95% and 99%)
# if Ftest > 4.71  --> Reject H₀: a₀ = 𝛾 = 0.
if (Ftest < 4.72) { cat( " Do NOT Reject H₀: a₀ = 𝛾 = 0 --> Series has a Unit-root without drift! " )} else
{ cat(" Reject H₀: a₀ = 𝛾 = 0 --> Series is stationary!") }

plot_acf(dy)
