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
pacman::p_load(	tictoc,matlab,forecast,polynom,dplyr )
# CHECKS if R_utility_functions.R at D:/matlab.tools/db.toolbox exist, if not, (down)loads online GoogleDrive version
utility.functions = "file:///D:/matlab.tools/db.toolbox/R_utility_functions.R";  # use file before to avoid Positron issues not opening files
local.Rfunction 	= "R_utility_functions.R";
if (file.exists(local.Rfunction)){source(local.Rfunction)
} else if(file.exists(substr(utility.functions,9,nchar(utility.functions)))){source(utility.functions)
} else {download.file("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc", destfile = local.Rfunction, mode = "wb")
  source(url("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc"))}
# define some printing to pdf parameters and graphics directory
print2pdf = 0
path2graphics = "./graphics/"
# SET WORKING DIRECTORY if needed
# setwd('D:/_teaching/_current.teaching/_SU.TSEF/code-TSEF')
# set.seed(04)   			# fix seed if needed for reproducibility of results

# %%  SCRIPT STARTS HERE
source(utility.functions)  # load utility functions, if not already loaded above
T = 1e5;
# mean of the ARMA process
mu = 2; 			
# MA parameters
b1 = .5; b2 = 1.2;		
bL = c(1, b1, b2);  	# MA lag polynomial coefficients
# cat(" α(L) Roots \n"); roots(aL)
cat(" β(L) Roots \n"); roots(bL)
# SIMULATE ARMA. NOTE: you have to add the mean seperately, otherwise the mean is zero
y = mu + arima.sim(n = T, model = list( ma = c(b1, b2)))
# Estimate and summarize 
arma_out 		= arima(y, order = c(0, 0, 2), include.mean = TRUE)
print_arma 	= print_results(arma_out)

# %% Check invertibility by inverting the MA lag polynomial and checking the roots of the inverted lag polynomial
# bout = print_results(lm(y ~ lag(as.vector(y),1)))
# psi1 = arma2ar(ar.L = c(1,-0.9) , ma.L = bL, max.lag = 1e4)  
lams = roots(bL,1)
inv.lams = 1/lams
a = Re(inv.lams[1])
b = Im(inv.lams[1])
bL.plus = round(c(1,a*2, a^2+b^2), digits = 4)
roots(bL.plus)
cat("Invertible MA(2): ", gsub("x","L",as.polynomial(bL.plus)),"\n")
