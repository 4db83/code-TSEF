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
pacman::p_load( polynom,matlab )
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
set.seed(1234)   			# fix seed if needed for reproducibility of results

# CHECKS if R_utility_functions.R at D:/matlab.tools/db.toolbox exist, if not, loads online GoogleDrive version
# must use file before the source command to avoid Positron issues not opening files
utility.functions = "file:///D:/matlab.tools/db.toolbox/R_utility_functions.R"
if (file.exists(utility.functions)) { source(utility.functions) } else
{source(url("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc")) }

# SET WORKING DIRECTORY if needed
# setwd('D:/_research/_current/LW03_2024/code')

# %% Lecture Example ARMA(3,2):
# AR Lag polynomial
aL = c(1, -1.3, .8, 0.1)
# aL = c(1, -.9999)
cat("AR Lag Polynomial is: "); print(gsub("x","L",as.polynomial(aL)))
# gsub("x","L",as.polynomial(aL)), "\n")
# MA Lag polynomial
bL = c(1, 0.4, -0.2)
# bL = 1
cat("MA Lag Polynomial is: "); print(gsub("x","L",as.polynomial(bL)))

# plot theoretical ACF
cat("Factored α(L) Roots are: \n")
roots.aL = round(polyroot(rev(aL)),4)
cat(roots.aL, "\n")
cat("Factored β(L) Roots are: \n")
roots.bL = round(polyroot(rev(bL)),4)
cat(roots.bL, "\n")
plot_acf0(aL,bL,50)
					
# Coefficients:
#   ar1     ar2     ma1     mean
# -0.9557  -0.014  0.9889  -0.0974
# s.e.   0.0592   0.059  0.0155   0.0547
# aL = c(1, 0.9557, 0.014); bL = c(1, 0.9889)