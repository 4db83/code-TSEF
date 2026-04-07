# clear screen and workspace ----
cat("\014"); rm(list = ls()); gc()
# set working directory if need

# INSTALL PACMAN PACKAGE MANAGER IF NOT INSTALLED (Note: may need to disable windows firewall)
if (!"pacman" %in% installed.packages()){install.packages("pacman"); cat("pacman installed\n")}
# MAKE SOURCE DATA DIRECTORY 
# if (!dir.exists(raw_data)){cat("Making director"); dir.create(raw_data)}
# LOAD REQUIRED PACKAGES 
pacman::p_load(polynom, matlab)
source("./R_utility_functions.R")

# Lecture Example ARMA(3,2):
# AR Lag polynomial
aL = c(1, -1.3, 0.8, 0.1)
cat("AR Lag Polynomial is: "); print(as.polynomial(aL))
# MA Lag polynomial
bL = c(1, 0.4, -0.2)
cat("MA Lag Polynomial is: "); print(as.polynomial(bL))

# plot theoretical ACF
# aL = c(1, 0.9557, 0.014); bL = c(1, 0.9889)
round(polyroot(fliplr(aL)),4)
plot.acf0(aL,bL,50)
 
# Coefficients:
#   ar1     ar2     ma1     mean
# -0.9557  -0.014  0.9889  -0.0974
# s.e.   0.0592   0.059  0.0155   0.0547