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
local.Rfunction   = "R_utility_functions.R"; 
if (file.exists(local.Rfunction)){source(local.Rfunction) 
} else if(file.exists(substr(utility.functions,9,nchar(utility.functions)))){source(utility.functions)
} else {download.file("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc", destfile = local.Rfunction, mode = "wb")
  source(url("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc"))}
# define some printing to pdf parameters and graphics directory
print2pdf = 0
path2graphics = "./graphics/"
# SET WORKING DIRECTORY if needed
# setwd('D:/_teaching/_current.teaching/_SU.TSEF/code-TSEF')
set.seed(1234)        # fix seed if needed for reproducibility of results

# %% Non-invertible MA Lag polynomial 
b1 = -3.5 ; b2 = -2;
bL = c(1, +b1, +b2);   

# plot theoretical ACF/PACF of non-ivertible MA
plot_acf0(1,bL,50)

# lag-polynomial and roots
cat("For the non-invertible process \n")
cat("Lag Polynomial: ", gsub("x","L",as.polynomial(bL)), "\n")
lag.roots = round(polyroot(bL),4)
if (sum(Im(lag.roots)==0)) {
  cat("Lag polynomial roots are:    ", Re(lag.roots), "\n") } else {
	cat("Lag polynomial roots are:", lag.roots, "\n" ) 
}
# factored-polynomial and roots ----
fact.roots = round(polyroot(fliplr(bL)),4)
if (sum(Im(fact.roots)==0)) {
  cat("Factored Polynomial roots are:", Re(fact.roots), "\n\n") } else {
  cat("Factored Polynomial roots are:", fact.roots, "\n\n" ) 
}

# Invertibel MA Lag polynomial ----
# make process invertible 
b1_plus = -( Re(fact.roots[1]) + 1/Re(fact.roots[2]))
b2_plus =  ( Re(fact.roots[1])*1/Re(fact.roots[2]))
bL_plus = c(1, +b1_plus, +b2_plus)

cat("For the invertible process \n")
cat("Lag Polynomial: ", gsub("x","L",as.polynomial(bL_plus)), "\n")

lag.roots_plus = round(polyroot(bL_plus),4)
if (sum(Im(lag.roots_plus)==0)) {
  cat("Lag polynomial roots are:", Re(lag.roots_plus), "\n") } else {
  cat("Lag polynomial roots are:", lag.roots_plus, "\n" ) 
}

# factored-polynomial and roots ----
fact.roots_plus = round(polyroot(fliplr(bL_plus)),4)
if (sum(Im(fact.roots_plus)==0)) {
  cat("Factored Polynomial roots are:", Re(fact.roots_plus), "\n\n") } else {
  cat("Factored Polynomial roots are:", fact.roots_plus, "\n\n" ) 
}

# plot theoretical ACF/PACF of non-ivertible MA
plot_acf0(1,bL_plus,50)


