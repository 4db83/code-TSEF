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

# %% AR Lag polynomial
a1 =  1.50 
a2 = -0.56 
aL = c(1, -a1, -a2)
# companion matrix Phi
Phi = matrix( c(a1,a2,1,0), nrow = 2,ncol = 2)

# lag-polynomial and roots
cat("Lag Polynomial: ", gsub("x","L",as.polynomial(aL)), "\n")
lag.roots = round(polyroot(aL),4)
if (sum(Im(lag.roots)==0)) {
  cat("Lag Polynomial roots are:", Re(lag.roots), "\n\n") } else {
	cat("Lag Polynomial roots are:", lag.roots, "\n\n" ) 
}

# factored-polynomial and roots ----
cat("Factored Polynomial: ", gsub("x","\u03BB",as.polynomial(fliplr(aL))), '\n')
fact.roots = round(polyroot(fliplr(aL)),4)
if (sum(Im(fact.roots)==0)) {
  cat("Factored Polynomial roots are:", Re(fact.roots), "\n\n") } else {
  cat("Factored Polynomial roots are:", fact.roots, "\n\n" ) 
}

# companion matrix Phi and its roots/eigenvalues
char.roots = eigen(Phi)[1] # returns a list
cat("Characteristics roots (eigenvalues of Phi) are:", unlist(char.roots), "\n")

# plot lag-polynomial -----
p2 = par(mfrow=c(1,2), mar = c(3,4,2,3), mgp = c(2.2,0.7,0) )
plot(	polynomial(aL), xlim = c(1,1.5), ylim = c(-0.01, 0.01), col="dodgerblue", lwd=2, 
      xlab = "(a) Roots of lag polynomial", ylab = "")
abline(h = 0); abline(v = Re(lag.roots), col="red", lwd=2, lty=2)
# and factored-polynomial
plot(	polynomial(fliplr(aL)), xlim = c(0.5,.9), ylim = c(-0.01, 0.01), col="dodgerblue", lwd=2,
			xlab = "(b) Characteristic roots", ylab = "")
abline(h = 0); abline(v = Re(fact.roots), col="red", lwd=2, lty=2)

# plot theoretical ACF/PACF -----
plot_acf0(aL,1,50)