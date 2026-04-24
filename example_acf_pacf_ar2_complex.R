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
# CHECKS if R_utility_functions.R at D:/matlab.tools/db.toolbox exist, if not, loads online GoogleDrive version
# must use file before the source command to avoid Positron issues not opening files
utility.functions = "file:///D:/matlab.tools/db.toolbox/R_utility_functions.R"
if (file.exists(utility.functions)) { source(utility.functions) } else
{source(url("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc")) }
# SET WORKING DIRECTORY if needed
# setwd('D:/_teaching/_current.teaching/_SU.TSEF/code-TSEF')
set.seed(1234)

# %% AR Lag polynomial 
a1 = 1.4 ; a2 = -0.85
aL = c(1, -a1, -a2)
# companion matrix Phi
Phi = matrix( c(a1,a2,1,0), nrow = 2,ncol = 2)

# lag-polynomial and roots
cat("Lag Polynomial: ", gsub("x","L",as.polynomial(aL)), "\n")
lag.roots = round(polyroot(aL),4)
if (sum(Im(lag.roots)==0)) {
  cat("Lag Polynomial roots are:", Re(lag.roots), "\n") } else {
	cat("Lag Polynomial roots are:", lag.roots, "\n" ) 
}
cat("Modulus of Lag Polynomial roots is:", round(Mod(lag.roots)[1], digits = 4), "\n\n")

# factored-polynomial and roots 
cat("Factored Polynomial: ", gsub("x","\u03BB",as.polynomial(fliplr(aL))), '\n')
fact.roots = round(polyroot(fliplr(aL)),4)
if (sum(Im(fact.roots)==0)) {
  cat("Factored Polynomial roots are:", Re(fact.roots), "\n") } else {
  cat("Factored Polynomial roots are:", fact.roots, "\n" ) 
}
cat("Modulus of Factored Polynomial roots is:", round(Mod(fact.roots)[1], digits = 4), "\n\n")

# companion matrix Phi and its roots/eigenvalues
char.roots = eigen(Phi)[1] # returns a list
cat("Characteristics roots (eigenvalues of Phi) are:", unlist(char.roots), "\n")

# plot theoretical ACF/PACF 
plot_acf0(aL,1,50)

# plot of simulated series of sample size 200 
set.seed(123);
B = 2e2   # B = is the burn in period
T = 2e2   # sample size
c = 0.3   # constant
u = rnorm(B+T)
x = zeros(B+T,1)

for (t in 3:(B+T)){
  x[t] = c + a1*x[t-1] + a2*x[t-2] + u[t]
}
# using inbuild function: arima.sim simulates a zero mean ARIMA model -> need to add mean Mu = c/a(1) to it.
# x = c/sum(aL) + arima.sim(n = T+B, list(ar = -aL[-1], ma = 0) );
x = x[-(1:B)]; # remove the burn-in period
par(mfrow=c(1,1))
plot(x, type='l', col="dodgerblue", lwd = 2)
abline(h=c(0), lty=c(1), col=c(1))












