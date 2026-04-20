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
pacman::p_load(	tictoc,matlab,zoo,sandwich,furrr,parallel)
# CHECKS if R_utility_functions.R at D:/matlab.tools/db.toolbox exist, if not, loads online GoogleDrive version
# must use file before the source command to avoid Positron issues not opening files
utility.functions = "file:///D:/matlab.tools/db.toolbox/R_utility_functions.R"
if (file.exists(utility.functions)) { source(utility.functions) } else
{source(url("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc")) }
# SET WORKING DIRECTORY if needed
# setwd('D:/_teaching/_current.teaching/_SU.TSEF/code-TSEF')

# %%  SCRIPT STARTS HERE
phi = .0;			# autocorrelation coefficient for x and y
T 	= 5e2;		# sample size T
Nsim = 1e4;		# number of simulations
# set.seed(1234)
plan(multisession, workers = detectCores() - 1)
tic()
ols_sim <- future_map(1:Nsim, function(ii) {
  x = filter(rnorm(T), phi, method = "r")
  y = filter(rnorm(T), phi, method = "r")
  # x = rnorm(T); y = rnorm(T); 
  lm.out  = lm(y ~ x)
  rout = list(
    beta.hat = lm.out$coefficients, 
    tstat    = lm.out$coefficients / sqrt( diag( vcovHC(lm.out, type = "const")) )
  )
}, .options = furrr_options(seed = TRUE) )

plan(sequential)
cat("\n")
toc()

# Unpack ols_sim list into separate vectors
beta.hat  <- sapply(ols_sim, `[[`, "beta.hat"); 
tstat  		<- sapply(ols_sim, `[[`, "tstat"); 

# PRINT SINGLE CORE VERSION
# cat( paste0('t-stat.		: ', tstat[2], '\n'))
# cat( paste0('beta.hat.	: ', beta.hat[2]) )

# %% plot the distribution of the OLS estimates
xg = seq(-6,6, length.out = 1e3)
hist(	tstat[2,], breaks = 1e2, freq = FALSE, col = "lightblue", xlim = 6*c(-1, 1), ylim = c(0, 0.5))
lines(xg, dnorm(xg, mean=0,sd=1), col = "red", lwd = 2)

# mean(beta.hat)	# mean of the OLS estimates
# std(beta.hat)		# standard deviation of the OLS estimates
alpha = 0.025
quantile(tstat[2,], probs = c(alpha, 1-alpha))	# quantiles of the t-statistics))
