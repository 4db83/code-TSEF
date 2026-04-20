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
pacman::p_load( polynom,matlab,forecast )
# CHECKS if R_utility_functions.R at D:/matlab.tools/db.toolbox exist, if not, loads online GoogleDrive version
# must use file before the source command to avoid Positron issues not opening files
utility.functions = "file:///D:/matlab.tools/db.toolbox/R_utility_functions.R"
if (file.exists(utility.functions)) { source(utility.functions) } else
{source(url("https://drive.google.com/uc?export=download&id=1lCbHBcijii-Ff6c3_EJnJeUGkPtK8Mbc")) }
# SET WORKING DIRECTORY if needed
# setwd('D:/_teaching/_current.teaching/_SU.TSEF/code-TSEF')
set.seed(1234)

# %% Main code for overfitting example
Pmax  = 5;
Qmax  = 5;
T     = 3e1;
a0    = rep(0,Pmax);
b0    = rep(0,Qmax);
T0    = proc.time();
Nsim  = 10;

# space allocation;
arma.terms = matrix(0,Nsim,2)    
# set simulation seed
set.seed(1)
dx = matrix( rnorm(T*Nsim), T, Nsim )

# start Matlab style timer for entire simulation 
tic()
for (i in 1:Nsim){
    aout = auto.arima( dx[,i],
        d=0, D=0, max.Q=0, max.P=0, 
        max.p=Pmax, max.q=Qmax,
        start.p=a0, start.q=b0,
        seasonal=FALSE, trace=FALSE, stepwise=FALSE,
        ic=c("aic"), 
        max.order=20 )
    
    # print iterations to screen
    t.tmp = Nsim*(proc.time() - T0)
    if (i == 1){
    cat("---------------------------------------------------------------------\n")
    cat("    Total time is approximately: ", t.tmp[3], "Seconds\n")
    cat("---------------------------------------------------------------------\n")
    }
    # store the ARMA terms
    arma.terms[i,] = aout$arma[1:2]
    cat("Iteration number =", sprintf("%03d", i), "ARMA terms:", aout$arma[1:2], "\n")
}

#  tT = proc.time() - T0 ----
cat("Elapased time is:", (proc.time() - T0)[3], "\n" )
print(arma.terms)
N_AR_terms = sum(arma.terms[,1]>0)
N_MA_terms = sum(arma.terms[,2]>0)
# print to screen the number of wrongly identified ARMA terms
cat(paste0("True model is ARMA(0,0), yet Auto.arima finds \n", 
    N_AR_terms, "× non-zero AR terms detected, and \n",
    N_MA_terms, "× non-zero MA terms detected!!! \n") )
toc()
