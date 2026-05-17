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
pacman::p_load(forecast,matlab,tidyverse,readxl,arrow,curl,mFilter,furrr,parallel) 
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

# A. Computer Exercises 1) 
N = 1e5
# Increase to 1 Gigabyte (1,073,741,824 bytes)   # # Increase to 5 Gigabytes  # options(future.globals.maxSize = 5 * 1024^3)
# options(future.globals.maxSize = 1 * 1024^3)
set.seed(0) # set seed for reproducibility

# Define sample size for the random walk process, T, as 80 + 1 to account for the lag in the regression
T  = 250
Ts = 1 + T
# generate U~(T+1)*(N) matrix of standard normal random numbers
Ut = matrix(rnorm(Ts*N,0,1),nrow = Ts,ncol = N)
# make pure RW process Y = cumsum(U) = Yₜ = ∑uⱼ
Yt = apply(Ut, 2, cumsum)
# make time trend variable
time.trend = 1:(Ts)

# make storage space for coefficient estimates as well as tstats
rho0   = zeros(N,1); tstat0  = zeros(N,1); 
rhoC   = zeros(N,1); tstatC  = zeros(N,1); 
rhoCT  = zeros(N,1); tstatCT = zeros(N,1);

# instead progress bar with parallel processing using furrr package
pb <- txtProgressBar(min = 1, max = N, style = 3)

# %% set up parallel processing with furrr package, using all available cores minus one 
plan(multisession, workers = detectCores() - 1)

tic() # start timer and main loop
results <- future_map(1:N, function(ii) {
  setTxtProgressBar(pb, ii)
  yt   = Yt[, ii]
  yt_1 = lag(yt)
  # run the regressions and store rho_hat as well as tstat = (rho-1)/se(rho)
  df0  = lm(yt ~ 0 + yt_1)   							# no components, force intercept to zero with ~ 0 
  dfC  = lm(yt ~ 1 + yt_1)       					# default is to include an intercept, so we can omit the 1 here and just write lm(yt ~ yt_1)
  dfCT = lm(yt ~ 1 + yt_1 + time.trend)  	# but for readability, we can include the 1 here to make it clear that we are including an intercept
  list(
    rho0    = df0$coefficients[1],
    rhoC    = dfC$coefficients[2],
    rhoCT   = dfCT$coefficients[2],
    tstat0  = (df0$coefficients[1]  - 1) / sqrt(diag(vcov(df0)))[1],
    tstatC  = (dfC$coefficients[2]  - 1) / sqrt(diag(vcov(dfC)))[2],
    tstatCT = (dfCT$coefficients[2] - 1) / sqrt(diag(vcov(dfCT)))[2]
  )
}, .options = furrr_options(seed = TRUE))
# loop sequence
plan(sequential)
cat("\n")
toc()

# Unpack results list into separate vectors
tstat0  <- sapply(results, `[[`, "tstat0");  rho0  <- sapply(results, `[[`, "rho0"); 
tstatC  <- sapply(results, `[[`, "tstatC");  rhoC  <- sapply(results, `[[`, "rhoC"); 
tstatCT <- sapply(results, `[[`, "tstatCT"); rhoCT <- sapply(results, `[[`, "rhoCT"); 

# %% DENSITY PLOTS OF THE T(ρ-1) 
# COMPUTE PERCENTILS OF CRITICAL VALUES
# which percentiles for critical values
ug = seq(-35,5,length=1e3); # KS0 <- density(rho0)

fnt = 1.8 # font size
op = par( mfrow = c(1, 1), family = "serif", las = 1,
mgp = c(  2,.5, 0),      # axis title, labels, line, position of y,x labels to axis
          mar = c(  4, 4, 1, 2),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(  1, 1, 1, 1)  ) # outer margins for overall spacing
          
density_rho0 <- density(T*(rho0-1), from = -35, to = 5, n = 1e3)
density_rhoC <- density(T*(rhoC-1), from = -35, to = 5, n = 1e3)
density_rhoCT <- density(T*(rhoCT-1), from = -35, to = 5, n = 1e3)

plot( ug, density_rho0$y, type = "l", lwd = 2, las = 1, ylab="", xlab="Distribution of the t-statistics",
      xlim=c(-35, 5), 
      ylim=c(0, .35), 
      col='dodgerblue3', 
      cex.axis=1.3,
      pch  = 16,
      # Font sizes
      cex.lab  = fnt ,
      # Remove default y,x-axis labels
      xaxt = "n", yaxt = "n",
      )
# Add other densities
lines(ug, density_rhoC$y, 	col = 'brown2', lwd = 2)
lines(ug, density_rhoCT$y, 	col = 'orange', lwd = 2)
# Add a legend
legend( "topleft", 
        legend = c(	"No-constant and no trend", 
										"With constant", 
                    "With constant and trend" 
                    ), 
        col = c("dodgerblue3", "brown2", "orange" ),
        lwd = 3.5,
        lty = 1,
        cex = fnt - .1 ,
        bty = "n",
        # text.font = 1,
        x.intersp = .4,
        y.intersp = .6,
        seg.len = 0.7,
        inset = 0.00,      # Distance from plot border
)  

# axis labels
y.grid = seq( 0,.35,.05)                # x-axis labels
x.grid = seq(-35, 5, 1)                 # y-axis labels

# source(utility.functions)
# add cross axis ticks
cross_ticks( side = 1, at = x.grid, tcl = .2, cex = fnt )
cross_ticks( side = 2, at = y.grid, tcl = .2, cex = fnt )
cross_ticks( side = 4, at = y.grid, tcl = .2, cex = fnt )
# add grid [solid, dashed, dotted, dotdash, longdash, twodash]
abline(lty = "dotdash", lwd = .5, h = y.grid, v = x.grid, col="lightgray")
# Add horizontal line at y=0

# %% DENSITY PLOTS OF THE T-STATISTICS 
# COMPUTE PERCENTILS OF CRITICAL VALUES
# which percentiles for critical values
source(utility.functions)

pctls  = c(14, 86)/100
DF = rbind( qnorm(pctls), 
            quantile(tstat0,  pctls),
            quantile(tstatC,  pctls),
            quantile(tstatCT, pctls)
)
rownames(DF) = c("N(0,1)", "No constant, no trend", "With constant", "With constant and trend")
cat(strrep("---", 30), "\n")
print(" Simulations completed. Critical values for the Dickey-Fuller test:")
cat(strrep("---", 30), "\n")
print(round(DF,digits = 6))

ug = seq(-4, 4, length=1e3); # KS0 <- density(rho0)

fnt = 1.8 # font size
op = par( mfrow = c(1, 1), family = "serif", las = 1,
          mgp = c(  2,.5, 0),      # axis title, labels, line, position of y,x labels to axis
          mar = c(  4, 4, 1, 2),   # bottom, left, top, right per plot, position of the subplots inside the total plot area
          oma = c(  1, 1, 1, 1)  ) # outer margins for overall spacing

plot( ug, dnorm(ug), type = "l", lwd = 2, las = 1, ylab="", xlab="Distribution of the t-statistics",
      xlim=c(-6, 6), 
      ylim=c(0, .6), 
      col='black', 
      cex.axis=1.3,
      pch  = 16,
      # Font sizes
      cex.lab  = fnt ,
      # Remove default y,x-axis labels
      xaxt = "n", yaxt = "n",
      )
lines( density(tstat0),  col='dodgerblue3', lwd=2 )
lines( density(tstatC),  col='brown2'     , lwd=2 )
lines( density(tstatCT), col='orange'     , lwd=2 )
# abline(h=0)
# Add a legend
legend( "topleft", 
        legend = c(	"No-constant and no trend", 
                    "With constant", 
                    "With constant and trend", 
                    "N(0,1)"), 
        col = c("dodgerblue3", "brown2", "orange", "black" ),
        lwd = 3.5,
        lty = 1,
        cex = fnt - .1 ,
        bty = "n",
        # text.font = 1,
        x.intersp = .4,
        y.intersp = .6,
        seg.len = 0.7,
        inset = 0.00,      # Distance from plot border
)  

# axis labels
y.grid = seq( 0,.6,.1)                 # x-axis labels
x.grid = seq(-6, 5, 1)                 # y-axis labels

# source(utility.functions)
# add cross axis ticks
cross_ticks( side = 1, at = x.grid, tcl = .2, cex = fnt )
cross_ticks( side = 2, at = y.grid, tcl = .2, cex = fnt )
cross_ticks( side = 4, at = y.grid, tcl = .2, cex = fnt )
# add grid [solid, dashed, dotted, dotdash, longdash, twodash]
abline(lty = "dotdash", lwd = .5, h = y.grid, v = x.grid, col="lightgray")
# Add horizontal line at y=0
abline(h=0) 
abline(h=0) 