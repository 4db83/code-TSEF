# R utility or helper add on functions.
# Add these to the top of an R script
######################################################################################

#----------------------------------------------------------------------------
# Download data from FRED. It returns quarterly data.  User must provide the
# FRED url.
#----------------------------------------------------------------------------
getFREDdata <- function(url, destinationfile = 'tmp.txt', freq = "Quarterly", convert2tif = TRUE) {
  # Download the data from FRED
  download.file(url, destfile = destinationfile)
  FREDraw <- readLines(destinationfile)
  # txt.file.name <- paste0("rawData/",substr(url, regexpr('[a-zA-z0-9]*.txt',url),1000))
  # if (!file.exists(txt.file.name)){
  #   # Download the data from FRED
  #   #download.file(url, destfile = 'FREDtemp.txt', method = "wget")
  #   system(paste0('wget --no-check-certificate "', url, '"'))
  #   system(paste('mv',substr(url, regexpr('[a-zA-z0-9]*.txt',url),1000),txt.file.name))
  # }
  # FREDraw <- readLines(txt.file.name) 
  # Frequency
  freq.FRED <- gsub(' ', '',substr(FREDraw[which(regexpr('Frequency', FREDraw)==1)],
                                   (nchar('Frequency')+2),100))    
  # Where does the data start
  datastart = which(gsub(' ', '',FREDraw)=='DATEVALUE') - 2
  #data <- read.table('FREDtemp.txt', skip = datastart, header = TRUE)
  data <- read.table(destinationfile, skip = datastart, header = TRUE)
  data[,1] = as.Date(data[,1])
  # browser()
  first.year  <- as.numeric(format(as.Date(data$DATE[1]),'%Y'))
  first.month <- as.numeric(format(as.Date(data$DATE[1]),'%m'))
  # this here calls the tis and tif libraries which is causing the problem with data from before 1700
  
  # remove the tmp file if not stored
  if (destinationfile == 'tmp.txt') { file.remove('tmp.txt')}

  if (convert2tif){
    # Adjust frequency`
    if (freq.FRED == 'Quarterly'){
      first.q  <- (first.month-1)/3 + 1
      data.tis <- tis(data$VALUE, start = c(first.year, first.q), tif = 'quarterly')
    } else if (freq.FRED == 'Monthly') {
      data.tis <- tis(data$VALUE, start = c(first.year, first.month), tif = 'monthly')
    } else if (freq.FRED == 'Annual') {
      data.tis <- tis(data$VALUE, start = c(first.year, first.month), tif = 'annual')
    }
    # Convert frequency Monthly to Quarterly
    if (freq.FRED == 'Monthly' & freq == 'Quarterly') {
      data.tis <- convert(data.tis, tif = 'quarterly', method = 'constant', observed. = 'averaged')
    }
    # Convert Annual to Quarterly
    if (freq.FRED == 'Annual' & freq == 'Quarterly') {
      data.tis <- convert(data.tis, tif = 'quarterly', method = 'linear', observed. = 'averaged')
    }
    return(data.tis)
  }
  else return(data)
} 

#----------------------------------------------------------------------------
# Download data from FRED. It returns quarterly data.   # User must provide the FRED url.
#----------------------------------------------------------------------------
readFREDrawData <- function(destinationfile, freq = "Quarterly" , convert2tif = TRUE) {
  # Download the data from FRED
  txt.file.name = destinationfile
  FREDraw <- readLines(txt.file.name) 
  # Frequency
  freq.FRED <- gsub(' ', '',substr(FREDraw[which(regexpr('Frequency', FREDraw)==1)],
                                   (nchar('Frequency')+2),100))    
  # Where does the data start
  datastart = which(gsub(' ', '',FREDraw)=='DATEVALUE') - 2
  #data <- read.table('FREDtemp.txt', skip = datastart, header = TRUE)
  data <- read.table(destinationfile, skip = datastart, header = TRUE)
  first.year  <- as.numeric(format(as.Date(data$DATE[1]),'%Y'))
  first.month <- as.numeric(format(as.Date(data$DATE[1]),'%m'))
  # browser()
#  # Adjust frequency
#  if (freq.FRED == 'Quarterly'){
#    first.q  <- (first.month-1)/3 + 1
#    data.tis <- tis(data$VALUE, start = c(first.year, first.q), tif = 'quarterly')
#  } else if (freq.FRED == 'Monthly') {
#    data.tis <- tis(data$VALUE, start = c(first.year, first.month), tif = 'monthly')
#  }
#  
#  # Convert frequency
#  if (freq.FRED == 'Monthly' & freq == 'Quarterly') {
#    data.tis <- convert(data.tis, tif = 'quarterly', method = 'constant', observed. = 'averaged')
#  }
#  
#  return(data.tis)
#}
 if (convert2tif){
    # Adjust frequency
    if (freq.FRED == 'Quarterly'){
      first.q  <- (first.month-1)/3 + 1
      data.tis <- tis(data$VALUE, start = c(first.year, first.q), tif = 'quarterly')
    } else if (freq.FRED == 'Monthly') {
      data.tis <- tis(data$VALUE, start = c(first.year, first.month), tif = 'monthly')
    } else if (freq.FRED == 'Annual') {
      data.tis <- tis(data$VALUE, start = c(first.year, first.month), tif = 'annual')
    }
    # Convert frequency Monthly to Quarterly
    if (freq.FRED == 'Monthly' & freq == 'Quarterly') {
      data.tis <- convert(data.tis, tif = 'quarterly', method = 'constant', observed. = 'averaged')
    }
    # Convert Annual to Quarterly
    if (freq.FRED == 'Annual' & freq == 'Quarterly') {
      data.tis <- convert(data.tis, tif = 'quarterly', method = 'linear', observed. = 'averaged')
    }
    return(data.tis)
    }
  else return(data)
} 

#----------------------------------------------------------------------------
# Shift quarter 
#----------------------------------------------------------------------------
shiftQuarter <- function(original.start,shift){
#################################################################
# This function takes in a (year,quarter) date in time series format
# and a shift number, and returns the (year,quarter) date corresponding
# to the shift. Positive values of shift produce leads and negative values
# of shift produce lags.
# For example, entering 2014q1 with a shift of -1 would return 2013q4.
# Entering 2014q1 with a shift of 1 would return 2014q2.
# In each case, the first argument of the function must be entered as
# a two-element vector, where the first element corresponds to the year
# and the second element corresponds to the quarter.
# For example, Q12014 must be entered as "c(2014,1)".
################################################################    
# Leads (positive values of shift)
    if (shift > 0) {
        new.start = c(0,0)
        sum = original.start[2] + shift
        # Get the year value
        if (sum <= 4) {
            new.start[1] = original.start[1]
        }
        else {
            new.start[1] = original.start[1] + ceiling(sum/4) - 1
        }
        # Get the quarter value
        if (sum %% 4 > 0) {
            new.start[2] = sum %% 4
        }
        else {
            new.start[2] = sum %% 4 + 4
        }
    }
# Lags (negative values of shift)
    else {
        new.start = c(0,0)
        diff = original.start[2] - abs(shift)
        # Get the year value
        if (diff > 0) {
            new.start[1] = original.start[1]
        }
        else {
            new.start[1] = original.start[1] - (1 + floor(abs(diff)/4))
        }
        # Get the quarter value
        if (diff %% 4 > 0) {
            new.start[2] = diff %% 4
        }
        else {
            new.start[2] = diff %% 4 + 4
        }
    }
return(new.start)}

#----------------------------------------------------------------------------
# Shift Month 
#----------------------------------------------------------------------------
shiftMonth <- function(original.start,shift){
#################################################################
# This function takes in a (year,month) date in time series format
# and a shift number, and returns the (year,month) date corresponding
# to the shift. Positive values of shift produce leads and negative values
# of shift produce lags.
# For example, entering 2014m1 with a shift of -1 would return 2013m12.
# Entering 2014m1 with a shift of 1 would return 2014m2.
# In each case, the first argument of the function must be entered as
# a two-element vector, where the first element corresponds to the year
# and the second element corresponds to the month.
# This function is analogous to shiftQuarter().
################################################################    
# Leads (positive values of shift)
    if (shift > 0) {
        new.start = c(0,0)
        sum = original.start[2] + shift
        # Get the year value
        if (sum <= 12) {
            new.start[1] = original.start[1]
        }
        else {
            new.start[1] = original.start[1] + ceiling(sum/12) - 1
        }
        # Get the month value
        if (sum %% 12 > 0) {
            new.start[2] = sum %% 12
        }
        else {
            new.start[2] = sum %% 12 + 12
        }
    }
# Lags (negative values of shift)
    else {
        new.start = c(0,0)
        diff = original.start[2] - abs(shift)
        # Get the year value
        if (diff > 0) {
            new.start[1] = original.start[1]
        }
        else {
            new.start[1] = original.start[1] - (1 + floor(abs(diff)/12))
        }
        # Get the month value
        if (diff %% 12 > 0) {
            new.start[2] = diff %% 12
        }
        else {
            new.start[2] = diff %% 12 + 12
        }
    }
return(new.start)}

    
#----------------------------------------------------------------------------
# getFRED requires wget utility
#----------------------------------------------------------------------------
# getFRED <- function(url, freq = "Quarterly") {
# ##########################################################################################
# # This function downloads data from FRED. It returns quarterly data.
# # User must provide the FRED url.
# ########################################################################################### 
#     # Download the data from FRED
#     #download.file(url, destfile = 'FREDtemp.txt', method = "wget")
#     #FREDraw <- readLines('FREDtemp.txt')
#     txt.file.name <- paste0("rawData/",substr(url, regexpr('[a-zA-z0-9]*.txt',url),1000))
#     if (!file.exists(txt.file.name)){
#         # Download the data from FRED
#         #download.file(url, destfile = 'FREDtemp.txt', method = "wget")
#         system(paste0('wget --no-check-certificate "', url, '"'))
#         system(paste('mv',substr(url, regexpr('[a-zA-z0-9]*.txt',url),1000),txt.file.name))
#     }
#     FREDraw <- readLines(txt.file.name) 
#     # Frequency
#     freq.FRED <- gsub(' ', '',substr(FREDraw[which(regexpr('Frequency', FREDraw)==1)],
#                                      (nchar('Frequency')+2),100))    
#     # Where does the data start
#     datastart = which(gsub(' ', '',FREDraw)=='DATEVALUE') - 2
#     #data <- read.table('FREDtemp.txt', skip = datastart, header = TRUE)
#     data <- read.table(txt.file.name, skip = datastart, header = TRUE)
#     first.year  <- as.numeric(format(as.Date(data$DATE[1]),'%Y'))
#     first.month <- as.numeric(format(as.Date(data$DATE[1]),'%m'))
#     # Adjust frequency
#     if (freq.FRED == 'Quarterly'){
#         first.q  <- (first.month-1)/3 + 1
#         data.tis <- tis(data$VALUE, start = c(first.year, first.q), tif = 'quarterly')
#     } else if (freq.FRED == 'Monthly') {
#         data.tis <- tis(data$VALUE, start = c(first.year, first.month), tif = 'monthly')
#     }
#     # Convert frequency
#     if (freq.FRED == 'Monthly' & freq == 'Quarterly') {
#         data.tis <- convert(data.tis, tif = 'quarterly', method = 'constant', observed. = 'averaged')
#     }
#     return(data.tis)
# } 

#----------------------------------------------------------------------------
# splice data together
#----------------------------------------------------------------------------
splice <- function(s1, s2, splice.date, freq) {
##########################################################################################
# This function splices two series, with the series s2 beginning at splice.date
# and extended back using the growth rate at the splice.date times series s1
# The freq argument accepts two values - 'quarterly' and 'monthly' -
# but it could be modified to take more.
##########################################################################################    
    t <- splice.date #renaming for convenience
    if (freq == "quarterly" | freq == "Quarterly") {
        t.minus.1 <- shiftQuarter(t,-1)
    }
    else if (freq == "monthly" | freq == "Monthly") {
        t.minus.1 <- shiftMonth(t,-1)
    }
    else { stop("You must enter 'quarterly' or 'monthly' for freq.") }
    ratio <- as.numeric(window(s2,start = t, end = t)/
                        window(s1,start = t, end = t))
    return(mergeSeries(ratio*window(s1,end = t.minus.1),window(s2, start = t)))
}

#----------------------------------------------------------------------------
# gradient unused
#----------------------------------------------------------------------------
# gradient <- function(f, x, delta = x * 0 + 1.0e-5) {
# ##########################################################################################
# # This function computes the gradient of a function f given a vector input x.
# ##########################################################################################   
#     g <- x * 0
#     for (i in 1:length(x)) {
#         x1 <- x
#         x1[i] <- x1[i] + delta[i]
#         f1 <- f(x1)
#         x2 <- x
#         x2[i] <- x2[i] - delta[i]
#         f2 <- f(x2)
#         g[i] <- (f1 - f2) / delta[i] / 2
#     }
#     return(g)
# }

#----------------------------------------------------------------------------
# head2tail
#----------------------------------------------------------------------------
head2tail <- function(data.in, nL = 30){
    print(head(data.in, n = nL) ) ;
    print( tail(data.in, n = nL) )
}

#----------------------------------------------------------------------------
# ARMA to AR(infinity)
#----------------------------------------------------------------------------
arma2ar = function(ar.L,ma.L ,max.lag){
	#if (is.null(max.lag)) max.lag=10
	ar.inf = ARMAtoMA(ma=ar.L[-1],ar=-ma.L[-1],max.lag)
	ar.inf.out = rbind(1,as.matrix(ar.inf))
	return(ar.inf.out) 
}

#----------------------------------------------------------------------------
# ARMA to MA(infinity)
#----------------------------------------------------------------------------
arma2ma = function(ar.L,ma.L ,max.lag){
	#if (is.null(max.lag)) max.lag=10
	ma.inf = ARMAtoMA(ar=-ar.L[-1],ma=ma.L[-1],max.lag)
	ma.inf.out = rbind(1,as.matrix(ma.inf))
	return(ma.inf.out) 
}

#----------------------------------------------------------------------------
# plot theoretical ACF/PACF
#----------------------------------------------------------------------------
plot.acf0 = function(ARpolynomial, MApolynomial, max.lag = 50){
	# if (is.null(max.lag)) max.lag = 51
	ARterms = -ARpolynomial[-1]
	MAterms =  MApolynomial[-1]
	# add one to it to get the required lag structure because of the zapsmall function
	max.lag = max.lag+1
	ACF 	= ARMAacf(ARterms,MAterms,(max.lag),pacf=FALSE)[2:max.lag]
	PACF 	= zapsmall(ARMAacf(ARterms,MAterms,max.lag,pacf=TRUE))[1:(max.lag-1)]
	LAG 	= 1:(max.lag-1)
	minA 	= min(ACF)
	minP	= min(PACF)
	LW 		= 6.6
	colr 	= "skyblue2"
	minu 	= min(minA,minP)-.01
	old.par <- par(no.readonly = TRUE)
	par(mfrow=c(1,2), mar = c(3,3,2,0.8),oma = c(1,1.2,1,1), mgp = c(1.7,0.5,0))
	plot(LAG, ACF, type="h",ylim=c(minu,1) ,lwd=LW, las=1, lend = 1, col=colr, xlim=c(1, max.lag),
	     xlab="Lag")
	#main=paste("Series: ",deparse(substitute(series))))
	abline(h=c(0), lty=c(1), col=c(1))
	plot(LAG, PACF, type="h",ylim=c(minu,1) ,lwd=LW, las=1, lend = 1, col=colr, xlim=c(1, max.lag),
	     xlab="Lag")
	abline(h=c(0), lty=c(1), col=c(1))
	on.exit(par(old.par))
	ACF<-round(ACF,2); PACF<-round(PACF,2)    
	return(invisible(cbind(ACF, PACF))) 
}

#----------------------------------------------------------------------------
# plot sample ACF/PACF
#----------------------------------------------------------------------------
plot.acf=function(data_in, max.lag = 50){
  # remove nans
  ytmp  = unlist(as.vector(data_in), use.names = FALSE)
  ytmp[is.infinite(ytmp)] = NA
  yy    = na.omit(ytmp)
	Nsize = length(yy)
  # browser()
	if (max.lag > (Nsize-1)) stop("Number of lags exceeds number of observations")
	ACF 	= acf(yy, max.lag, plot=FALSE)$acf[-1]
	PACF 	= pacf(yy, max.lag, plot=FALSE)$acf
	#LAG=1:max.lag/frequency(data_in)
	LAG 	= 1:max.lag
	minA 	= min(ACF)
	minP 	= min(PACF)
	U 		= 2/sqrt(Nsize)
	L 		=-U
	LW 		= 6.6
	colr 	= "skyblue2"
	minu 	= min(minA,minP,L)-.01
	old.par <- par(no.readonly = TRUE)
	par(mfrow=c(1,2), mar = c(3,3,2,0.8),oma = c(1,1.2,1,1), mgp = c(1.7,0.5,0))
	plot(LAG, ACF, type="h",ylim=c(minu,1) ,lwd=LW, las=1, lend = 1, col=colr, xlim=c(1, max.lag))
	  #main=paste("Series: ",deparse(substitute(series))))
	  abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,2,2))
	plot(LAG, PACF, type="h",ylim=c(minu,1) ,lwd=LW, lend = 1, col=colr, xlim=c(1, max.lag))
	  abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,2,2))
	on.exit(par(old.par))  
	ACF<-round(ACF,2); PACF<-round(PACF,2)    
	return(invisible(cbind(ACF, PACF))) 
}

#----------------------------------------------------------------------------
# print TSIBBLE with formatting and rounding of numbers
#----------------------------------------------------------------------------
print.ts = function(ts_data,digits = 4, n = 50){
  aout = mutate(ts_data, across(where(is.numeric), ~ num(.x, digits = digits)))
  print(aout, n = n)
}

#----------------------------------------------------------------------------
# A simple lag function for matrices
#-----------------------------------------------------------------------------
lag.matrix <- function(m, nLags) {
  nargin <- length(as.list(match.call())) - 1      
  if (nargin != 2) {        
     stop('Check function inputs')    
  }
  lagM <- c()
  for(i in seq(nLags)) {
    for(j in seq(ncol(m))) {
      tmp <- c(rep(NA, i), trimr(m[,j], 0, i))      
      lagM <- cbind(lagM, tmp)      
    }    
  }  
  return(lagM)
}

#----------------------------------------------------------------------------
# rounding of data frame function for nice looking table output with columns of
# characters
#----------------------------------------------------------------------------
round.df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  return(x)
}

#----------------------------------------------------------------------------
# Print the OLS output/results in Nice format such as eviews
#----------------------------------------------------------------------------
print.results = function(lm.Object, HAC.type = 0, digits = 8, Prewhite = FALSE, mLag = ceiling(0.75*N^(1/3)), Hide = 0){
  # HAC.type =-2 -->  homoskedastic OLS not recommended
  # HAC.type =-1 -->  White (1982) no DF adjustment 
  # HAC.type = 0 -->  White (1982) with DF adjutment, MacKinnon and White (1985).
  # HAC.type = 1 -->  Newey-West (1994) m-lag-selection, no prewhitening, 
  #                   if mLag is supplied, the supplied values is used, otherwise mLag = ceiling(0.75*N^(1/3)
  #                   if Prewhite = FALSE, no prewhitening is performed. 
  #---------------------------------------------------------------------------------------------------------------------
  
  # check if HAC package is installed. 
  if (!"sandwich" %in% installed.packages()){
    cat("Install sandwich R package")
    require("sandwich")
  }
  library("sandwich")

  summary_ols = summary(lm.Object)
  # get the number of nans or missing values that were removed 
  N.nas   = summary_ols$na.action
  uhat    = as.matrix(lm.Object$residuals)
  bhat    = as.matrix(lm.Object$coefficients)
  yhat    = as.matrix(lm.Object$fitted.values)
  y       = as.matrix(yhat + uhat)
  N       = length(y)
  std_y   = sqrt(var(y))
  mean_y  = mean(y)
  SSE     = sum(uhat^2)
  MSE     = SSE/N
  # browser() 
  LL      = -N/2*( log(2*pi) + log(SSE/N) + 1 );
  # check if intercept included in model
  I = as.numeric("(Intercept)" %in% names(lm.Object$coefficients)) 
  K = length(lm.Object$coefficients) # includes intercept term if it exists
  # information criteria
  SSY     = sum( (y-mean_y)^2 )
  R2      = 1 - SSE/SSY
  Rbar2   = 1 - (N-1)/(N-K)*(1-R2)
  IC_AIC  = (-2*LL + 2*K)/N    
  IC_AICc = (N*IC_AIC+ 2*K*(K+1)/(N-K-1))/N
  IC_BIC  = (-2*LL + K*log(N)) /N
  IC_HQ   = (-2*LL + 2*K*log(log(N)))/N;
  u.1     = uhat[2:N] - uhat[1:N-1]
  DW      = sum(u.1^2)/SSE;
  varNames= variable.names(lm.Object)
  X = as.matrix(lm.Object$model[-1])
  if (I == 1) {
    varNames[1] = "Constant"
    X = cbind(1,as.matrix(lm.Object$model[-1]))
  }
  # F-statistic, DFs adn pvalues
  Fstat = summary_ols$fstatistic
  Fstat_pval = pf(Fstat[1],Fstat[2],Fstat[3],lower.tail = FALSE)
  # standard error of regression
  sigma_u     = summary_ols$sigma
  sigma_u_MLE = sqrt(SSE/N)
  # COMPUTE THE VARIANCE COVARIANC MATRIX OF THE POINT ESTIMATES
  # plain vanilla, Homskedasticity OLSsame as inv(X'X)*Var(U)
  vcv0 = vcov(lm.Object) 
  # White (1982)
  vcv1 = vcovHC(lm.Object, type = "HC0") 
  # White (1982) with DF adjustment, MacKinnon and White (1985). This is what EViews Reports
  vcv2 = vcovHC(lm.Object, type = "HC1") # set to default if not Time Series/Dynamic data. 
  # Newey-West 
  # mNW    = ceiling(0.75*N^(1/3))
  # vcvNW0 = NeweyWest(lm.Object, lag = mNW, pre-white = FALSE)
  # print(sqrt(diag(vcvNW0)))
  vcvNW = NeweyWest(lm.Object, lag = mLag, prewhite = Prewhite)
  # choose which VCV to print out
  if (HAC.type ==-2) VCV = vcv0  # HAC.type =-2 --> homoskedastic OLS
  if (HAC.type ==-1) VCV = vcv1  # HAC.type =-1 --> # White (1982) no DF adjustment 
  if (HAC.type == 0) VCV = vcv2  # HAC.type = 0 --> White (1982) with DF adjustment, MacKinnon and White (1985).
  if (HAC.type == 1) VCV = vcvNW # HAC.type = 1 --> Newey-West (1994) m-lag-selection, no pre-whitening, if 
  HAC_lag = mLag
  if (HAC.type <= 0) HAC_lag = 0
  pre_white_I = as.numeric(Prewhite)
  stderr = sqrt(diag(VCV))
  tstat  = bhat/stderr
  pvalue = 2*pnorm(abs(tstat), lower.tail = FALSE) # it is a two-sided pvalue
  
  # PRINT THE RESULTS TO SCREEN NOW
  wdth = 80
  if (!Hide){
  # cat(strrep("-", wdth)); cat("\n")
  cat(strrep("-", wdth)); cat("\n")
  cat( "  Dependent Variables:",  all.vars(lm.Object$call)[1], "\n" )
  cat( "  Sample Size =", N, "|",  "Observations deleted due to NaN =" , max(N.nas), "\n") }
  # cat( "  Standard Error Type: \n")
  # print(paste0(max(N.nas), " Observations deleted due to missingness: "))
  # make output table for main coefficients
  # baseoutput = matrix(rep(NA,K*9),K,9)
  baseoutput = matrix(rep(NA,(K+1)*9),(K+1),9)
  a0 = "   "
  a1 = "   "
  colnames(baseoutput) = c("Variable ",   a1,
                           "Estimate  ",  a0,
                           "Std.Error ",  a0,
                           "t-statistic", a0,
                           "p-value  ")
  # rownames(baseoutput) = c(varNames,":")
  # browser()
  baseoutput[1,c(1,5,9)]  = "-----------"
  baseoutput[1,c(3,7)]    = "------------"
  # baseoutput[1,c(7)]    = "-------------"
  bdf = as.data.frame(baseoutput)
  bdf[1,c(2,4,6,8)] = ""
  bdf[2:(K+1),1] = varNames
  bdf[2:(K+1),2] = a0
  bdf[2:(K+1),3] = formatC(bhat,   digits = digits, format = "f");   bdf[2:(K+1),4] = a0
  bdf[2:(K+1),5] = formatC(stderr, digits = digits, format = "f");   bdf[2:(K+1),6] = a0
  bdf[2:(K+1),7] = formatC(tstat,  digits = digits, format = "f");   bdf[2:(K+1),8] = a0
  bdf[2:(K+1),9] = formatC(pvalue, digits = digits, format = "f")
  
  # browser()
  # cat("\014"); 
  # print here some other info
  if (!Hide){
  cat(strrep("-", wdth)); cat("\n")
  print( round.df(bdf, digits = digits), row.names = FALSE)
  # print( round.df(bdf, digits = 6), col.names = FALSE)
  cat(strrep("-", wdth))}
  # browser()
  # storage for output for left side
  xtraout1 = matrix(rep(NA,10*6),10,6)
  # colnames(xtraout1) = c("","")
  rownames(xtraout1) = c(" R²                    ",  # 1   
                         " Adjusted R²           ",  # 2 
                         " SE of Regression      ",  # 3 
                         " Sum of Squared Errors ",  # 4 
                         " Log-Likelihood        ",  # 5 
                         " F-statistic           ",  # 6 
                         " Pr(F-statistic)       ",  # 7 
                         " No. of observations   ",  # 8 
                         " Std.Err.MLE (div by N)",  # 9 
                         " Include Pre-whitening ")  # 10 
  # make data.frame and now fill the respective columns
  xdf = as.data.frame(xtraout1)
  colnames(xdf) = character(6)
  # storage for output for left side
  ColI    = c("|")
  Col2 = c( R2,
            Rbar2,
            sigma_u,
            SSE,
            LL,
            Fstat[1],
            Fstat_pval,
            N,
            sigma_u_MLE,
            pre_white_I)
  Col3 = c( "No. of Regressors    ",  # 1           
            "Plus Const.(if exist)",  # 2           
            "Mean(y)              ",  # 3           
            "Std.Deviation(y)     ",  # 4
            "AIC                  ",  # 5
            "AICc                 ",  # 6
            "BIC                  ",  # 7
            "HQ-IC                ",  # 8
            "DW-statistic         ",  # 9
            "HAC Trunct.Lag       ")  # 10      
  Col4 = c(K - I,
           K,
           mean_y,
           std_y,
           IC_AIC,  
           IC_AICc, 
           IC_BIC,   
           IC_HQ,  
           DW,
           HAC_lag)
  xdf[,1] = c(": ")
  xdf[,2] = Col2
  xdf[,3] = ColI
  xdf[,4] = Col3
  xdf[,5] = c(": ")
  xdf[,6] = Col4
  if (!Hide){
  print(round.df(xdf, digits = digits))
  cat(strrep("-", wdth)); cat("\n")
  # stderr_type = 
  if (HAC.type ==-2)      { stderr_type = "Homoskedastic OLS inv(X'X)σ²" }
  else if (HAC.type ==-1) { stderr_type = "White (1982) no DF adjustment" }
  else if (HAC.type == 0) { stderr_type = "White (1982) with DF adjustment" }
  else if (HAC.type == 1) { stderr_type = "Newey-West (1994) m-lag-selection, no pre-whitening" }
  HAC_lag = mLag
  cat( "  Standard Error Type: ", stderr_type, "\n") 
  cat(strrep("-", wdth)); cat("\n") }
  # cat( "  Standard Error Type: \n")
  # Save variables to return
  ols_out <- list()
    ols_out$uhat        = uhat
    ols_out$bhat        = bhat
    ols_out$yhat        = yhat
    ols_out$y           = y
    ols_out$X           = X
    ols_out$K           = K
    ols_out$N           = N     
    ols_out$DF          = N-K
    ols_out$std_y       = std_y 
    ols_out$mean_y      = mean_y
    ols_out$SSE         = SSE   
    ols_out$LogLike     = LL    
    ols_out$MSE         = MSE     
    ols_out$SSY         = SSY     
    ols_out$R2          = R2      
    ols_out$Rbar2       = Rbar2   
    ols_out$IC_AIC      = IC_AIC  
    ols_out$IC_BIC      = IC_BIC  
    ols_out$IC_AICc     = IC_AICc 
    ols_out$IC_HQ       = IC_HQ   
    ols_out$DW_stat     = DW      
    ols_out$varNames    = varNames
    ols_out$No_missing  = N.nas
    ols_out$lm_Object   = lm.Object
  # return(ols_out) but do not print to screen by default when called, only when called as aout=print.results()
  invisible(ols_out)
}

