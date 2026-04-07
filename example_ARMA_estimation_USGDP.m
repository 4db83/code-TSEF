% Script: example_ARMA_estimation_USGDP.m
% Example of ARMA model fitted to US Real GDP;
clear; clc; clf;
addpath(genpath('./db.toolbox/'))
% set to 1 to get new data
get_new_data = 0;

if get_new_data
  % get data from FRED2
  % Billions of Chained 2012 Dollars, Seasonally Adjusted Annual Rate
  T1 = datestr(datenum('Q4-2023','qq-yyyy'),'yyyy-mm-dd');
  T0 = datestr(datenum('Q1-1947','qq-yyyy'),'yyyy-mm-dd');
  usdata = as_timetable(getFredData('GDPC1', T0, T1,'lin','q'),'gdpc1'); 
  usdata = synchronize(usdata, as_timetable(getFredData('USRECQ', T0, T1,'lin','q'),'NBER'));
  save('./data/real_gdp_US_2023Q4.mat', 'usdata');
  % print/export to xlsx if needed
  print2xls(usdata,'/data/real_gdp_US_2023Q4.xlsx')
  % write also to parquet file for easy reading in R
  parquetwrite('./data/real_gdp_US_2023Q4.parquet',usdata)
  % save also in traditional matlab format
  save('./data/real_gdp_US_2023Q4.mat', 'usdata');
  disp('done saving the data')
else
  % load './data/real_gdp_US_2021Q4.mat';
  load './data/real_gdp_US_2023Q4.mat';
  % OR READ FROM .XLSX USING READTABLE AND THEN CONVERT TO TIMETABLE AS USED BELOW
%   tmp_tbl = readtable('./data/real_gdp_US_2021Q4.xlsx');
%   usdata  = table2timetable(tmp_tbl(:,2:end), 'RowTimes', datetime(tmp_tbl{:,1},'InputFormat','dd.MM.yyyy'));
end

%% GENERATE Y = LOG-GDP AND DY = ANNUALIZED GPD GROWTH.
usdata.y  = log(usdata.gdpc1);
usdata.dy = 400*(usdata.y - lag(usdata.y,1));
% % uncomment to write to csv file 
% writetimetable(usdata,'real_gdp_US_2021Q4.csv','Delimiter',',')
% ss = timerange('Q1-1947', 'Q4-2019', 'closed');
ss = timerange('01.01.1947', '31.12.2019', 'closed');
usdata = usdata(ss,:);
% head2tail(usdata);
T2 = 151; % break in volatility

% PLOT CONTROLS
set(groot,'defaultLineLineWidth',2); % sets the default linewidth;
set(groot,'defaultAxesXTickLabelRotationMode','manual')
% plot controls
fig.ds  = 33;
fig.fs  = 19;
fig.st  = -1.21;
fig.dim = [.85 .2];
fig.pos = @(x) ([.07 x]);

% 2x1 grid of plots
figure(1);clf
subplot(2,1,1)
hold on;
  addrecessionBars(usdata.NBER, [7.5 10]); 
  plot(usdata.y,'Color',clr(1));
hold off; vline(T2,'r:'); 
setplot([fig.pos(.60) fig.dim],[],[],6/5);
setyticklabels(7.5:.5:10, 1)
setdateticks(usdata.Time, fig.ds, 'yyyy:QQ', fig.fs);	
setoutsideTicks
add2yaxislabel
addsubtitle('(a) Log of US real GDP (level series)', fig.st)

subplot(2,1,2)
hold on;
  addrecessionBars(usdata.NBER, [-12 18]); 
  plot(usdata.dy,'Color',clr(1));
hold off; vline(T2,'r:'); 
box on; grid on;
setplot([fig.pos(.34) fig.dim],[],[],6/5);
setyticklabels(-12:4:18, 0)
setdateticks(usdata.Time, fig.ds, 'yyyy:QQ', fig.fs);	
set(gca,'GridLineStyle',':','GridAlpha',1/3);
hline(-12);hline(16);hline(0);
ylim([-12 16])
setoutsideTicks
add2yaxislabel
tickshrink(.9)
addsubtitle('(b) First difference of log of US real GDP (annualized growth rate)', fig.st)

% UNCOMMENT TO PRINT TO PDF
% print2pdf('USGDP_level_growth','../graphics')

%% PLOT SAMPLE ACF/PACF OF ANNUALIZED GDP 
figure(2);
plotacf(usdata.dy);
% addsubtitle('Sample ACF/PACF ofx US GDP growth', [-1.35 -.66], 20)
% UNCOMMENT TO PRINT TO PDF
% print2pdf('acf_USGDP_growth','../graphics')

%% ESTIMATE THE ARMA MODELS
% set upper bounds for p* and q* to search over the ARMA model: CHOOSE THESE CAREFULLY.
P = 2;
Q = 2;

% Space allocation for bic and aic values
BIC_pq	= zeros(P+1,Q+1);
AIC_pq  = zeros(P+1,Q+1);
HQC_pq	= zeros(P+1,Q+1);
pq			= {};						% cell array to display the ARMA orders if needed.

for q = 1:(Q+1)
	for p = 1:(P+1)
		% create the PP and QQ vector entries to be used in estiamte_armax function
		PP = 1:(p-1);
		QQ = 1:(q-1);
		% this stores the ARMA(p,q) orders in a cell array. Not really needed
    %	pq{p,q}		= [num2str(max([0 PP])) ',' num2str(max([0 QQ]))];
		% estimate the different ARMA models and store the ICs.
		tmp_ = estimate_armax(usdata.dy,1,PP,QQ,[],[],[],[],[],1);
		BIC_pq(p,q) = tmp_.diagnostics.SBIC;
		AIC_pq(p,q) = tmp_.diagnostics.AIC;
		HQC_pq(p,q)	= tmp_.diagnostics.HQC;
  end
end
	
%% Store the bic and aic matrices
AIC = [[nan (0:Q)];[(0:P)' AIC_pq]];
BIC = [[nan (0:Q)];[(0:P)' BIC_pq]];
HQC = [[nan (0:Q)];[(0:P)' HQC_pq]];
ICs = [AIC BIC HQC];

% Display the best ARMA(p,q) orders for AIC, BIC and HQC
% fprintf('---------------------------------------------------------------------------\n');
[p_aic q_aic]=find(min(min(AIC_pq))==AIC_pq);
fprintf('AIC best fitting ARMA model is: ARMA(%d,%d)  \n', [p_aic q_aic]-1)
[p_bic q_bic]=find(min(min(BIC_pq))==BIC_pq);
fprintf('BIC best fitting ARMA model is: ARMA(%d,%d)  \n', [p_bic q_bic]-1)
[p_hqc q_hqc]=find(min(min(HQC_pq))==HQC_pq);
fprintf('HQC best fitting ARMA model is: ARMA(%d,%d)  \n', [p_hqc q_hqc]-1)

%% Estimate the final 'best' models based on IC
arma_aic = estimate_armax(usdata.dy,1,1:(p_aic-1),1:(q_aic-1)); print_arma_results(arma_aic);
arma_bic = estimate_armax(usdata.dy,1,1:(p_bic-1),1:(q_bic-1)); print_arma_results(arma_bic);
arma_hqc = estimate_armax(usdata.dy,1,1:(p_hqc-1),1:(q_hqc-1)); print_arma_results(arma_hqc);

%% PLOT ACTUAL AND FITTED VALUES/RESIDUALS
% recession bar color
rec_CLR = .83*ones(3,1); 
figure(3);
subplot(2,1,1)
hold on; LG = [];
  bar( usdata.NBER*16, 1, 'FaceColor', rec_CLR); 
  bar(-usdata.NBER*12, 1, 'FaceColor', rec_CLR); 
LG(1) = plot(usdata.dy,'Color',clr(1),'LineWidth',2.75);
LG(2) = plot(addnans(arma_aic.yhat,1),'Color',clr(2),'LineWidth',2.5);
LG(3) = plot(addnans(arma_bic.yhat,1),'Color',clr(3),'LineStyle','--');
% LG(4) = plot(addnans(arma_hqc.yhat,1),'Color',clr(5),'LineStyle',':');
hold off; vline(T2,'r:'); 
box on; grid on;
setplot([fig.pos(.6) fig.dim],[],[],6/5);
setyticklabels(-12:4:16, 0)
setdateticks(usdata.Time, fig.ds, 'yyyy:QQ', fig.fs);	
set(gca,'GridLineStyle',':','GridAlpha',1/3);
hline(-12);hline(16);hline(0);
ylim([-12 16])
setoutsideTicks
add2yaxislabel
tickshrink(.9)
addsubtitle('(a) Actual and fitted values', fig.st)
legendflex(LG,{'GDP growth','AIC-ARMA(2,2)','BIC-ARMA(1,0)'}, 'fontsize', fig.fs - 1, 'anchor',3.*[1 1],'Interpreter','Latex')

subplot(2,1,2)
hold on; LG = [];
  bar( usdata.NBER*16, 1, 'FaceColor', rec_CLR); 
  bar(-usdata.NBER*12, 1, 'FaceColor', rec_CLR); 
% LG(1) = plot(usdata.dy,'Color',clr(1));
LG(1) = plot(addnans(arma_aic.uhat,1),'Color',clr(5),'LineWidth',2.75);
LG(2) = plot(addnans(arma_bic.uhat,1),'Color',clr(2),'LineStyle','--');
% LG(4) = plot(addnans(arma_hqc.yhat,1),'Color',clr(5),'LineStyle',':');
hold off; vline(T2,'r:'); 
box on; grid on;
setplot([fig.pos(.34) fig.dim],[],[],6/5);
setyticklabels(-12:4:16, 0)
setdateticks(usdata.Time, fig.ds, 'yyyy:QQ', fig.fs);	
set(gca,'GridLineStyle',':','GridAlpha',1/3);
hline(-12);hline(16);hline(0);
ylim([-12 16])
setoutsideTicks
add2yaxislabel
tickshrink(.9)
addsubtitle('(b) Residuals', fig.st)
legendflex(LG,{'AIC-ARMA(2,2)','BIC-ARMA(1,0)'}, 'fontsize', fig.fs - 1, 'anchor',3.*[1 1],'Interpreter','Latex')
% UNCOMMENT TO PRINT TO PDF
% print2pdf('fitted_values_US', 1);

%% SOME POST ESTIMATOIN PLOTS 
% plot sample acf/pacf of the residual series of model selected by BIC and AIC
figure(4);
plotacf(arma_bic.uhat);
addsubtitle('Sample ACF/PACF of residuals from BIC model', [-1.35 -.66], 20)
% UNCOMMENT TO PRINT TO PDF
% print2pdf('acf_arma_bic_fit', 1)

figure(5);
plotacf(arma_aic.uhat);
addsubtitle('Sample ACF/PACF of residuals from AIC model', [-1.35 -.66], 20)
% UNCOMMENT TO PRINT TO PDF
% print2pdf('acf_arma_aic_fit', 1)
 
%% THEORETICAL ACF/PACF VALUES OF FITTED MODEL
aL_bic = arma_bic.aL;
bL_bic = arma_bic.bL;

aL_aic = arma_aic.aL;
bL_aic = [1];

% plot theoretical ACF/PACF values of fitted model to visually compare to sample ACF/PACF
figure(6);
plotacf0(aL_bic,bL_bic);
% plotacf0(aL_aic,bL_aic);
addsubtitle('Theoretical ACF/PACF of AIC model', [-1.35 -.66], 20)
% UNCOMMENT TO PRINT TO PDF
% print2pdf('acf0_ar1', 1);




% clc
% plotacf0([1 -0.8],1);
% clc;clf
% plotacf0([1 -1.3 0.4],[1 -.5]);
% roots([1 -1.3 0.4])
% roots([1 -0.5])





































%EOF 

