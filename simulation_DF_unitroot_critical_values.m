% Script: simulation_DF_unitroot_critical_values.m
% Simulation of Dickey-Fuller critical values. Cleaned up. does not require fastols anymore, uses
% fstols function defined at the end of this script.
% uncomment print2pdf generate pdf from plot using the print2pdf function.
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

% some controls
N     = 1e5;
T     = 500;
C     = ones(T,1);
trnd  = (1:T)';
rng(1234)

% space allocation for storage of tstats and coeffs
tstat0  = zeros(N,1);   rho0 = zeros(N,1);
tstatC  = zeros(N,1);   rhoC = zeros(N,1);
tstatCT = zeros(N,1);   rhoCT= zeros(N,1);

tic;
% main loop
for jj = 1:N
  % generate pure random walk
  y = cumsum(randn(T+1,1));
  % make Y X variables
  Y = y(2:end); 			% y(t)
  X = y(1:end-1);		  % y(t-1)

	% run the 3 separate regressions
  [bhat,se]      = fastols(Y,X);  
  [Cbhat,Cse]    = fastols(Y,[X C]);
  [CTbhat,CTse]  = fastols(Y,[X C trnd]);
  
  % store bhat (rho_hat) coeffcients
  rho0(jj) 		= bhat(1);
  rhoC(jj) 		= Cbhat(1);
  rhoCT(jj)		= CTbhat(1);
  % store t-stats now for the null of a unit-root
  tstat0(jj)  = (bhat(1)-1)  /se(1);
  tstatC(jj)  = (Cbhat(1)-1) /Cse(1);
  tstatCT(jj) = (CTbhat(1)-1)/CTse(1);
end 
toc

%% COMPUTE PERCENTILS OF CRITICAL VALUES
pctls   = [1 2.5 5 10 50 90 95 97.5 99]/100; 
% DF-critical values
pct_t0  = quantile(tstat0 , pctls ); % histogram(tstat0, 100, 'Normalization','pdf')
pct_tC  = quantile(tstatC , pctls ); % histogram(tstatC, 100, 'Normalization','pdf')
pct_tCT = quantile(tstatCT, pctls ); % histogram(tstatCT,100, 'Normalization','pdf')
% now for T(rho_hat -1) 
pct_p0  = quantile(T*(rho0-1) , pctls ); % histogram(tstat0, 100, 'Normalization','pdf')
pct_pC  = quantile(T*(rhoC-1) , pctls ); % histogram(tstatC, 100, 'Normalization','pdf')
pct_pCT = quantile(T*(rhoCT-1), pctls ); % histogram(tstatCT,100, 'Normalization','pdf')

% make a pretty output table
PCT_t = [pct_t0; pct_tC; pct_tCT];
PCT_p = [pct_p0; pct_pC; pct_pCT];

% print to screen
caseNamaes = {'No Constant', 'Contant','Constant & trend'};
% sep(140);print2screen(PCT_t,[['T = ',num2str(T),' (rho-1)/se(rho)'], caseNamaes],num2str(pctls'),2)
% sep(140);print2screen(PCT_p,[['T = ',num2str(T),' T*(rho-1)      '] ,caseNamaes],num2str(pctls'),2)

%% PLOT CONTROLS
clf;
set(groot,'defaultLineLineWidth',1.5); % sets the default linewidth;
set(groot,'defaultAxesXTickLabelRotationMode','manual')
fig.dim = [.85 .25];
fig.pos = @(x) ([.07 x]);

% compute the of the tstatistic density over xg grid
xg  = linspace(-6,4,1e3)';              % xgrid for tstats
t0  = ksdensity(tstat0, xg); 
tC  = ksdensity(tstatC, xg);
tCT = ksdensity(tstatCT,xg);

% compute the density of T(rho_hat -1).
pxg = linspace(-35,5,1e3)';             % xgrid for T(rho_hat -1)
p0  = ksdensity(T*(rho0-1), pxg); 
pC  = ksdensity(T*(rhoC-1), pxg);
pCT = ksdensity(T*(rhoCT-1),pxg);
 
% plots of 
figure(1);clf; 
hold on; LG = [];
LG(1) = plot(xg,t0);
LG(2) = plot(xg,tC);
LG(3) = plot(xg,tCT);
LG(4) = plot(xg,normpdf(xg,0,1),'-k');
hold off; 
box on; grid on;
setplot([fig.pos(.30) fig.dim],16,[],6/5);
set(gca,'GridLineStyle',':','GridAlpha',1/3);
setyticklabels([0:0.1:0.6], 1)
setoutsideTicks
add2yaxislabel
tickshrink(.9)

legNames = {'$\tau_0=(\hat{\rho}_0-1)/se(\hat{\rho}_0)$' ;
       			'$\tau_{\mu}=(\hat{\rho}_\mu-1)/se(\hat{\rho}_\mu)$' ;
       			'$\tau_{\tau}=(\hat{\rho}_\tau-1)/se(\hat{\rho}_\tau)$' ;
       			'$N(0,1)$' }; 
legendflex(LG,legNames,'Interpreter','Latex')

%% plots
figure(2);clf;
hold on; LG = [];
LG(1) = plot(pxg,p0);
LG(2) = plot(pxg,pC);
LG(3) = plot(pxg,pCT);
hold off; 
box on; grid on;
setplot([fig.pos(.30) fig.dim],16,[],6/5);
set(gca,'GridLineStyle',':','GridAlpha',1/3);
setyticklabels([0:0.05:0.25])
setoutsideTicks
add2yaxislabel
tickshrink(.9)

legNames = {'$T(\hat{\rho}_0-1)$' ;
       			'$T(\hat{\rho}_\mu-1)$' ;
       			'$T(\hat{\rho}_\tau-1)$'}; 
legendflex(LG,legNames,'Interpreter','Latex', 'anchor',[1 1])

%% FAST OLS IN SAME FILE SO THAT YOU CAN SEE WHAT IS COMPUTED
function [beta,se_beta] = fastols(y,X)
  [T, k]  = size(X);
  beta    = X\y;
  uhat		= y-X*beta;
  se_beta = sqrt(diag(inv(X'*X)*(uhat'*uhat)/T));
end

















































%EOF