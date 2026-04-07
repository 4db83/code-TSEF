% Script: example_acf_non_invertible_ma2.m
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

% MA lag polynomial
b1 = -3.5 ; b2 = -2;
bL = [1 +b1 +b2];   

% theoretical ACF and PACF 
acf_t  = acf0 (1,bL,50);
pacf_t = pacf0(1,bL,50);

% lag polynomial and characteristic/factored roots of non-invertible model
lag_roots		= roots(fliplr(bL));  % lag polynomial roots
fact_roots 	= 1./lag_roots;       % factored roots
% fact_roots 	= roots(bL);        % also the factored roots, but this reverses the order of them,

fprintf(' For the non-invertible process \n')
fprintf(' Lag Polynomial roots are     : % 2.2f % 2.2f \n',   lag_roots);
fprintf(' Factored Polynomial roots are: % 2.2f % 2.2f \n\n', fact_roots);

% Make process invertible 
b1_plus = -( fact_roots(1) + 1/fact_roots(2) );
b2_plus =  ( fact_roots(1)*1/fact_roots(2) );
bL_plus = [1 +b1_plus +b2_plus]; 

% theoretical ACF and PACF of invertible process
acf_t_plus  = acf0( 1,bL_plus,50);
pacf_t_plus = pacf0(1,bL_plus,50);

% lag polynomial and characteristic/factored roots of invertible model
lag_roots_plus		= roots(fliplr(bL_plus));  % lag polynomial roots
fact_roots_plus 	= 1./lag_roots;            % factored roots
% fact_roots_plus 	= roots(bL_plus);        % factored roots to make invertibel model or roots(bL), this reverses the order,

fprintf(' For the invertible process \n')
fprintf(' Lag Polynomial roots are     : % 2.2f % 2.2f \n', lag_roots_plus);
fprintf(' Factored Polynomial roots are: % 2.2f % 2.2f \n', fact_roots_plus);

% PLOTS
% clear plotting area and set default line-width to 2
set(groot,'defaultLineLineWidth',1.5); 
xg  = linspace(0,1.5,1e3) ;   % grid for plotting
fns = 15;                     % font size for plots
stp = -1.2;                   % subtitle position adjustment
dims = [.3 .20];              % subfigure dims
tspc = .34;                   % topspace
ytcks = [-.2:.1:.3];          % control y-axis ticks 

% 2x2 grid of plots
subplot(2,2,1);
  bar(acf_t(2:end), 'FaceColor', [.7 .8 1]);
setplot([.15 .6 dims], 1, fns);
setyticklabels(ytcks, 1); 
setoutsideTicks
addsubtitle('(a) ACF $\beta(z)=(1-3.5z-2z^2)$', stp,[],1)

subplot(2,2,2);
  bar(pacf_t(2:end), 'FaceColor', [.7 .8 1]);
setplot([.55 .6 dims], 1, fns);
setyticklabels(ytcks, 1); 
setoutsideTicks
addsubtitle('(b) PACF $\beta(z)=(1-3.5z-2z^2)$', stp,[],1)

subplot(2,2,3); % ACF
  bar(acf_t_plus(2:end), 'FaceColor', [.7 .8 1]);
setplot([.15 tspc dims], 1, fns);
setyticklabels(ytcks, 1); 
setoutsideTicks
addsubtitle('(c) ACF $\beta(z)=(1+0.25z-0.125z^2)$', stp,[],1)

subplot(2,2,4);
  bar(pacf_t_plus(2:end), 'FaceColor', [.7 .8 1]);
setplot([.55 tspc dims], 1, fns);
setyticklabels(ytcks, 1); 
setoutsideTicks
addsubtitle('(d) PACF $\beta(z)=(1+0.25z-0.125z^2)$', stp,[],1)

% UNCOMMENT TO PRINT TO PDF 
% print2pdf('example_acf_non_invertible_ma2', 1)

































%EOF

