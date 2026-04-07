% Script: example_acf_pacf_ar2.m
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

% lag polynomial
a1  = 1.5 ; a2 =-0.56;
aL  = [1 -a1 -a2];               
% companion matrix Phi
Phi = [a1 a2;
        1 0];

% theoretical ACF and PACF 
acf_t  = acf0 (aL,1,50);
pacf_t = pacf0(aL,1,50);

% lag polynomial and characteristic/factored roots 
lag_roots		= roots(fliplr(aL));  % lag polynomial roots
fact_roots 	= roots(aL);          % factored roots
char_roots 	= eig(Phi);          	% characteristic roots (eigenvalues of Phi)

% print roots to screen
fprintf(' Lag Polynomial roots are:       %2.4f %2.4f \n', lag_roots)
fprintf(' Factored Polynomial roots are:  %2.4f %2.4f \n', fact_roots)
fprintf(' Characteristic roots are:       %2.4f %2.4f \n', char_roots)

% PLOTS
% clear plotting area and set default line-width to 2
set(groot,'defaultLineLineWidth',1.5); 
xg  = linspace(0,1.5,1e3) ; % grid for plotting
fns = 15;                   % font size for plots
stp = -1.2;                 % subtitle position adjustment
dims = [.3 .20];            % subfigure dims
tspc = .34;                 % topspace

% 2x2 grid of plots
subplot(2,2,1);
hold on;
  plot(xg,polyval(fliplr(aL),xg))
  hline(0,'-k'); vline(lag_roots(1),'--r'); vline(lag_roots(2),'--r')
hold off;
ylim([-0.01 0.01]); xlim([1 1.5]); 
setplot([.15 .6 dims], 3, fns);
setoutsideTicks; 
addsubtitle('(a) Roots of lag polynomial', stp)

subplot(2,2,2); 
hold on;
  plot(xg,polyval(aL,xg));
  hline(0,'-k'); vline(char_roots(1),'--r'); vline(char_roots(2),'--r')
hold off;
ylim([-0.01 0.01]); xlim([.5 .9]);
setplot([.55 .6 dims], 3, fns);
setoutsideTicks
addsubtitle('(b) Characteristic roots', stp)

subplot(2,2,3); % ACF
  bar(acf_t(2:end), 'FaceColor', [.7 .8 1]);%, 'EdgeColor', 'none');
setplot([.15 tspc dims], 1, fns);
setoutsideTicks
addsubtitle('(c) Theoretical ACF', stp)

subplot(2,2,4);
  bar(pacf_t(2:end), 'FaceColor', [.7 .8 1]);
setplot([.55 tspc dims], 1, fns);
setoutsideTicks
addsubtitle('(d) Theoretical PACF', stp)

% UNCOMMENT TO PRINT TO PDF 
% print2pdf('example_acf_pacf_ar2', 1)


































%EOF 