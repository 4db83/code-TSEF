% Script: plot_MA1_rho_beta_mapping.m
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

% create inline function
f = inline('b./(1+b.^2)','b');
x = linspace(-3,3,100); % grid where to evaluate the function

% plot
set(groot,'defaultLineLineWidth',1.5); 
fns = 17; % font size for plots

hold on;
  plot(x,f(x))
  hline(0,'k-'); % vline(0,'k-')
  vline(-1,'k--');vline(1,'k--')
  hline(0.4,'r-',[],[],'r');vline(0.5,'r-');vline(2,'r-')
hold off;		
setplot([.1 .44 .8 .28], 1, fns);
setyticklabels([-.5:.1:.5], 1); 
setoutsideTicks; 
xlabel('$\beta_1$','Interpreter','Latex')
ylabel('$\rho(1)$','Interpreter','Latex')
moveylabel(.05);  
movexlabel(-.05);
add2yaxislabel

% UNCOMMENT TO PRINT TO PDF 
% print2pdf('plot_MA1_rho_beta_mapping', 1)
