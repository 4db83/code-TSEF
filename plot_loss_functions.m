% Script: plot_loss_functions.m
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

% parameter values
e  = linspace(-1,1,1e3)';

%	Quadratic Error Loss: 
QEL = @(e) (e.^2);
% Absolute Error Loss: 
AEL = @(e) (abs(e));
% LinEx Error Loss: 
b = 3; a = 1;
LINEX = @(e) ( b*(exp(a*e) - a*e-1) );
% LinLin Error Loss: 
b = 3; a = 1; I = e > 0;
LINLIN = @(e) ( b*abs(e).*I + a*abs(e).*(1-I)  );

% clear plotting area and set default linewidth to 2
clf; 
set(groot,'defaultLineLineWidth',2); 
% font size
Fns = 17; 
% plotting begins
hold on; LG = [];
  LG(1) = plot(e,QEL(e));
  LG(2) = plot(e,AEL(e));
  LG(3) = plot(e,LINEX(e));
  LG(4) = plot(e,LINLIN(e));
hold off;
vline(0,'k:');
grid on; box on;
setplot([.06 .6 .89 .24],1,Fns);
% ylabel('$I\hspace{-1mm}L(e)$', 'Interpreter','latex'); moveylabel(-.01); 
ylabel('$\mathrm{I\hspace{-.77mm}L}(e)$', 'Interpreter','latex'); moveylabel(-.01); 
setoutsideTicks
xlabel('$e$', 'Interpreter','latex'); movexlabel(-.04); 
add2yaxislabel
% add legend
legnames = { 'Quadratic',...
             'Absolute',...
             'LinEx',...
             'LinLin'}; 
addlegend(LG, legnames, 1, Fns - 1)

% UNCOMMENT TO PRINT TO PDF 
% print2pdf('loss_functions_2022a', 1)
















%EOF
