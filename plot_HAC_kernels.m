%Script: plot_kernels
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

% parameter values
z  = linspace(0,3,100)';

%	BARTLETT
B  = @(z) ((1-z).*(z<1));
% PARZAN`
PZ  = @(z) ( (1-6*z.^2+6*abs(z).^3).*(abs(z)<0.5) + ... 
						 (2*(1-abs(z)).^3)		 .*((abs(z)>0.5).*(abs(z)<1)) );
%	QUADRATIC SPECTRAL					
QSa = 25/12./(pi^2*z.^2);
QSb = sin(6*pi*z/5)./(6*pi*z/5)-cos(6*pi*z/5);
QS	= @(z) ( QSa.*QSb );
% DANIEL
D  = @(z) (sin(pi*z)./(pi*z));
% TUKEY-HANNING 
TK = @(z) ((1+cos(pi*z))/2.*(abs(z)<1));

% clear plotting area and set default linewidth to 2
clf; 
set(groot,'defaultLineLineWidth',2); 
Fns = 18; % font size
hold on; LG = [];
  LG(1) = plot(z,B(z));
  LG(2) = plot(z,PZ(z));
  LG(3) = plot(z,QS(z));
  LG(4) = plot(z,D(z), 'Linestyle','--');
  LG(5) = plot(z,TK(z),'Linestyle','-.');
hold off;
hline(0);
setplot([.5 .9 .22], 1, Fns);
ylim([-.25 1.05])
setoutsideTicks
add2yaxislabel

% add legend
legnames = { 'Bartlett',...
             'Parzan',...
             'Quadratic Spectral',...
             'Daniel',...
             'Tukey-Hanning' }; 
addlegend(LG, legnames, 3, Fns - 1)

% UNCOMMENT TO PRINT TO PDF 
% print2pdf('kernels_2022', 1)



























%EOF
