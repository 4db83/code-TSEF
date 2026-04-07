% generating a Browninan Motion/Wiener Process with different values of T
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

% set seed
seed(123)
% size of the time interval
T = 1e4;
t = (0:1:T)'/T;                       % t is the column vector [0 1/T 2/T ... T/T=1]
W = [0; cumsum(randn(T,1))]/sqrt(T);  % S is running sum of N(0,1/T) variables

% plot the path of the Wiener process
plot(t,W);          
hline(0,'k-')
%title({['Wiener process with $T = ' int2str(T) '$']},'Interpreter','Latex')
setytick(1)
setplot([.5],1,14);
%ylim([-.801 .401])
%print2pdf(['../lectures/graphics/bm_' num2str(T)])


