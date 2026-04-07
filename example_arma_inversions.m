% example ARMA to MA(infty) and ARMA to AR(infty) using Matlabs polynomial division function 
% deconvolution 
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

R  = 10;                    % number of terms in the inversion.
a1 = 0.5; a2 = -0.8;    
b1 =-0.6; b2 =  0.08;

b  = [b1 0.1 0 0 0 0 0 0 0 0.1 0.001]; %b = b1;
a  = [a1 a2  0 0 0 0 0 0.1 0.1 0.001]; %a = a1;
aL = [1 -a];
bL = [1 b];

ar_inf = arma2ar(aL,bL,R);
ma_inf = arma2ma(aL,bL,R);

% using matlabs deconvolution function
arout = deconv([aL(:);zeros(R+1,1)],bL(:));
maout = deconv([bL(:);zeros(R+1,1)],aL(:));

% this to show that matlabs garchar function gives the wrong coefficients
%[ar_inf(2:end) garchar(a(:),b(:),10) arout(2:end)]
%[ma_inf(2:end) garchma(a(:),b(:),10)]



aL = [1 -1.2 .35];
bL = [1 .5 .04];

maout = arma2ma(aL,bL,10)