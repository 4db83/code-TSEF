% Script: example_VAR2_forecasting.m
% Example of VAR(2) forecasting.
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

% SET UP PARAMETERS
p = 2; % lag order
k = 2; % number of variables

A1	= [0.5 0.1 ;...
			 0.4 0.3];
		
A2	= [-0.2 0.1 ;...
			 -0.3 0.2]; 

c		= [0.2; 0.3];

sig = [1.75 0.25 ;...
			 0.25 3.00];

% set up companion form parameters		
Ik	= eye(k);
Ok	= zeros(k,k);

A		= [A1 A2 ;...
		   Ik Ok];

C		= [c; Ok(:,1)];

Sig = [sig Ok;...
			 Ok Ok];
	
% unconditional mean
mu		= inv(Ik - A1 - A2)*c;
S			= [Ik Ok];						% selection vector
Mu		= inv(eye(p*k)-A)*C;

% variance/covariance matrix
Gam0	= reshape(inv(eye(16)-kron(A,A))*Sig(:),4,4); 
gam0	= Gam0(1:2,1:2);

%% forecasting 
H		= 40;							% upper forecast horizon cap.
% (pkx1) vector of observed (stacked) X_t at time t used as conditioning info, starting point
Xt	= [0 0 0 0]';			
Xt	= [5 1 1 -5]';			% an alternative starting point
% constructing the Psi weights
Psi	= zeros(k,k,H);
% create psi function
psi = @(A,h) (S*A^h*S');

% looping through different time horizons
for jj = 1:H
	Psi(:,:,jj) = psi(A,jj-1);
end
% initilize space
Xhat_h = []; SigU_h = [];
for h =	1:H							
  Xhat_h(:,h)		= S*(Mu + A^h*(Xt-Mu));						% h-step ahead forecasts
	SigU_h(:,:,h) = Psi(:,:,h)*sig*Psi(:,:,h)';	    % h-step ahead Sig weiths that need to be summed.
end

% PLOT THE FORECASTS
% some plotting controls
set(groot,'defaultLineLineWidth',2); % sets the default linewidth;
set(groot,'defaultAxesXTickLabelRotationMode','manual')
pl.fs = 18;
pl.ps = @(x) ([.07 x .885 .70]);

clf; 
hold on;
  plot(1:H,Xhat_h(1,:),'color',clr(1));
  plot(1:H,Xhat_h(2,:),'color',clr(2));
hold off;
hline(mu(1),':',[],1.5,clr(1));
hline(mu(2),':',[],1.5,clr(2));
box on; grid on;
ylim([0 1.4])
setplot(pl.ps(.2),1,pl.fs,6/5)
setgridlinestyle
setoutsideTicks
add2yaxislabel
tickshrink(.5)

% forecast error variance converges to gam0
SigU_H = sum(SigU_h,3);
disp(' Forecast error variance matrix');
disp(SigU_H)
disp(' Gamma(0) covariance matrix');
disp(gam0)































