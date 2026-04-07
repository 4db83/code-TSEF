% Example of a spurious regression
clear; clf; clc;
% set plotting defaults
set(groot,'defaultLineLineWidth',2)
set(groot,'defaultAxesFontSize',14)
set(groot,'defaultAxesXTickLabelRotationMode','manual')
set(groot,'defaultAxesFontName','Times New Roman')
% set path to local toolbox if needed
addpath(genpath('./db.toolbox/'))

% specify paramters of the simulation
rho_x = 0;
rho_y = rho_x;
T = 5e2;
N = 5e4;
% make storage space
b_hat   = NaN(N,2);
t_ratio = NaN(N,2);
seed(1234)

tic;
for n = 1:N
  y = zeros(T,1);
  x = zeros(T,1);
  % simulate AR processes with different levels of persistance
  for t = 2:T
    y(t) = rho_y*y(t-1) + randn;
    x(t) = rho_x*x(t-1) + randn;
  end
  
  % add constant
  X = [ones(size(x,1),1) x];
  bhat = X\y;
  uhat = y-X*bhat;
  se_bhat = sqrt(diag(inv(X'*X)*sum(uhat.^2)/(T-2)));
  tstat = bhat./se_bhat;
  % store the beta hat and t-statistics for each simulation runs (N
  b_hat(n,:)   = bhat';
  t_ratio(n,:) = tstat';
end
toc;

% plot distributions
histogram(b_hat(:,2))
subplot(2,1,1)
hold on;
histogram(b_hat(:,2),'Normalization','pdf'); 
plot(linspace(-5,5,1e3),normpdf(linspace(-5,5,1e3),mean(b_hat(:,2)),std(b_hat(:,2))))
hold off;

subplot(2,1,2)
hold on;
histogram(t_ratio(:,2),'Normalization','pdf'); 
plot(linspace(-5,5,1e3),normpdf(linspace(-5,5,1e3),0,1))
hold off;
xlim([-5 5])
