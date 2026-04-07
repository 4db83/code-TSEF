% Compute IRFs from VAR2
clear; clc; clf;
addpath(genpath('./db.toolbox/'))

A = [0.4	0.1; 0.2	0.5];
Sig2 = [0.25	0.3; 0.3	0.9];

P = chol(Sig2)';
d = diag(P);
D = diag(d);

%% SET UNIT == 1 BELOW TO COMPUTE ONE UNIT SHOCK BASED IRFS
UNIT = 0;
if UNIT; P = P*inv(D); end

N = 15;
k = size(A,1);
irf = zeros(N+1,k^2);
phi = zeros(k,k,N+1);

diag_sphi = zeros(N+1,k);
sumirf2   = zeros(N+1,k^2);

for i=1:N+1
  tmp_  = A^(i-1)*P;
  % irf(i,:)    = (tmp_(:))';
  irf(i,:)    = vec(tmp_');
  phi(:,:,i)  = tmp_;
  phi2(:,:,i) = phi(:,:,i).^2;
  sum_phi2(:,:,i) = sum(phi2(:,:,1:i),3);
  irf2(i,:)     = irf(i,:).^2;
  sumirf2(i,:)  = sum(irf2(1:i,:),1);
  phiphi(:,:,i) = phi(:,:,i)*phi(:,:,i)';
  sphi(:,:,i)   = sum(phiphi(:,:,1:i),3);
  diag_sphi(i,:) = (diag(sphi(:,:,i)))';
  % fevd(i,1) = sum(ej'*phi(:,:,i)*el)^2/diag_sphi(j)
end
fevd = sumirf2./repmat(diag_sphi,1,k);
reshape(fevd,(N+1)*k,k);

% do the plotting now
clf; POS = [.5 .35 .35]; FNS = 16; PS2 = -1.17;
subplot(1,k,1)
  plot(irf(:,[1:k]))
  ylim([0 .6])
  if UNIT; ylim([0 1.2]); end
  xlim([0 15])
setplot([.12 POS],[],FNS)
% add2yaxislabel
addlegend({'$u_1$','$u_2$'})
addsubtitle('Response of $x_1$ to shocks in', PS2)
hline(0,'k-');

subplot(1,k,2);
  plot(irf(:,[k+1:end]));
  ylim([0 .8])
  if UNIT; ylim([0 1.2]); end
  xlim([0 15])
setplot([ .55 POS],[],FNS)
% add2yaxislabel
addlegend({'$u_1$','$u_2$'})
addsubtitle('Response of $x_2$ to shocks in', PS2)
hline(0,'k-');

% % UNCOMMENT TO PRINT TO PDF 
% if UNIT==0
%   print2pdf('IRFs1', 1)
% else 
%   print2pdf('IRFs2', 1);
% end

%% LOCAL FUNCTION DEFINITION
% make vec operator function
function xout = vec(xin)
  xout = xin(:);
end


