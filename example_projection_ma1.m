%% Example MA(1) projection from page 74 in Brockwell and Davies
clear; clc; clf;
addpath(genpath('./db.toolbox/'))
% analytic calculations (requires symbolic math toolbox)
syms b a real

Rt = toeplitz([1 a a^2 a^3]);

% AR(1)
R = [1		a		a^2	a^3	a^4;
		 a		1		a		a^2	a^3;
		 a^2	a		1		a		a^2;
		 a^3	a^2	a		1		a	 ;
		 a^4	a^3	a^2	a		1	 ;];

r = [a;
		 a^2;
		 a^3;
		 a^4;
		 a^5;];

t = 3;
sol_ar = simplify(inv(R(1:t,1:t))*r(1:t));
disp('Solution is:');pretty(sol_ar)
	 
%% MA(1)
p = b/(1+b^2);

Rt = toeplitz([1 p 0 0 0])
R = [1   p   0   0   0;
     p   1   p   0   0;
     0   p   1   p   0;
     0   0   p   1   p;
     0   0   0   p   1;];

r = [p;
     0;
     0;
     0;
     0;];

t = 3;
sol_ma = simplify(inv(R(1:t,1:t))*r(1:t));
pretty(sol_ma)

% substitute in numbers.
subs(sol_ma,b,-0.9)

%% Numerical MA(1) example taken from page of 74 Brockwell and Davies with 
T = 50;
bet  = -0.9;
rho1 = bet/(1+bet^2);
R    = diag(ones(T,1)) + diag(repmat(rho1,1,T-1),1) + diag(repmat(rho1,1,T-1),-1);
r    = zeros(T,1);r(1)=rho1;

for t = 1:4
	inv(R(1:t,1:t))*r(1:t)
end

% compute the comparison [based on projection beta*pi(L)], where pi(L) is the AR(inf) 
% representation of a general ARMA(p,q) process.
comp  = [inv(R)*r bet*arma2ar(1,[1 -0.9],T-1)];

fprintf(' %2.5f %2.5f \n', comp(1:20,:)') % prints by row

% plot(comp)
