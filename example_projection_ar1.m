%% Example Projection with AR(1) see page 63 in Neusser 
clear; clc; clf;
addpath(genpath('./db.toolbox/'))
% analytic calculations (requires symbolic math toolbox)
syms a b h
R = [1   a   a^2 a^3 a^4;
     a   1   a   a^2 a^3;
     a^2 a   1   a   a^2;
     a^3 a^2 a   1   a  ;
     a^4 a^3 a^2 a   1  ;
     ]

r = [a^h;
     a^(h+1);
     a^(h+2);
     a^(h+3);
     a^(h+4);
     ] 
%
sol = inv(R)*r;
simplify(sol)
