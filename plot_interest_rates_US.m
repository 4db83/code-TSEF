% plot US interest rates: TB3MS 3-Month Treasury Bill Secondary Market Rate, Federal Funds Effective Rate (DFF)
clear; clc; clf;
addpath(genpath('./db.toolbox/'))
% % get data from FREDwebsite if not in ./data/ directory
% usdat = as_timetable(                  getFredData('TB3MS'  , '1934-01-01', '2023-02-01','lin','m'),'Tbill');
% usdat = synchronize(usdat,as_timetable(getFredData('DFF'    , '1947-01-01', '2023-02-01','lin','m'),'ffrate'));
% usdat = synchronize(usdat,as_timetable(getFredData('USRECM' , '1934-01-01', '2023-02-01','lin','m'),'NBER'));
% save('./data/interest_rates_US_2023M2.mat', 'usdat');

% load consumption_income_US data for us
load('./data/interest_rates_US_2023M2.mat')
% load('./data/interest_rates_US_2022M1.mat')
clc;clf;
% usdat.c = log(usdat.pcecc96);
% usdat.y = log(usdat.dpic96);
tt = timerange('01-Jan-1934', '01-Feb-2023', 'closed');
% tt = timerange('Q1-1947', 'Q4-2021', 'closed');
ss_usdat = usdat(tt,:);
head2tail(ss_usdat );

%% PLOTTING
% recession indicators
NBER_I = ss_usdat.NBER;
% recession bar color
rec_CLR = .83*ones(3,1); 
Fns = 30;
LG  = [];
whp = [.5 .2825 .20];	% width/hight of plot
% whp = [.5 .48 .38];	% width/hight of plot
sbl = -1.30;
Dsp = 85;
% --------------------------------------------------------------------------------------------------

clf
set(groot,'defaultAxesXTickLabelRotationMode','manual')
set(groot,'defaultLineLineWidth',2); % sets the default linewidth to 1.5;
subplot(1,3,1)
hold on;
  bar( NBER_I*20, 1, 'FaceColor', rec_CLR); 
  LG(1) = plot(ss_usdat.Tbill ,'Color',clr(1));
  LG(2) = plot(ss_usdat.ffrate,'Color',clr(2),'LineStyle','--');
hold off;
setplot([.05 whp],0);
setdateticks(ss_usdat.Time,Dsp,'yy', Fns);	
setoutsideTicks
addsubtitle('(a) Levels', sbl)
addlegend(LG,{'T-Bill','FFR'})

subplot(1,3,2)
hold on; LG = [];
  bar( NBER_I*4, 1, 'FaceColor', rec_CLR); 
  bar(-NBER_I*6, 1, 'FaceColor', rec_CLR); 
LG(1) = plot(delta(ss_usdat.Tbill),'Color',clr(1));
hold off;
setplot([.385 whp],0);
setdateticks(ss_usdat.Time,Dsp,'yy', Fns);	
hline(0,'k');
setoutsideTicks
addsubtitle('(b) Differences', sbl)
% ADD LEGEND
addlegend(LG,{'T-Bill'})

subplot(1,3,3)
dhndl = density(delta(ss_usdat.Tbill),[0.10 53]);
addsubtitle('(c) Density of differences', sbl)
tickshrink(.66)
box on; ax = gca;
ax.LineWidth = 1.25;
grid on;
set(gca,'GridLineStyle',':','GridAlpha',1/3);
setyticklabels(0:1:4,0)
setxticklabels(-4:1:2)
xlim([min(delta(ss_usdat.Tbill)) max(delta(ss_usdat.Tbill))])
set(gca,'Position',[.71 whp],'FontSize',Fns);
% set(gca,'Position',[.21 whp],'FontSize',Fns);

% PRINT TO PDF
% print2pdf('interest_rates_US_2022M1', 1);






































%  EOF 

