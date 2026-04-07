% plot US retail trade, seasonally unadjusted.
clear; clc; clf;
addpath(genpath('./db.toolbox/'))
% % get data from FREDwebsite if not in ./data/ directory
% usdat = as_timetable(getFredData('RSXFSN', '1947-01-01', '2021-12-31','lin','m'),'RetailSales');
% % usdat = synchronize(usdat,as_timetable(getFredData('USRECQ', '1947-01-01', '2021-12-31','lin','q'),'NBER'));
% usdat = synchronize(usdat,as_timetable(getFredData('USRECM', '1947-01-01', '2021-12-31','lin','m'),'NBER'));
% % combs = synchronize(us_data_merged,usdat);
% save('./data/retail_sales_US_2022M1.mat', 'usdat');

% load consumption_income_US data for us
load('./data/retail_sales_US_2022M1.mat')
% usdat.c = log(usdat.pcecc96);
% usdat.y = log(usdat.dpic96);
% tt = timerange('01-Jan-1992', '01-Jan-2022', 'closed');
tt = timerange('Jan-1992', 'Dec-2022', 'closed');
% tt = timerange('Q1-1947', 'Q4-2021', 'closed');
ss_usdat = usdat(tt,:);
head2tail(ss_usdat);

% recession indicators
NBER_I = ss_usdat.NBER;
% recession bar color
rec_CLR = .83*ones(3,1); 
Fns = 20;
LG  = [];
whp = [.5 .41 .18];	% width/hight of plot
stp = -1.24;

clf; 
set(groot,'defaultLineLineWidth',2); 
subplot(1,2,1)
hold on;
  addrecessionBars(NBER_I,[0 700], rec_CLR); 
  LG(1) = plot(ss_usdat.RetailSales*1e-3,'Color',clr(2));
hold off;
setplot([.06 whp],0);
setdateticks(ss_usdat.Time,31,'yy', Fns);	
setoutsideTicks
addsubtitle('(a) Raw series (Billions of Dollars)', stp)

subplot(1,2,2)
hold on;
  addrecessionBars(NBER_I,[11.5 13.5], rec_CLR); 
  LG(1) = plot(log(ss_usdat.RetailSales),'Color',clr(2));
hold off;
setplot([.55 whp],1);
setyticklabels(11.5:.5:13.5,1,Fns)
setdateticks(ss_usdat.Time,31,'yy', Fns);	
setoutsideTicks
addsubtitle('(b) Log-transformed', stp)

% UNCOMMENT TO PRINT TO PDF
% print2pdf('retail_sales_US_2022M1', 1);






































%  EOF