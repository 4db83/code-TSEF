function addrecessionBars(I_NBER,bounds,rec_color)
% simple function to add recession indicator Bars to plot
% call as:  addrecessionBars(I_NBER,bounds,rec_color)
% or simply: 
%           addrecessionBars(I_NBER) accepting default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where: 
%     - I_NBER    = binary variable, 0,1
%     - bounds    = either scalar of upper bound,
%                   or (2x1) giving lower and upper bounds.
%                   default value [0 100].
%     - rec_color = either sclarr between [0,1] for gray
%                   color bars, or full RGB (1x3) color vector.
%                   default value 0.9.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SetDefaultValue(2,'bounds', 100)
SetDefaultValue(3,'rec_color', 0.9)

% default colors
if length(rec_color)>1
  C = rec_color;
else
  C = rec_color*ones(1,3);
end

if length(bounds)==1
  upp = bounds;
  low = 0;
elseif length(bounds)==2
  upp = bounds(2);
  low = bounds(1);
else
  error('Too many inputs')
end

% US RECESSION BARS
bar( I_NBER*upp,1,'FaceColor',C,'ShowBaseLine','On');
% hline(0)
bar( I_NBER*low,1,'FaceColor',C,'ShowBaseLine','On');
set(gca, 'Layer', 'top')
% yaxis([low upp])