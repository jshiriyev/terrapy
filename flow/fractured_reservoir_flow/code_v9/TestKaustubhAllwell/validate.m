addpath('C:\Users\js68897\repos\greensfrac')

% QfTotal = sum(sum(sol.qf,2).*frac.Length)*res.zLength;
% 
% QwTotal = sum(sol.qw);


% minumum = min(abs(sol.qf),[],2);
% max(abs(sol.qf))

avg = mean(abs(sol.qf),2);

idx0 = and(1e-5<avg,avg<=1e-4);
idx1 = and(1e-6<avg,avg<=1e-5);
idx2 = and(1e-7<avg,avg<=1e-6);
idx3 = and(1e-8<avg,avg<=1e-7);
idx4 = and(1e-9<avg,avg<=1e-8);
idx5 = (avg<=1e-9);

figure('Name','Fracture Network','NumberTitle','off','visible','on');

% resultPlot.node(frac);

% prop.Color = 'k';
% prop.LineWidth = 1.5;
% resultPlot.frac(frac,prop);

% prop.MarkerSize = 10;
% prop.Color = 'k';
% resultPlot.well(frac,well,prop);

% xlim([0,res.Length(1)])
% ylim([0,res.Length(2)])

h0 = plot([frac.p1.Xcoord(idx0),frac.p2.Xcoord(idx0)]',...
          [frac.p1.Ycoord(idx0),frac.p2.Ycoord(idx0)]','LineWidth',2); hold on

h1 = plot([frac.p1.Xcoord(idx1),frac.p2.Xcoord(idx1)]',...
          [frac.p1.Ycoord(idx1),frac.p2.Ycoord(idx1)]','LineWidth',2); hold on

h2 = plot([frac.p1.Xcoord(idx2),frac.p2.Xcoord(idx2)]',...
          [frac.p1.Ycoord(idx2),frac.p2.Ycoord(idx2)]','LineWidth',2); hold on

h3 = plot([frac.p1.Xcoord(idx3),frac.p2.Xcoord(idx3)]',...
          [frac.p1.Ycoord(idx3),frac.p2.Ycoord(idx3)]','LineWidth',2); hold on

h4 = plot([frac.p1.Xcoord(idx4),frac.p2.Xcoord(idx4)]',...
          [frac.p1.Ycoord(idx4),frac.p2.Ycoord(idx4)]','LineWidth',2); hold on

h5 = plot([frac.p1.Xcoord(idx5),frac.p2.Xcoord(idx5)]',...
          [frac.p1.Ycoord(idx5),frac.p2.Ycoord(idx5)]','LineWidth',2); hold on
 
 legend([h0;h1;h2;h3;h4;h5],'1E-5,1E-4','1E-6,1E-5','1E-7,1E-6','1E-8,1E-7','1E-9,1E-8','<1E-9')