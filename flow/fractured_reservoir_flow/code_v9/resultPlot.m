classdef resultPlot
    
    properties
        q1 % flow at the starting point of fracture segment
        q2 % flow at the ending point of fracture segment
        qw % wellbore flow rate
        qf % flux to fracture segments
        pn % pressure at the nodes
        tau % calculated time points
    end
    
    methods (Static)
        
        function node(frac)
            plot(frac.nodeCoord(:,1),frac.nodeCoord(:,2),'k.');
            hold on
        end
        
        function frac(frac,prop)
            switch nargin
                case 1
                    plot([frac.p1.Xcoord,frac.p2.Xcoord]',...
                         [frac.p1.Ycoord,frac.p2.Ycoord]'); hold on
                case 2
                    plot([frac.p1.Xcoord,frac.p2.Xcoord]',...
                         [frac.p1.Ycoord,frac.p2.Ycoord]',prop); hold on
            end
        end
        
        function well(frac,well,prop)
            plot(frac.nodeCoord(well.wellID,1),...
                 frac.nodeCoord(well.wellID,2),'x',prop); hold on
        end
        
        function bhp(sol,time,well,prop)
            ptime = resultPlot.conversion(time.tau,'sec','FU');
            pwell = resultPlot.conversion(sol.pn(well.wellID,:),'Pa','FU');
            switch nargin
                case 3
                    plot(ptime,pwell);
                case 4
                    plot(ptime,pwell,prop);
            end
            hold on
        end

        function flowRate(sol,time,prop)
            ptime = resultPlot.conversion(time.tau,'sec','FU');
            qwell = resultPlot.conversion(sum(sol.qw,1),'m3/sec','FU');       % /1.273
            plot(ptime,qwell,prop);
            hold on
        end
        
        function obs = map2D(res,Nx,Ny)
            [obs.X,obs.Y,obs.Z] = ...
                meshgrid(0:res.xLength/Nx:res.xLength,...
                         0:res.yLength/Ny:res.yLength,...
                           res.zLength/2);
            obs.Xcoord = reshape(obs.X,[],1);
            obs.Ycoord = reshape(obs.Y,[],1);
            obs.Zcoord = reshape(obs.Z,[],1);
        end
        
        function map2Dplot(obs,sol,res,frac,time,green)
            
            numSnaps = size(time.idxSnapTime,1);
            gterm = green*time.deltaTime;
            
            for i = 1:numSnaps
                
                idxQ = time.idxSnapTime(i):-1:1;
                idxG = 1:time.idxSnapTime(i);

                pressure = res.initPressure-...
                 sum(sum(permute(sol.qf(:,idxQ),[3,1,2]).*gterm(:,:,idxG),3),2);
        
                pressure = resultPlot.conversion(pressure,'Pa','FU');
        
                figure('Name','Reservoir Domain','NumberTitle','off','visible','off')

                Z = griddata(obs.Xcoord,obs.Ycoord,...
                    pressure,obs.X,obs.Y);

                h = pcolor(obs.X,obs.Y,Z);

                set(h,'EdgeColor','none');
                shading interp

                colormap(jet)
                colorbar
                
                caxis([1000,4200])
                
                hold on
                
                prop.Color = 'w';
                prop.LineWidth = 2;
                
                resultPlot.frac(frac,prop);
                
%                 prop.MarkerSize = 10;
%                 prop.Color = 'k';
%                 
%                 resultPlot.well(frac,prop); hold off
                figName = ['time ',num2str(time.snapTime(i)),' days'];
                saveas(gcf,['results/',figName,'.png'])
                close(gcf)
                
            end
        
        end
        
        function value = conversion(value,unit,system)
            var.value = value;
            var.unit = unit;
            var.system = system;
            value = inpput.conversion(var).value;
        end
        
    end
    
end

