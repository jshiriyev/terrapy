classdef plotAll
    
    methods (Static)
        
        function node(frac,prop)
            
            switch nargin
                case 1
                    plot(frac.nodeCoord(:,1),frac.nodeCoord(:,2),'.');
                case 2
                    plot(frac.nodeCoord(:,1),frac.nodeCoord(:,2),'.',prop);
            end
            
        end
        
        function fracture(frac,prop)
            
            switch nargin
                case 1
                    plot([frac.point1.Xcoord,frac.point2.Xcoord]',...
                         [frac.point1.Ycoord,frac.point2.Ycoord]');
                case 2
                    plot([frac.point1.Xcoord,frac.point2.Xcoord]',...
                         [frac.point1.Ycoord,frac.point2.Ycoord]',prop);
            end
            
        end
        
        function well(frac,well,prop)
            
            switch nargin
                case 2
                    plot(frac.center.Xcoord(well.wellID),...
                         frac.center.Ycoord(well.wellID),'x');
                case 3
                    plot(frac.center.Xcoord(well.wellID),...
                         frac.center.Ycoord(well.wellID),'x',prop);
            end
             
        end
        
        function pressure1D(obs,pressure,time)
            
            time.snapTime = time.snapTime/inpput.convFactorDetermine('time');
            
            if length(unique(obs.Xcoord))>1
                xaxis = obs.Xcoord;
            elseif length(unique(obs.Ycoord))>1
                xaxis = obs.Ycoord;
            end
            
            figName = 'Reservoir Pressure';
            
            figure('Name',figName,'NumberTitle','off')
            
            plot(xaxis,pressure); hold on
            
            xlim([min(xaxis),max(xaxis)]);
%           ylim([2000,4500]);
            
            xlabel('distance [m]');
            ylabel('pressure [psi]');
            
%           legend('0.1 day','10 day','1000 day','Location','SouthEast');
            
            savefig(gcf,['results/',figName,'.fig'])
            close(gcf)
            
        end
            
        function pressure2D(obs,pressure,frac,time,interp)
            
            time.snapTime = time.snapTime/inpput.convFactorDetermine('time');
            
            for i = 1:time.numSnaps
                
                switch nargin
                    case 4
                        OBS = obs;
                        vq = reshape(pressure(:,i),obs.Ynum,obs.Xnum);
                    case 5
                        OBS = plotAll.calc2Dnodes(...
                            [min(obs.Xcoord),max(obs.Xcoord),interp(1)],...
                            [min(obs.Ycoord),max(obs.Ycoord),interp(2)]);
                        vq = griddata(obs.Xcoord,obs.Ycoord,pressure(:,i),...
                              OBS.Xcoord,OBS.Ycoord,'natural');
                        vq = reshape(vq,OBS.Ynum,OBS.Xnum);
                end
            
                figName = ['time ',num2str(time.snapTime(i)),' days'];

                figure('Name',figName,'NumberTitle','off')

                imagesc(OBS.Xcoord,OBS.Ycoord,vq);

    %           set(h,'EdgeColor','none');
    %           shading interp

                colormap(jet)
                colorbar
    %           caxis([2000,4200])

                xlim([min(OBS.Xcoord),max(OBS.Xcoord)]);
                ylim([min(OBS.Ycoord),max(OBS.Ycoord)]);

                hold on

                prop.Color = 'w';
    %           prop.LineWidth = 1;

                plotAll.fracture(frac,prop);

                savefig(gcf,['results/',figName,'.fig'])
                
                close(gcf)
            
            end
                
        end
        
        function obs = calc1Dnodes(Lmin,Lmax,Ndata)
            
            switch nargin
                case 1
                    obs.num = 1;
                    obs.range = Lmin;
                case 2
                    obs.num = 20;
                    obs.range = linspace(Lmin,Lmax,obs.num);
                case 3
                    obs.num = Ndata;
                    obs.range = linspace(Lmin,Lmax,obs.num);
            end
            
        end
        
        function obs = calc2Dnodes(X,Y)
            
            % xnum and ynum are the number of nodes
            % number of elements = number of nodes - 1
            
            if length(X) == 1
                XX = plotAll.calc1Dnodes(X(1));
            elseif length(X) == 2
                XX = plotAll.calc1Dnodes(X(1),X(2));
            elseif length(X) == 3
                XX = plotAll.calc1Dnodes(X(1),X(2),X(3));
            end
            
            if length(Y) == 1
                YY = plotAll.calc1Dnodes(Y(1));
            elseif length(Y) == 2
                YY = plotAll.calc1Dnodes(Y(1),Y(2));
            elseif length(Y) == 3
                YY = plotAll.calc1Dnodes(Y(1),Y(2),Y(3));
            end
            
            [Xmat,Ymat] = meshgrid(XX.range,YY.range);
            
            obs.Xnum = XX.num;
            obs.Ynum = YY.num;
            
            obs.Xcoord = Xmat(:);
            obs.Ycoord = Ymat(:);
            obs.Zcoord = ones((XX.num)*(YY.num),1);
            
        end
        
        function pressure = calcPressure(sol,res,time,green)
            
            gterm = green*time.deltaTime;
            
            pressure = zeros(size(green,1),time.numSnaps);
            
            for i = 1:time.numSnaps
                
                P = res.initPressure-...
                 solver.convolution(gterm,sol.fracflux,1,time.idxSnapTime(i));
                
                pressure(:,i) = P/inpput.convFactorDetermine('pressure');
                
            end
        end
        
    end
    
end

