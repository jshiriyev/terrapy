classdef fracControl
    
    properties
        fileDir
        nodeCoord
        map
        permeability
        width
        porosity
        fracID
        nodeID
        numAfrac
        numAnode
        conductivity
        p1
        p2
        Length
        areatoreservoir
        areatofracture
        volume
        center
        signX
        signY
        azimuth
    end
    
    methods (Static)
        
        function obj = fracControl(string,outputInUnitSystem)
            
            obj.fileDir = [string,'\fracture\'];
            
            file1Name = [obj.fileDir,'coordinates.txt'];
            file2Name = [obj.fileDir,'map.txt'];
            file3Name = [obj.fileDir,'permeability.txt'];
            file4Name = [obj.fileDir,'width.txt'];
            file5Name = [obj.fileDir,'porosity.txt'];
            
            obj.nodeCoord = inpput.read(file1Name,outputInUnitSystem).list.value;
            obj.map = inpput.read(file2Name,outputInUnitSystem).list.value;
            obj.permeability = inpput.read(file3Name,outputInUnitSystem).list.value;
            obj.width = inpput.read(file4Name,outputInUnitSystem).list.value;
            obj.porosity = inpput.read(file5Name,outputInUnitSystem).list.value;
            
            obj = fracControl.calculate(obj);
            
        end
        
        function obj = calculate(obj)
            
            % nodal properties
            
            obj.fracID = unique(cumsum(any(obj.map>0,2)));
            obj.nodeID = unique(obj.map);
            
            % number of fracture segments and nodes
            
            obj.numAfrac = size(obj.fracID,1);	% # of fracture segments
            obj.numAnode = size(obj.nodeID,1);	% # of nodes
            
            % calculation of fracture conductivity
            
            obj.conductivity = obj.permeability.*obj.width;
            
            % coordinates of point 1 and 2 at each fracture segment
            
            obj.p1.Xcoord = obj.nodeCoord(obj.map(:,1),1);
            obj.p1.Ycoord = obj.nodeCoord(obj.map(:,1),2);
            obj.p1.Zcoord = obj.nodeCoord(obj.map(:,1),3);
            
            obj.p2.Xcoord = obj.nodeCoord(obj.map(:,2),1);
            obj.p2.Ycoord = obj.nodeCoord(obj.map(:,2),2);
            obj.p2.Zcoord = obj.nodeCoord(obj.map(:,2),3);
            
            % calculation of fracture geometry

            obj.Length = sqrt((obj.p2.Xcoord-obj.p1.Xcoord).^2+...
                              (obj.p2.Ycoord-obj.p1.Ycoord).^2);

            obj.areatoreservoir = obj.Length.*obj.p1.Zcoord;
            obj.areatofracture = obj.width.*obj.p1.Zcoord;
            
            obj.volume = obj.Length.*obj.width.*obj.p1.Zcoord;
            
            obj.center.Xcoord = (obj.p1.Xcoord+obj.p2.Xcoord)/2;
            obj.center.Ycoord = (obj.p1.Ycoord+obj.p2.Ycoord)/2;
            obj.center.Zcoord = (obj.p1.Zcoord+obj.p2.Zcoord)/2;
            
            % calculation of fracture azimuth
            
            obj.signX = sign(obj.p2.Xcoord-obj.p1.Xcoord);
            obj.signY = sign(obj.p2.Ycoord-obj.p1.Ycoord);
            
            obj.azimuth = atan(abs(obj.p2.Ycoord-obj.p1.Ycoord)./...
                               abs(obj.p2.Xcoord-obj.p1.Xcoord));
            
        end

%         function plotK
%             sigma_hormin = 4747.75;
%             pressure_avg = linspace(500,4200);
%             sigma_closure = sigma_hormin-pressure_avg;
%             lognorm_fraccond = -0.0004*sigma_closure+0.2191;
%             norm_fraccond = power(10,lognorm_fraccond);
%             plot(pressure_avg,norm_fraccond)
%             ylim([0,1]);
%             xlim([pressure_avg(1),pressure_avg(end)]);
%             grid on
%         end
        
    end
    
end