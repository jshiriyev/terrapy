classdef fracControl
    
    % The fracture segment is defined as a plane joining two node points
    % (point1 and point2). The heigth of fracture plane is taken the same
    % as reservoir thickness (it is not diffcult to model shorter planes).
    % z-coordinate of the points is given as the reservoir depth.
    
    properties
        fileDir
        nodeCoord
        map
        permeability
        width
        fracID
        nodeID
        numAfrac
        numAnode
        conductivity
        point1
        point2
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
            
            obj.nodeCoord = inpput.read(file1Name,outputInUnitSystem).list.value;
            obj.map = inpput.read(file2Name,outputInUnitSystem).list.value;
            obj.permeability = inpput.read(file3Name,outputInUnitSystem).list.value;
            obj.width = inpput.read(file4Name,outputInUnitSystem).list.value;
            
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
            
            % coordinates of points 1 and 2 at each fracture segment
            
            obj.point1.Xcoord = obj.nodeCoord(obj.map(:,1),1);
            obj.point1.Ycoord = obj.nodeCoord(obj.map(:,1),2);
            obj.point1.Zcoord = obj.nodeCoord(obj.map(:,1),3);
            
            obj.point2.Xcoord = obj.nodeCoord(obj.map(:,2),1);
            obj.point2.Ycoord = obj.nodeCoord(obj.map(:,2),2);
            obj.point2.Zcoord = obj.nodeCoord(obj.map(:,2),3);
            
            % calculation of fracture geometry

            obj.Length = sqrt((obj.point2.Xcoord-obj.point1.Xcoord).^2+...
                              (obj.point2.Ycoord-obj.point1.Ycoord).^2);

            obj.areatoreservoir = obj.Length.*obj.point1.Zcoord;
            obj.areatofracture = obj.width.*obj.point1.Zcoord;
            
            obj.volume = obj.Length.*obj.width.*obj.point1.Zcoord;
            
            obj.center.Xcoord = (obj.point1.Xcoord+obj.point2.Xcoord)/2;
            obj.center.Ycoord = (obj.point1.Ycoord+obj.point2.Ycoord)/2;
            obj.center.Zcoord = (obj.point1.Zcoord+obj.point2.Zcoord)/2;
            
            % calculation of fracture azimuth
            
            obj.signX = sign(obj.point2.Xcoord-obj.point1.Xcoord);
            obj.signY = sign(obj.point2.Ycoord-obj.point1.Ycoord);
            
            obj.azimuth = atan(abs(obj.point2.Ycoord-obj.point1.Ycoord)./...
                               abs(obj.point2.Xcoord-obj.point1.Xcoord));
            
        end
        
    end
    
end