classdef fracControl
    
    properties
        fileDir
        xcoordofnode
        ycoordofnode
        zcoordofnode
        nodenumofp1
        nodenumofp2
        permeability
        width
%         fracCond
%         xcoordofp1
%         ycoordofp1
%         zcoordofp1
%         xcoordofp2
%         ycoordofp2
%         zcoordofp2
%         Length
%         area
%         xcoordofcenter
%         ycoordofcenter
%         zcoordofcenter
%         nodeonjustp1
%         nodeonjustp2
%         nodeonbothps
%         numAnodes
%         numAfracs
%         numDnodes
    end
    
    methods (Static)
        
        function obj = read(string)
            
            obj.fileDir = [string,'\fracture\'];
            
            cord = fracControl.iusread([obj.fileDir,'coordinates.txt']);
            mapp = fracControl.iusread([obj.fileDir,'map.txt']);
            perm = fracControl.iusread([obj.fileDir,'permeability.txt']);
            wdth = fracControl.iusread([obj.fileDir,'width.txt']);
            
            obj.xcoordofnode = assign.property(cord.value(:,1),cord.quantity,cord.system);
            obj.ycoordofnode = assign.property(cord.value(:,2),cord.quantity,cord.system);
            obj.zcoordofnode = assign.property(cord.value(:,3),cord.quantity,cord.system);
            
            obj.nodenumofp1 = assign.property(mapp.value(:,1));
            obj.nodenumofp2 = assign.property(mapp.value(:,2));
            
            obj.permeability = assign.property(perm.value,perm.quantity,perm.system);
            
            obj.width = assign.property(wdth.value,wdth.quantity,wdth.system);
            
        end
        
        function obj = property(obj)
            
            % calculation of fracture conductivity
            
            obj.fracCond = obj.permeability.SI.*obj.width.SI;
            
            % coordinates of point 1 and 2 at each fracture segment
            
            obj.xcoordofp1 = obj.xcoordofnode.SI(obj.nodenumofp1.DL);
            obj.ycoordofp1 = obj.ycoordofnode.SI(obj.nodenumofp1.DL);
            obj.zcoordofp1 = obj.zcoordofnode.SI(obj.nodenumofp1.DL);
            
            obj.xcoordofp2 = obj.xcoordofnode.SI(obj.nodenumofp2.DL);
            obj.ycoordofp2 = obj.ycoordofnode.SI(obj.nodenumofp2.DL);
            obj.zcoordofp2 = obj.zcoordofnode.SI(obj.nodenumofp2.DL);
            
            % calculation of fracture geometry

            obj.Length = sqrt((obj.xcoordofp2-obj.xcoordofp1).^2+...
                              (obj.ycoordofp2-obj.ycoordofp1).^2);

            obj.area = obj.zcoordofp1.*obj.Length;

            obj.xcoordofcenter = (obj.xcoordofp1+obj.xcoordofp2)/2;
            obj.ycoordofcenter = (obj.ycoordofp1+obj.ycoordofp2)/2;
            obj.zcoordofcenter = (obj.zcoordofp1+obj.zcoordofp2)/2;
            
            % finding node points with single and double neighbors
            
            obj.nodeonjustp1 = setdiff(obj.nodenumofp1.DL,obj.nodenumofp2.DL);
            obj.nodeonjustp2 = setdiff(obj.nodenumofp2.DL,obj.nodenumofp1.DL);
            
            obj.nodeonbothps = intersect(obj.nodenumofp1.DL,obj.nodenumofp2.DL);
            
            % number of nodes, fracture segments, double neighbor nodes
            
            obj.numAnodes = length(obj.xcoordofnode.SI);    % number of all nodes
            obj.numAfracs = length(obj.permeability.SI);    % number of all fracture segments
            obj.numDnodes = length(obj.nodeonbothps);       % number of double neighbor nodes
            
        end
        
        function [] = plot(obj,plotlim)
            
            figure('Name','Fracture Structure','NumberTitle','off');
            
            plot(obj.xcoordofnode.SI,obj.ycoordofnode.SI,'k.'); hold on
            
            plot([obj.xcoordofp1,obj.xcoordofp2]',...
                 [obj.ycoordofp1,obj.ycoordofp2]',...
                 'k','LineWidth',1)
            
            switch nargin
                case 1
                    xlim([min(obj.xcoordofnode.SI)-20,...
                          max(obj.xcoordofnode.SI)+20]);
                    ylim([min(obj.ycoordofnode.SI)-20,...
                          max(obj.ycoordofnode.SI)+20]);
                case 2
                    xlim(plotlim(1:2))
                    ylim(plotlim(3:4))
            end
            
        end
        
        function prop = iusread(string)
            
            % string specifies name and path to the file
            % numtoskip specifies number of rows to skip
            
            fid = fopen(string);
            
            numtoskip = 1;
            
            while true
                line = fgetl(fid);
                firstchar = line(find(~isspace(line),1));
                if firstchar ~= "%"
                    break
                end
                numtoskip = numtoskip+1;
            end
            
            unitinfo = strtrim(strsplit(line,','));
            
            fclose(fid);
            
            prop.quantity = unitinfo{1};
            prop.system = unitinfo{2};
            
            prop.value = dlmread(string,'\t',numtoskip,0);
            
        end
        
    end
    
end

