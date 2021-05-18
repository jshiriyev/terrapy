classdef wellControl
    
    properties
        fileDir
        wellNodes
        consPressure
        consFlowrate
    end
    
    methods (Static)
        
        function obj = property(string)
            
            obj.fileDir = [string,'\wellbore\'];
            
            well = dlmread([obj.fileDir,'input.txt'],' ',4,0);
            
            wtype = well(1);
            wcond = well(2);
            wnode = well(3:end);
            
            if wtype == 1
                obj.consPressure = assign.property(wcond,'psi');
            elseif wtype == 2
                obj.consFlowrate = assign.property(wcond,'bbl/day');
            end
            
            obj.wellNodes = assign.property(wnode);
            
            % ---------------------------------------------------------- %
            
            obj.numWnode = length(obj.wellNodes);
            
        end
        
    end
    
end

