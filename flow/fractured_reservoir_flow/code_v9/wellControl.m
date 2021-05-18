classdef wellControl
    
    properties
        fileDir
        radius
        consPressure
        consFlowrate
        wellID
        numWnode
    end
    
    methods (Static)
        
        function obj = wellControl(string,outSystem)
            
            obj.fileDir = [string,'\well\'];
            
            file1Name = [obj.fileDir,'input.txt'];
            
            well = inpput.read(file1Name,outSystem);
            
            obj.radius = well.prop1.value;
            
            if strcmp(well.prop2.quantity,'pressure')
                obj.consPressure = well.prop2.value;
            elseif strcmp(well.prop2.quantity,'flowrate')
                obj.consFlowrate = well.prop2.value;
            end
            
            obj.wellID = well.list.value;
            
            obj = wellControl.calculate(obj);
            
        end
        
        function obj = calculate(obj)
            
            obj.numWnode = size(obj.wellID,1);	% # of nodes on the well
            
        end
        
    end
    
end