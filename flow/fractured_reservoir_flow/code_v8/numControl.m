classdef numControl
    
    properties
        fileDir
        totalTime
        deltaTime
    end
    
    methods (Static)
        
        function obj = property(string)
            
            obj.fileDir = [string,'\numerics\'];
            
            time = dlmread([obj.fileDir,'input.txt'],' ',2,0);
            
            obj.totalTime = assign.property(time(1),'day');
            obj.deltaTime = assign.property(time(2),'day');

        end
        
    end
    
end

