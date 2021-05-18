classdef numControl
    
    properties
        fileDir
        totalTime
        deltaTime
        numTimeStep
        snapTime
        idxSnapTime
        numSnaps
        tau
    end
    
    methods (Static)
        
        function obj = numControl(string,outputInUnitSystem)
            
            obj.fileDir = [string,'\numerics\'];
            
            file1Name = [obj.fileDir,'input.txt'];
            
            time = inpput.read(file1Name,outputInUnitSystem);
            
            obj.totalTime = time.prop1.value;
            obj.deltaTime = time.prop2.value;
            obj.snapTime = time.list.value;
            
            obj = numControl.calculate(obj);
            
        end
        
        function obj = calculate(obj)
            
            % number of time steps
            
            obj.numTimeStep = round(obj.totalTime/obj.deltaTime);
            
            % index of snapshot times
            
            obj.numSnaps = size(obj.snapTime,1);
            obj.idxSnapTime = round(obj.snapTime/obj.deltaTime);
            
            % array of time steps
            
            obj.tau = linspace(obj.deltaTime,obj.totalTime,obj.numTimeStep)';
            
        end
        
    end
    
end

