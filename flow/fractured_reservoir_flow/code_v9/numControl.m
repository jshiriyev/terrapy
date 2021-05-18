classdef numControl
    
    properties
        fileDir
        totalTime
        deltaTime
        snapTime
        numTimeStep
        idxSnapTime
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
            
            obj.idxSnapTime = round(obj.snapTime/obj.deltaTime);
            
            % array of time steps
            obj.tau = linspace(0,obj.totalTime,obj.numTimeStep+1)';
            obj.tau(1) = obj.deltaTime/100;
%             obj.tau = linspace(obj.deltaTime,obj.totalTime,...
%                                obj.numTimeStep)';
            
        end
        
    end
    
end

