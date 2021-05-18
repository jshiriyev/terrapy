classdef resControl
    
    properties
        fileDir
        Length
        xLength
        yLength
        zLength
        porosity
        permeability
        xPermeability
        yPermeability
        zPermeability
        initPressure
        diffusivity
        xDiffusivity
        yDiffusivity
        zDiffusivity
        isotropic
        anisotropic
        rockCompressibility
        oilViscosity
        oilFVF      % formation volume factor
        oilCompressibility
        totCompressibility
    end
    
    methods (Static)
        
        function obj = resControl(string,outputInUnitSystem)
            
            obj.fileDir = [string,'\reservoir\'];
            
            file1Name = [obj.fileDir,'input.txt'];
            
            all = inpput.read(file1Name,outputInUnitSystem);
            
            obj.Length = all.prop1.value;
            obj.porosity = all.prop2.value;
            obj.permeability = all.prop3.value;
            obj.initPressure = all.prop4.value;
            obj.rockCompressibility = all.prop5.value;
            
            obj.oilViscosity = all.prop6.value;
            obj.oilFVF = all.prop7.value;
            obj.oilCompressibility = all.prop8.value;
            
            obj = resControl.calculate(obj);
            
        end
        
        function obj = calculate(obj)
            
            obj.xLength = obj.Length(1);
            obj.yLength = obj.Length(2);
            obj.zLength = obj.Length(3);
            
            obj.totCompressibility = obj.oilCompressibility+...
                                     obj.rockCompressibility;
            
            poreComp = obj.porosity*obj.totCompressibility;
            
            fluidTime = poreComp*obj.oilViscosity;
            
            obj.diffusivity = obj.permeability/fluidTime;
            
            obj.isotropic = length(obj.permeability)==1;
            obj.anisotropic = length(obj.permeability)==3;
            
            if obj.isotropic
                obj.xPermeability = obj.permeability;
                obj.yPermeability = obj.permeability;
                obj.zPermeability = obj.permeability;
                obj.xDiffusivity = obj.diffusivity;
                obj.yDiffusivity = obj.diffusivity;
                obj.zDiffusivity = obj.diffusivity;
            elseif obj.anisotropic
                obj.xPermeability = obj.permeability(1);
                obj.yPermeability = obj.permeability(2);
                obj.zPermeability = obj.permeability(3);
                obj.xDiffusivity = obj.diffusivity(1);
                obj.yDiffusivity = obj.diffusivity(2);
                obj.zDiffusivity = obj.diffusivity(3);
            end
            
        end
        
    end
    
end

