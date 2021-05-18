classdef resControl
    
    properties
        fileDir
        xLength
        yLength
        zLength
        porosity
        totCompressbility
        fluidViscosity
        xPermeability
        yPermeability
        zPermeability
        initPressure
    end
    
    methods (Static)
        
        function obj = property(string)
            
            obj.fileDir = [string,'\reservoir\'];
            
            fid = fopen([obj.fileDir,'input.txt'],'r');
            
            size = str2num(strtok(fgetl(fid),'%'));
            pore = str2num(strtok(fgetl(fid),'%'));
            comp = str2num(strtok(fgetl(fid),'%'));
            visc = str2num(strtok(fgetl(fid),'%'));
            perm = str2num(strtok(fgetl(fid),'%'));
            pres = str2num(strtok(fgetl(fid),'%'));
            
            fclose(fid);
            
            obj.xLength = assign.property(size(1),'ft');
            obj.yLength = assign.property(size(2),'ft');
            obj.zLength = assign.property(size(3),'ft');
            
            obj.porosity = assign.property(pore);

            obj.totCompressibility = assign.property(comp,'1/psi');

            obj.fluidViscosity = assign.property(visc,'cp');

            obj.xPermeability = assign.property(perm(1),'mD');
            obj.yPermeability = assign.property(perm(2),'mD');
            obj.zPermeability = assign.property(perm(3),'mD');

            obj.initPressure = assign.property(pres,'psi');
            
            % ---------------------------------------------------------- %
            % calculation of hydraulic diffusivity
            
            obj.xdiffusivity = obj.xPermeability.SI/...
                (obj.porosity.DL*obj.totCompressibility.SI*obj.fluidViscosity.SI);
            obj.ydiffusivity = obj.yPermeability.SI/...
                (obj.porosity.DL*obj.totCompressibility.SI*obj.fluidViscosity.SI);
            obj.zdiffusivity = obj.zPermeability.SI/...
                (obj.porosity.DL*obj.totCompressibility.SI*obj.fluidViscosity.SI);

        end
        
    end
    
end

