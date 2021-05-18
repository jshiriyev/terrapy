classdef setup
    
    % For the semianalytical solution of reservoir-fracture-wellbore flow,
    % there must be four different input files in the input directory.
    %
    % reservoir.inp
    % fracture.inp
    % well.inp
    % numerics.inp
    %
    % Every file will have four fields defined while reading it:
    %
    % file.name
    % file.identity
    % file.activerow
    % file.activeline
    %
    % For every input file there will be an structure created with the
    % specified number of fields:
    %
    % obj.(field1)
    % obj.(field2)
    % obj.(field3)
    % .
    % .
    % obj.(fieldN)
    %
    % Number of fields is dependent on the number of "DATA" phrase in the
    % file. Every field of the structure is the data value defined under
    % "DATA". In the class, the data structure has the following fields.
    %
    % data.name
    % data.value
    % data.unit
    % data.system
    % data.quantity
    
    methods (Static)
        
        function structures(inputDir)
            
            res = setup.read([inputDir,'\','reservoir.inp'],'SI');
            frac = setup.read([inputDir,'\','fracture.inp'],'SI');
            well = setup.read([inputDir,'\','well.inp'],'SI');
            time = setup.read([inputDir,'\','numerics.inp'],'SI');
            
            res = setup.reservoir(res);
            frac = setup.fracture(frac);
            well = setup.well(well,res,frac);
            time = setup.numerical(time);
            
            assignin('base','res',res)
            assignin('base','frac',frac);
            assignin('base','well',well);
            assignin('base','time',time);
            
        end

        function obj = reservoir(obj)
            
            % FIELDS:
            % Length, xLength, yLength, zLength,
            % porosity,
            % permeability, xPermeability, yPermeability, zPermeability,
            % initPressure,
            % diffusivity, xDiffusivity, yDiffusivity, zDiffusivity,
            % isotropic,
            % anisotropic,
            % rockCompressibility,
            % oilViscosity,
            % oilFVF,
            % oilCompressibility,
            % totCompressibility

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
        
        function obj = fracture(obj)

            % FIELDS:
            % nodeCoord,
            % map,
            % permeability,
            % width,
            % fracID,
            % nodeID,
            % numAfrac,
            % numAnode,
            % conductivity,
            % point1,
            % point2,
            % Length,
            % areatoreservoir,
            % areatofracture,
            % volume,
            % center,
            % signX,
            % signY,
            % azimuth

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

        function obj = well(obj,res,frac)

            % FIELDS:
            % radius,
            % consPressure,
            % consFlowrate,
            % wellID,
            % numWnode
            
            % index number of fracture containing well
            obj.numWnode = size(obj.wellID,1);
            
            % calculation of equivalent radius where the pressure equals
            % average block pressure. Here the block is fracture segment
            obj.requivalent = 0.14*sqrt(res.zLength^2+...
                                        frac.Length(obj.wellID).^2);
            
        end

        function obj = numerical(obj)

            % FIELDS:
            % totalTime,
            % deltaTime,
            % numTimeStep,
            % snapTime,
            % idxSnapTime,
            % numSnaps,
            % tau
            
            % number of time steps
            
            obj.numTimeStep = round(obj.totalTime/obj.deltaTime);
            
            % index of snapshot times
            
            obj.numSnaps = size(obj.snapTime,1);
            obj.idxSnapTime = round(obj.snapTime/obj.deltaTime);
            
            % array of time steps
            
            obj.tau = linspace(obj.deltaTime,obj.totalTime,obj.numTimeStep)';
            
        end

        function obj = read(fileName,unitSystem)
            
            % It reads the file with a given "fileName" searching for a
            % phrase DATA. Once found it calls dataReader to create a field
            % for the data in specified "unitSystem".
            
            file.name = fileName;
            
            file.identity = fopen(file.name);
            
            file.activerow = -1;
            
            while true
                
                file.activeline = strtok(fgetl(file.identity),'%');
                file.activerow = file.activerow+1;
                
                if file.activeline == -1
                    fclose(file.identity);
                    break
                elseif strcmp(strtok(file.activeline,['.',',',' ']),'DATA')
                    [data,file] = setup.dataReader(file);
                    data.system = unitSystem;
                    data = setup.conversion(data);
                    obj.(data.name) = data.value;
                end
                
            end

        end
        
        function [data,file] = dataReader(file)
            
            dimdata = str2num(extractAfter(file.activeline,'DATA'));
            
            numrow = dimdata(1);
            numcol = dimdata(2);
            
            file.activeline = strtok(fgetl(file.identity),'%');
            file.activerow = file.activerow+1;
            
            while isempty(file.activeline)
                file.activeline = strtok(fgetl(file.identity),'%');
                file.activerow = file.activerow+1;
            end

            if strcmp(strtok(file.activeline,['.',',',' ']),'PROPERTY')
                string = extractAfter(file.activeline,'PROPERTY');
                data.name = strtrim(strtok(string,'#'));
                data.unit = strtrim(extractAfter(string,'#'));
            end

            file.activeline = strtok(fgetl(file.identity),'%');
            file.activerow = file.activerow+1;

            while isempty(file.activeline)
                file.activeline = strtok(fgetl(file.identity),'%');
                file.activerow = file.activerow+1;
            end

            data.value = dlmread(file.name,'',...
                [file.activerow,0,file.activerow+numrow-1,0+numcol-1]);
            
            for i = 1:numrow-1
                fgetl(file.identity);
                file.activerow = file.activerow+1;
            end

        end
        
        function var = conversion(var)
            
            % input variable structure should have:
            % var.value -- the value of variable -- ex. 5 (any value type)
            % var.unit -- the unit of variable -- ex. m, ft, psi or sec
            % var.system -- the system of units for output -- ex. SI or FU
            
            % the following field is added to the output "var" structure:
            % var.quantity -- ex. length, time, or pressure
            
            unitLib(1,:) = {'length','m','ft'};
            unitLib(2,:) = {'mass','kg','lbm'};
            unitLib(3,:) = {'time','sec','day'};
            unitLib(4,:) = {'temperature','K','F'};
            unitLib(5,:) = {'pressure','Pa','psi'};
            unitLib(6,:) = {'permeability','m2','mD'};
            unitLib(7,:) = {'compressibility','1/Pa','1/psi'};
            unitLib(8,:) = {'viscosity','Pa.s','cp'};
            unitLib(9,:) = {'flowrate','m3/sec','bbl/day'};
            unitLib(10,:) = {'velocity','m/sec','ft/day'};
            
            cond.value = setup.isfieldnull(var,'value');
            cond.unit = setup.isfieldnull(var,'unit');
            cond.system = setup.isfieldnull(var,'system');
            
            if cond.value
                error('The value of variable is not defined.')
            elseif cond.unit
                var.unit = [];
                var.system = [];
                var.quantity = [];
            elseif cond.system
                error('The system of units for output is not defined.')
            else
                [row,col] = find(strcmp(var.unit,unitLib));
                if isempty(col)
                    error(['Check the unit of value. ',var.unit,' is not defined'])
                elseif col(1) == 2
                    var.quantity = unitLib(row(1),1);
                    if and(~strcmpi(var.system,'SI'),strcmpi(var.system,'FU'))
                        convFactor = setup.convFactorDetermine(var.quantity);
                        var.value = var.value/convFactor;
                        var.unit = unitLib(row(1),3);
                    elseif and(~strcmpi(var.system,'SI'),~strcmpi(var.system,'FU'))
                        error('Check the required system of units for outputs')
                    end
                elseif col(1) == 3
                    var.quantity = unitLib(row(1),1);
                    if and(~strcmpi(var.system,'FU'),strcmpi(var.system,'SI'))
                        convFactor = setup.convFactorDetermine(var.quantity);
                        var.value = var.value*convFactor;
                        var.unit = unitLib(row(1),2);
                    elseif and(~strcmpi(var.system,'SI'),~strcmpi(var.system,'FU'))
                        error('Check the required system of units for outputs')
                    end
                else
                    error('Something wrong went while mathcing input unit.')
                end
            end
            
        end
        
        function convFactor = convFactorDetermine(quantity)
            
            % convFactor is defined as from FU (field units) to SI units
            
            if strcmp(quantity,'length')
                convFactor = 0.3048;             % [ft] to [m]
            elseif strcmp(quantity,'pressure')
                convFactor = 6894.76;            % [psi] to [Pa]
            elseif strcmp(quantity,'permeability')
                convFactor = 9.869233e-16;       % [mD] to [m2]
            elseif strcmp(quantity,'compressibility')
                convFactor = 1/6894.76;          % [1/psi] to [1/Pa]
            elseif strcmp(quantity,'viscosity')
                convFactor = 1e-3;               % [cp] to [Pa.s]
            elseif strcmp(quantity,'flowrate')
                convFactor = 1/(6.29*24*60*60);  % [bbl/day] to [m3/sec]
            elseif strcmp(quantity,'time')
                convFactor = 24*60*60;           % [day] to [sec]
            elseif strcmp(quantity,'velocity')
                convFactor = 0.3048/(24*60*60);  % [ft/day] to [m/sec]
            end
            
        end
        
        function cond = isfieldnull(var,fieldName)

            cond = true;

            if isfield(var,fieldName)
                if ~isempty(var.(fieldName))
                    cond = false;
                end
            end
            
        end

    end
    
end