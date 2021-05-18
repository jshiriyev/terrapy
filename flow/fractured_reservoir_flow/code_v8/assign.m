classdef assign
    
    properties
        SI
        FU
    end
    
    methods (Static)
        
        function obj = property(value,unitorquantity,unitsystem)
            
            % value -- ex. 5 (any value)
            % unit name -- ex. m, ft, psi, Pa or sec
            % unit quantity -- ex. length, time, or pressure
            % unit system -- ex. SI or FU
            
            switch nargin
                case 1
                    obj.DL = value;
                case 2
                    unit = assign.systDetermine(unitorquantity);
                    if strcmp(unit.system,'SI')
                        obj.SI = value;
                        obj.FU = value/unit.conv;
                    elseif strcmp(unit.system,'FU')
                        obj.SI = value*unit.conv;
                        obj.FU = value;
                    else
                        obj.DL = value;
                    end
                case 3
                    conv = assign.convDetermine(unitorquantity);
                    if strcmp(unitsystem,'SI')
                        obj.SI = value;
                        obj.FU = value/conv;
                    elseif strcmp(unitsystem,'FU')
                        obj.SI = value*conv;
                        obj.FU = value;
                    else
                        error([unitsystem,' is not defined. SI and FU are the options'])
                    end
            end
            
        end
        
        function unit = systDetermine(unittype)
            
            unitLib(1,:) = {'length','m','ft'};
            unitLib(2,:) = {'pressure','Pa','psi'};
            unitLib(3,:) = {'permeability','m2','mD'};
            unitLib(4,:) = {'compressibility','1/Pa','1/psi'};
            unitLib(5,:) = {'viscosity','Pa.s','cp'};
            unitLib(6,:) = {'flowrate','m3/sec','bbl/day'};
            unitLib(7,:) = {'time','sec','day'};
            
            [row,col] = find(strcmp(unittype,unitLib));
            
            if isempty(col)
                error(['Check the unit of value. ',unittype,' is not defined'])
            elseif col(1) == 2
                unit.system = 'SI';
                unit.quantity = unitLib(row(1),1);
                unit.conv = assign.convDetermine(unit.quantity);
            elseif col(1) == 3
                unit.system = 'FU';
                unit.quantity = unitLib(row(1),1);
                unit.conv = assign.convDetermine(unit.quantity);
            else
                error('Something wrong went while mathcing input unit.')
            end
            
        end
        
        function convFactor = convDetermine(quantity)
            
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
            end
            
        end
        
    end
    
end

