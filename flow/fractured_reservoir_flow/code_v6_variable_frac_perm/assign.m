classdef assign
    
    % UNITCONV does the unit conversion for all types
    % sen
    
    properties
        SI
        FU
    end
    
    methods (Static)
        
        function obj = property(value,type,unit)
            
            % convFactor is defined as FU to SI
            
            if strcmp(type,'length')
                convFactor = 0.3048;             % [ft] to [m]
            elseif strcmp(type,'pressure')
                convFactor = 6894.76;            % [psi] to [Pa]
            elseif strcmp(type,'permeability')
                convFactor = 9.869233e-16;       % [mD] to [m2]
            elseif strcmp(type,'compressibility')
                convFactor = 1/6894.76;          % [1/psi] to [1/Pa]
            elseif strcmp(type,'viscosity')
                convFactor = 1e-3;               % [cp] to [Pa.s]
            elseif strcmp(type,'flowrate')
                convFactor = 1/(6.29*24*60*60);  % [bbl/day] to [m3/sec]
            elseif strcmp(type,'time')
                convFactor = 24*60*60;           % [bbl/day] to [m3/sec]
            else
                error(['Check the value type. ',type,' is not defined'])
            end
            
            if strcmp(unit,'SI')
                obj.SI = value;
                obj.FU = value/convFactor;
            elseif strcmp(unit,'FU')
                obj.FU = value;
                obj.SI = value*convFactor;
            else
                error('Check the value unit. SI and FU are the defined unit systems')
            end
            
        end
        
    end
    
end

