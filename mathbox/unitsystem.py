# unit conversion

class Conversion():

    def __init__(self):

        pass
    
        # obj.prop1.(value)(unit)(system)(quantity)
        # obj.prop2.(value)(unit)(system)(quantity)
        # .
        # .
        # obj.list.(value)(unit)(system)(quantity)
            
    def conversion(var)
                
        # input variable structure consist of:
        # var.value -- the value of variable -- ex. 5 (any value)
        # var.unit -- the unit of variable -- ex. m, ft, psi, Pa or sec
        # var.system -- the system of units for output -- ex. SI or FU

        # the following field is added to the output variable structure:
        # var.quantity -- ex. length, time, or pressure
        
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
        
        cond.value = inpput.isfieldnull(var,'value');
        cond.unit = inpput.isfieldnull(var,'unit');
        cond.system = inpput.isfieldnull(var,'system');
        
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
                    convFactor = inpput.convFactorDetermine(var.quantity);
                    var.value = var.value/convFactor;
                    var.unit = unitLib(row(1),3);
                elseif and(~strcmpi(var.system,'SI'),~strcmpi(var.system,'FU'))
                    error('Check the required system of units for outputs')
                end
            elseif col(1) == 3
                var.quantity = unitLib(row(1),1);
                if and(~strcmpi(var.system,'FU'),strcmpi(var.system,'SI'))
                    convFactor = inpput.convFactorDetermine(var.quantity);
                    var.value = var.value*convFactor;
                    var.unit = unitLib(row(1),2);
                elseif and(~strcmpi(var.system,'SI'),~strcmpi(var.system,'FU'))
                    error('Check the required system of units for outputs')
                end
            else
                error('Something wrong went while mathcing input unit.')
            end

        return var
    
            
    def convFactorDetermine(quantity)
                
        # convFactor is defined as from FU (field units) to SI units
        
        if strcmp(quantity,'length')
            convFactor = 0.3048;             # [ft] to [m]
        elseif strcmp(quantity,'pressure')
            convFactor = 6894.76;            # [psi] to [Pa]
        elseif strcmp(quantity,'permeability')
            convFactor = 9.869233e-16;       # [mD] to [m2]
        elseif strcmp(quantity,'compressibility')
            convFactor = 1/6894.76;          # [1/psi] to [1/Pa]
        elseif strcmp(quantity,'viscosity')
            convFactor = 1e-3;               # [cp] to [Pa.s]
        elseif strcmp(quantity,'flowrate')
            convFactor = 1/(6.29*24*60*60);  # [bbl/day] to [m3/sec]
        elseif strcmp(quantity,'time')
            convFactor = 24*60*60;           # [day] to [sec]
        elseif strcmp(quantity,'velocity')
            convFactor = 0.3048/(24*60*60);  # [ft/day] to [m/sec]
        end

        return convFactor
    
    def isfieldnull(var,fieldName)
        
        cond = false;
        
        try
           if isempty(var.(fieldName))
               cond = true;
           end
        catch
            cond = true;
        end

        return cond
