import numpy as np 

if __name__ == "__main__":
    import setup

class coordinates():

    def __init__(self):

        pass

    def cartesiantoradial(self):

        pass

    def radialtocartesian(self):

        pass

    def cartesiantospherical(self):

        pass

    def sphericaltocartesian(self):

        pass

class units():

    # (value)(unit)(system)(quantity)

    def __init__(self,unitsystem="SI"):

        """
        SI: international unit system
        FU: field unit system
        """

        self.unitsystem = unitsystem

    def tosiunits(self,quantity,variable):

        if self.unitsystem != "SI":
            variable *= self.get_fieldtosi(quantity)

        return variable

    def tofieldunits(self,quantity,variable):

        if self.unitsystem != "FU":
            variable *= self.get_sitofield(quantity)

        return variable

        # def conversion(var):
                    
        #     # input variable structure consist of:
        #     # var.value -- the value of variable -- ex. 5 (any value)
        #     # var.unit -- the unit of variable -- ex. m, ft, psi, Pa or sec
        #     # var.system -- the system of units for output -- ex. SI or FU

        #     # the following field is added to the output variable structure:
        #     # var.quantity -- ex. length, time, or pressure
            
        #     unitLib( 1,:) = {'length','m','ft'};
        #     unitLib( 2,:) = {'mass','kg','lbm'};
        #     unitLib( 3,:) = {'time','sec','day'};
        #     unitLib( 4,:) = {'temperature','K','F'};
        #     unitLib( 5,:) = {'pressure','Pa','psi'};
        #     unitLib( 6,:) = {'permeability','m2','mD'};
        #     unitLib( 7,:) = {'compressibility','1/Pa','1/psi'};
        #     unitLib( 8,:) = {'viscosity','Pa.s','cp'};
        #     unitLib( 9,:) = {'flowrate','m3/sec','bbl/day'};
        #     unitLib(10,:) = {'velocity','m/sec','ft/day'};
            
        #     cond.value = inpput.isfieldnull(var,'value');
        #     cond.unit = inpput.isfieldnull(var,'unit');
        #     cond.system = inpput.isfieldnull(var,'system');
            
        #     if cond.value
        #         error('The value of variable is not defined.')
        #     elseif cond.unit
        #         var.unit = [];
        #         var.system = [];
        #         var.quantity = [];
        #     elseif cond.system
        #         error('The system of units for output is not defined.')
        #     else
        #         [row,col] = find(strcmp(var.unit,unitLib));
        #         if isempty(col)
        #             error(['Check the unit of value. ',var.unit,' is not defined'])
        #         elseif col(1) == 2
        #             var.quantity = unitLib(row(1),1);
        #             if and(~strcmpi(var.system,'SI'),strcmpi(var.system,'FU'))
        #                 convFactor = inpput.convFactorDetermine(var.quantity);
        #                 var.value = var.value/convFactor;
        #                 var.unit = unitLib(row(1),3);
        #             elseif and(~strcmpi(var.system,'SI'),~strcmpi(var.system,'FU'))
        #                 error('Check the required system of units for outputs')
        #             end
        #         elseif col(1) == 3
        #             var.quantity = unitLib(row(1),1);
        #             if and(~strcmpi(var.system,'FU'),strcmpi(var.system,'SI'))
        #                 convFactor = inpput.convFactorDetermine(var.quantity);
        #                 var.value = var.value*convFactor;
        #                 var.unit = unitLib(row(1),2);
        #             elseif and(~strcmpi(var.system,'SI'),~strcmpi(var.system,'FU'))
        #                 error('Check the required system of units for outputs')
        #             end
        #         else
        #             error('Something wrong went while mathcing input unit.')
        #         end

        #     return var

    def get_siunits(self,quantity):

        if quantity == "length":
            return "meter"
        elif quantity == "time":
            return "second"
        elif quantity == "amount":
            return "mole"
        elif quantity == "current":
            return "ampere"
        elif quantity == "temp_relative":
            return "celsius"
        elif quantity == "temperature":
            return "kelvin"
        elif quantity == "luminosity":
            return "candela"
        elif quantity == "mass":
            return "kilogram"
        elif quantity == 'pressure':
            return "pascal"
        elif quantity == 'permeability':
            return "meter square"
        elif quantity == 'compressibility':
            return "one over pascal"
        elif quantity == 'viscosity':
            return "pascal second"
        elif quantity == 'flowrate':
            return "meter cube per second"
        elif quantity == 'time':
            return "second"
        elif quantity == 'velocity':
            return "meter per second"

    def get_fieldunits(self,quantity):

        if quantity == "length":
            return "foot"
        elif quantity == "time":
            return "day"
        elif quantity == "amount":
            return "lbmole"
        elif quantity == "temp_relative":
            return "fahrenheit"
        elif quantity == "temperature":
            return "rankine"
        elif quantity == "mass":
            return "lbm"
        elif quantity == 'pressure':
            return "psi"
        elif quantity == 'permeability':
            return "milli-Darcy"
        elif quantity == 'compressibility':
            return "one over psi"
        elif quantity == 'viscosity':
            return "centipoise"
        elif quantity == 'flowrate':
            return "bbl per day"
        elif quantity == 'time':
            return "day"
        elif quantity == 'velocity':
            return "foot per day"

    def get_sitofield(self,quantity):

        if quantity == "length":
            return 3.28084

        elif quantity == "time":
            return 1.15741e-5

        elif quantity == "amount":
            return 0.00220462

        elif quantity == "temperature":
            return 1.8

        elif quantity == "mass":
            return 2.20462

        elif quantity == 'pressure':
            return 0.0001450376807894691

        elif quantity == 'permeability':
            return 1013249965828144.9

        elif quantity == 'compressibility':
            return 6894.76

        elif quantity == 'viscosity':
            return 1000 # [cp] to [Pa.s]

        elif quantity == 'flowrate':
            return 543456

        elif quantity == 'time':
            return 1.1574074074074073e-05

        elif quantity == 'velocity':
            return 283464.56692913384

    def get_fieldtosi(self,quantity):

        if quantity == "length":
            return 0.3048

        elif quantity == "time":
            return 86400

        elif quantity == "amount":
            return 453.592909

        elif quantity == "temperature":
            return 0.555556

        elif quantity == "mass":
            return 0.453592

        elif quantity == 'pressure':
            return 6894.76 # [psi] to [Pa]

        elif quantity == 'permeability':
            return 9.869233e-16 # [mD] to [m2]

        elif quantity == 'compressibility':
            return 0.0001450376807894691

        elif quantity == 'viscosity':
            return 1e-3 # [cp] to [Pa.s]

        elif quantity == 'flowrate':
            return 1.8400753694871342e-06

        elif quantity == 'time':
            return 86400

        elif quantity == 'velocity':
            return 3.527777777777778e-06

    def get_tempreltoabs(self):

        if self.unitsystem == "SI":
            return 273.15
        elif self.unitsystem == "FU":
            return 459.67

if __name__ == "__main__":

    us = units(unitsystem="FU")

    fu = us.tofieldunits("time",5)

    print(fu)