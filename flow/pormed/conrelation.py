import matplotlib.pyplot as plt
import numpy as np

if __name__ == "__main__":
    import setup

class darcy_relation():

    pass

class relative_permeability_balhoff():

    def __init__(self,Swr,Sor,krwo,kroo,nw,no):

        self.Swr    = Swr
        self.Sor    = Sor

        self.krwo   = krwo
        self.kroo   = kroo
        
        self.nw     = nw
        self.no     = no

    def oil_water(self,Sw):

        # Sw[Sw<self.Swr] = self.Swr
        # Sw[Sw>1-self.Sor] = 1-self.Sor

        S = (Sw-self.Swr)/(1-self.Swr-self.Sor)

        krw = self.krwo*S**self.nw
        kro = self.kroo*(1-S)**self.no

        return krw,kro

    def gas_oil(self):

        pass

class relative_permeability():

    """

    This Model Provides IMBIBITION Relative Permeability MODELS for a system provided.

    So      = oil saturation
    Sw      = water saturation
    Sg      = gas saturation
    
    MODEL: OIL-WATER system

    Sorow   = residual oil saturation in oil-water system
    Swc     = connate water saturation
    krowc   = oil relative permeability at connate water saturaton
    krwor   = water relative permeability at the residual oil saturation
    no      = oil exponent on relative permeability curve
    nw      = water exponent on relative permeability curve

    MODEL: GAS-OIL system

    Sorgo   = residual oil saturation in gas-oil system
    Swc     = connate water saturation
    Slc     = critical liquid saturation = Swc+Sor
    Sgc     = critical gas saturation
    krogc   = oil relative permeability at critical gas saturation
    krglc   = gas relative permeability at critical liquid saturation
    no      = oil exponent on relative permeability curve
    ng      = gas exponent on relative permeability curve

    MODEL: THREE-PHASE system

    Som     = minimum oil saturation in three-phase system
    Sorow   = residual oil saturtaion in oil-water system
    Sorgo   = residual oil saturation in gas-oil system
    Swc     = connate water saturation
    Sgc     = criticial gas saturation
    krowc   = oil relative permeability at connate water saturaton in oil-water system
    krwor   = water relative permeability at the residual oil saturation in oil-water system
    krogc   = oil relative permeability at critical gas saturation in gas-oil system
    krglc   = gas relative permeability at critical liquid saturation in gas-oil system
    no      = oil exponent on relative permeability curve
    nw      = water exponent on relative permeability curve
    ng      = gas exponent on relative permeability curve
    
    """

    def __init__(self,
                 Sorow=0.4,
                 Sorgo=0.4,
                 Swc=0.1,
                 Sgc=0.05,
                 krowc=0.8,
                 krwor=0.3,
                 krogc=0.8,
                 krglc=0.3,
                 no=2,
                 nw=2,
                 ng=2,
                 Som=None):

        self.Sorow = Sorow
        self.Sorgo = Sorgo
        self.Swc = Swc
        self.Sgc = Sgc
        self.krowc = krowc
        self.krwor = krwor
        self.krogc = krogc
        self.krglc = krglc
        self.no = no
        self.nw = nw
        self.ng = ng

        self.Som = Som

    def system2phase(self,Sw,Sg=0,model="oil-water"):

        So = 1-Sw-Sg

        if model == "oil-water":
            self.kro,self.krw = self._oil_water(Sw,So)
        elif model == "gas-oil":
            self.kro,self.krg = self._gas_oil(Sw,So,Sg)
        
    def _oil_water(self,Sw,So):

        movable_o = So-self.Sorow
        movable_w = Sw-self.Swc
        movable_l = 1-self.Sorow-self.Swc
        
        kro = self.krowc*(movable_o/movable_l)**self.no
        krw = self.krwor*(movable_w/movable_l)**self.nw

        return kro,krw

    def _gas_oil(self,Sw,So,Sg):

        Slc = self.Sorgo+self.Swc
        
        movable_o = 1-Slc-Sg
        movable_g = Sg-self.Sgc
        movable_f = 1-Slc-self.Sgc

        kro = self.krogc*(movable_o/movable_f)**self.no
        krg = self.krglc*(movable_g/movable_f)**self.ng

        return kro,krg

    def system3phase(self,Sw,So,Sg,model="Stone's Model I",n=None):

        if self.Som is None:
            self._estimate_Som(Sg)

        if model=="Stone's Model I":
            self.kro,self.krw,self.krg = self._stones_model_I(Sw,So,Sg)
        elif model=="Aziz and Settari":
            self.kro,self.krw,self.krg = self._aziz_settari(Sw,So,Sg)
        elif model=="Stone's Model II":
            self.kro,self.krw,self.krg = self._stones_model_II(Sw,So,Sg)
        elif model=="Hustad-Holt Correlation":
            self.kro,self.krw,self.krg = self._hustad_holt(Sw,So,Sg,n)

    def _stones_model_I(self,Sw,So,Sg):

        movable_o = So-self.Som
        movable_w = Sw-self.Swc
        movable_g = Sg

        movable_f = 1-self.Swc-self.Som

        So_star = movable_o/movable_f
        Sw_star = movable_w/movable_f
        Sg_star = movable_g/movable_f

        kroow,krw = self._oil_water(Sw,So)
        krogo,krg = self._gas_oil(Sw,So,Sg)

        beta_w = (kroow)/(1-Sw_star)
        beta_g = (krogo)/(1-Sg_star)

        kro = So_star*beta_w*beta_g

        return kro,krw,krg

    def _estimate_Som(self,Sg):

        alpha = 1-(Sg)/(1-self.Swc-self.Sorgo)

        Som = alpha*self.Sorow+(1-alpha)*self.Sorgo

        return Som

    def _aziz_settari(self,Sw,So,Sg):
        
        movable_o = So-self.Som
        movable_w = Sw-self.Swc
        movable_g = Sg

        movable_f = 1-self.Swc-self.Som

        So_star = movable_o/movable_f
        Sw_star = movable_w/movable_f
        Sg_star = movable_g/movable_f

        kroow,krw = self._oil_water(Sw,So)
        krogo,krg = self._gas_oil(Sw,So,Sg)

        beta = (So_star)/(1-Sw_star)/(1-Sg_star)

        kro = (kroow*krogo)/(self.krowc)*beta

        return kro,krw,krg

    def _stones_model_II(self,Sw,So,Sg):

        movable_o = So-self.Som
        movable_w = Sw-self.Swc
        movable_g = Sg

        movable_f = 1-self.Swc-self.Som

        So_star = movable_o/movable_f
        Sw_star = movable_w/movable_f
        Sg_star = movable_g/movable_f

        kroow,krw = self._oil_water(Sw,So)
        krogo,krg = self._gas_oil(Sw,So,Sg)

        kro = self.krowc*((kroow/self.krowc+krw)*(krogo/self.krowc+krg)-(krw+krg))

        return kro,krw,krg

    def _hustad_holt(self,Sw,So,Sg,n):

        movable_o = So-self.Som
        movable_w = Sw-self.Swc
        movable_g = Sg-self.Sgc

        movable_f = 1-self.Som-self.Swc-self.Sgc

        So_star = movable_o/movable_f
        Sw_star = movable_w/movable_f
        Sg_star = movable_g/movable_f

        kroow,krw = self._oil_water(Sw,So)
        krogo,krg = self._gas_oil(Sw,So,Sg)

        beta = (So_star)/(1-Sw_star)/(1-Sg_star)

        kro = (kroow*krogo)/(self.krowc)*beta**n

        return kro,krw,krg
    
if __name__ == "__main__":

    import unittest

    from flow.pormed.tests.conrelation import TestRelativePermeability

    unittest.main()

    # rp = relative_permeability_balhoff(
    #     Swi=0.2,
    #     Swr=0.2,
    #     krwo=0.2,
    #     kroo=1.0,
    #     nw=3,
    #     no=3,
    #     )

    # Sw = 0.2001

    # rp.system2phase(Sw=Sw,model="oil-water")

    # print(rp.krw)