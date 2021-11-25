import matplotlib.pyplot as plt
import numpy as np

if __name__ == "__main__":
    import setup

class singlephase():

    def compressible(self):

        pass

    def slightly_compressible(self):

        pass

    def incompressible(self):

        pass

    def homogeneous_mixture(self):

        pass

    def set_compressibility(self,compressibility):

        self.compressibility = compressibility

    def set_viscosity(self,viscosity):

        self.viscosity = viscosity

    def set_fvf(self,fvf):
        # formation volume factor

        self.fvf = fvf

class multiphase(singlephase):

    def __init__(self,phases):

        self.phases = phases
        
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)