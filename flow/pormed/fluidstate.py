import matplotlib.pyplot as plt
import numpy as np

if __name__ == "__main__":
    import setup

class singlephase():

    # formation volume factor
    # viscosity
    # compressibility

    def compressible(self,compressibility):

        self.viscosity = viscosity
        self.compressibility = compressibility

    def slightly_compressible(self,viscosity):

        self.viscosity = viscosity

    def incompressible(self,viscosity):

        self.viscosity = viscosity

    def homogeneous_mixture(self,solvent,solute):

        self.solvent = solvent
        self.solute = solute

class multiphase(singlephase):

    def __init__(self,phases):

        self.phases = phases
        
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)