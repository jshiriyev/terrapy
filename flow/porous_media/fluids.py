import io
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

class compressible():

    def __init__(self,compressibility):

        self.viscosity = viscosity
        self.compressibility = compressibility

class slightly_compressible():

    def __init__(self,viscosity):

        self.viscosity = viscosity

class incompressible():

    def __init__(self,viscosity):

        self.viscosity = viscosity

class homogeneous_mixture():

    def __init__(self,solvent,solute):

        self.solvent = solvent
        self.solute = solute

class multiphase():

    def __init__(self,phases):

        self.phases = phases
        
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)