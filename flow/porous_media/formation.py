import io
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse import csr_matrix as csr

from scipy.sparse.linalg import spsolve as sps

from scipy.special import j0
from scipy.special import j1
from scipy.special import y0
from scipy.special import y1

class singlephase():

    def __init__(self,height,radius,porosity,permeability,compressibility):

        self.height = height
        self.radius = radius

        self.porosity = porosity
        self.permeability = permeability
        self.compressibility = compressibility

    def set_total_compressibility(self,fluid):

        self.ct = self.cr+fluid.cf
        
    def set_diffusivity(self,viscosity,total_compressibility):
        
        self.eta = (self.k)/(self.phi*self.mu*self.ct)

    def set_well(self,radius,location,true_vertical_depth,measured_depth):

        self.radius = radius
        
    def set_fracture_nodes(self,thickness,nodes):

        self.thickness = thickness

    def set_hydraulic_diffusivity(self,hydraulic_diffusivity=None):

        if hydraulic_diffusivity is not None:
            self.eta = hydraulic_diffusivity

    def transient_flow(self,time):
        """line source solution"""

        well.radius = radius

        t0 = 100.*well_radius**2/self.eta
        te = 0.25*self.radius**2/self.eta

        if time>t0 and time<te:
            p = pi-5

    def steady_state_flow(self):
        pass

    def pseudo_steady_state_flow(self):
        """solution with shape factor"""
        pass

    def cartesian_2D(self):
        """green's solution"""
        pass

    def cartesian_3D(self):
        """green's solution"""
        pass
    
        
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)