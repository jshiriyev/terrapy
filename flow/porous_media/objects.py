import io
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

# % The fracture segment is defined as a plane joining two node points
# % (point1 and point2). The heigth of fracture plane is taken the same
# % as reservoir thickness (it is not diffcult to model shorter planes).
# % z-coordinate of the points is given as the reservoir depth.

# fileDir
# nodeCoord
# map
# permeability
# width
# fracID
# nodeID
# numAfrac
# numAnode
# conductivity
# point1
# point2
# Length
# areatoreservoir
# areatofracture
# volume
# center
# signX
# signY
# azimuth

# fileDir
# Length
# xLength
# yLength
# zLength
# porosity
# permeability
# xPermeability
# yPermeability
# zPermeability
# initPressure
# diffusivity
# xDiffusivity
# yDiffusivity
# zDiffusivity
# isotropic
# anisotropic
# rockCompressibility
# oilViscosity
# oilFVF      % formation volume factor
# oilCompressibility
# totCompressibility

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
            
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)
