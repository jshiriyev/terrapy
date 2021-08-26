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

class singlephase():

    def __init__(self):
        pass
    
        
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)