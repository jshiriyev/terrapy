import os
import sys
import unittest

sys.path.append(os.path.dirname(os.getcwd()))

##import matplotlib.pyplot as plt
import numpy as np

from setitem import reader

class TestSetItem(unittest.TestCase):

    def test_reading_nparray(self):

        data = np.array([[100,11],[50,15],[35,21],[47,6]])
    
        name = np.array(['permeability','porosity'])

        krig = reader()

        krig.set_property(data,header=name)

        np.testing.assert_array_equal(data[:,0],krig.permeability)
        np.testing.assert_array_equal(data[:,1],krig.porosity)

##unittest.main()

