import os
import sys
import unittest

sys.path.append(os.path.dirname(os.getcwd()))

##import matplotlib.pyplot as plt
import numpy as np

from connectivity import variogram

from connectivity import item
from spatial import kriging

class TestItem(unittest.TestCase):

    def test_reading_nparray(self):

        data = np.array([[100,11],[50,15],[35,21],[47,6]])
        
        data_item = item(data)

        np.testing.assert_array_equal(data.ravel(),data_item.property)
        
class TestEstimation(unittest.TestCase):

    def test_exercise_peters(self):

        """
        Example 4.2 (Kriging) and 4.3 (Simulation) page 187, Peters Volume 1
        """

        x = np.array([2,4,6])
        y = np.array([30,50,20])

        V = variogram(y,X=x)

        V.set_theoretical(V.distance,vtype='exponential',vsill=100,vrange=10,vnugget=0)

        X = np.array([1,2,3,4,5,6,7,8])
        Y = np.array([29.897,30.000,39.549,50.000,34.766,20.000,22.489,24.332])

        E = kriging(V,X=X)

        E.ordinary()

        np.testing.assert_array_almost_equal(Y,E.property,decimal=3)

##    def test_exercise_pyrcz(self):
##        pass

##    def test_exercise_kriging(self):
##
##        prop = np.array([0.25,0.43,0.56])
##
##        X = np.array([600,400,800])
##        Y = np.array([800,700,100])
##
##        V = variogram(prop,X=X,Y=Y)
##
##        V.set_theoretical(V.distance,vtype='spherical',vsill=0.0025,vrange=700,vnugget=0)
##
##        E = kriging(V,X=np.array([500]),Y=np.array([500]))
##
##        E.mean = 0.38
##        
##        E.simple()
##        
##        ske1 = E.property
##        skv1 = E.variance
##
##        V.range = 300
##        
##        E = kriging(V,X=np.array([500]),Y=np.array([500]))
##
##        E.mean = 0.38
##        
##        E.simple()
##        
##        ske2 = E.property
##        skv2 = E.variance
##        
##        V.range = 1100
##        
##        E = kriging(V,X=np.array([500]),Y=np.array([500]))
##
##        E.mean = 0.38
##        
##        E.simple()
##        
##        ske3 = E.property
##        skv3 = E.variance
##
##        V.nugget = 0.0025*0.1
##        V.range = 700
##        
##        E = kriging(V,X=np.array([500]),Y=np.array([500]))
##
##        E.mean = 0.38
##        
##        E.simple()
##        
##        ske4 = E.property
##        skv4 = E.variance
##        
##        V.nugget = 0.0025*0.5
##        
##        E = kriging(V,X=np.array([500]),Y=np.array([500]))
##
##        E.mean = 0.38
##        
##        E.simple()
##        
##        ske5 = E.property
##        skv5 = E.variance
##
##        V.nugget = 0.0025*0.9
##        
##        E = kriging(V,X=np.array([500]),Y=np.array([500]))
##
##        E.mean = 0.38
##        
##        E.simple()
##        
##        ske6 = E.property
##        skv6 = E.variance
##        
##        np.testing.assert_almost_equal(ske1,0.4092,decimal=4)
##        np.testing.assert_almost_equal(ske2,0.3855,decimal=4)
##        np.testing.assert_almost_equal(ske3,0.4327,decimal=4)
##        np.testing.assert_almost_equal(ske4,0.4024,decimal=4)
##        np.testing.assert_almost_equal(ske5,0.3861,decimal=4)
##        np.testing.assert_almost_equal(ske6,0.3802,decimal=4)
##
##        np.testing.assert_almost_equal(skv1,0.0017,decimal=4)
##        np.testing.assert_almost_equal(skv2,0.0025,decimal=4)
##        np.testing.assert_almost_equal(skv3,0.0011,decimal=4)
##        np.testing.assert_almost_equal(skv4,0.0019,decimal=4)
##        np.testing.assert_almost_equal(skv5,0.0023,decimal=4)
##        np.testing.assert_almost_equal(skv6,0.0025,decimal=4)

##unittest.main()

if __name__ == "__main__":

    A = TestEstimation()
    A.test_exercise_peters()

