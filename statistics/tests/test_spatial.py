import os
import sys
import unittest

sys.path.append(os.path.dirname(os.getcwd()))

##import matplotlib.pyplot as plt
import numpy as np

from connectivity import variogram

from spatial import item
from spatial import estimation

class TestItem(unittest.TestCase):

    def test_reading_nparray(self):

        data = np.array([[100,11],[50,15],[35,21],[47,6]])
        
        krig = item(data)

        np.testing.assert_array_equal(data[:,0],krig.property[:,0])
        np.testing.assert_array_equal(data[:,1],krig.property[:,1])

class TestEstimation(unittest.TestCase):

    def test_exercise_peters(self):

        """
        Example 4.2 (Kriging) and 4.3 (Simulation) page 187, Peters Volume 1
        """

        x = np.array([2,4,6])
        y = np.array([30,50,20])

        V = variogram(y,X=x)

        V.type = 'exponential'
        V.nugget = 0
        V.sill = 100
        V.range = 10

        V.set_theoretical()

        X = np.array([1,2,3,4,5,6,7,8])
        Y = np.array([29.897,30.000,39.549,50.000,34.766,20.000,22.489,24.332])

        E = estimation(V,X=X)

        E.kriging_ordinary()

        np.testing.assert_array_almost_equal(Y,E.property,decimal=3)

    def test_exercise_pyrcz(self):
        pass

    def test_exercise_kriging(self):

        prop = np.array([0.25,0.43,0.56])

        X = np.array([600,400,800])
        Y = np.array([800,700,100])

        observation = variogram(prop,X=X,Y=Y)

        observation.type = 'spherical'
        observation.nugget = 0.0025*0
        observation.sill = 0.0025
        observation.range = 700

        observation.set_theoretical()

        E = estimation(observation,X=np.array([500]),Y=np.array([500]))

        E.mean = 0.38
        
        E.kriging_simple()
        
        ske1 = E.property
        skv1 = E.variance

        observation.range = 300
        
        E = estimation(observation,X=np.array([500]),Y=np.array([500]))

        E.mean = 0.38
        
        E.kriging_simple()
        
        ske2 = E.property
        skv2 = E.variance
        
        observation.range = 1100
        
        E = estimation(observation,X=np.array([500]),Y=np.array([500]))

        E.mean = 0.38
        
        E.kriging_simple()
        
        ske3 = E.property
        skv3 = E.variance

        observation.nugget = 0.0025*0.1
        observation.range = 700
        
        E = estimation(observation,X=np.array([500]),Y=np.array([500]))

        E.mean = 0.38
        
        E.kriging_simple()
        
        ske4 = E.property
        skv4 = E.variance
        
        observation.nugget = 0.0025*0.5
        
        E = estimation(observation,X=np.array([500]),Y=np.array([500]))

        E.mean = 0.38
        
        E.kriging_simple()
        
        ske5 = E.property
        skv5 = E.variance

        observation.nugget = 0.0025*0.9
        
        E = estimation(observation,X=np.array([500]),Y=np.array([500]))

        E.mean = 0.38
        
        E.kriging_simple()
        
        ske6 = E.property
        skv6 = E.variance
        
        np.testing.assert_almost_equal(ske1,0.4092,decimal=4)
        np.testing.assert_almost_equal(ske2,0.3855,decimal=4)
        np.testing.assert_almost_equal(ske3,0.4327,decimal=4)
        np.testing.assert_almost_equal(ske4,0.4024,decimal=4)
        np.testing.assert_almost_equal(ske5,0.3861,decimal=4)
        np.testing.assert_almost_equal(ske6,0.3802,decimal=4)

        np.testing.assert_almost_equal(skv1,0.0017,decimal=4)
        np.testing.assert_almost_equal(skv2,0.0025,decimal=4)
        np.testing.assert_almost_equal(skv3,0.0011,decimal=4)
        np.testing.assert_almost_equal(skv4,0.0019,decimal=4)
        np.testing.assert_almost_equal(skv5,0.0023,decimal=4)
        np.testing.assert_almost_equal(skv6,0.0025,decimal=4)

##unittest.main()

if __name__ == "__main__":

    A = TestEstimation()
    A.test_exercise_peters()

