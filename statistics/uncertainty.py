"""Standard library imports"""
import os
import sys

"""Third party imports"""
import matplotlib.pyplot as plt
import numpy as np

from scipy.stats import norm

"""
univariate classes uses a single property data item with optional
spatial information and includes following ananlysis:

    - non-spatial similarity measurements (heterogeneity)
    - non-spatial estimation calculations (uncertainty)
    
    - spatial continuity measurements (variogram)
    - spatial estimation calculations (kriging, gaussian simulation)
    
"""

class uncertainty():

    def __init__(self,prop,**kwargs):

        self.set_property(prop,**kwargs)

    def jacknife(self):

        pass

    def bootstrap(self,X,Nrealization):

        """
        X should be an array with one dimension,
        The size of X defines number of rows, and
        Nrealization specifies number of columns of an array
        created for bootstrap analyzes
        """
        
        N = X.size
        
        idx = np.random.randint(0,N,(N,Nrealization))
        
        return idx

if __name__ == "__main__":

    pass
