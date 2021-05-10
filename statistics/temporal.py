# Standard library imports
import os
import sys

sys.path.append(os.getcwd())

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

from scipy import sparse as sps

# Local application imports

"""
bivariate analysis:
- findslope: calculates slope and intercept from linear regression
- correlation: correlation coefficient calculator

the functions below looks like more of a univariate analysis:
    
- ranking: ranks the given 1D numpy array (more of a function for now)
- bootstrap: bootstraps the given 1D numpy array
- interpolation: 1D dimensional analysis
"""

class item():
    """statistical item locating data and its time attachment"""

    def __init__(self,props,**kwargs):
        
        self.set_property(props,**kwargs)

    def set_property(self,props,time=None,dtime=1):
        """it creates best x,y,z values for the given properties"""
        
        ## it inputs props as dictionary and does not really do any check
        
        if type(props) is dict:
            ones = np.ones_like(np.array(list(props.values())[0]))
            for key,value in props.items():
                setattr(self,key,value.ravel())
        ##if props is not None:
            ##ones = np.ones_like(props)
            ##self.property = props.ravel()
        elif time is not None:
            ones = np.ones_like(time)
        else:
            return

        if time is None:
            try: self.time = (np.cumsum(ones,0)-1).ravel()*dtime
            except: self.time = ones.ravel()
        else: self.time = time.ravel()
        
if __name__ == "__main__":

    raman = bivariate("univariate.csv")

    print(raman.correlation('porosity','permeability'))
