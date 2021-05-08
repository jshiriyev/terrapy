# Standard library imports
import os
import sys

sys.path.append(os.getcwd())

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

# Local application imports
from data import item

class item():
    """statistical item locating data and its spatial and time attachment"""

    def __init__(self,props,**kwargs):
        
        self.set_property(props,**kwargs)

    def set_property(self,props,X=None,Y=None,Z=None,dX=1,dY=1,dZ=1):
        """it creates best x,y,z values for the given properties"""
        
        ## it inputs props as dictionary and does not really do any check
        
        if type(props) is dict:
            ones = np.ones_like(np.array(list(props.values())[0]))
            for key,value in props.items():
                setattr(self,key,value.ravel())
        ##if props is not None:
            ##ones = np.ones_like(props)
            ##self.property = props.ravel()
        elif X is not None:
            ones = np.ones_like(X)
        elif Y is not None:
            ones = np.ones_like(Y)
        elif Z is not None:
            ones = np.ones_like(Z)
        else:
            return

        if X is None:
            try: self.x = (np.cumsum(ones,0)-1).ravel()*dX
            except: self.x = ones.ravel()
        else: self.x = X.ravel()

        if Y is None:
            try: self.y = (np.cumsum(ones,1)-1).ravel()*dY
            except: self.y = ones.ravel()
        else: self.y = Y.ravel()

        if Z is None:
            try: self.z = (np.cumsum(ones,2)-1).ravel()*dZ
            except: self.z = ones.ravel()
        else: self.z = Z.ravel() 

class multivariate():

    def __init__(self):
        pass
        
if __name__ == "__main__":


