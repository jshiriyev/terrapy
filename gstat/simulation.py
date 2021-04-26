# Standard library imports
import copy
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

# Local application imports
from kriging import kriging

"""
This is a PNGE 436 - Reservoir Characterization Class Module including

    - sequential gaussian simulation
    
"""

class sgs():

    def __init__(self,obs):

        self.X = copy.deepcopy(obs.X)
        self.Y = copy.deepcopy(obs.Y)
        self.Z = copy.deepcopy(obs.Z)
        
        self.F = copy.deepcopy(obs.F)

        self.type = copy.deepcopy(obs.type)
        self.nugget = copy.deepcopy(obs.nugget)
        self.sill = copy.deepcopy(obs.sill)
        self.range = copy.deepcopy(obs.range)

    def update(self,node):

        self.X = np.append(self.X,node.X)
        self.Y = np.append(self.Y,node.Y)
        self.Z = np.append(self.Z,node.Z)

        f_m = node.F
        f_v = node.F_variance

        f_r = np.random.normal(f_m,np.sqrt(f_v))

        self.F = np.append(self.F,f_r)

    def simulate(self,est):

        class node: pass
        class nodes: pass

        nodes.X = copy.deepcopy(est.X)
        nodes.Y = copy.deepcopy(est.Y)
        nodes.Z = copy.deepcopy(est.Z)

        n = self.X.size
        N = nodes.X.size

        while N>0:

            randint = np.random.randint(0,N)
            print(randint)

            node.X = nodes.X[randint]
            node.Y = nodes.Y[randint]
            node.Z = nodes.Z[randint]

            krig = kriging(self)
            krig.ordinary(node)
            
            self.update(node)
            
            nodes.X = np.delete(nodes.X,randint)
            nodes.Y = np.delete(nodes.Y,randint)
            nodes.Z = np.delete(nodes.Z,randint)
            
            N = nodes.X.size

        est.X = self.X[n:]
        est.Y = self.Y[n:]
        est.Z = self.Z[n:]
        est.F = self.F[n:]

        return 

if __name__ == "__main__":

    class observation: pass
    class estimation: pass

    observation.X = np.array([1,2,4,5,6])
    observation.Y = np.array([2,10,4,6,14])
    observation.Z = np.array([1,1,1,1,1])
    
    observation.F = np.array([15,30,25,18,30])

    observation.type = 'spherical'
    observation.nugget = 0
    observation.sill = 60
    observation.range = 8

    estimation.X = np.array([2,4]) #1,3,5,7,
    estimation.Y = np.array([6,12]) #1,1,1,1,
    estimation.Z = np.array([1,1]) #1,1,1,1,

    class obs1(observation): pass
    class obs2(observation): pass

    class est1(estimation): pass
    class est2(estimation): pass
    
    smlt = sgs(obs1)
    smlt.simulate(est1)

    krig = kriging(obs2)
    krig.ordinary(est2)

    obs1.X = np.append(obs1.X,est1.X)
    obs1.F = np.append(obs1.F,est1.F)
    
    obs2.X = np.append(obs2.X,est2.X)
    obs2.F = np.append(obs2.F,est2.F)

##    idx1 = np.argsort(obs1.X)
##    idx2 = np.argsort(obs2.X)
##    
##    plt.plot(obs1.X[idx1],obs1.F[idx1])
##    plt.plot(obs2.X[idx2],obs2.F[idx2])
##    plt.scatter(observation.X,observation.F,c='k')
##
##    plt.xlim([0,9])
##    plt.ylim([0,60])
##
##    plt.legend(('simulation','kriging','given data'))
##
##    plt.show()
    
