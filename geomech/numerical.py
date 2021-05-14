"""1-Standard Library imports"""
import os
import sys

"""2-Third Party Imports"""
import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse import csr_matrix as csr
from scipy.sparse.linalg import spsolve

class finite_element():

    def __init__(self):

        pass

    def cartesian(self,length,edgespacing):

        num_x = int(np.ceil(length[0]/edgespacing))
        num_y = int(np.ceil(length[1]/edgespacing))

        xnode = np.linspace(0,length[0],num_x+1)

        xaxis = np.zeros((2*num_y+1,num_x+1))

        xaxis[0::2,:] = xnode
        xaxis[1::2,:] = xnode+length[0]/(2*num_x)
        
        ynode = np.linspace(0,length[1],2*num_y+1)

        yaxis = ynode.repeat(num_x+1)

        self.node_num = (num_x+1)*(num_y+1)+num_x*num_y

        self.node = np.zeros((self.node_num,3))

        idx_del = np.arange(2*num_x+1,self.node_num,2*(num_x+1))
        
        self.node[:,0] = np.delete(xaxis.ravel(),idx_del)
        self.node[:,1] = np.delete(yaxis.ravel(),idx_del)

        self.elm_num = 4*num_x*num_y

        ## self.id = np.zeros((self.elm_num,4))

        idx = np.arange(num_x)

        idy = np.arange(num_y)[:,np.newaxis]*(2*num_x+1)

        c0 = (idx+1*num_x+1+idy).ravel()
        c1 = (idx+idy).ravel()
        c2 = (idx+1+idy).ravel()
        c3 = (idx+2*num_x+1+idy).ravel()
        c4 = (idx+2*num_x+2+idy).ravel()

        t0 = np.concatenate((c1,c2,c0,c1)).reshape((4,-1)).T
        t1 = np.concatenate((c1,c0,c3,c1)).reshape((4,-1)).T
        t2 = np.concatenate((c2,c4,c0,c0)).reshape((4,-1)).T
        t3 = np.concatenate((c0,c4,c3,c0)).reshape((4,-1)).T

        self.id = np.concatenate((t0,t1,t2,t3))

##        self.center
##        self.size
##        self.area
        
    def radial(self,length,edge_node_num):

        """cylindrical shape"""
        
        """
        inner radius
        external radius

        inner edge size
        external edge size
        """

        r_inner = length[0]
        r_outer = length[1]

        thickness = length[2]

        theta = np.linspace(0,2*np.pi,edge_node_num,endpoint=False)

        xaxis0 = r_inner*np.cos(theta)
        yaxis0 = r_inner*np.sin(theta)

        inn_node = np.concatenate((xaxis0,yaxis0)).reshape((2,-1)).T

        xaxis1 = r_outer*np.cos(theta)
        yaxis1 = r_outer*np.sin(theta)

        out_node = np.concatenate((xaxis1,yaxis1)).reshape((2,-1)).T

        self.node = np.concatenate((inn_node,out_node))
        
if __name__ == "__main__":

##    """3-Local Application Imports"""
##
##    from tests import test_mesh_rectangular_domain
##
##    test_mesh_rectangular_domain

    elm = finite_element()

    rinner = 1
    router = 5

    elm.radial((rinner,router,0),12)

    plt.plot(rinner*np.cos(np.linspace(0,2*np.pi,200)),
             rinner*np.sin(np.linspace(0,2*np.pi,200)))

    plt.plot(router*np.cos(np.linspace(0,2*np.pi,200)),
             router*np.sin(np.linspace(0,2*np.pi,200)))

    plt.plot(elm.node[:,0],elm.node[:,1],'.')

    plt.show()
