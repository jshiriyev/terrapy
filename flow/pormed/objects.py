import matplotlib.pyplot as plt
import numpy as np

if __name__ == "__main__":
    import setup

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

class Core():

    def __init__(self):

        pass

class Formation():

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

class Fracture():

    def __init__(self):

        pass

class RegularCartesian():

    def __init__(self):

        pass

    def cartesian(self,length,grid_num):

        """
        length is a tuple with three entries for size in x,y,z direction
        for rectangular parallelepiped:
        """

        self.length_x = length[0]
        self.length_y = length[1]
        self.length_z = length[2]

        """
        grid_num is a tuple with three entries for discretization in x,y,z direction
        for rectangular parallelepiped:
        """

        self.num_x = grid_num[0]
        self.num_y = grid_num[1]
        self.num_z = grid_num[2]

        """
        self.num is a total number of grids for rectangular parallelepiped
        """
        self.num = self.num_x*self.num_y*self.num_z

        """
        self.id is a connectivity map and contain index of all grids
        their neighbours.
        """

        idx = np.arange(self.num)
        
        self.id = np.tile(idx,(7,1)).T

        self.id[idx.reshape(-1,self.num_x)[:,1:].ravel(),1] -= 1
        self.id[idx.reshape(-1,self.num_x)[:,:-1].ravel(),2] += 1
        self.id[idx.reshape(self.num_z,-1)[:,self.num_x:],3] -= self.num_x
        self.id[idx.reshape(self.num_z,-1)[:,:-self.num_x],4] += self.num_x
        self.id[idx.reshape(self.num_z,-1)[1:,:],5] -= self.num_x*self.num_y
        self.id[idx.reshape(self.num_z,-1)[:-1,:],6] += self.num_x*self.num_y

        """
        self.size is the size of grids in x,y,z direction
        """

        node_x = np.linspace(0,self.length_x,self.num_x+1)
        node_y = np.linspace(0,self.length_y,self.num_y+1)
        node_z = np.linspace(0,self.length_z,self.num_z+1)
        
        xsize = node_x[1:]-node_x[:-1]
        ysize = node_y[1:]-node_y[:-1]
        zsize = node_z[1:]-node_z[:-1]
        
        self.size = np.zeros((self.num,3))
        self.size[:,0] = np.tile(xsize,self.num_y*self.num_z)
        self.size[:,1] = np.tile(ysize.repeat(self.num_x),self.num_z)
        self.size[:,2] = zsize.repeat(self.num_x*self.num_y)

        """
        self.area is the area of three faces of grids in x,y,z direction
        """
        
        self.area = np.zeros((self.num,3))
        self.area[:,0] = self.size[:,1]*self.size[:,2]
        self.area[:,1] = self.size[:,2]*self.size[:,0]
        self.area[:,2] = self.size[:,0]*self.size[:,1]

        """
        self.volume is the volume of grids in x,y,z direction
        """

        self.volume = np.prod(self.size,axis=1)

        xcenter = node_x[:-1]+xsize/2
        ycenter = node_y[:-1]+ysize/2
        zcenter = node_z[:-1]+zsize/2

        """
        self.center is the x,y,z coordinate of the center of grids
        """
        
        self.center = np.zeros((self.num,3))
        self.center[:,0] = np.tile(xcenter,self.num_y*self.num_z)
        self.center[:,1] = np.tile(ycenter.repeat(self.num_x),self.num_z)
        self.center[:,2] = zcenter.repeat(self.num_x*self.num_y)

class RegularRadial():

    def __init__(self):

        pass

class IrregularCartesian():

    def __init__(self):

        pass
           
if __name__ == "__main__":

    import unittest

    from tests import test_porous_media

    unittest.main(test_porous_media)
