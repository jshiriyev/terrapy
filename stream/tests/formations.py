import unittest

import numpy as np

if __name__ == "__main__":
    import setup

from interfaces.items import Formation

class TestSimpleFormation(unittest.TestCase):

    def rectangular(self):

        res = Formation()

        res.set_dimensions(dimensions=(10,10,10))

        res.discretize((10,1,1))

        res.center[:,0]
        res.center[:,1]
        res.center[:,2]

    def cylindrical(self):

        pass

class TestRegularCartesianMesh(unittest.TestCase):

    def mesh1D(self):

        pass

    def mesh2D(self):
        
        grids = RegularCartesian()

        num_x = 5
        num_y = 6

        grids.cartesian((5,6,2),(num_x,num_y,1))

        layer = np.arange(grids.num_x*grids.num_y)

        ##plt.scatter(grids.center[layer,0],grids.center[layer,1],c='r')
        ##
        ##for i in np.arange(grids.size[0,0],grids.length_x,grids.size[0,0]):
        ##    plt.axvline(i,0,grids.length_y,c='k',linestyle='--')
        ##
        ##for j in np.arange(grids.size[0,1],grids.length_y,grids.size[0,1]):
        ##    plt.axhline(j,0,grids.length_x,c='k',linestyle='--')
        ##
        ##plt.xlim([0,grids.length_x])
        ##plt.ylim([0,grids.length_y])
        ##
        ##plt.xlabel('x-axis')
        ##plt.ylabel('y-axis')
        ##
        ##for idx in grids.id[layer,0]:
        ##    plt.annotate(idx,(grids.center[idx,0]+0.05,grids.center[idx,1]+0.05))
        ##
        ##plt.show()

    def mesh3D(self):

        grids = RegularCartesian()

        num_x = 4
        num_y = 3
        num_z = 2

        grids.cartesian((num_x*1.,num_y*1.,num_z*1.),
                        (num_x,num_y,num_z))

        X = grids.center[:,0].reshape(num_x,num_y,num_z)
        Y = grids.center[:,1].reshape(num_x,num_y,num_z)
        Z = grids.center[:,2].reshape(num_x,num_y,num_z)

        ##fig = plt.figure()
        ##
        ##ax = fig.add_subplot(111, projection='3d')
        ##
        ##ax.scatter(X,Y,Z,alpha=0.5,c='r')
        ##
        ##ax.set_xlim3d([0,grids.length_x])
        ##ax.set_ylim3d([0,grids.length_y])
        ##ax.set_zlim3d([0,grids.length_z])
        ##
        ##ax.set_xlabel('x-axis')
        ##ax.set_ylabel('y-axis')
        ##ax.set_zlabel('z-axis')
        ####fig.colorbar(scat, shrink=0.5, aspect=5)
        ##
        ##plt.show()

class TestRegularRadialMesh(unittest.TestCase):

    def mesh1D(self):
        pass

    def mesh2D(self):
        pass

    def mesh3D(self):
        pass

class TestUnstructuredMesh(unittest.TestCase):

    def mesh1D(self):
        pass

    def mesh2D(self):
        pass

    def mesh3D(self):
        pass
                       
if __name__ == "__main__":

    unittest.main()
