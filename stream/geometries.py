import numpy as np

if __name__ == "__main__":
    import setup

from stream.dataset import dataset

from stream.graphics import View3D

## BASIC GEOMETRIES

class Line(View3D):

    def __init__(self):

        pass

    def set_tracks(self,tracks):
        
        self.tracks = np.array(tracks)

    def set2Dview(self,axis,view='zy'):

        if view == 'yx':
            axis.plot(self.tracks[:,0],self.tracks[:,1])
            axis.set_xlabel("x-axis")
            axis.set_ylabel("y-axis")

        elif view == 'zy':
            axis.plot(self.tracks[:,1],self.tracks[:,2])
            axis.set_xlabel("y-axis")
            axis.set_ylabel("z-axis")
            axis.invert_yaxis()

        elif view == 'xz':
            axis.plot(self.tracks[:,2],self.tracks[:,0])
            axis.set_xlabel("z-axis")
            axis.set_ylabel("x-axis")

        elif view == 'xy':
            axis.plot(self.tracks[:,1],self.tracks[:,0])
            axis.set_xlabel("y-axis")
            axis.set_ylabel("x-axis")

        elif view == 'yz':
            axis.plot(self.tracks[:,2],self.tracks[:,1])
            axis.set_xlabel("z-axis")
            axis.set_ylabel("y-axis")

        elif view == 'zx':
            axis.plot(self.tracks[:,0],self.tracks[:,2])
            axis.set_xlabel("x-axis")
            axis.set_ylabel("z-axis")
            axis.invert_yaxis()

    def set3Dview(self,axis):

        axis.plot3D(*self.tracks.T)
        axis.set_xlabel("x-axis")
        axis.set_ylabel("y-axis")
        axis.set_zlabel("z-axis")
        axis.invert_zaxis()

class Rectangle(View3D):

    # it is a 2D object in 3D space

    def __init__(self,lengths=None,thickness=1):

        if lengths is not None:
            self.set_lengths(lengths)

        self.set_thickness(thickness)

        self.gridFlag = False

    def set_lengths(self,lengths):

        self.lengths = lengths
        self.aspect = lengths[0]/lengths[1]

        self.set_rectangle()

    def set_area(self,area,aspect=1):
        
        self.area = area

        length1 = np.sqrt(self.area*aspect)
        length2 = length1/aspect

        self.set_lengths(lengths=(length1,length2))

    def set_thickness(self,thickness):

        self.thickness = thickness

        if hasattr(self,"lengths"):
            self.set_rectangle()

    def set_rectangle(self):

        self.edge_vertices = np.zeros((4,2))

        self.edge_vertices[0,:] = (0,0)
        self.edge_vertices[1,:] = (self.lengths[0],0)
        self.edge_vertices[2,:] = (self.lengths[0],self.lengths[1])
        self.edge_vertices[3,:] = (0,self.lengths[1])

        indices = np.empty((4,2),dtype=int)

        indices[:,0] = (0,1,2,3)
        indices[:,1] = (1,2,3,0)
        
        x_aspects = self.edge_vertices[:,0][indices]
        y_aspects = self.edge_vertices[:,1][indices]

        self.boundaries = []

        for x_aspect,y_aspect in zip(x_aspects,y_aspects):
            self.boundaries.append(np.array([x_aspect,y_aspect]))

    def set_grids(self,grid_num):

        # grid_num must be a tuple or list with length equal to 2
        
        self.grid_num = grid_num

        self.grid_numtot = np.prod(self.grid_num)

        idx = np.arange(self.grid_numtot)
        
        self.grid_indices = np.tile(idx,(7,1)).T

        self.grid_indices[idx.reshape(-1,self.grid_num[0])[:,1:].ravel(),1] -= 1
        self.grid_indices[idx.reshape(-1,self.grid_num[0])[:,:-1].ravel(),2] += 1
        self.grid_indices[idx.reshape(1,-1)[:,self.grid_num[0]:],3] -= self.grid_num[0]
        self.grid_indices[idx.reshape(1,-1)[:,:-self.grid_num[0]],4] += self.grid_num[0]
        self.grid_indices[idx.reshape(1,-1)[1:,:],5] -= self.grid_num[0]*self.grid_num[1]
        self.grid_indices[idx.reshape(1,-1)[:-1,:],6] += self.grid_num[0]*self.grid_num[1]

        self.grid_hasxmin = ~(self.grid_indices[:,0]==self.grid_indices[:,1])
        self.grid_hasxmax = ~(self.grid_indices[:,0]==self.grid_indices[:,2])
        self.grid_hasymin = ~(self.grid_indices[:,0]==self.grid_indices[:,3])
        self.grid_hasymax = ~(self.grid_indices[:,0]==self.grid_indices[:,4])
        self.grid_haszmin = ~(self.grid_indices[:,0]==self.grid_indices[:,5])
        self.grid_haszmax = ~(self.grid_indices[:,0]==self.grid_indices[:,6])

        self.grid_xnodes = np.linspace(0,self.lengths[0],self.grid_num[0]+1)
        self.grid_ynodes = np.linspace(0,self.lengths[1],self.grid_num[1]+1)
        self.grid_znodes = np.linspace(-self.thickness/2,self.thickness/2,2)
        
        xsize = self.grid_xnodes[1:]-self.grid_xnodes[:-1]
        ysize = self.grid_ynodes[1:]-self.grid_ynodes[:-1]
        zsize = self.grid_znodes[1:]-self.grid_znodes[:-1]
        
        self.grid_sizes = np.zeros((self.grid_numtot,3))
        self.grid_sizes[:,0] = np.tile(xsize,self.grid_num[1])
        self.grid_sizes[:,1] = ysize.repeat(self.grid_num[0])
        self.grid_sizes[:,2] = zsize.repeat(self.grid_num[0]*self.grid_num[1])

        self.grid_areas = np.zeros((self.grid_numtot,3))
        self.grid_areas[:,0] = self.grid_sizes[:,1]*self.thickness
        self.grid_areas[:,1] = self.grid_sizes[:,0]*self.thickness
        self.grid_areas[:,2] = self.grid_sizes[:,0]*self.grid_sizes[:,1]

        self.grid_volumes = np.prod(self.grid_sizes,axis=1)

        xcenter = self.grid_xnodes[:-1]+xsize/2
        ycenter = self.grid_ynodes[:-1]+ysize/2
        zcenter = self.grid_znodes[:-1]+zsize/2
        
        self.grid_centers = np.zeros((self.grid_numtot,3))
        self.grid_centers[:,0] = np.tile(xcenter,self.grid_num[1])
        self.grid_centers[:,1] = ycenter.repeat(self.grid_num[0])
        self.grid_centers[:,2] = zcenter.repeat(self.grid_num[0]*self.grid_num[1])

        self.gridFlag = True     # it is a crucial property to know whether the geometry is gridded or not

    def plot(self,axis,showVertices=True,showBounds=True,showGridEdges=True,showGridCenters=True):

        if showVertices:
            axis.scatter(*self.edge_vertices.T)

        if showBounds:
            for line in self.boundaries:
                axis.plot(*line,color='grey')

        if showGridEdges:
            for node in self.grid_xnodes[1:-1]:
                axis.vlines(x=node,ymin=0,ymax=self.lengths[1],linestyle="--")
            for node in self.grid_ynodes[1:-1]:
                axis.hlines(y=node,xmin=0,xmax=self.lengths[0],linestyle="--")

        if showGridCenters:
            axis.scatter(*self.grid_centers.T[:2,:])

        axis.set_box_aspect(self.lengths[1]/self.lengths[0])

class Ellipse(View3D):

    # This class is supposed to create 2-D surface in 3-D space

    # origin: location of the center of ellipse
    # lengths: (major radius, minor radius)
    # thickness: thickness of the ellipse
    # rinner: inner radius
    # dip_angle: 

    # lamda: node spacing, radius ratio

    def __init__(self,thickness=1,inner_radii):

        numverts = 50

        thetas = np.linspace(0,2*np.pi,numverts+1)[:-1]

        self.edge_vertices = np.zeros((2*numverts,3))

        self.edge_vertices[:,0] = np.tile(self.radii[0]/2*np.cos(thetas),2)+self.radii[0]/2
        self.edge_vertices[:,1] = np.tile(self.radii[1]/2*np.sin(thetas),2)+self.radii[1]/2
        self.edge_vertices[:,2] = np.append(np.zeros(numverts),self.thickness*np.ones(numverts))

        indices = np.empty((2*numverts,2),dtype=int)

        vertices_0 = np.arange(numverts)
        vertices_1 = np.append(np.arange(numverts)[1:],0)

        indices[:,0] = np.append(vertices_0,vertices_0+numverts)
        indices[:,1] = np.append(vertices_1,vertices_1+numverts)

        x_aspects = self.edge_vertices[:,0][indices]
        y_aspects = self.edge_vertices[:,1][indices]
        z_aspects = self.edge_vertices[:,2][indices]

        self.boundaries = []

        for x_aspect,y_aspect,z_aspect in zip(x_aspects,y_aspects,z_aspects):
            self.boundaries.append(np.array([x_aspect,y_aspect,z_aspect]))

        self.gridFlag = False

    def set_origin(self,origin=(0,0,0)):

        self.origin = origin # origing has not been implemented yet

    def set_radii(self,radii=(1,1)):

        if type(radii) == int:
            self.radii = (float(radii),float(radii))
        elif type(radii) == float:
            self.radii = (radii,radii)
        elif type(radii) == str:
            raise ValueError('Radii must be defined as a number or tuple.')
        elif len(radii) == 2:
            self.radii = radii

        self.radiusMajor = max(radii)
        self.radiusMinor = min(radii)

    def set_area(self,area,aspect=1):
        
        self.area = area

        radius1 = np.sqrt(area*aspect/np.pi)
        radius2 = radius1/aspect

        self.set_radii(radii=(radius1,radius2))

    def set_inradii(self,inradii=(0,0)):

        if type(inradii) == int:
            self.inradii = (float(inradii),float(inradii))
        elif type(inradii) == float:
            self.inradii = (inradii,inradii)
        elif type(inradii) == str:
            raise ValueError('Inner radii must be defined as a number or tuple.')
        elif len(inradii) == 2:
            self.inradii = inradii

        self.inradiusMajor = max(inradii)
        self.inradiusMinor = min(inradii)

    def set_thickness(self,thickness=1):

        self.thickness = thickness

    def grid(self,lamda):

        self.lamda = lamda

    def plot(self,axis,showVertices=False,showBounds=True,showGridEdges=False,showGridCenters=False):

        if showVertices:
            axis.scatter(*self.edge_vertices.T)

        if showBounds:
            for line in self.boundaries:
                axis.plot(*line,color='grey')

        if showGridEdges:
            for node in self.grid_xnodes[1:-1]:
                axis.vlines(x=node,ymin=0,ymax=self.lengths[1],linestyle="--")
            for node in self.grid_ynodes[1:-1]:
                axis.hlines(y=node,xmin=0,xmax=self.lengths[0],linestyle="--")

        # axis.set_box_aspect(self.radii[1]/self.radii[0])

        if showGridCenters:
            axis.scatter(*self.grid_centers.T)

class Cuboid(View3D):

    """
    For rectangular parallelepiped, dimensions is a tuple
    with three entries for sizes in x,y,z direction.
    """

    def __init__(self,lengths):

        self.lengths = lengths

        self.edge_vertices = np.zeros((8,3))

        self.edge_vertices[0,:] = (0,0,0)
        self.edge_vertices[1,:] = (self.lengths[0],0,0)
        self.edge_vertices[2,:] = (self.lengths[0],self.lengths[1],0)
        self.edge_vertices[3,:] = (0,self.lengths[1],0)

        self.edge_vertices[4,:] = (0,0,self.lengths[2])
        self.edge_vertices[5,:] = (self.lengths[0],0,self.lengths[2])
        self.edge_vertices[6,:] = (self.lengths[0],self.lengths[1],self.lengths[2])
        self.edge_vertices[7,:] = (0,self.lengths[1],self.lengths[2])

        indices = np.empty((12,2),dtype=int)

        indices[:,0] = (0,1,2,3,0,1,2,3,4,5,6,7)
        indices[:,1] = (1,2,3,0,4,5,6,7,5,6,7,4)
        
        x_aspects = self.edge_vertices[:,0][indices]
        y_aspects = self.edge_vertices[:,1][indices]
        z_aspects = self.edge_vertices[:,2][indices]

        self.boundaries = []

        for x_aspect,y_aspect,z_aspect in zip(x_aspects,y_aspects,z_aspects):
            self.boundaries.append(np.array([x_aspect,y_aspect,z_aspect]))

        self.gridFlag = False

    def grid(self,grid_num):

        """
        self.grid_num        : number of grids in x, y, z directions
        self.grid_numtot     : number of totla grids 
        self.grid_indices    : connectivity map containing index of all grids and their neighbours.
        self.grid_sizes      : size of grids in all directions.
        self.grid_areas      : area of all faces
        self.grid_volumes    : volume of grids
        self.grid_centers    : coordinates of the center of grids
        """
        
        self.grid_num = grid_num

        self.grid_numtot = np.prod(self.grid_num)

        idx = np.arange(self.grid_numtot)
        
        self.grid_indices = np.tile(idx,(7,1)).T

        self.grid_indices[idx.reshape(-1,self.grid_num[0])[:,1:].ravel(),1] -= 1
        self.grid_indices[idx.reshape(-1,self.grid_num[0])[:,:-1].ravel(),2] += 1
        self.grid_indices[idx.reshape(self.grid_num[2],-1)[:,self.grid_num[0]:],3] -= self.grid_num[0]
        self.grid_indices[idx.reshape(self.grid_num[2],-1)[:,:-self.grid_num[0]],4] += self.grid_num[0]
        self.grid_indices[idx.reshape(self.grid_num[2],-1)[1:,:],5] -= self.grid_num[0]*self.grid_num[1]
        self.grid_indices[idx.reshape(self.grid_num[2],-1)[:-1,:],6] += self.grid_num[0]*self.grid_num[1]

        self.grid_hasxmin = ~(self.grid_indices[:,0]==self.grid_indices[:,1])
        self.grid_hasxmax = ~(self.grid_indices[:,0]==self.grid_indices[:,2])
        self.grid_hasymin = ~(self.grid_indices[:,0]==self.grid_indices[:,3])
        self.grid_hasymax = ~(self.grid_indices[:,0]==self.grid_indices[:,4])
        self.grid_haszmin = ~(self.grid_indices[:,0]==self.grid_indices[:,5])
        self.grid_haszmax = ~(self.grid_indices[:,0]==self.grid_indices[:,6])

        self.grid_xnodes = np.linspace(0,self.lengths[0],self.grid_num[0]+1)
        self.grid_ynodes = np.linspace(0,self.lengths[1],self.grid_num[1]+1)
        self.grid_znodes = np.linspace(0,self.lengths[2],self.grid_num[2]+1)
        
        xsize = self.grid_xnodes[1:]-self.grid_xnodes[:-1]
        ysize = self.grid_ynodes[1:]-self.grid_ynodes[:-1]
        zsize = self.grid_znodes[1:]-self.grid_znodes[:-1]
        
        self.grid_sizes = np.zeros((self.grid_numtot,3))
        self.grid_sizes[:,0] = np.tile(xsize,self.grid_num[1]*self.grid_num[2])
        self.grid_sizes[:,1] = np.tile(ysize.repeat(self.grid_num[0]),self.grid_num[2])
        self.grid_sizes[:,2] = zsize.repeat(self.grid_num[0]*self.grid_num[1])

        self.grid_areas = np.zeros((self.grid_numtot,3))
        self.grid_areas[:,0] = self.grid_sizes[:,1]*self.grid_sizes[:,2]
        self.grid_areas[:,1] = self.grid_sizes[:,2]*self.grid_sizes[:,0]
        self.grid_areas[:,2] = self.grid_sizes[:,0]*self.grid_sizes[:,1]

        self.grid_volumes = np.prod(self.grid_sizes,axis=1)

        xcenter = self.grid_xnodes[:-1]+xsize/2
        ycenter = self.grid_ynodes[:-1]+ysize/2
        zcenter = self.grid_znodes[:-1]+zsize/2
        
        self.grid_centers = np.zeros((self.grid_numtot,3))
        self.grid_centers[:,0] = np.tile(xcenter,self.grid_num[1]*self.grid_num[2])
        self.grid_centers[:,1] = np.tile(ycenter.repeat(self.grid_num[0]),self.grid_num[2])
        self.grid_centers[:,2] = zcenter.repeat(self.grid_num[0]*self.grid_num[1])

        self.gridFlag = True

    def plot(self):

        pass

class Cylinder(View3D):

    """
    For cylindrical disk, dimensions is a tuple with two entries for sizes in r,z direction
    """

    def __init__(self,lengths):

        self.lengths = lengths

        numverts = 50

        thetas = np.linspace(0,2*np.pi,numverts+1)[:-1]

        self.edge_vertices = np.zeros((2*numverts,3))

        self.edge_vertices[:,0] = np.tile(self.lengths[0]/2*np.cos(thetas),2)+self.lengths[0]/2
        self.edge_vertices[:,1] = np.tile(self.lengths[1]/2*np.sin(thetas),2)+self.lengths[1]/2
        self.edge_vertices[:,2] = np.append(np.zeros(numverts),self.lengths[2]*np.ones(numverts))

        indices = np.empty((2*numverts,2),dtype=int)

        vertices_0 = np.arange(numverts)
        vertices_1 = np.append(np.arange(numverts)[1:],0)

        indices[:,0] = np.append(vertices_0,vertices_0+numverts)
        indices[:,1] = np.append(vertices_1,vertices_1+numverts)

        x_aspects = self.edge_vertices[:,0][indices]
        y_aspects = self.edge_vertices[:,1][indices]
        z_aspects = self.edge_vertices[:,2][indices]

        self.boundaries = []

        for x_aspect,y_aspect,z_aspect in zip(x_aspects,y_aspects,z_aspects):
            self.boundaries.append(np.array([x_aspect,y_aspect,z_aspect]))

    def grid(self):
        pass

    def plot(self):
        pass

if __name__ == "__main__":

    pass