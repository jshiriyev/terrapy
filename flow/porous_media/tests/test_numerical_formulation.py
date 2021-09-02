import os
import sys
import unittest

sys.path.append(os.path.dirname(os.getcwd()))

import numpy as np

from porous_media import core_singlephase
from porous_media import core_multiphase
from porous_media import formation_singlephase
from porous_media import computational_singlephase

class TestFiniteDifference(unittest.TestCase):
    
    def laplace1D(self):
        
        bnd1 = (1,-1,50)
        bnd2 = (1,0,100)

        grids = computational_singlephase()

        grids.cartesian((7,1,1),(7,1,1))

        grids.initialize()

        grids.transmissibility()

        grids.central(order=2)

        grids.implement_bc(b_xmin=bnd1,b_xmax=bnd2)

        ##analytical = core_singlephase()
        ##analytical.cartesian_poisson_1D((0,7),(bnd1,bnd2),0)

        grids.solve()

    def laplace2D(self):

        grids = computational_singlephase()

        num_x = 50
        num_y = 50

        grids.cartesian((20,10,10),(num_x,num_y,1))

        grids.initialize()

        grids.transmissibility()

        grids.central(order=2)

        grids.implement_bc(b_xmin=(1,0,0),
                           b_xmax=(0,1,0),
                           b_ymin=(1,0,0),
                           b_ymax=(1,0,100))

        grids.solve()

        ##X = grids.center[:,0].reshape(num_x,num_y)
        ##Y = grids.center[:,1].reshape(num_x,num_y)
        ##
        ##Z = grids.unknown.reshape(num_x,num_y)
        ##
        ##plt.contourf(X,Y,Z,alpha=1,cmap=cm.PuBu)
        ##plt.colorbar()
        ##
        ##plt.title('Pressure Map',fontsize=14)
        ##plt.xlabel('x-axis',fontsize=14)
        ##plt.ylabel('y-axis',fontsize=14)
        ##
        ##plt.xlim([0,20])
        ##plt.ylim([0,10])
        ##
        ##plt.show()

    def laplace3D(self):

        solver = computational_singlephase()

        num_x = 25
        num_y = 25
        num_z = 10

        solver.cartesian((num_x*1.,num_y*1.,num_z*1.),(num_x,num_y,num_z))

        solver.initialize()

        solver.transmissibility()

        solver.central()

        solver.implement_bc(b_xmin=(1,0,0),
                            b_xmax=(0,1,0),
                            b_ymin=(1,0,0),
                            b_ymax=(1,0,100),
                            b_zmin=(0,1,0),
                            b_zmax=(0,1,0))

        solver.solve()

        ##X = solver.center[:,0].reshape(num_x,num_y,num_z)
        ##Y = solver.center[:,1].reshape(num_x,num_y,num_z)
        ##Z = solver.center[:,2].reshape(num_x,num_y,num_z)
        ##
        ####P = solver.unknown.reshape(num_x,num_y,num_z)
        ##
        ##fig = plt.figure()
        ##
        ##ax = fig.add_subplot(111, projection='3d')
        ##
        ##scatter = ax.scatter(X,Y,Z,alpha=0.5,c=solver.unknown)
        ##
        ##fig.colorbar(scatter,shrink=0.5,aspect=5)
        ##
        ##ax.set_xlim3d([0,solver.length_x])
        ##ax.set_ylim3d([0,solver.length_y])
        ##ax.set_zlim3d([0,solver.length_z])
        ##
        ##ax.set_xlabel('x-axis')
        ##ax.set_ylabel('y-axis')
        ##ax.set_zlabel('z-axis')
        ##
        ##plt.show()

    def poisson1D(self):
        
        """Start Input"""
        xN0 = 0
        xN1 = 7
        beta = 10
        bL = (1,0,200)
        bU = (0,1,50)
        """End Input"""

        grids = computational_singlephase()

        grids.cartesian((xN1,1,1),(70,1,1))

        grids.initialize()

        grids.transmissibility()

        grids.central(order=2)

        grids.implement_bc(bL,bU)

        grids.solve(rhs=beta)

        ##analytical = diffusivity()
        ##
        ##analytical.cartesian_poisson_1D((xN0,xN1),(bL,bU),beta)
        
        ##plt.scatter(grids.center[:,0],grids.unknown,c='r',label='Finite Difference')
        ##
        ####plt.plot(analytical.x,analytical.u,'k',label='Analytical')
        ##
        ##plt.xlabel('x-axis')
        ##plt.ylabel('pressure')
        ##
        ##plt.legend(bbox_to_anchor=(0., 1.02, 1., .102),
        ##           loc='lower left',ncol=2, mode="expand", borderaxespad=0.)
        ##
        ##plt.xlim([0,7])
        ####plt.ylim([0,None])
        ##
        ##plt.show()

    def poisson2D(self):

        solver = computational_singlephase()

        num_x = 5
        num_y = 5

        solver.cartesian((1.,1.,1.),(num_x,num_y,1))

        solver.initialize()

        solver.transmissibility()

        solver.central()

        solver.implement_bc(b_xmin=(1,0,0),
                            b_xmax=(1,0,0),
                            b_ymin=(1,0,10),
                            b_ymax=(1,1,5))

        solver.solve(rhs=solver.center[:,0]**2)

        X = solver.center[:,0].reshape(num_x,num_y)
        Y = solver.center[:,1].reshape(num_x,num_y)

        P = solver.unknown.reshape(num_x,num_y)

        ##plt.contourf(X,Y,P,alpha=1,cmap=cm.PuBu)
        ##plt.colorbar()
        ##
        ##plt.title('Pressure Map',fontsize=14)
        ##plt.xlabel('x-axis',fontsize=14)
        ##plt.ylabel('y-axis',fontsize=14)
        ##
        ##plt.xlim([0,1])
        ##plt.ylim([0,1])
        ##
        ##plt.show()

    def poisson3D(self):

        pass

    def singlephase(self):
        
        """MAIN INPUT"""

        L = 10#m

        p0 = 0#psi
        pL = 1000#psi

        bnd1 = (1,0,p0)
        bnd2 = (1,0,pL)

        permeability = 100#mD

        porosity = 0.2
        viscosity = 1#cp
        compressibility = 1e-5#1/psi

        time = np.array([1,10,20,40,100]) #these are seconds

        """UNIT CONVERSION"""

        p0 *= 6894.76
        pL *= 6894.76

        bnd1 = list(bnd1)
        bnd2 = list(bnd2)

        bnd1[2] *= 6894.76
        bnd2[2] *= 6894.76

        permeability *= 9.8692326671601e-16

        viscosity *= 0.001
        compressibility /= 6894.76

        """ANALYTICAL CALL"""

        eta = (permeability)/(porosity*viscosity*compressibility)

        analytical = core_singlephase(L,1,porosity,permeability,compressibility)

        analytical.set_hydraulic_diffusivity(eta)

        analytical.cartesian_1D((0,L),(bnd1,bnd2),time)

        """NUMERICAL CALL"""

        grids = computational_singlephase()

        grids.cartesian((L,1,1),(20,1,1))

        grids.initialize(permeability=permeability,
                         porosity=porosity,
                         viscosity=viscosity,
                         compressibility=compressibility,
                         timetot=time[-1],
                         timestep=0.01,
                         pressure=pL)

        grids.transmissibility()

        grids.central(order=2)

        grids.implement_bc(b_xmin=bnd1,b_xmax=bnd2)

        grids.solve()

        """PLOTTING"""

        ##for i,t in enumerate(time):
        ##    plt.plot(analytical.x.flatten(),analytical.pressure[i,:]/6894.76,'k',zorder=1)
        ##    plt.plot(grids.center[:,0],grids.pressure[:,10000]/6894.76,'o',c='r',markerfacecolor="None",zorder=2)
        ##
        ##plt.xlabel('x-axis')
        ##plt.ylabel('pressure [psi]')
        ##
        ##yaxis = 0.75*(p0+pL)/(6894.76)
        ##xaxis = analytical.x.flatten()[
        ##    np.argmin(np.abs(analytical.pressure/6894.76-yaxis),axis=1)]
        ##
        ##for i,t in enumerate(time):
        ##    plt.text(xaxis[i]-0.10,yaxis,t,backgroundcolor='w',
        ##             bbox = dict(boxstyle=f"circle",fc="w",ec='w'))
        ##
        ##plt.show()

    def multiphase(self):
        pass
                       
if __name__ == "__main__":

    unittest.main()
