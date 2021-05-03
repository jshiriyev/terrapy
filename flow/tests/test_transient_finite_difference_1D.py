import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from analytical import diffusivity

from computational import finite_difference

L = 10#m

p0 = 0#psi
pL = 1000#psi

permeability = 100#mD

porosity = 0.2
viscosity = 1#cp
compressibility = 1e-5#1/psi



p0 *= 6894.76
pL *= 6894.76

bnd1 = (1,0,p0)
bnd2 = (1,0,pL)

permeability *= 9.8692326671601e-16

viscosity *= 0.001
compressibility /= 6894.76

eta = (permeability)/(porosity*viscosity*compressibility)

##grids = finite_difference()
##
##grids.cartesian((7,1,1),(7,1,1))
##
##grids.set_property((1.,1.,1.),1.,1.,1.)
##
##grids.transmissibility()
##
##grids.central(order=2)
##
##grids.implement_bc(b_xmin=bnd1,b_xmax=bnd2)
##
##grids.solve()
##
##plt.scatter(grids.center[:,0],grids.unknown,c='r')

time = np.array([1,10,20,50,100])

analytical = diffusivity(eta)

analytical.cartesian_1D((0,L),(bnd1,bnd2),time)

for i,t in enumerate(time):
    plt.plot(analytical.x.flatten(),analytical.pressure[i,:]/6894.76)

plt.xlabel('x-axis')
plt.ylabel('pressure [psi]')

for i,t in enumerate(time):
    plt.text(np.linspace(0,L,time.shape[0])[i],3*(p0+pL)/(4*6894.76),t)

plt.show()
