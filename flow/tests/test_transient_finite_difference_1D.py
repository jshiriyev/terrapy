import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt
import numpy as np

from analytical import diffusivity

from computational import finite_difference

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

analytical = diffusivity(eta)

analytical.cartesian_1D((0,L),(bnd1,bnd2),time)

"""NUMERICAL CALL"""

grids = finite_difference()

grids.cartesian((L,1,1),(100,1,1))

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

for i,t in enumerate(time):
    plt.scatter(grids.center[:,0],grids.pressure[:,100]/6894.76)
    plt.plot(analytical.x.flatten(),analytical.pressure[i,:]/6894.76)

plt.xlabel('x-axis')
plt.ylabel('pressure [psi]')

yaxis = 0.75*(p0+pL)/(6894.76)
xaxis = analytical.x.flatten()[
    np.argmin(np.abs(analytical.pressure/6894.76-yaxis),axis=1)]

for i,t in enumerate(time):
    plt.text(xaxis[i],yaxis,t,backgroundcolor='w')

plt.show()
