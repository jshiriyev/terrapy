import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt

import numpy as np

from connectivity import variogram

from spatial import estimation

## Example 4.2 (Kriging) and 4.3 (Simulation) page 187, Peters Volume 1

x = np.array([2,4,6])
y = np.array([30,50,20])

plt.grid(alpha=0.2)
plt.xlabel('x-axis',fontsize=14)
plt.ylabel('property',fontsize=14)

plt.xlim([0,9])
plt.ylim([0,70])

V = variogram(y,X=x)

V.type = 'exponential'
V.nugget = 0
V.sill = 100
V.range = 10

V.set_theoretical()

xe = np.linspace(1,8,71)

E = estimation(V,X=xe)

E.kriging_ordinary()

y0 = E.property

E.kriging_ordinary(perc=0.975)

y1 = E.property

E.kriging_ordinary(perc=0.025)

y2 = E.property

E.simulation_gaussian()

y3 = E.property

plt.plot(E.x,y0,c='k')

plt.fill_between(E.x,y1,y2,fc='lightgrey')

plt.scatter(E.x,E.property,s=4,c='r')

plt.scatter(x,y,marker='X',c='k')

plt.show()
