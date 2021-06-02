import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np

from numerical import finite_difference

solver = finite_difference()

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

plt.contourf(X,Y,P,alpha=1,cmap=cm.PuBu)
plt.colorbar()

plt.title('Pressure Map',fontsize=14)
plt.xlabel('x-axis',fontsize=14)
plt.ylabel('y-axis',fontsize=14)

plt.xlim([0,1])
plt.ylim([0,1])

plt.show()
