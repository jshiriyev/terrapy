import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np

from analytical import poisson

from computational import finite_difference

solver = finite_difference()

num_x = 25
num_y = 25
num_z = 10

solver.cartesian((num_x*1.,num_y*1.,num_z*1.),(num_x,num_y,num_z))

solver.central()

bvector = np.zeros((solver.Amatrix.shape[0],1))

solver.implement_bc(bvector,
                    b_xmin=(1,0,0),
                    b_xmax=(0,1,0),
                    b_ymin=(1,0,0),
                    b_ymax=(1,0,100),
                    b_zmin=(0,1,0),
                    b_zmax=(0,1,0))

solver.solve()

X = solver.center[:,0].reshape(num_x,num_y,num_z)
Y = solver.center[:,1].reshape(num_x,num_y,num_z)
Z = solver.center[:,2].reshape(num_x,num_y,num_z)

##P = solver.unknown.reshape(num_x,num_y,num_z)

fig = plt.figure()

ax = fig.add_subplot(111, projection='3d')

scatter = ax.scatter(X,Y,Z,alpha=0.5,c=solver.unknown)

fig.colorbar(scatter,shrink=0.5,aspect=5)

ax.set_xlim3d([0,solver.length_x])
ax.set_ylim3d([0,solver.length_y])
ax.set_zlim3d([0,solver.length_z])

ax.set_xlabel('x-axis')
ax.set_ylabel('y-axis')
ax.set_zlabel('z-axis')

plt.show()
