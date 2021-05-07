import matplotlib.pyplot as plt
import numpy as np

x = np.array([1,2,3,4,1,2,3,4,1,2,3,4,3])
y = np.array([1,1,1,1,2,2,2,2,3,3,3,3,1.5])

lagdis = 1
lagdis_tol = lagdis/2

azimuth = 45
azimuth_tol = 22.5

bandwidth = 2

dx = x-x.reshape((-1,1))
dy = y-y.reshape((-1,1))

distance = np.sqrt(dx**2+dy**2)

theta = np.arctan2(dy,dx)*180/np.pi

dtheta = np.abs(theta-azimuth)

con1 = np.abs(dtheta)<=azimuth_tol
con2 = np.sin(dtheta/180*np.pi)*distance<=(bandwidth/2.)

con = np.logical_and(con1,con2)

xmatch = x[con[1,:]]
ymatch = y[con[1,:]]

plt.scatter(x,y)
plt.scatter(xmatch,ymatch,c='r')

plt.xlim([0,5])
plt.ylim([0,4])

plt.show()
