import numpy as np

import matplotlib.pyplot as plt

data = np.loadtxt("data_3_petrophysics.txt",skiprows=1)

A = data[:,0]
B = data[:,1]

##A = np.random.normal(25,10,100)
##B = np.random.normal(25,5,100)

percentile = np.linspace(0,100,101)

fA = np.percentile(A,percentile)
fB = np.percentile(B,percentile)

zmin = np.min((fA.min(),fB.min()))
zmax = np.max((fA.max(),fB.max()))

plt.plot(np.array([zmin,zmax]),np.array([zmin,zmax]),'--',c='k')
plt.scatter(fA,fB,c='r')

plt.xlabel('A',fontsize=14)
plt.ylabel('B',fontsize=14)

ax = plt.gca()
ax.set_aspect('equal')

plt.show()
