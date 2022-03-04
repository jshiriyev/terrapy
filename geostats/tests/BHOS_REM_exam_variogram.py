import matplotlib.pyplot as plt
import numpy as np

from bhospy.statistics.connectivity import variogram

well = np.loadtxt("BHOS_REM_well",skiprows=1)

V = variogram(well[:,1],Z=well[:,0])

V.set_experimental(lagmax=35)

V.set_theoretical(np.linspace(0,50),vsill=18,vrange=25)

plt.scatter(V.bins_experimental,V.experimental,c='r')
plt.plot(V.bins_theoretical,V.theoretical,'k')

plt.xlabel("lag distance")
plt.ylabel("semi-variance")

plt.show()
