import numpy as np

import matplotlib.pyplot as plt

data = np.loadtxt("petrophysics_data.txt",skiprows=1)

layerID = data[:,0]
height = data[:,1]
permeability = data[:,2]
porosity = data[:,3]
saturation = data[:,4]

fig, (ax1,ax2) = plt.subplots(1,2)

ax1.boxplot(np.log(permeability))
ax1.set_ylabel("Natural Log of Permeability")
ax1.set_xticks([])

ax2.boxplot(porosity,showmeans=True)
ax2.set_ylabel("Porosity")
ax2.set_xticks([])

plt.tight_layout()

plt.show()
