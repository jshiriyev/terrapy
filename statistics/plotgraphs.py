import numpy as np

import matplotlib.pyplot as plt

data = np.loadtxt("petrophysics_data.txt",skiprows=1)

layerID = data[:,0]
height = data[:,1]
permeability = data[:,2]
porosity = data[:,3]
saturation = data[:,4]

depth = np.cumsum(height)

fig,(ax1,ax2,ax3) = plt.subplots(1,3)

ax1.semilogx(permeability,depth,drawstyle="steps")
ax1.grid(True,which="both")
ax1.set_title('Permeability [mD]')
ax1.set_ylabel('Relative Depth [m]')
ax1.set_xlim(10.,10000.)
ax1.set_ylim((0,180))
ax1.invert_yaxis()
ax1.xaxis.tick_top()

ax2.plot(porosity,depth,drawstyle="steps")
ax2.grid(True,which="major")
ax2.set_title('Porosity')
ax2.set_xlim(0.1,0.3)
ax2.set_ylim(0,180)
ax2.set_yticklabels([])
ax2.invert_yaxis()
ax2.xaxis.tick_top()

ax3.plot(saturation,depth,drawstyle="steps")
ax3.grid(True,which="major")
ax3.set_title('Water Saturation')
ax3.set_xlim(0.1,0.3)
ax3.set_ylim(0,180)
ax3.set_yticklabels([])
ax3.invert_yaxis()
ax3.xaxis.tick_top()

plt.show()
