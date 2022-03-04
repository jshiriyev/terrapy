# Standard library imports
import csv
import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

# Third party imports
import matplotlib.pyplot as plt
import numpy as np

# Local application imports
from univariate import variogram

with open('reservoir.csv') as csvfile:
    
    intext = list(csv.reader(csvfile))
    
    header = intext[0]
    values = np.array(intext[1:]).T.astype('float64')

data = {header[3]: values[3],
        header[4]: values[4]}

raman = variogram(data)

raman.x = values[0]
raman.y = values[1]

plt.figure(1)

##plt.scatter(raman.x,raman.y,s=20,c=raman.porosity,alpha=0.2,cmap="PuOr")
##plt.colorbar()

##plt.tight_layout()

ax = plt.gca()
ax.set_aspect(1)

##plt.show()

raman.set_distance(raman)

raman.set_experimental('porosity',
                       500,
                       2000,
                       azimuth=45,
                       azimuth_tol=30,
                       bandwidth=600)

dist = np.sqrt(raman.x**2+raman.y**2)

raman.draw_search_box(origin_x=raman.x[dist.argsort()[1]],
                      origin_y=raman.y[dist.argsort()[1]])

xmin = 0;
xmax = 4000;

ymin = 0;
ymax = 4000;

##plt.scatter(XX,YY,s=5,c='k')

plt.scatter(raman.x,raman.y,c='k',s=1)

plt.title('Porosity Map',fontsize=14)
plt.xlabel('x-axis',fontsize=14)
plt.ylabel('y-axis',fontsize=14)

plt.xlim([xmin,xmax])
plt.ylim([ymin,ymax])

plt.show()
