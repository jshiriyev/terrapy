import os
import sys

sys.path.append(os.path.dirname(os.getcwd()))

import matplotlib.pyplot as plt

from numerical import finite_element

"""
how to check finite elements, mesh:
 - nodes must be unique
 - nodes can not be too close to each other
"""

elm = finite_element()

elm.cartesian((4,3,1),1)

plt.plot(elm.node[:,0],elm.node[:,1],'.',c='k')

for triangle in elm.id:
    plt.plot(elm.node[triangle,0],elm.node[triangle,1])
    
plt.show()
