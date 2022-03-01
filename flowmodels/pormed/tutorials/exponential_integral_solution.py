import matplotlib.pyplot as plt

import numpy as np

if __name__ == "__main__":
    import setup

from interfaces.items import Ellipse
from interfaces.items import Formation
from interfaces.items import Wells
from interfaces.items import Fluids

from flowmodels.pormed.radial import transient

geo = Ellipse(radii=(8000,8000),thickness=50,unitsystem="FU")

fig = plt.figure()

##axis = plt.axes(projection="3d")
axis = fig.add_subplot(111)

geo.plot(axis)

plt.show()

res = Formation(geo,"FU")

res.set_permeability(80)
res.set_compressibility(3e-6)
res.set_porosity(0.18)

fluids = Fluids(1)

fluids.set_names(("oil","water"))
fluids.set_compressibility((5e-6,3e-6))
fluids.set_fvf((1.2,None))
fluids.set_viscosity((3.0,None))

wells = Wells(None)
wells.set_names(["producer"])
wells.set_flowconds(conditions=("rate",),limits=(300,),fluids=("oil",))
wells.set_skinfactors((0,))
wells.set_radii((0.25,))
