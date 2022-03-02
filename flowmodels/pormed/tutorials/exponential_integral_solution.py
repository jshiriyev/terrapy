import matplotlib.pyplot as plt

import numpy as np

import pint

if __name__ == "__main__":
    import setup

from flowmodels.pormed.radial import transient

ur = pint.UnitRegistry()

k = ur.Quantity(80,'millidarcy').to('m^2').magnitude
p = 0.18
h = ur.Quantity(50,'ft').to('m').magnitude
# R = ur.Quantity(8000,'ft').to('m').magnitude

cf = ur.Quantity(3e-6,'1/psi').to('1/Pa').magnitude
cw = ur.Quantity(3e-6,'1/psi').to('1/Pa').magnitude
co = ur.Quantity(5e-6,'1/psi').to('1/Pa').magnitude

Sw = 0.25
So = 0.75

qo = ur.Quantity(300*1.2,'bbl').to('m^3').magnitude

muo = ur.Quantity(3,'centipoise').to('Pa.s').magnitude

rw = ur.Quantity(0.25,'ft').to('m').magnitude

Pi = ur.Quantity(5000,'psi').to('Pa').magnitude

times = ur.Quantity((1,10,100),'days').to('sec').magnitude

solver = transient(qo)

# geo = Ellipse(radii=(R,R),thickness=h)

# fig = plt.figure()

##axis = plt.axes(projection="3d")
# axis = fig.add_subplot(111)

# geo.plot(axis)

# plt.show()

solver.formation.set_permeability(k)
solver.formation.set_compressibility(cf)
solver.formation.set_porosity(p)

solver.formation.geometry.set_thickness(h)

solver.fluids.set_names("oil","water")
solver.fluids.set_compressibility(co,cw)
solver.fluids.set_viscosity(muo)

solver.wells.set_names(["1"])
solver.wells.set_flowconds(conditions=("rate",),limits=(qo,),fluids=("oil",))
solver.wells.set_skinfactors((0,))
solver.wells.set_radii((0.25,))

solver.initialize(Pi,Sw)

solver.get_tmin()

solver.get_exterior_radius(max(times))

solver.set_times(times)

solver.set_observers()

solver.solve()
