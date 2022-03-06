import matplotlib.pyplot as plt

import numpy as np

import pint

if __name__ == "__main__":
    import setup

from fluidflow.pormed.radial import transient

## INPUT

ur = pint.UnitRegistry()

k = ur.Quantity(80,'millidarcy').to('m^2').magnitude
p = 0.18
h = ur.Quantity(50,'ft').to('m').magnitude

cf = ur.Quantity(3e-6,'1/psi').to('1/Pa').magnitude
cw = ur.Quantity(3e-6,'1/psi').to('1/Pa').magnitude
co = ur.Quantity(5e-6,'1/psi').to('1/Pa').magnitude

Sw = 0.25
So = 0.75

qo = ur.Quantity(300*1.2,'oil_bbl/day').to('m^3/second').magnitude

muo = ur.Quantity(3,'centipoise').to('Pa.s').magnitude

rw = ur.Quantity(0.25,'ft').to('m').magnitude

Pi = ur.Quantity(5000,'psi').to('Pa').magnitude

times = ur.Quantity((1,10,100),'days').to('sec').magnitude

observers = ur.Quantity(np.linspace(0.25,1000),'ft').to('m').magnitude

## SETTING MODEL

solver = transient(qo)

solver.PorRock.set_permeability(k)
solver.PorRock.set_compressibility(cf)
solver.PorRock.set_porosity(p)
solver.PorRock.set_thickness(h)

solver.Fluids.set_names("oil","water")
solver.Fluids.set_compressibility(co,cw)
solver.Fluids.set_viscosity(muo)

solver.Well.set_names(1)
solver.Well.set_skinfactors(0)
solver.Well.set_radii(rw)

## CALCULATIONS

solver.initialize(Pi,Sw)

solver.set_tmin()
solver.set_tmax(max(times))
solver.set_times(times)
solver.set_observers(observers)
solver.solve()

## RESULTS

ur.Quantity(solver.observers,'m').ito('ft')
ur.Quantity(solver.pressure,'Pa').ito('psi')

plt.plot(solver.observers,solver.pressure[:,0],label='P(1 day)')
plt.plot(solver.observers,solver.pressure[:,1],label='P(10 days)')
plt.plot(solver.observers,solver.pressure[:,2],label='P(100 days)')

plt.xlabel('radius [ft]')
plt.ylabel('pressure [psi]')

plt.legend()

plt.grid()

plt.show()
