import numpy as np

from scipy.optimize import minimize
from scipy.optimize import minimize_scalar

if __name__ == "__main__":
    import setup

from fluidflow.pipes.cylindrical import single_phase

sp = single_phase(20+273.15)    # Kelvin

sp.Pipe.set_length(10*1000)     # meters
sp.Pipe.set_diameter(12*0.0254) # meters

sp.Fluid.set_molarweight(0.016) # kilogram per moles
sp.Fluid.set_viscosity(1.03e-5) # Pascal-second

sp.set_Reynolds(mass_rate=10)   # kilogram per seconds

sp.set_friction(method="simple",pipe_smoothness=True)

sp.set_criticalRatio()

sp.set_uppressure(200*6894.76)  # Pascal

Pdown = sp.get_downpressure()

rho1 = (sp.Pup*sp.Fluid.molarweight[0])/(sp.UGC*sp.temperature)

rho2 = (Pdown*sp.Fluid.molarweight[0])/(sp.UGC*sp.temperature)

u1 = sp.mass_rate/(rho1*sp.Pipe.csa)
u2 = sp.mass_rate/(rho2*sp.Pipe.csa)
