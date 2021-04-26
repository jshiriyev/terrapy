import matplotlib.pyplot as plt
import numpy as np

from scipy import optimize

def packed_bed(voidage,viscosity,length,us,particle_diameter):

    part1 = 150*(1-voidage)**2/voidage**3
    part2 = viscosity*length*us/particle_diameter**2

    return part1*part2

def objective(voidage,viscosity,length,us,particle_diameter,pressure_drop):

    Pc = packed_bed(voidage,viscosity,length,us,particle_diameter)
    Pm = pressure_drop

##    print(Pc)
##    print(Pm)

    return np.sum((Pc-Pm)**2)

velocity = np.array([9.10,14.9,20.5,25.1,30.0,36.1]) #mm/s
velocity *= 1e-3

pressure = np.array([1208,1694,2796,3810,5445,6566]) #Pa

diameter_particle = 3.9 #mm
diameter_particle *= 1e-3

height = 625 #mm
height *= 1e-3

rho_water = 1000 #kg/m3
mu_water = 1e-3 #Pa.s

Re = (rho_water*velocity*diameter_particle)/mu_water

plt.scatter(Re,pressure)
plt.show()

voidage = optimize.minimize_scalar(
                    objective,
                    args=(mu_water,height,velocity,diameter_particle,pressure),
                    bounds=((0,1)),
                    method='bounded').x

