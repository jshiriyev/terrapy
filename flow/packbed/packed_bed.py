import matplotlib.pyplot as plt
import numpy as np

from scipy import optimize

class single_phase():

    pass

def forward(voidage,viscosity,length,us,particle_diameter):

    part1 = 150*(1-voidage)**2/voidage**3
    part2 = viscosity*length*us/particle_diameter**2

    return part1*part2

def objective(voidage,viscosity,length,us,particle_diameter,pressure_drop):

    Pm = pressure_drop
    Pc = forward(voidage,viscosity,length,us,particle_diameter)

    return np.sum((Pc-Pm)**2)

"""
Question 2
"""
flow_rate = 10 #ml/min
pressure_drop = 0.5 #Pa
length = 40 #cm
diameter = 10 #cm
diameter_particle = 4 #mm

flow_rate *= 1e-6/60
length *= 1e-2
diameter *= 1e-2
diameter_particle *= 1e-3

mu_water = 1e-3 # Pa.s

csa = np.pi*diameter**2/4

us = flow_rate/csa

voidage = optimize.minimize_scalar(
                    objective,
                    args=(mu_water,length,us,diameter_particle,pressure_drop),
                    bounds=((0,1)),
                    method='bounded').x

"""
Question 4
"""

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
