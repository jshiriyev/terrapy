import matplotlib.pyplot as plt
import numpy as np

from scipy import optimize

def packed_bed(voidage,viscosity,length,us,particle_diameter):

    part1 = 150*(1-voidage)**2/voidage**3
    part2 = viscosity*length*us/particle_diameter**2

    return part1*part2

def objective(voidage,viscosity,length,us,particle_diameter,pressure_drop):

    Pm = pressure_drop
    Pc = packed_bed(voidage,viscosity,length,us,particle_diameter)

    return np.sum((Pc-Pm)**2)

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

