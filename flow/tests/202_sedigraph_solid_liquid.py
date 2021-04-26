import numpy as np

import matplotlib.pyplot as plt

mu_fluid = 1.156e-3

rho_fluid = 745
rho_solid = 2200

data = np.loadtxt("200_sedigraph.txt",skiprows=2)

vs = data[:,0]
wt = data[:,1]

dd = np.sqrt((18*mu_fluid*vs/1e6)/9.81/(rho_solid-rho_fluid))*1e6

plt.scatter(dd[1:],wt[1:])

plt.xlabel("Particle Size [micro-meter]")
plt.ylabel("Weight Fraction [%]")

plt.ylim([-1,30])

plt.grid(which="both",axis="both")

plt.show()

Re = rho_fluid*vs/1e6*dd/1e6/mu_fluid
