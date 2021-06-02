import numpy as np

import matplotlib.pyplot as plt

mu_fluid = 1.156e-3

rho_fluid = 745
rho_solid = 2200

# Vset unit 1e6m/s
vs = np.array([4.4e+03, 2.5e+03, 1.7e+03, 6.2e+02, 1.5e+02, 6.9e+01, 4.4e+01,
       2.5e+01, 1.7e+01, 1.1e+01, 6.2e+00, 2.7e+00, 6.9e-01, 4.4e-01,
       2.5e-01, 1.7e-01, 1.1e-01, 2.7e-02])

# Wt %
wt = np.array([0.6,  0.1,  0.7,  2.5,  3. ,  2.9,  6.6,  6.7, 11.2, 18.4, 26. ,
       20. ,  0.2,  0.2,  0.4,  0.2,  0.1,  0.1])

dd = np.sqrt((18*mu_fluid*vs/1e6)/9.81/(rho_solid-rho_fluid))*1e6

plt.scatter(dd[1:],wt[1:])

plt.xlabel("Particle Size [micro-meter]")
plt.ylabel("Weight Fraction [%]")

plt.ylim([-1,30])

plt.grid(which="both",axis="both")

plt.show()

Re = rho_fluid*vs/1e6*dd/1e6/mu_fluid
