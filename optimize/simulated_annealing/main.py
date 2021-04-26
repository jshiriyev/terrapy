from vfsa import annealing
from plotting import historyPlot

import numpy as np
import matplotlib.pyplot as plt

nog = 100 
nom = 25
nop = 5

m_min = np.ones([1,nop])*-10
m_max = np.ones([1,nop])*10

model = annealing(m_min,m_max,nog,nom,nop)

#T = model.temperature('straight')
T = model.temperature('geometric')
#T = model.temperature('reciprocal')
#T = model.temperature('logarithmic')

models,energy = model.iterating(T)

plot = historyPlot(m_min,m_max,models,energy)
plot.plot_error()

"""
plt.figure(num=1)

X = np.array(range(1,nog+1))

plt.plot(X,T1)
plt.plot(X,T2)
plt.plot(X,T3)
plt.plot(X,T4)

plt.xlim([0.,nog])
plt.ylim([0.,1.])

plt.show()
"""
