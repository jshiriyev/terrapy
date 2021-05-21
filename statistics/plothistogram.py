import numpy as np

import matplotlib.pyplot as plt

from heterogeneity import heterogeneity as ht

def histplot(data,xlabel):
    plt.hist(data,bins=10,range=(11,13))
    plt.xlabel(xlabel,fontsize=14)
    plt.ylabel('frequency',fontsize=14)
    plt.show()

data = np.loadtxt("petrophysics_data.txt",skiprows=1)

layerID = data[:,0]
height = data[:,1]
permeability = data[:,2]
porosity = data[:,3]
saturation = data[:,4]

hc = ht(data)

CD = hc.dykstraparsons()
CL = hc.lorenz()

##fig, ax = plt.subplots()
##ax.hist(permeability,bins=30,align='mid')
##ax.set_ylabel("Frequency")
##ax.set_xlabel("Permeability [mD]")
##plt.show()
##
##fig, ax = plt.subplots()
##ax.hist(porosity,bins=8,align='mid')
##ax.set_ylabel("Frequency")
##ax.set_xlabel("Porosity")
##plt.show()
##
##fig, ax = plt.subplots()
##ax.hist(np.log(permeability),bins=13,align='mid',histtype='bar')
##ax.set_ylabel("Frequency")
##ax.set_xlabel("Natural Log of Permeability")
##plt.show()
