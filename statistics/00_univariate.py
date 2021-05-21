import numpy as np
#import matplotlib.pyplot as plt
from scipy.stats import norm

def lorenz(permeability,porosity,height):
    
    flowcapacity = permeability*height
    storagecapacity = porosity*height

    fcapacity = np.cumsum(flowcapacity)/np.sum(flowcapacity)
    scapacity = np.cumsum(storagecapacity)/np.sum(storagecapacity)

    area = np.trapz(fcapacity,scapacity)

    coefficient = (area-0.5)/0.5
    
    return coefficient

def dykstraParson(permeability):
    
    numdata = permeability.shape[0]

    probs = 1/(numdata+1)

    xaxis = np.linspace(1,numdata,numdata)
    xaxis = xaxis*probs
    xaxis = norm.ppf(xaxis)

    yaxis = np.log(permeability)
    #yaxis2 = np.log10(sortedPerm)

    #plt.plot(xaxis,yaxis,'k.')

    m,c = np.polyfit(xaxis,np.log(permeability),1)

    ybestfit = m*xaxis+c

    #plt.plot(xaxis,ybestfit,'k')

    #plt.show()

    k50p0 = np.exp(m*norm.ppf(0.5)+c)
    k84p1 = np.exp(m*norm.ppf(0.841)+c)

    coefficient = (k50p0-k84p1)/k50p0
    
    return coefficient

data = np.loadtxt("petrophysics_data.txt",skiprows=1)

data = data[np.argsort(data[:,2])]
data = np.flipud(data)

layerID = data[:,0]
height = data[:,1]
permeability = data[:,2]
porosity = data[:,3]
wsaturation = data[:,4]

LC = lorenz(permeability,porosity,height)
DC = dykstraParson(permeability)


