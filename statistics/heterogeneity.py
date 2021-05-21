import numpy as np

from scipy.stats import norm
import matplotlib.pyplot as plt

class heterogeneity():

    def __init__(self,data):
        
        data = data[np.argsort(data[:,2])]
        data = np.flipud(data)

        self.porosity = data[:,0]
        self.permeability = data[:,1]
        self.F = data[:,2]
    
##        self.layerID = data[:,0]
        self.height = 1#data[:,1]
##        self.permeability = data[:,2]
##        self.porosity = data[:,3]
##        self.saturation = data[:,4]

    def cumplot(data,xlabel):
        N = data.shape[0]
        x = 1/(N+1)*np.linspace(1,N,N)
        plt.plot(np.sort(data),x,c='k',marker='.')
        plt.xlabel(xlabel,fontsize=14)
        plt.ylabel('cumulative frequency',fontsize=14)
        plt.show()

    def standard(self):

        return self.permeability.std()/self.permeability.mean()
        
    def dykstraparsons(self):

        p = np.flip(self.permeability.argsort())

        sk = self.permeability[p]

        numdata = sk.shape[0]

        probs = 1/(numdata+1)
        
        xaxis = np.linspace(1,numdata,numdata)
        xaxis = norm.ppf(xaxis*probs)
        
        yaxis = np.log(sk)
        
##        plt.scatter(xaxis,yaxis)
##        plt.show()
        
        m,c = np.polyfit(xaxis,yaxis,1)
        
        ybestfit = m*xaxis+c

        k50p0 = np.exp(m*norm.ppf(0.50)+c)
        k84p1 = np.exp(m*norm.ppf(0.841)+c)

        coefficient = (k50p0-k84p1)/k50p0

        return coefficient

    def lorenz(self):

        p = np.flip(self.permeability.argsort())

        sk = self.permeability[p]
        sp = self.porosity[p]
        sh = 1
        
        flowcapacity = self.permeability*self.height
        storagecapacity = self.porosity*self.height

        flow = np.cumsum(sk*sh)/np.sum(sk*sh)
        storage = np.cumsum(sp*sh)/np.sum(sp*sh)

        area = np.trapz(flow,storage)
        
        coefficient = (area-0.5)/0.5

        return coefficient

if __name__ == "__main__":

    data = np.loadtxt("assignment2.txt",skiprows=1)

    hc = heterogeneity(data)

    CV = hc.standard()
    DP = hc.dykstraparsons()
    LR = hc.lorenz()

    print(CV)
    print(DP)
    print(LR)






























