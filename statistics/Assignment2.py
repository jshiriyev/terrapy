import numpy as np
import math
from math import e
from scipy.stats import norm
import matplotlib.pyplot as plt


## I have added height value to the dataset in the text file. Thats why I have sent text file as well.

class heterogeneity():

    def __init__(self,data):
        
        data = data[np.argsort(data[:,2])]
        data = np.flipud(data)
        self.height = data[:,0]
        self.por = data[:,1]
        self.perm = data[:,2]

##        height = np.arange(0, data.shape[0])
##        height = height.reshape(data.shape[0],1)
##        data = np.hstack((height,data))
## Height values could be added via this code too.

    def COV(self):

        return self.perm.std()/self.perm.mean()
        
    def dykstra(self):

        N = self.perm.shape[0]
        probs = 1/(N+1)
        xaxis = np.linspace(1,N,N)
        xaxis = norm.ppf(xaxis*probs)
        m,c = np.polyfit(xaxis,np.log(self.perm),1)
        yaxis = np.exp(m*xaxis+c)

        k50 = np.exp(m*norm.ppf(0.50)+c)
        k841 = np.exp(m*norm.ppf(0.841)+c)

        coeff = (k50-k841)/k50

        return coeff

    def lorenz(self):
        
        flow_capacity = self.perm*self.height
        storage_capacity = self.por*self.height
        
        norm_flow_capacity = np.cumsum(flow_capacity)/np.sum(flow_capacity)
        norm_storage_capacity = np.cumsum(storage_capacity)/np.sum(storage_capacity)
        norm_flow_capacity = np.append(0,norm_flow_capacity)
        norm_storage_capacity = np.append(0,norm_storage_capacity)
        area = np.trapz(norm_flow_capacity,x=norm_storage_capacity)
        coefficient = (area-0.5)/0.5
        return coefficient



class linear():
    def __init__(self,data):
        
        data = data[np.argsort(data[:,2])]
        data = np.flipud(data)
        self.height = data[:,0]
        self.por = data[:,1]
        self.perm = data[:,2]
        self.rf = data[:,3]
    

    def ranking(X):
        rank = np.empty_like(X)
        rank[X.argsort()] = np.arange(len(X))
        return rank
    

    def correlation(self):
        N=self.rf.shape[0]
        x = linear.ranking(self.por)
        y = linear.ranking(np.log10(self.rf))
        cov_XY = 1/(N-1)*np.sum((x-x.mean())*(y-y.mean()))
        std_por = np.sqrt(1/(N-1)*np.sum((x-x.mean())**2)) 
        std_rf = np.sqrt(1/(N-1)*np.sum((y-y.mean())**2))
        rho = cov_XY/(std_por*std_rf)
        return rho   ## I used both before and after ranking it almost resulted in the same answer
    
    def bestfit(self):
        x = self.por
        y= np.log10(self.perm)
        b = np.sum((x - x.mean())*(y-y.mean()))/np.sum((x-x.mean())**2)
        c = y.mean()-b*x.mean()
        return b,c
    
        


if __name__ == "__main__":

    data = np.loadtxt("Assignment.txt",skiprows=1)
    hc = heterogeneity(data)
    CV = hc.COV()
    DP = hc.dykstra()
    LR = hc.lorenz()
    print(CV)
    print(DP)
    print(LR)
    ln = linear(data)
    rho = ln.correlation()
    print(rho)
    m,c = ln.bestfit()
    print(m,c)
    new = np.array([data[0],data[1],data[2]])
    covMatrix = np.cov(new)
    print(covMatrix)
    
    
    
    
