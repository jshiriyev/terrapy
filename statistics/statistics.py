"""Standard library imports"""
import os
import sys

"""Third party imports"""
import matplotlib.pyplot as plt
import numpy as np

from scipy.stats import norm

"""
univariate classes uses a single property data item with optional
spatial information and includes following ananlysis:

    - non-spatial similarity measurements (heterogeneity)
    - non-spatial estimation calculations (uncertainty)
    
    - spatial continuity measurements (variogram)
    - spatial estimation calculations (kriging, gaussian simulation)
    
"""

class uncertainty():

    def __init__(self,prop,**kwargs):

        self.set_property(prop,**kwargs)

    def jacknife(self):

        pass

    def bootstrap(self,X,Nrealization):

        """
        X should be an array with one dimension,
        The size of X defines number of rows, and
        Nrealization specifies number of columns of an array
        created for bootstrap analyzes
        """
        
        N = X.size
        
        idx = np.random.randint(0,N,(N,Nrealization))
        
        return idx

class heterogeneity():

    """
    univariate class carries calculations on non-spatial data

    ...

    Attributes
    ----------
    
    Methods
    -------
    set_property():
        assigns input properties to the self
    standard():
        calculates standard variance
    dykstraparson():
        calculates Dykstra-Parson coefficient
    """

    def __init__(self,prop,**kwargs):

        self.set_property(prop,**kwargs)

    def standard(self,prop):

        values = getattr(self,prop)

        return values.std()/values.mean()
    
    def dykstraparson(self,prop):

        values = getattr(self,prop)

        pr = np.flip(values.argsort())

        sk = values[pr]
        
        numdata = sk.shape[0]

        probs = 1/(numdata+1)

        xaxis = np.linspace(1,numdata,numdata)
        xaxis = norm.ppf(xaxis*probs)

        yaxis = np.log(sk)
        ##yaxis2 = np.log10(sortedPerm)
        ##plt.plot(xaxis,yaxis,'k.')

        m,c = np.polyfit(xaxis,yaxis,1)

        ybestfit = m*xaxis+c

        ##plt.plot(xaxis,ybestfit,'k')
        ##plt.show()

        k50p0 = np.exp(m*norm.ppf(0.5)+c)
        k84p1 = np.exp(m*norm.ppf(0.841)+c)

        coefficient = (k50p0-k84p1)/k50p0
        
        return coefficient

    def lorenz(self):

        permt = getattr(self,"permeability")
        pores = getattr(self,"porosity")
        thick = getattr(self,"thickness")

        pr = np.flip(permt.argsort())

        sk = permt[pr]
        sp = pores[pr]
        st = thick[pr]

        flowing_capacity = sk*st
        storing_capacity = sp*st
        
        Xaxis = np.cumsum(flowing_capacity)/np.sum(flowing_capacity)
        Yaxis = np.cumsum(storing_capacity)/np.sum(storing_capacity)

        ##plt.plot(Xaxis,Yaxis)
        ##plt.show()

        area = np.trapz(Xaxis,Yaxis)
        
        coefficient = (area-0.5)/0.5
        
        return coefficient

class multivariate():
    
    def __init__(self,Y,X=None):
        
        self.yvalues = Y.ravel()

        if X is None:
            self.xvalues = np.arange(self.yvalues.size)
        else:
            self.xvalues = X.ravel()

        self.N
    
    def correlation(self):

        X = self.x
        Y = self.y

        N = X.shape[0]

        std_X = np.sqrt(1/(N-1)*np.sum((X-X.mean())**2))
        std_Y = np.sqrt(1/(N-1)*np.sum((Y-Y.mean())**2))
        
        cov_XY = 1/(N-1)*np.sum((X-X.mean())*(Y-Y.mean()))  
        rho_XY = cov_XY/(std_X*std_Y)
        
        return rho_XY

    def qqplot(self):

        ##A = np.random.normal(25,10,100)
        ##B = np.random.normal(25,5,100)

        percentile = np.linspace(0,100,101)

        fA = np.percentile(A,percentile)
        fB = np.percentile(B,percentile)

        zmin = np.min((fA.min(),fB.min()))
        zmax = np.max((fA.max(),fB.max()))

        plt.plot(np.array([zmin,zmax]),np.array([zmin,zmax]),'--',c='k')
        plt.scatter(fA,fB,c='r')

        plt.xlabel('A',fontsize=14)
        plt.ylabel('B',fontsize=14)

        ax = plt.gca()
        ax.set_aspect('equal')

    def linear_regression_train(self,Xnew=None):

        N = self.xvalues.size

        O = np.ones((N,1))
        X = self.xvalues.reshape(N,-1)
        Y = self.yvalues.reshape(N,-1)

        G = np.concatenate((O,X),axis=1)

        A = np.dot(G.transpose(),G)
        b = np.dot(G.transpose(),Y)

        x = np.linalg.solve(A,b)

        self.slope = x[0]
        self.yintercept = x[1]

        self.Yest = self.intercept+self.slope*self.xvalues

    def linear_regression_guess(self,Xnew):
        
        self.Xnew = Xnew
        self.Ynew = self.intercept+self.slope*self.Xnew

    def monte_carlo(self):
        pass

class NeuralNetwork():
    
    def __init__(self):
        np.random.seed(1)
        self.weights = 2*np.random.random((3,1))-1

    def sigmoid(self,x):
        phi = 1/(1+np.exp(-x))
        return phi
    
    def sigmoid_derivative(self,x):
        phi_derivative = x*(1-x)
        return phi_derivative

    def train(self,trainingInput,trainingOutput,iterationNumber):
        for iteration in range(iterationNumber):
            output = self.forward(trainingInput)
            error = trainingOutput-output
            adjustment = error*self.sigmoid_derivative(output)
            self.weights += np.dot(trainingInput.T,adjustment)
            
    def forward(self,input_layer):
        temp = np.dot(input_layer,self.weights)
        output = self.sigmoid(temp)
        return output

if __name__ == "__main__":

    NN = NeuralNetwork()
    
##    print(NN.weights)
    
    trainingInput = np.array([[0,0,1],[1,1,1],[1,0,1],[0,1,1]])
    trainingOutput = np.array([[0,1,1,0]]).T
    
    NN.train(trainingInput,trainingOutput,20000)
    
##    print(NN.weights)
    
    newInput = np.array([[1,0,0]])
    newOutput = NN.forward(newInput)

    print("The trained input is:")
    print(trainingInput)
    print("The trained output is:")
    print(trainingOutput)
    print("The questioned input is:")
    print(newInput)
    print("The Answer is:")
    print(newOutput)
