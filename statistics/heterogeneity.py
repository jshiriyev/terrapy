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
