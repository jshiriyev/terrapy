import os
import sys

import matplotlib.pyplot as plt
import numpy as np

from scipy.stats import norm

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

    def ridge_regression(self):
        pass

    def monte_carlo(self):
        pass

def kneighbor(k=3,xtrained=None,ytrained=None,xguessed=None):

    if xtrained is None:
        xtrained = np.array([[1,2],[3,4],[5,6],[6,9],[11,15],[7,0]])

    if ytrained is None:
        ytrained = np.array([['A'],['A'],['B'],['B'],['A'],['B']])

    if xguessed is None:
        xguessed = np.array([[4,7],[8,2]])

    AA = xtrained.reshape(xtrained.shape[0],1,-1)
    BB = xguessed.reshape(1,xguessed.shape[0],-1)

    euclidean_distance = np.sqrt(((AA-BB)**2).sum(2))

    idx = np.argpartition(euclidean_distance,k,axis=0)

    yguessed = np.array([ytrained[idx[:,i],0][:k] for i in range(idx.shape[1])]).T
    
    return yguessed

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
