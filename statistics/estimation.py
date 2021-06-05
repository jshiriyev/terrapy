import os
import sys

import numpy as np

class item():

    def __init__(self,observed_points,observed_values):

        self.xobs = observed_points
        self.yobs = observed_values
    
class regression(item):

    def __init__(self,observed_points,observed_values):

        super(regression,self).__init__(observed_points,observed_values)

    def linear_train(self):

        N = self.xobs.shape[0]

        O = np.ones((N,1))
        X = self.xvalues.reshape(N,-1)
        Y = self.yvalues.reshape(N,-1)

        G = np.concatenate((O,X),axis=1)

        A = np.dot(G.transpose(),G)
        b = np.dot(G.transpose(),Y)

        x = np.linalg.solve(A,b)

        self.slope = x[0]
        self.yintercept = x[1]

    def ridge(self):
        pass

    def estimate(self,estimated_points):
        
        self.Xest = estimated_points

        Yest = self.intercept+self.slope*self.Xest

        return Yest

class kneighbor(item):

    def __init__(self,observed_points,observed_values):

        super(kneighbor,self).__init__(observed_points,observed_values)

    def eucledian_distance(self,estimated_points):

        self.xest = estimated_points

        #xobs = np.array([[1,2],[3,4],[5,6],[6,9],[11,15],[7,0]])
        #xest = np.array([[4,7],[8,2]])

        mobs = xtrained.reshape(self.xobs.shape[0],1,-1)
        mest = xguessed.reshape(1,self.xest.shape[0],-1)

        self.distance = np.sqrt(((mobs-mest)**2).sum(2))

    def estimate(self,k):

        idx = np.argpartition(self.distance,k,axis=0)

        #ytrained = np.array([['A'],['A'],['B'],['B'],['A'],['B']])

        yest = np.array([self.yobs[idx[:,i],0][:k] for i in range(idx.shape[1])]).T
    
        return yest

class neuralnetwork(item):
    
    def __init__(self,observed_points,observed_values):

        super(neuralnetwork,self).__init__(observed_points,observed_values)
        
        np.random.seed(1)
        
        self.weights = 2*np.random.random((3,1))-1

    def sigmoid(self,x):
        
        phi = 1/(1+np.exp(-x))
        
        return phi
    
    def sigmoid_derivative(self,x):
        
        phi_derivative = x*(1-x)
        
        return phi_derivative

    def train(self,iterationNumber):
        
        for iteration in range(iterationNumber):
            
            output = self.estimate(self.xobs)
            
            error = self.yobs-output
            
            adjustment = error*self.sigmoid_derivative(output)
            
            self.weights += np.dot(self.xobs.T,adjustment)
            
    def estimate(self,estimated_points):

        self.xest = estimated_points
        
        temp = np.dot(self.xest,self.weights)
        
        yest = self.sigmoid(temp)
        
        return yest

if __name__ == "__main__":
    
    xobs = np.array([[0,0,1],
                     [1,1,1],
                     [1,0,1],
                     [0,1,1]])
    
    yobs = np.array([[0,1,1,0]]).T

    NN = neuralnetwork(xobs,yobs)
    
##    print(NN.weights)
    
    NN.train(20000)
    
##    print(NN.weights)
    
    xest = np.array([[1,0,0]])
    yest = NN.estimate(xest)

    print("The trained input is:")
    print(xobs)
    print("The trained output is:")
    print(yobs)
    print("The questioned input is:")
    print(xest)
    print("The Answer is:")
    print(yest)
