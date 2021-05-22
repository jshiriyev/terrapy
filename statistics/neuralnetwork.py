# -*- coding: utf-8 -*-

import numpy as np

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
