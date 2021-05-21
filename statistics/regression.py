import numpy as np

import matplotlib.pyplot as plt

class nominal():

    def __init__(self,X=None,Y):

        self.yvalues = Y.ravel()

        if X is None:
            self.xvalues = np.arange(self.yvalues.size)
        elif:
            self.xvalues = X.ravel()

        self.N

    def linear_regression(self):

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

    def estimate(self,Xnew=None)

        Yapp = x[0]+x[1]*X
        
        self.Yapp = Yapp
