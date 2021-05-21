import numpy as np

import matplotlib.pyplot as plt

def findslope(X,Y):

    N = X.size

    O = np.ones((N,1))
    X = X.reshape(N,-1)
    Y = Y.reshape(N,-1)

    G = np.concatenate((O,X),axis=1)

    A = np.dot(G.transpose(),G)
    b = np.dot(G.transpose(),Y)

    x = np.linalg.solve(A,b)

    #x[0] is the intercept which is "a" on the slide
    #x[1] is the slope which is "b" on the slide

    Yapp = x[0]+x[1]*X
    
    return x

data = np.loadtxt("assignment2.txt",skiprows=1)

p = data[:,0]
k = data[:,1]
F = data[:,2]

x = findslope(p,np.log(k))

##plt.hist(x[1,:])
##plt.xlabel('slope',fontsize=14)
##
##plt.show()
##
##plt.hist(x[0,:])
##plt.xlabel('intercept',fontsize=14)
##
##plt.show()
##
##x,Yapp = findslope(raman.porosity,raman.log10k)
##print(x)
##print(correlation(raman.porosity,raman.log10k)**2)
##
##plt.scatter(raman.porosity,raman.log10k,c='k',marker='o')
##plt.plot(raman.porosity,Yapp,c='r')
##plt.xlabel('porosity',fontsize=14)
##plt.ylabel('log-permeability',fontsize=14)
##plt.show()
