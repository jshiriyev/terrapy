import numpy as np

import matplotlib.pyplot as plt

def covariance(X,Y):

    N = X.shape[0]

    A = 1/(N-1)*np.sum((X-X.mean())*(Y-Y.mean()))

    return A

def correlation(X,Y):

    N = X.shape[0]

    A = 1/(N-1)*np.sum((X-X.mean())*(Y-Y.mean()))

    std_X = np.sqrt(1/(N-1)*np.sum((X-X.mean())**2))
    std_Y = np.sqrt(1/(N-1)*np.sum((Y-Y.mean())**2))
    
    rho = A/(std_X*std_Y)

    return rho

def ranking(X):

    rank = np.empty_like(X)
    
    rank[X.argsort()] = np.arange(len(X))
    
    return rank

data = np.loadtxt("assignment2.txt",skiprows=1)

##data = np.loadtxt("data_4_petrophysics.txt",skiprows=1)

p = data[:,0]
k = data[:,1]
F = data[:,2]

print(covariance(p,p))
print(covariance(k,k))
print(covariance(F,F))
print(covariance(p,k))
print(covariance(p,F))
print(covariance(k,F))


##rho1 = correlation(np.log(F),p)
##rho2 = correlation(ranking(XX),ranking(YY))

##print(rho1)
##print(rho2)

##X = np.random.rand(20,1)
##Y = np.random.rand(20,1)
##
##X_outlier = 3
##Y_outlier = 3
##
##XX = np.append(X,X_outlier)
##YY = np.append(Y,Y_outlier)
##
##rho1 = correlation(XX,YY)
##rho2 = correlation(ranking(XX),ranking(YY))
####cov = covariance(pA,np.log10(kA))
####print(cov)
##print(rho1)
##print(rho2)
##
##plt.scatter(XX,YY)
##
##plt.xlabel('X')
##plt.ylabel('Y')
##
##plt.show()


