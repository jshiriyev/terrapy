import numpy as np

import matplotlib.pyplot as plt

from scipy.stats import pearsonr

def covariance(X,Y):
    
    N = X.shape[0]
    
    cov_XY = 1/(N-1)*np.sum((X-X.mean())*(Y-Y.mean()))

    return cov_XY

def correlation(X,Y):

    N = X.shape[0]

    cov_XY = 1/(N-1)*np.sum((X-X.mean())*(Y-Y.mean()))
    std_X = np.sqrt(1/(N-1)*np.sum((X-X.mean())**2))
    std_Y = np.sqrt(1/(N-1)*np.sum((Y-Y.mean())**2))

    rho_XY = cov_XY/(std_X*std_Y)

    return rho_XY

def ranking(X):
    
    rank = np.empty_like(X)
    rank[X.argsort()] = np.arange(len(X))

    return rank

##data = np.loadtxt("data_4_petrophysics.txt",skiprows=1)
##
####X = data[:,0]
####Y = data[:,1]
##
##pA = data[:,0]
##kA = data[:,1]
##
##pB = data[:,2]
##kB = data[:,3]
##
##print(correlation(pA,kA))
##print(correlation(pB,kB))
##print(correlation(pA,np.log10(kA)))
##print(correlation(pB,np.log10(kB)))
####print(pearsonr(##data = np.loadtxt("data_4_petrophysics.txt",skiprows=1)
##
####X = data[:,0]
####Y = data[:,1]
##
##pA = data[:,0]
##kA = data[:,1]
##
##pB = data[:,2]
##kB = data[:,3]
##
##print(correlation(pA,kA))
##print(correlation(pB,kB))
##print(correlation(pA,np.log10(kA)))
##print(correlation(pB,np.log10(kB)))
####print(pearsonr(X,Y)[0])
##
##plt.scatter(pA,np.log10(kA))
##
##plt.show()

##
##plt.scatter(pA,np.log10(kA))
##
##plt.show()

##porosity = ranking(raman.porosity)
##permeability = ranking(raman.permeability)
##
##print(correlation(rPorosity,rPermeability))
##
##plt.hist(raman.porosity)
##plt.show()
##
##plt.hist(mp)
##plt.show()
##
##print(correlation(raman.permeability,raman.porosity))
##print(correlation(raman.permeability,raman.vsh))
##print(correlation(raman.porosity,raman.vsh))
##
##print(correlation(raman.log_permeability,raman.porosity))
##print(correlation(raman.log_permeability,raman.vsh))
##print(correlation(raman.porosity,raman.vsh))

##kRank = np.empty_like(raman.permeability)
##pRank = np.empty_like(raman.porosity)
##
##kRank[raman.permeability.argsort()] = np.arange(len(raman.permeability))
##pRank[raman.porosity.argsort()] = np.arange(len(raman.porosity))
##
##print(correlation(kRank,pRank))
##
##plt.scatter(pRank,kRank,c='k',marker='o',s=20)
##
##plt.grid()
##
##plt.xlabel('porosity-rank',fontsize=14)
##plt.ylabel('permeability-rank',fontsize=14)
##
##plt.show()        
##    petrophysics[row[0]] = {'}
##    data1 = reservoir()
##    for h in headers:
##        column[h] = []
##
##for i in range(10):
##    x = np.random.rand(19,1)
##    y = np.random.rand(19,1)
##    x = np.append(x,5)
##    y = np.append(y,5)
##    print(correlation(x,y))
##
##data = np.loadtxt("formations_A_B.txt",skiprows=1)
##
##porosityA = data[:,0]
##permeabilityA = data[:,1]
##porosityB = data[:,2]
##permeabilityB = data[:,3]
##
##rhoA1 = correlation(porosityA,permeabilityA)
##rhoB1 = correlation(porosityB,permeabilityB)
##
##rhoA2 = correlation(porosityA,np.log10(permeabilityA))
##rhoB2 = correlation(porosityB,np.log10(permeabilityB))
##
##plt.scatter(porosityB,permeabilityB,c='k',marker='.')
##plt.grid('major')
##plt.title('Formation B',fontsize=14)
##
##plt.xlim(0,25)
##plt.ylim(0,1000)
##
##plt.xlabel('porosity',fontsize=14)
##plt.ylabel('permeability [mD]',fontsize=14)
##
##plt.show()

##x = -1+2*np.random.rand(100,1)
##y = -1+2*np.random.rand(100,1)
##
##x = np.append(x,10)
##y = np.append(y,10)
##y = -x+0.2*np.random.rand(100,1)
##
##x = np.linspace(-1,1,20)
##y = x**2

x = np.random.rand(19,1)
y = np.random.rand(19,1)

xnew = 5
ynew = 5

print(correlation(x,y)**2)
print(correlation(np.append(x,xnew),np.append(y,ynew))**2)

##plt.grid('major')#

plt.scatter(x,y,c='k',marker='.',s=200)
plt.scatter(xnew,ynew,c='r',marker='.',s=200)

##plt.xlabel('X1',fontsize=18)
##plt.ylabel('X2',fontsize=18)
##
##plt.xlim(-2,2)
##plt.ylim(-2,2)
##
##X = np.array([-2,-1.5,-1,-0.5,0,0.5,1,1.5,2])
##Y = np.array([-2,-1.5,-1,-0.5,0,0.5,1,1.5,2])
##
##X = np.array([0,2,4,6,8,10])
##Y = np.array([0,0.2,0.4,0.6,0.8,1.0])
##
##plt.xticks(X," ")
##plt.yticks(Y," ")

plt.show()
