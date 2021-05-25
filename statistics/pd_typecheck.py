import numpy as np

import matplotlib.pyplot as plt

from scipy.stats import norm as norm

##np.random.seed(1)

numcols = 40
numrows = 10

X = np.random.rand(numrows,numcols)

X[X<0.3] = 0

X[X>=0.3] = 1

S = X.sum(axis=0)

##print(S)

hy,hx = np.histogram(S-1,bins=11,range=(-1,10))

##plt.bar(hx[1:],hy/numcols,width=0.5)
##
##plt.show()

SS = np.sort(S)

probs = 1/(numcols+1)

xaxis = norm.ppf(np.linspace(1,numcols,numcols)*probs)

m,c = np.polyfit(xaxis,SS,1)

ybestfit = m*xaxis+c

plt.plot(xaxis,SS,'b.')
plt.plot(xaxis,ybestfit,'r')



plt.show()























##from scipy.stats import norm





##numdata = 40
##
##X = np.random.rand(10,numdata)
##
##X[X<=0.3] = 0
##X[X>0.3] = 1
##
##S = X.sum(axis=0)
##
##plt.hist(S,bins=16,range=(3,11),align='left')
##
##plt.xlabel('number of ones')
##plt.ylabel('Frequency')
##plt.title('Histogram of S')
##
##plt.show()
##
##SS = np.sort(S)
##
##probs = 1/(numdata+1)
##
##xaxis = norm.ppf(np.linspace(1,numdata,numdata)*probs)
##
##m,c = np.polyfit(xaxis,SS,1)
##
##ybestfit = m*xaxis+c
##
##plt.plot(xaxis,SS,'b.')
##plt.plot(xaxis,ybestfit,'k')
##
##plt.xlim([-3,3])
##plt.ylim([0,12])
##
##plt.grid()
##
##plt.show()
