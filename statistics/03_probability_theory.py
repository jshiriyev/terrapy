import numpy as np
import matplotlib.pyplot as plt

from scipy.stats import norm

#np.random.seed(1)

numdata = 40

X = np.random.rand(10,numdata)

X[X<=0.3] = 0
X[X>0.3] = 1

S = X.sum(axis=0)

plt.hist(S,bins=16,range=(3,11),align='left')

plt.xlabel('number of ones')
plt.ylabel('Frequency')
plt.title('Histogram of S')

plt.show()

SS = np.sort(S)

probs = 1/(numdata+1)

xaxis = norm.ppf(np.linspace(1,numdata,numdata)*probs)

m,c = np.polyfit(xaxis,SS,1)

ybestfit = m*xaxis+c

plt.plot(xaxis,SS,'b.')
plt.plot(xaxis,ybestfit,'k')

plt.xlim([-3,3])
plt.ylim([0,12])

plt.grid()

plt.show()
