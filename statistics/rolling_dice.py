import numpy as np

import matplotlib.pyplot as plt

from scipy.special import erfc

def gaussian(x,mean,variance):
        f = 1/variance/np.sqrt(2*np.pi)*np.exp(-(x-mean)**2/2/variance**2)
        F = 1/2*(2-erfc((x-mean)/(variance*np.sqrt(2))))
        return f,F

N = 1000000

dice1 = np.random.randint(1,7,N)
dice2 = np.random.randint(1,7,N)
dice3 = np.random.randint(1,7,N)

dice = dice1+dice2+dice3

hy,hx = np.histogram(dice-1,bins=17,range=(1,18))

plt.bar(hx[1:],hy/N,width=0.5)

plt.xlabel('value of sum',fontsize=14)
plt.ylabel('frequency',fontsize=14)
plt.title('Rolling Three Dices '+str(N)+' Times',fontsize=14)

x = np.linspace(3,18,100)

f,F = gaussian(x,10.5,3)

##plt.plot([0.5,6.5],[1./6,1./6],'r--')

plt.plot(x,f,'r--')

plt.xlim([1.5,18.5])
plt.ylim([0,0.3])

plt.show()
