import numpy as np

from scipy.stats import t
from scipy.stats import norm
from scipy.special import erfc

import matplotlib.pyplot as plt

mean = 0
std = 1

x = np.linspace(-4,4,200)

fg = norm(mean,std).pdf(x)

ft1 = t.pdf(x,1)
ft2 = t.pdf(x,3)
ft3 = t.pdf(x,5)
ft4 = t.pdf(x,10)
ft5 = t.pdf(x,100)

plt.plot(x,fg,'k',linewidth=5)
plt.plot(x,ft1)
plt.plot(x,ft2)
plt.plot(x,ft3)
plt.plot(x,ft4)
plt.plot(x,ft5)
plt.legend(['Gaussian','t-dist df=1',
            't-dist df=3','t-dist df=5',
            't-dist df=10','t-dist df=100'])
plt.show()

##def gaussian(x,mean,variance):
##
##        m = mean
##        s = np.sqrt(variance)
##
##        b = (x-m)/(np.sqrt(2)*s)
##        
##        f = 1/(s*(2*np.pi)**0.5)*np.exp(-b**2)
##        
##        F = 1/2*(2-erfc(b))
##        
##        return f,F

##mean = 0
##variance = 1
##std = np.sqrt(variance)
##
##x = 2
##
##f1,F1 = gaussian(x,mean,variance)
##print(f1)
##print(F1)

#OR#
##f2 = norm(mean,std).pdf(x)
##F2 = norm(mean,std).cdf(x)
##print(f2)
##print(F2)

##x = np.linspace(-4,4,1000)
##
##f,F = gaussian(x,mean,variance)
##
##plt.plot(x,f)
##plt.plot(x,F)
##
##plt.xlabel('x')
##
##plt.annotate('f(x)', xy=(3,0.05), fontsize=15, style='italic')
##plt.annotate('F(x)', xy=(3,0.90), fontsize=15, style='italic')
##
##plt.title('Normalized Gaussian Distribution')
##
##plt.show()

