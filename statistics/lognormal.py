import numpy as np
import matplotlib.pyplot as plt

from scipy.special import erfc

mean = 0

variance = 1

x = np.linspace(0.000001,5,1000)

f = 1/variance/np.sqrt(2*np.pi)/x*np.exp(-(np.log(x)-mean)**2/2/variance**2)

F = 1/2*(2-erfc((np.log(x)-mean)/(variance*np.sqrt(2))))

plt.plot(x,f)
plt.plot(x,F)

plt.xlabel('x')

plt.ylim(top=1)

plt.annotate('f(x)', xy=(4,0.07), fontsize=15, style='italic')
plt.annotate('F(x)', xy=(4,0.85), fontsize=15, style='italic')

plt.title('Log-Normal Distribution')

plt.show()

