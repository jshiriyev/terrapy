import numpy as np
import matplotlib.pyplot as plt

N = 100

volume = 1000*1000*100

conversion = 6.290

volume = volume*conversion

mean_NTG, std_NTG = 0.3, 0.05
mean_porosity, std_porosity = 0.15, 0.03
mean_saturation, std_saturation = 0.7, 0.08

NTG = np.random.normal(mean_NTG,std_NTG,N)
porosity = np.random.normal(mean_porosity,std_porosity,N)
saturation = np.random.normal(mean_saturation,std_saturation,N)

OOIP = volume*NTG*porosity*saturation

sOOIP = np.sort(OOIP)

probs = 1/(N+1)

x = np.linspace(1,N,N)*probs

plt.plot(sOOIP,x,'k')

plt.xlim([0,4e7])
plt.ylim([0,1])

plt.xlabel('OOIP [bbl]')
plt.ylabel('Cumulative Probability')

plt.title('OOIP Uncertainty Model')

plt.show()

p90 = np.percentile(sOOIP,90)

print(sOOIP.min())
print(np.percentile(sOOIP,10))
print(np.percentile(sOOIP,50))
print(np.percentile(sOOIP,90))
print(sOOIP.max())
