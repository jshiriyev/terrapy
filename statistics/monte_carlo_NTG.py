import numpy as np
import matplotlib.pyplot as plt

N = 10000 #number of realizations

meanA, stdA = 10, 2
meanB, stdB = 24, 4

thicknessA = np.random.normal(meanA,stdA,N)
thicknessB = np.random.normal(meanB,stdB,N)

thickness_total = thicknessA+thicknessB

sortedTT = np.sort(thickness_total)

probs = 1/(N+1)

x = np.linspace(1,N,N)*probs
#cx = np.cumsum(x)

fig,ax = plt.subplots()

plt.plot(sortedTT,x,'k.')
plt.xlim([0,60])
plt.ylim([0,1])
ax.set_aspect(30)

plt.grid()
plt.show()

mean_total_thickness = np.mean(sortedTT)
var_total_thickness = np.var(sortedTT)

p10 = np.percentile(sortedTT,10)
p90 = np.percentile(sortedTT,90)

print(mean_total_thickness)
print(var_total_thickness)
print(p10)
print(p90)
