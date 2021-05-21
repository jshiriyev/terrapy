import numpy as np
import matplotlib.pyplot as plt

def bootstrap(X,Ncol):
    
    Nrow = X.shape[0]
    idx = np.random.randint(0,Nrow,(Nrow,Ncol))
##    print(idx)
    newX = X[idx]
##    print(newX)
    
    return np.mean(newX,axis=0)

data = np.loadtxt("petrophysics_data.txt",skiprows=1)

por = data[:,1]

##print(por.mean())
por_boots = bootstrap(por,1000)

plt.hist(por_boots,bins=10,align="left")
plt.xlabel("mean porosity",fontsize=14)
plt.ylabel("frequency",fontsize=14)
plt.show()
