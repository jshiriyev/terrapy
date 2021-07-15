import matplotlib.pyplot as plt

import numpy as np

from scipy.special import j0
from scipy.special import j1
from scipy.special import y0
from scipy.special import y1

from scipy.optimize import minimize
from scipy.optimize import root_scalar

R = 1000

def equation(_beta,tol=1e-20):
    
    if isinstance(_beta,int):
        _beta = tol if _beta==0 else _beta
        print("was int")
    elif isinstance(_beta,float):
        _beta = tol if _beta==0.0 else _beta
        print("was float")
    elif isinstance(_beta,list):
        _beta[_beta==0] = tol
        print("was list")
    elif isinstance(_beta,np.ndarray):
        _beta[_beta==0] = tol
        print("was numpy")
    elif isinstance(_beta,tuple):
        return
    
    return j1(_beta*R)*y1(_beta)-j1(_beta)*y1(_beta*R)

def objective(_beta):
    return np.abs(equation(_beta))

##res = root_scalar(equation,bracket=[0,10],method="brentq") #bracket=[0,10]
##print(res.x)

a = np.linspace(0,0.01,10000)

y = objective(a)

plt.plot(a,y)
##plt.scatter(res.root,0)

plt.show()
