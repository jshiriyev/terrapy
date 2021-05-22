<<<<<<< HEAD
import numpy as np

import matplotlib.pyplot as plt

def objective(x):
    return (x-2)**2+2

##def objective(x):
##    F = 10+0.2*x**(2.3)
##    G = 10+80*np.exp(-0.3*x)
##    return F+G

##def objective(x):
##    return x*(x-2)*(x-3)*(x-6)+15

##def objective(x):
##    return (x**2)/(1-x)

def dichotomous(xL,xU,r=1/2,Nmax=50,tol=1e-3):
    
    for i in range(Nmax):

        x1 = xL+r*(xU-xL)
        x2 = xU-r*(xU-xL)
        
        f1 = objective(x1)
        f2 = objective(x2)

        if f1<f2:
            xL = x2
        else:
            xU = x1

        if abs(x1-x2)<tol:
            xopt = (x1+x2)/2
            break

    try:
        print("Converged result is: %.8f" %xopt)
        print("Number of iterations is: %d" %(i+1))
    except:
        print("Convergence could not be obtained with %d iterations" %(Nmax))

xL = -3
xU = 3

Gr = (5**(1/2)-1)/2

dichotomous(xL,xU,r=Gr,Nmax=200,tol=1e-5)

x = np.linspace(xL,xU,200)

plt.plot(x,objective(x))
plt.xlabel('x-axis')
plt.ylabel('objective function')

plt.show()
=======
import numpy as np

import matplotlib.pyplot as plt

def objective(x):
    return (x-2)**2+2

##def objective(x):
##    F = 10+0.2*x**(2.3)
##    G = 10+80*np.exp(-0.3*x)
##    return F+G

##def objective(x):
##    return x*(x-2)*(x-3)*(x-6)+15

##def objective(x):
##    return (x**2)/(1-x)

def dichotomous(xL,xU,r=1/2,Nmax=50,tol=1e-3):
    
    for i in range(Nmax):

        x1 = xL+r*(xU-xL)
        x2 = xU-r*(xU-xL)
        
        f1 = objective(x1)
        f2 = objective(x2)

        if f1<f2:
            xL = x2
        else:
            xU = x1

        if abs(x1-x2)<tol:
            xopt = (x1+x2)/2
            break

    try:
        print("Converged result is: %.8f" %xopt)
        print("Number of iterations is: %d" %(i+1))
    except:
        print("Convergence could not be obtained with %d iterations" %(Nmax))

xL = -3
xU = 3

Gr = (5**(1/2)-1)/2

dichotomous(xL,xU,r=Gr,Nmax=200,tol=1e-5)

x = np.linspace(xL,xU,200)

plt.plot(x,objective(x))
plt.xlabel('x-axis')
plt.ylabel('objective function')

plt.show()
>>>>>>> 7b477b82f966a4333360962f631514d3c9a7e602
