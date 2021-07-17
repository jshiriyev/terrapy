import matplotlib.pyplot as plt

import numpy as np

##from pycse import odelay
from scipy.integrate import odeint

from scipy.sparse import csr_matrix as csr

##from scipy.special import j0 as bessel_j0
from scipy.special import j1 as bessel_j1
##from scipy.special import y0 as bessel_y0
from scipy.special import y1 as bessel_y1

from scipy.special import jvp as bessel_jvp
from scipy.special import yvp as bessel_yvp

from scipy.optimize import minimize
from scipy.optimize import root_scalar



##class everdingen():
##
##    def __init__(self,endpoint,N):
##
##        self.beta = -np.linspace(-endpoint,0,N,endpoint=False)
##        self.beta.sort()
##
####        if isinstance(beta,int):
####            beta = tol if beta==0 else beta
####        elif isinstance(beta,float):
####            beta = tol if beta==0.0 else beta
####        elif isinstance(beta,list):
####            beta[beta==0] = tol
####        elif isinstance(beta,np.ndarray):
####            beta[beta==0] = tol
####        elif isinstance(beta,tuple):
####            return

def odelay(func, y0, xspan, events, TOLERANCE=1e-6,
           fsolve_args=None, **kwargs):
    """Solve an ODE with events.

    Parameters
    ----------
    func : y' = func(Y, x)
        func takes an independent variable x, and the Y value(s),
        and returns y'.

    y0 : The initial conditions at xspan[0].

    xspan : array to integrate the solution at.
        The initial condition is at xspan[0].

    events : list of callable functions with signature event(Y, x).
        These functions return zero when an event has happened.

        [value, isterminal, direction] = event(Y, x)

        value is the value of the event function. When value = 0, an event
        is triggered

        isterminal = True if the integration is to terminate at a zero of
        this event function, otherwise, False.

        direction = 0 if all zeros are to be located (the default), +1
        if only zeros where the event function is increasing, and -1 if
        only zeros where the event function is decreasing.

    TOLERANCE : float
        Used to identify when an event has occurred.

    fsolve_args : a dictionary of options for fsolve

    kwargs : Additional keyword options you want to send to odeint.

    Returns
    -------
    [x, y, te, ye, ie]
        x is the independent variable array
        y is the solution
        te is an array of independent variable values where events occurred
        ye is an array of the solution at the points where events occurred
        ie is an array of indices indicating which event function occurred.
    """
    if 'full_output' in kwargs:
        raise Exception('full_output not supported as an option')

    if fsolve_args is None:
        fsolve_args = {}

    x0 = xspan[0]  # initial point

    X = [x0]
    sol = [y0]
    TE, YE, IE = [], [], []  # to store where events occur

    # initial value of events
    e = np.zeros((len(events), len(xspan)))
    for i, event in enumerate(events):
        e[i, 0], isterminal, direction = event(y0, x0)

    # now we step through the integration
    for i, x1 in enumerate(xspan[0:-1]):
        x2 = xspan[i + 1]
        f1 = sol[i]

        f2 = odeint(func, f1, [x1, x2], **kwargs)

        X += [x2]
        sol += [f2[-1, :]]

        # check event functions. At each step we compute the event
        # functions, and check if they have changed sign since the
        # last step. If they changed sign, it implies a zero was
        # crossed.
        for j, event in enumerate(events):
            e[j, i + 1], isterminal, direction = event(sol[i + 1], X[i + 1])

            if ((e[j, i + 1] * e[j, i] < 0) or      # sign change in
                                                    # event means zero
                                                    # crossing
                np.abs(e[j, i + 1]) < TOLERANCE or  # this point is
                                                    # practically 0
                np.abs(e[j, i]) < TOLERANCE):

                xLt = X[-1]       # Last point
                fLt = sol[-1]

                # we need to find a value of x that makes the event zero
                def objective(x):
                    # evaluate ode from xLT to x
                    txspan = [xLt, x]
                    tempsol = odeint(func, fLt, txspan, **kwargs)
                    sol = tempsol[-1, :]
                    val, isterminal, direction = event(sol, x)
                    return val

                from scipy.optimize import fsolve

                # this should be the value of x that makes the event zero
                xZ, = fsolve(objective, xLt, **fsolve_args)

                # now evaluate solution at this point, so we can
                # record the function values here.
                txspan = [xLt, xZ]
                tempsol = odeint(func, fLt, txspan, **kwargs)
                fZ = tempsol[-1, :]

                vZ, isterminal, direction = event(fZ, xZ)

                COLLECTEVENT = False
                if direction == 0:
                    COLLECTEVENT = True
                elif (e[j, i + 1] > e[j, i]) and direction == 1:
                    COLLECTEVENT = True
                elif (e[j, i + 1] < e[j, i]) and direction == -1:
                    COLLECTEVENT = True

                if COLLECTEVENT:
                    TE.append(xZ)
                    YE.append(fZ)
                    IE.append(j)

                    if isterminal:
                        X[-1] = xZ
                        sol[-1] = fZ
                        return (np.array(X),
                                np.array(sol),
                                np.array(TE),
                                np.array(YE),
                                np.array(IE))

    # at the end, return what we have
    return (np.array(X),
            np.array(sol),
            np.array(TE),
            np.array(YE),
            np.array(IE))

def root_function(beta):

    J1B = bessel_j1(beta)
    Y1B = bessel_y1(beta)
    
    J1BR = bessel_j1(beta*R)
    Y1BR = bessel_y1(beta*R)

    return J1BR*Y1B-J1B*Y1BR
    
def root_function_first_derivative(root_function,beta):

    J1B = bessel_j1(beta)
    Y1B = bessel_y1(beta)
    
    J1BR = bessel_j1(beta*R)
    Y1BR = bessel_y1(beta*R)

    J1B_prime = bessel_jvp(1,beta)
    Y1B_prime = bessel_yvp(1,beta)

    J1BR_prime = R*bessel_jvp(1,beta*R)
    Y1BR_prime = R*bessel_yvp(1,beta*R)

    return J1BR_prime*Y1B+J1BR*Y1B_prime-J1B_prime*Y1BR-J1B*Y1BR_prime

def e1(root_function,x):
    "event function to find zeros of f"
    isterminal = False
    value = root_function
    direction = 0
    return value, isterminal, direction

##    def root_function_first_derivative_numerical(self,R):
##        
##        num = self.beta.shape[0]
##
##        idx = list(range(num))
##        
##        idx_diag_ends = np.array([0,num-1])
##
##        Amatrix = csr((num,num))
##
##        Amatrix += csr((np.ones(num-1),(idx[:-1],idx[1:])),shape=(num,num))
##        Amatrix -= csr((np.ones(num-1),(idx[1:],idx[:-1])),shape=(num,num))
##        Amatrix += csr((np.array([-1,1]),(idx_diag_ends,idx_diag_ends)),shape=(num,num))
##
##        y_prime = Amatrix*self.root_function(R)
##        x_prime = Amatrix*self.beta
##
##        return y_prime/x_prime

if __name__ == "__main__":
    
##    beta = np.linspace(0,10,500)

##    beta = 0

##    P = everdingen(10,1000)
##
    R = 10
##
##    f0 = P.root_function(R)
##    f1A = P.root_function_first_derivative(R)
##    f1N = P.root_function_first_derivative_numerical(R)
##
    ##def f(x):
    ##    "function we want roots for"
    ##    return x * jn(1, x) - Bi * jn(0, x)
    ##
    ##def fprime(f, x):
    ##    "df/dx"
    ##    return x * jn(0, x) - Bi * (-jn(1, x))
    ##
    ##def e1(f, x):
    ##    "event function to find zeros of f"
    ##    isterminal = False
    ##    value = f
    ##    direction = 0
    ##    return value, isterminal, direction
    ##
    f0 = root_function(1e-20)
    beta = np.linspace(0.01,10,999)
    ##
    x, fsol, XE, FE, IE = odelay(root_function_first_derivative,f0,beta, events=[e1])
    ##
    ##plt.plot(x, fsol, '.-', label='Numerical solution')
    ##plt.plot(xspan, f(xspan), '--', label='Analytical function')
    
    plt.plot(beta,root_function(beta),label="zeroth order")
    
    plt.axhline(y=0,xmin=0,xmax=beta[-1],color='r')
    plt.plot(XE,FE,'ro',label='roots')

    ##plt.ylim([-10,10])

    plt.legend()

    plt.show()

