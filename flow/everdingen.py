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

from scipy.optimize import fsolve
from scipy.optimize import minimize
from scipy.optimize import root_scalar

# class everdingen():

#    def __init__(self,RR,tt,num_of_terms):

##        if isinstance(beta,int):
##            beta = tol if beta==0 else beta
##        elif isinstance(beta,float):
##            beta = tol if beta==0.0 else beta
##        elif isinstance(beta,list):
##            beta[beta==0] = tol
##        elif isinstance(beta,np.ndarray):
##            beta[beta==0] = tol
##        elif isinstance(beta,tuple):
##            return

    # def odelay(func, y0, xspan, events, TOLERANCE=1e-7,
    #            fsolve_args=None, **kwargs):
    #     """Solve an ODE with events.

    #     Parameters
    #     ----------
    #     func : y' = func(Y, x)
    #         func takes an independent variable x, and the Y value(s),
    #         and returns y'.

    #     y0 : The initial conditions at xspan[0].

    #     xspan : array to integrate the solution at.
    #         The initial condition is at xspan[0].

    #     events : list of callable functions with signature event(Y, x).
    #         These functions return zero when an event has happened.

    #         [value, isterminal, direction] = event(Y, x)

    #         value is the value of the event function. When value = 0, an event
    #         is triggered

    #         isterminal = True if the integration is to terminate at a zero of
    #         this event function, otherwise, False.

    #         direction = 0 if all zeros are to be located (the default), +1
    #         if only zeros where the event function is increasing, and -1 if
    #         only zeros where the event function is decreasing.

    #     TOLERANCE : float
    #         Used to identify when an event has occurred.

    #     fsolve_args : a dictionary of options for fsolve

    #     kwargs : Additional keyword options you want to send to odeint.

    #     Returns
    #     -------
    #     [x, y, te, ye, ie]
    #         x is the independent variable array
    #         y is the solution
    #         te is an array of independent variable values where events occurred
    #         ye is an array of the solution at the points where events occurred
    #         ie is an array of indices indicating which event function occurred.
    #     """
    #     if 'full_output' in kwargs:
    #         raise Exception('full_output not supported as an option')

    #     if fsolve_args is None:
    #         fsolve_args = {}

    #     x0 = xspan[0]  # initial point

    #     X = np.array([x0])
    #     sol = np.array([y0])
    #     TE, YE, IE = [], [], []  # to store where events occur

    #     # initial value of events
    #     e = np.zeros((len(events), len(xspan)))
    #     for i, event in enumerate(events):
    #         e[i, 0], isterminal, direction = event(y0, x0)

        

    #     # now we step through the integration
    #     for i, x1 in enumerate(xspan[0:-1]):
    #         x2 = xspan[i + 1]
    #         f1 = sol[i]

    #         f2 = odeint(func, f1, [x1, x2], **kwargs)

    # ##        print(type(f2[-1, :]))
    # ##        print(f2[-1, :])
            
    #         X = np.append(X,x2)
    #         sol = np.append(sol,f2[-1, :])

    #         # check event functions. At each step we compute the event
    #         # functions, and check if they have changed sign since the
    #         # last step. If they changed sign, it implies a zero was
    #         # crossed.
    #         for j, event in enumerate(events):
    #             e[j, i + 1], isterminal, direction = event(sol[i + 1], X[i + 1])

    #             if ((e[j, i + 1] * e[j, i] < 0) or      # sign change in
    #                                                     # event means zero
    #                                                     # crossing
    #                 np.abs(e[j, i + 1]) < TOLERANCE or  # this point is
    #                                                     # practically 0
    #                 np.abs(e[j, i]) < TOLERANCE):

    #                 xLt = X[-1]       # Last point
    #                 fLt = sol[-1]

    #                 # we need to find a value of x that makes the event zero
    #                 def objective(x):
    #                     # evaluate ode from xLT to x
    #                     txspan = [xLt, x[0]]
    # ##                    print(x)
    #                     tempsol = odeint(func, fLt, txspan, **kwargs)
    #                     sol = tempsol[-1, :]
    #                     val, isterminal, direction = event(sol, x)
    #                     return val

    #                 # this should be the value of x that makes the event zero
    #                 xZ, = fsolve(objective, xLt, **fsolve_args)
                    

    #                 # now evaluate solution at this point, so we can
    #                 # record the function values here.
    #                 txspan = [xLt, xZ]
    #                 tempsol = odeint(func, fLt, txspan, **kwargs)
    #                 fZ = tempsol[-1, :]

    #                 vZ, isterminal, direction = event(fZ, xZ)

    #                 COLLECTEVENT = False
    #                 if direction == 0:
    #                     COLLECTEVENT = True
    #                 elif (e[j, i + 1] > e[j, i]) and direction == 1:
    #                     COLLECTEVENT = True
    #                 elif (e[j, i + 1] < e[j, i]) and direction == -1:
    #                     COLLECTEVENT = True

    #                 if COLLECTEVENT:
    #                     TE.append(xZ)
    #                     YE.append(fZ)
    #                     IE.append(j)

    #                     if isterminal:
    #                         X[-1] = xZ
    #                         sol[-1] = fZ
    #                         return (np.array(X),
    #                                 np.array(sol),
    #                                 np.array(TE),
    #                                 np.array(YE),
    #                                 np.array(IE))

    #     # at the end, return what we have
        
    #     return (np.array(X),
    #             np.array(sol),
    #             np.array(TE),
    #             np.array(YE),
    #             np.array(IE))

def root_function(beta,RR):

    """
    This is the function that outputs values of root function 
    defined in Everdingen solution. At singularity point of \beta = 0,
    it outputs its value at limit.
    """

    if type(beta)==int or type(beta)==float:
        beta = np.array([beta])
    elif type(beta)==list or type(beta)==tuple:
        beta = np.array(beta)

    res = np.empty(beta.shape)

    res[beta==0] = -RR/np.pi+1/(np.pi*RR)

    J1B = bessel_j1(beta[beta>0])
    Y1B = bessel_y1(beta[beta>0])
    
    J1BR = bessel_j1(beta[beta>0]*RR)
    Y1BR = bessel_y1(beta[beta>0]*RR)
    
    res[beta>0] = J1BR*Y1B-J1B*Y1BR

    return res

def find_roots(RR,num_of_terms=1):

    roots = np.empty(num_of_terms)

    for idx in range(num_of_terms):

        lower_bound = ((2*idx+1)*np.pi)/(2*R-2)
        upper_bound = ((2*idx+3)*np.pi)/(2*R-2)

        bracket = (lower_bound,upper_bound)

        solver = root_scalar(root_function,args=(RR,),method="brentq",bracket=bracket)

        roots[idx] = solver.root

    return roots

    
def root_function_first_derivative(beta,RR):

    """
    it needs a treshold value of beta and two functions to calculate analytical values,
    one close to singularity \beta=0, the other at larger values of \betta
    """

    J1B = bessel_j1(beta)
    Y1B = bessel_y1(beta)
    
    J1BR = bessel_j1(beta*RR)
    Y1BR = bessel_y1(beta*RR)

    J1B_prime = bessel_jvp(1,beta)
    Y1B_prime = bessel_yvp(1,beta)

    J1BR_prime = RR*bessel_jvp(1,beta*RR)
    Y1BR_prime = RR*bessel_yvp(1,beta*RR)

    return J1BR_prime*Y1B+J1BR*Y1B_prime-J1B_prime*Y1BR-J1B*Y1BR_prime
    
# def e1(root_function,x):
#     "event function to find zeros of f"
#     isterminal = False
#     value = root_function
#     direction = 0
#     return value, isterminal, direction

def root_function_first_derivative_numerical(beta,RR):
   
    num = beta.shape[0]

    idx = list(range(num))

    idx_diag_ends = np.array([0,num-1])

    Amatrix = csr((num,num))

    Amatrix += csr((np.ones(num-1),(idx[:-1],idx[1:])),shape=(num,num))
    Amatrix -= csr((np.ones(num-1),(idx[1:],idx[:-1])),shape=(num,num))
    Amatrix += csr((np.array([-1,1]),(idx_diag_ends,idx_diag_ends)),shape=(num,num))

    y_prime = Amatrix*root_function(beta,RR)
    x_prime = Amatrix*beta

    return y_prime/x_prime

if __name__ == "__main__":

    # beta = np.logspace(-4,3,10000)

    beta = np.linspace(0,100,100000)

    R = 2.5

    roots = find_roots(R,num_of_terms=2)

    print(roots)

    # P = everdingen(10,1000)

    # f0 = P.root_function(R)
    # f1A = P.root_function_first_derivative(R)
    # f1N = P.root_function_first_derivative_numerical(R)

    # f0 = root_function(1e-20)

    # x, fsol, XE, FE, IE = odelay(root_function_first_derivative,f0,beta, events=[e1])

    # print(C[0]+D[0])

    # plt.plot(beta,root_function(beta,R),label="root function")
    # plt.plot(beta,root_function_first_derivative_numerical(beta,R),label="root function derivative")

    plt.plot(beta,root_function(beta,R),label="J1BR*Y1B-J1B*Y1BR")
    plt.plot(roots,np.zeros(roots.shape),'.')
    # plt.semilogy(beta,np.abs(B),label="J1BR*Y1B_prime")
    # plt.semilogy(beta,np.abs(C),label="J1B_prime*Y1BR")
    # plt.semilogy(beta,np.abs(D),label="J1B*Y1BR_prime")

    # plt.axhline(y=0,xmin=beta[0],xmax=beta[-1],color='k')
    # plt.axhline(y=0,xmin=np.log10(beta[0]),xmax=np.log10(beta[-1]),color='k')

    # plt.plot(XE,FE,'ro',label='roots')
    # plt.plot(x, fsol, '.-', label='Numerical solution')
    # plt.plot(xspan, f(xspan), '--', label='Analytical function')

    # plt.ylim([-10,10])

    # plt.legend()

    plt.show()