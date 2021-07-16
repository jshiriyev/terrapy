import matplotlib.pyplot as plt

import numpy as np

from pycse import odelay

from scipy.sparse import csr_matrix as csr

from scipy.special import j0 as bessel_j0
from scipy.special import j1 as bessel_j1
from scipy.special import y0 as bessel_y0
from scipy.special import y1 as bessel_y1

from scipy.special import jvp as bessel_jvp
from scipy.special import yvp as bessel_yvp

from scipy.optimize import minimize
from scipy.optimize import root_scalar

class everdingen():

    def __init__(self,beta,tol=1e-20):

        if isinstance(beta,int):
            beta = tol if beta==0 else beta
        elif isinstance(beta,float):
            beta = tol if beta==0.0 else beta
        elif isinstance(beta,list):
            beta[beta==0] = tol
        elif isinstance(beta,np.ndarray):
            beta[beta==0] = tol
        elif isinstance(beta,tuple):
            return

        self._beta = beta

    def zeroth(self,R):
        
        equation = bessel_j1(self._beta*R)*bessel_y1(self._beta)-\
                   bessel_j1(self._beta)*bessel_y1(self._beta*R)

        return equation

    def first_derivative(self,R):
        
        """
        [j1(x)]prime = j0(x)-1/x*j1(x)
        [y1(x)]prime = y0(x)-1/x*y1(x)
        """

        j1_beta_prime = bessel_jvp(1,self._beta)
        y1_beta_prime = bessel_yvp(1,self._beta)

        j1_beta_R_prime = R*bessel_jvp(1,self._beta*R)
        y1_beta_R_prime = R*bessel_yvp(1,self._beta*R)

        ##j1_beta_prime = bessel_j0(self._beta)-\
        ##                1/self._beta*bessel_j1(self._beta)
        ##
        ##y1_beta_prime = bessel_y0(self._beta)-\
        ##                1/self._beta*bessel_y1(self._beta)
        ##
        ##j1_beta_R_prime = R*bessel_j0(self._beta*R)-\
        ##                  1/self._beta*bessel_j1(self._beta*R)
        ##
        ##y1_beta_R_prime = R*bessel_y0(self._beta*R)-\
        ##                  1/self._beta*bessel_y1(self._beta*R)

        derivative = j1_beta_R_prime*bessel_y1(self._beta)+\
                     bessel_j1(self._beta*R)*y1_beta_prime-\
                     j1_beta_prime*bessel_y1(self._beta*R)-\
                     bessel_j1(self._beta)*y1_beta_R_prime

        return derivative

    def first_derivative_numerical(self,R):
        
        num = self._beta.shape[0]

        idx = list(range(num))

        shape = (num,num)

        Amatrix = csr(shape)

        Amatrix += csr((np.ones(beta.shape[0]-1),(idx[:-1],idx[1:])),shape=shape)
        Amatrix -= csr((np.ones(beta.shape[0]-1),(idx[1:],idx[:-1])),shape=shape)

        Amatrix += csr((np.array([-1,1]),(np.array([0,num-1]),np.array([0,num-1]))),shape=shape)

        dy = Amatrix*self.zeroth(R)
        dx = Amatrix*self._beta

        print(dy[0])
        print(dx[0])

        return dy/dx
        

beta = np.linspace(1e-5,10,500)

P = everdingen(beta)

R = 5

y000 = P.zeroth(R)
y1_a = P.first_derivative(R)
y1_n = P.first_derivative_numerical(R)



plt.plot(beta,y000,label="zeroth order")
plt.plot(beta,y1_a,label="first order")
plt.scatter(beta,y1_n,label="first order, numerical")
plt.axhline(y=0,xmin=0,xmax=beta[-1],color='r')

##plt.ylim([-10,10])

plt.legend()

plt.show()




