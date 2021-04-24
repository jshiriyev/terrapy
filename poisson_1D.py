import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse.linalg import spsolve

from diffmat import central
from diffmat import central2

def poisson_1D_A(lower_boundary,upper_boundary,beta):
    
    """
    lower_boundary is a list of boundary conditions specified for xL
    upper_boundary is a list of boundary conditions specified for xU

    they contain following entries

    x  : x-value of boundary line
    d  : Dirichlet boundary condition coefficient
    n  : Neumann boundary condition coefficient
    f  : scalar function known for the boundary    
    """
    
    xL = lower_boundary[0]
    dL = lower_boundary[1]
    nL = lower_boundary[2]
    fL = lower_boundary[3]
    
    xU = upper_boundary[0]
    dU = upper_boundary[1]
    nU = upper_boundary[2]
    fU = upper_boundary[3]

    rhs = np.zeros((2,1))

    rhs[0,0] = fL-dL*beta/2*xL**2-nL*beta*xL
    rhs[1,0] = fU-dU*beta/2*xU**2-nU*beta*xU

    mat = np.zeros((2,2))

    mat[0,0] = dL*xL+nL
    mat[0,1] = dL
    mat[1,0] = dU*xU+nU
    mat[1,1] = dU

    coeff = np.linalg.solve(mat,rhs)

    c1 = coeff[0]
    c2 = coeff[1]

    dx = (xU-xL)/1000
    
    x = np.arange(xL,xU+dx/2,dx)

    u = beta/2*x**2+c1*x+c2

    return x,u

def poisson_1D_N(dx,
                 lower_boundary,
                 upper_boundary,
                 beta):

    """
    it is a finite difference solution of one dimensional Poisson's equation
    generally defined as:

    \nabla^2 u=\beta

    beta can be a scalar value or a (function)?
    
    lower_boundary is a list of boundary conditions specified for xL
    upper_boundary is a list of boundary conditions specified for xU

    they contain following entries

    x  : x-value of boundary line
    d  : Dirichlet boundary condition coefficient
    n  : Neumann boundary condition coefficient
    f  : scalar function known for the boundary
    """
    
    xL = lower_boundary[0]
    dL = lower_boundary[1]
    nL = lower_boundary[2]
    fL = lower_boundary[3]
    
    xU = upper_boundary[0]
    dU = upper_boundary[1]
    nU = upper_boundary[2]
    fU = upper_boundary[3]

    A = central2(dx,order=2)

    b = np.full(A.shape[0],beta)

    """boundary conditions start"""
    # implementation of LHS boundary conditions
    bL = 8/((3*dx[0]+dx[1])*(dL*dx[0]-2*nL))
    A[0,0] = A[0,0]-bL*dL
    b[0] = b[0]-bL*fL
    # implementation of RHS boundary conditions
    bU = 8/((3*dx[-1]+dx[-2])*(dU*dx[-1]-2*nU))
    A[-1,-1] = A[-1,-1]-bU*dU
    b[-1] = b[-1]-bU*fU
    """boundary conditions end"""

    x_center = np.linspace(xL,xU,dx.shape[0]+1)[:-1]+dx/2

    u_center = spsolve(A,b)

    return x_center,u_center
    
""" INPUT """

beta = 10

"""
bound
first entry is for the node defined as boundary
second entry is for the dirichlet boundary condition coefficient
third entry is for the neumann bundary condition coefficient
fourth entry is for the f scalar value for the condition
"""

lower_bound = (0,1,0,200)
upper_bound = (7,0,1,0)

N = 70 #number of elements for FD solution

""" END of INPUT """

x_nodes = np.linspace(lower_bound[0],upper_bound[0],N+1)

dx = x_nodes[1:]-x_nodes[:-1]

xA,uA = poisson_1D_A(lower_bound,upper_bound,beta)
xN,uN = poisson_1D_N(dx,lower_bound,upper_bound,beta)

plt.scatter(xN,uN,c='r',label='Finite Difference')
plt.plot(xA,uA,'k',label='Analytical Solution')

plt.legend(bbox_to_anchor=(0., 1.02, 1., .102), loc='lower left',
           ncol=2, mode="expand", borderaxespad=0.)

plt.tight_layout()

plt.show()
