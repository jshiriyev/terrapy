import matplotlib.pyplot as plt
import numpy as np

from scipy.sparse.linalg import spsolve

from computation import finite_difference as fd 

def poisson_1D_A(xL,low_bound,xU,upp_bound,beta):
    
    """
    lower_boundary is a list of boundary conditions specified for xL
    upper_boundary is a list of boundary conditions specified for xU

    they contain following entries

    x  : x-value of boundary line
    d  : Dirichlet boundary condition coefficient
    n  : Neumann boundary condition coefficient
    f  : scalar function known for the boundary    
    """
    
    dL = low_bound[0]
    nL = low_bound[1]
    fL = low_bound[2]
    
    dU = upp_bound[0]
    nU = upp_bound[1]
    fU = upp_bound[2]

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

xN0 = 0
xN1 = 7

class grids(): pass

grids.number = 7
grids.ids = np.arange(grids.number)
grids.nodes = np.linspace(xN0,xN1,grids.number+1)
grids.sizes = grids.nodes[1:]-grids.nodes[:-1]
grids.centers = grids.nodes[:-1]+grids.sizes/2
grids.boundary_ids = np.array([[0,1],[grids.number-1,grids.number-2]])
grids.boundary_conditions = np.array([[1,0,200],[0,1,0]])

beta = 10

bvector = np.full(grids.number,beta)

solver = fd(grids)

solver.central(order=2)

solver.implement_bc(bvector)

pressure = spsolve(solver.Amatrix,solver.bvector)

xA,pressAnalytical = poisson_1D_A(xN0,grids.boundary_conditions[0],
                               xN1,grids.boundary_conditions[1],beta)

plt.scatter(grids.centers,pressure,c='r',label='Finite Difference')
plt.plot(xA,pressAnalytical,'k',label='Analytical')

plt.xlabel('x-axis')
plt.ylabel('pressure')

plt.legend(bbox_to_anchor=(0., 1.02, 1., .102), loc='lower left',
           ncol=2, mode="expand", borderaxespad=0.)

plt.xlim([0,7])
##plt.ylim([0,None])

plt.show()
