from scipy.sparse import csr_matrix as csr
from scipy.sparse import csc_matrix as csc
from scipy.sparse import hstack
from scipy.sparse import triu

from scipy.sparse.linalg import spsolve_triangular as sps
from scipy.sparse.linalg import splu

import matplotlib.pyplot as plt
import numpy as np

if __name__ == "__main__":
    import setup

from interfaces.items import Rectangle

class Poisson():

    """
    Finite Difference Solution of 2D Poisson's equation

    \\del^2 U/\\del x^2+\\del^2 U/\\del y^2 = f(x,y)

    with boundary conditions:

    u = 0 at left and right edges,
    u = 10 at bottom edge,
    and at top edge U+\\del U/\\del y = 5

    """

    def __init__(self,rectangle):

        self.rectangle = rectangle

    def solve(self):

        pass

if __name__ == "__main__":

    rec = Rectangle()

    rec.set_size((1,1))

    rec.grid((3,2))

    Nx = 5
    Ny = 5

    dx = 1./Nx
    dy = 1./Ny

