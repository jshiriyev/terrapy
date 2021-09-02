import os
import sys
import unittest

sys.path.append(os.path.dirname(os.getcwd()))

import numpy as np

from porous_media import fluid

class TestComplexFlow(unittest.TestCase):

    def theta_function(self):
        pass
    
    def point_source(self):
        pass
    
    def line_source(self):
        pass
    
    def plane_source(self):
        pass
                       
if __name__ == "__main__":

    unittest.main()
