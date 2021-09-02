import os
import sys
import unittest

sys.path.append(os.path.dirname(os.getcwd()))

import numpy as np

from porous_media import fluid

class TestLinearFlow(unittest.TestCase):

    def singlephase(self):
        pass

    def multiphase(self):
        pass
                       
if __name__ == "__main__":

    unittest.main()
