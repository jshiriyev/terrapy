import os
import sys
import unittest

sys.path.append(os.path.dirname(os.getcwd()))

import numpy as np

from porous_media import fluid

class TestStateFormulation(unittest.TestCase):

    def test_fluid_incompressible(self):
        pass

    def test_fluid_slightly_compressible(self):
        pass

    def test_fluid_compressible(self):
        pass
                       
if __name__ == "__main__":

    unittest.main()
