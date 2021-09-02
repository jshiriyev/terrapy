import os
import sys
import unittest

sys.path.append(os.path.dirname(os.getcwd()))

import numpy as np

from porous_media import fluid

class TestRadialFlow(unittest.TestCase):

    def conventional_solution(self):
        pass

    def everdingen_solution(self):
        pass

    def green_solution(self):
        pass
                       
if __name__ == "__main__":

    unittest.main()
