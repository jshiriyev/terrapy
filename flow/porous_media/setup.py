import os
import sys

path = __file__

while True:

    if os.path.basename(path)=="porous_media": break
    
    path = os.path.dirname(path)

sys.path.append(path)