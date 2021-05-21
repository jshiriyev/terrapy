import io

import numpy as np

import matplotlib.pyplot as plt

from PIL import Image

N = 1000

counter = 0

ax = plt.subplot(1,1,1)

ax.plot([0,N],[0.5,0.5],'r--')
ax.text(N+5,0.5,'True Area',c='r',fontstyle='italic')

ax.set_xlim((0,N))
ax.set_ylim((0,1))

ax.set_xlabel('Number of Iterations',fontsize=14)
ax.set_ylabel('counter per number of iterations',fontsize=14)

plt.tight_layout()

frames = []

for i in range(N):

    x = np.random.rand()
    y = np.random.rand()

    if x+y<=1:
        counter += 1 #counter = counter+1

    if i%10==0:
        dot = ax.scatter(i+1,counter/(i+1))
        #below the image is created in the memory and appended to frames
        buf = io.BytesIO()
        plt.savefig(buf)
        img = Image.open(buf)
        frames.append(img)

plt.close()

#generates the animation from the images created
frames[0].save('triangle.gif',
               format='GIF',
               append_images=frames[1:],
               save_all=True,
               duration=50,
               loop=0)
