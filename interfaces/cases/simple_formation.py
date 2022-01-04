import matplotlib.pyplot as plt

import setup

from interfaces.items import Formation
from interfaces.items import Wells

res = Formation("SI",None,geometry="rectangular")

res.set_dimensions(lengths=(10,5,2))

res.grid((11,11,2))

well = Wells(None)

well.set_names(["GD-601"])

well.set_tracks(None,res)

fig = plt.figure()

ax = plt.axes(projection='3d')

# ax.scatter3D(*res.edge_vertices.T)

for line in res.boundaries:
    ax.plot3D(*line,color='grey')

ax.plot3D(*well.tracks.T)

# ax.scatter3D(*res.grid_centers.T)

ax.set_box_aspect(res.lengths)

# ax.set_axis_off()
# plt.axis("off")

ax.margins(x=0,y=0)

plt.tight_layout()

plt.show()