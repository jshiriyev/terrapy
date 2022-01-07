import io

import matplotlib.pyplot as plt
import numpy as np

from PIL import Image

if __name__ == "__main__":
    import setup

from flow.pormed.conrelation import relative_permeability

from flow.pormed.numerical import IMPES

from interfaces.items import Rectangle
from interfaces.items import Formation
from interfaces.items import Wells
from interfaces.items import Fluids

# FORMATION

geo = Rectangle((1200,600),200)

geo.grid((3,3))

res = Formation(geo,"FU")

res.set_depth(-1000)

res.set_porosity(0.26)

res.set_permeability(1800)

res.set_compressibility(3e-6)

# FLUID

fluids = Fluids(2)

fluids.set_names(("water","oil"))
fluids.set_density((62.4,53.))
fluids.set_compressibility((2e-6,5e-6))
fluids.set_viscosity((1.,5.))
fluids.set_fvf((1.,1.))

# WELLS

wells = Wells(None)

wells.set_names(["inj","prod1","prod2"])

wells.set_tracks((
	(( 600,300,400),( 600,300,0)),
	(( 200,100,400),( 200,100,0)),
	((1000,500,400),(1000,500,0)),
	))

wells.set_flowconds(
	conditions=("rate","rate","bhp"),
	limits=(3000,-2000,800),
	fluids=("water","both","both"))

wells.set_skinfactors((0,0,0))

wells.set_radii((0.5,0.5,0.5))

# RELATIVE PERMEABILITY

relperm = relative_permeability(
	Sorow=0.2,
	Swc=0.2,
	krowc=1,
	krwor=0.2,
	no=3,
	nw=3)

# SOLVING

solver = IMPES(res,fluids,wells,relperm)

solver.set_transmissibility()
solver.set_wells()
solver.set_time(0.1,500)
solver.initialize(pressure=1000,saturation=0.2)
solver.solve()

# PLOTTING

fig = plt.figure()

ax = fig.add_subplot(111)

# ax = plt.axes(projection='3d')

# # ax.scatter3D(*res.edge_vertices.T)

# geo.plot(ax,showVertices=False,showGridCenters=False)

for track,name in zip(solver.wells.tracks,solver.wells.itemnames):

	ax.scatter(*track[0,:2],s=5,color="black")
	# ax.scatter(*track.T,color="black",linewidth=1)
	ax.text(*track[0,:2]+10,name)

frames = []

# for index,time in enumerate(solver.time_array):

X = np.reshape(res.grid_centers[:,0],(3,3))
Y = np.reshape(res.grid_centers[:,1],(3,3))

P = np.flipud(np.reshape(solver.pressure[:,-1],(3,3)))

im = ax.imshow(P,extent=[0,res.lengths[0],0,res.lengths[1]],cmap="Oranges")

cb = plt.colorbar(im)

plt.show()

# 	buf = io.BytesIO()

# 	plt.savefig(buf)

# 	img = Image.open(buf)

# 	frames.append(img)

# 	cb.remove()
# 	im.remove()

# generates the animation from the images created

# frames[0].save(
# 	'balhoff.gif',
# 	format='GIF',
# 	append_images=frames[1:],
# 	save_all=True,
# 	duration=100,
# 	loop=0)

# ax.scatter3D(*solver.res.grid_centers.T)

# ax.set_xlabel("x-axis")
# ax.set_ylabel("y-axis")

# ax.set_box_aspect(solver.res.lengths)

# ax.set_axis_off()

# ax.margins(x=0,y=0)

# plt.tight_layout()

# ax.set_box_aspect(res.lengths[1]/res.lengths[0])




# plt.figure()

# plt.plot(Sw,solver.rp.kro,"k--",label="oil")
# plt.plot(Sw,solver.rp.krw,"k-",label="water")

# plt.xlabel("Water Saturation")
# plt.ylabel("Relative Permeability")

# plt.legend()

# plt.xlim((0,1))