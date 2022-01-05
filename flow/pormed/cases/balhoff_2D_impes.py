if __name__ == "__main__":
    import setup

from flow.pormed.conrelation import relative_permeability

from flow.pormed.numerical import IMPES

from interfaces.items import Formation
from interfaces.items import Wells
from interfaces.items import Fluids

# FORMATION

res = Formation("FU",None)

res.set_dimensions((1200,600,200))

res.grid((3,3,1))

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

solver.initialize(pressure=1000,saturation=0.2)
solver.set_transmissibility()
solver.set_wells()
solver.set_time(0.1,500)
solver.solve()

# PLOTTING

# fig = plt.figure()

# ax = plt.axes(projection='3d')

# # ax.scatter3D(*res.edge_vertices.T)

# for line in solver.res.edge_lines:
#     ax.plot3D(*line,color='grey')

# for track,name in zip(solver.wells.tracks,solver.wells.itemnames):

# 	ax.plot3D(*track.T,color="black",linewidth=1)
# 	ax.text(*track[0,:],name)

# ax.scatter3D(*solver.res.grid_centers.T)

# ax.set_xlabel("x-axis")
# ax.set_ylabel("y-axis")

# ax.set_box_aspect(solver.res.lengths)

# ax.set_axis_off()

# ax.margins(x=0,y=0)

# plt.tight_layout()

# plt.figure()

# plt.plot(Sw,solver.rp.kro,"k--",label="oil")
# plt.plot(Sw,solver.rp.krw,"k-",label="water")

# plt.xlabel("Water Saturation")
# plt.ylabel("Relative Permeability")

# plt.legend()

# plt.xlim((0,1))

# plt.show()