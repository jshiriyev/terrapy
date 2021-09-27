import calendar

from datetime import datetime
from dateutil.parser import parse
from dateutil.relativedelta import relativedelta

import os
import re

import tkinter as tk

import warnings

import matplotlib.pyplot as plt

import numpy as np

if __name__ == "__main__":
    import setup

from interfaces.dataset import dataset

class Pipes():

    def __init__(self):

        pass

    def set_pipe(self,inner_diameter=None,outer_diameter=None,length=1,roughness=None):
        
        self.ID = inner_diameter
        self.OD = outer_diameter
        self.length = length
        self.roughness = roughness

        """cross sectional area"""
        self.csa = np.pi*self.ID**2/4

        """
        hydraulic_radius: the ratio of the cross-sectional area of a channel or pipe
        in which a fluid is flowing to the wetted perimeter of the conduit
        """
        self.hydraulic_radius = self.ID/4

        self.roughness_relative = self.roughness/self.ID

    def set_pipe2(self,inner_diameter=None,outer_diameter=None,length=1,roughness=None):

        self.ID2 = inner_diameter
        self.OD2 = outer_diameter
        self.length2 = length
        self.roughness2 = roughness

        """cross sectional area"""
        self.csa2 = np.pi*(self.ID2**2-self.OD**2)/4

        """
        hydraulic_radius: the ratio of the cross-sectional area of a channel or pipe
        in which a fluid is flowing to the wetted perimeter of the conduit
        """
        self.hydraulic_radius2 = (self.ID2-self.OD)/4

        self.roughness_relative2 = self.roughness2/self.ID2

    def set_nodes(self,zloc=None,elevation=[0,0]):

        """
        Nodes are the locations where the measurements are available, and
        coordinates are selected in such a way that:
        - r-axis shows radial direction
        - \theta-axis shows angular direction
        - z-axis shows lengthwise direction
        """

        if zloc is None:
            self.zloc = [0,self.length]

        self.elevation = elevation

class Core():

    def __init__(self):

        pass

class Formation():

    def __init__(self):
        pass

    def get_tops(self,formations,wellname=None):
        pass

class FormationCylindrical():
    # regular radial both numerically discretized and homogenous one
    def __init__(self,height,radius,porosity,permeability,compressibility):

        self.height = height
        self.radius = radius

        self.porosity = porosity
        self.permeability = permeability
        self.compressibility = compressibility

    def set_total_compressibility(self,fluid):

        self.ct = self.cr+fluid.cf
        
    def set_diffusivity(self,viscosity,total_compressibility):
        
        self.eta = (self.k)/(self.phi*self.mu*self.ct)

    def set_well(self,radius,location,true_vertical_depth,measured_depth):

        self.radius = radius
        
    def set_fracture_nodes(self,thickness,nodes):

        self.thickness = thickness

    def set_hydraulic_diffusivity(self,hydraulic_diffusivity=None):

        if hydraulic_diffusivity is not None:
            self.eta = hydraulic_diffusivity

class FormationRectangular():

    # fileDir
    # Length
    # xLength
    # yLength
    # zLength
    # porosity
    # permeability
    # xPermeability
    # yPermeability
    # zPermeability
    # initPressure
    # diffusivity
    # xDiffusivity
    # yDiffusivity
    # zDiffusivity
    # isotropic
    # anisotropic
    # rockCompressibility
    # oilViscosity
    # oilFVF      % formation volume factor
    # oilCompressibility
    # totCompressibility

    def __init__(self):

        pass

    def cartesian(self,length,grid_num):

        """
        length is a tuple with three entries for size in x,y,z direction
        for rectangular parallelepiped:
        """

        self.length_x = length[0]
        self.length_y = length[1]
        self.length_z = length[2]

        """
        grid_num is a tuple with three entries for discretization in x,y,z direction
        for rectangular parallelepiped:
        """

        self.num_x = grid_num[0]
        self.num_y = grid_num[1]
        self.num_z = grid_num[2]

        """
        self.num is a total number of grids for rectangular parallelepiped
        """
        self.num = self.num_x*self.num_y*self.num_z

        """
        self.id is a connectivity map and contain index of all grids
        their neighbours.
        """

        idx = np.arange(self.num)
        
        self.id = np.tile(idx,(7,1)).T

        self.id[idx.reshape(-1,self.num_x)[:,1:].ravel(),1] -= 1
        self.id[idx.reshape(-1,self.num_x)[:,:-1].ravel(),2] += 1
        self.id[idx.reshape(self.num_z,-1)[:,self.num_x:],3] -= self.num_x
        self.id[idx.reshape(self.num_z,-1)[:,:-self.num_x],4] += self.num_x
        self.id[idx.reshape(self.num_z,-1)[1:,:],5] -= self.num_x*self.num_y
        self.id[idx.reshape(self.num_z,-1)[:-1,:],6] += self.num_x*self.num_y

        """
        self.size is the size of grids in x,y,z direction
        """

        node_x = np.linspace(0,self.length_x,self.num_x+1)
        node_y = np.linspace(0,self.length_y,self.num_y+1)
        node_z = np.linspace(0,self.length_z,self.num_z+1)
        
        xsize = node_x[1:]-node_x[:-1]
        ysize = node_y[1:]-node_y[:-1]
        zsize = node_z[1:]-node_z[:-1]
        
        self.size = np.zeros((self.num,3))
        self.size[:,0] = np.tile(xsize,self.num_y*self.num_z)
        self.size[:,1] = np.tile(ysize.repeat(self.num_x),self.num_z)
        self.size[:,2] = zsize.repeat(self.num_x*self.num_y)

        """
        self.area is the area of three faces of grids in x,y,z direction
        """
        
        self.area = np.zeros((self.num,3))
        self.area[:,0] = self.size[:,1]*self.size[:,2]
        self.area[:,1] = self.size[:,2]*self.size[:,0]
        self.area[:,2] = self.size[:,0]*self.size[:,1]

        """
        self.volume is the volume of grids in x,y,z direction
        """

        self.volume = np.prod(self.size,axis=1)

        xcenter = node_x[:-1]+xsize/2
        ycenter = node_y[:-1]+ysize/2
        zcenter = node_z[:-1]+zsize/2

        """
        self.center is the x,y,z coordinate of the center of grids
        """
        
        self.center = np.zeros((self.num,3))
        self.center[:,0] = np.tile(xcenter,self.num_y*self.num_z)
        self.center[:,1] = np.tile(ycenter.repeat(self.num_x),self.num_z)
        self.center[:,2] = zcenter.repeat(self.num_x*self.num_y)

class Fractures():

    # % The fracture segment is defined as a plane joining two node points
    # % (point1 and point2). The heigth of fracture plane is taken the same
    # % as reservoir thickness (it is not diffcult to model shorter planes).
    # % z-coordinate of the points is given as the reservoir depth.

    # fileDir
    # nodeCoord
    # map
    # permeability
    # width
    # fracID
    # nodeID
    # numAfrac
    # numAnode
    # conductivity
    # point1
    # point2
    # Length
    # areatoreservoir
    # areatofracture
    # volume
    # center
    # signX
    # signY
    # azimuth

    def __init__(self):

        pass

class Wells():

    # INPUT: PRODUCTION, COMPLETION, TRAJECTORY, BOREHOLE LOGGING

    filename_wprod      = "woperation.bbp"
    filename_wcomp      = "wcompletion.bbp"
    filename_wtrack     = "wtrack.bbp"
    filename_wlog       = "wlogging.bbp"

    headers_wprod       = (
        "WELL","DATE","DAYS","OPROD","WPROD","GPROD","WINJ",
        )

    headers_wcomp       = (
        "Wells","Horizont","Top","Bottom","start","stoped",
        )

    headers_wtrack      = (
        "X","Y","Z","MD",
        )

    headers_wlog        = (
        )

    # OUTPUT: PRODUCTION, COMPLETION, TRAJECTORY, BOREHOLE LOGGING, SCHEDULE

    filename_production = "OPERATION.BBP"
    filename_completion = "COMPLETION.BBP"
    filename_track      = "WELLTRACK.BBP"
    filename_logging    = "WELLOGGING.BBP"
    filename_schedule   = "SCHEDULE.BBP"

    headers_production  = (
        "WELL","DATE","DAYS","OPTYPE",
        "ROIL","RWATER","RGAS",
        "TOIL","TWATER","TGAS",
        )

    headers_completion  = (
        "WELL","DATE","EVENT","TOP","BOTTOM","DIAM",
        )

    headers_compupdate  = (
        "WELL","DATE","COUNT",
        )

    headers_track       = (
        "WELL","X","Y","Z","MD",
        )

    headers_logging     = (
        "WELL",
        )

    headers_schedule    = (
        "DATE","KEYWORD","DETAILS",
        )

    # SCHEDULE TO BE WRITTEN WITH KEYWORDS [DATES,COMPDATMD,COMPORD,WCONHIST,WCONINJH,WEFAC,WELOPEN]

    schedule_dates      = " {} / "#.format(date)

    schedule_wspec      = " '{}'\t1*\t2* / "
    schedule_compdat    = " '{}'\t1*\t{}\t{}\tMD\t{}\t2*\t0.14 / "#.format(wellname,top,bottom,optype)
    schedule_compord    = " '{}'\tINPUT\t/ "#.format(wellname)
    schedule_prodhist   = " '{}'\tOPEN\tORAT\t{}\t{}\t{} / "#.format(wellname,oilrate,waterrate,gasrate)
    schedule_injhist    = " '{}'\tWATER\tOPEN\t{}\t7*\tRATE / "#.format(wellname,waterrate)
    schedule_wefac      = " '{}'\t{} / "#.format(wellname,efficiency)
    schedule_wopen      = " '{}'\tSHUT\t3* / "#.format(wellname)

    # GRAPH TEMPLATES

    welltemp0 = {
        "name": "Production-Completion Cross Plot",
        "subplots": [1,2],
        "twinx": [True,True],
        "title": ["BEFORE CORRECTIONS","AFTER CORRECTIONS"],
        "xlabel": ["Dates","Dates"],
        "ylabel": ["Total Production or Injection [m3/day]",None,None,
                   "Open Perforation Intervals"],
        "legends": [True,True],
        "xticks": [None,None],
        "yticks": [None,[],[],None],
        "grid": [False,False],
        #
        "sublines": [[2,1],[2,1]],
        "xaxes": [[1,1],[1],[1,1],[1]],
        "yaxes": [[3,3],[3],[4,4],[4]],
        "drawstyles": [
            ["default","steps-post"],
            ["steps-post"],
            ["default","steps-post"],
            ["steps-post"]
            ],
        "linestyles": [["-",""],[],[],[]],
        "colors": [[None,"b"],["r"],[None,"b"],["r"]],
    }

    welltemp1 = {
        "name": "Production History Match",
        "subplots": [2,2],
        "twinx": [True,True,True,False],
        "title": ["NW","NE","SW","SE"],
        "xlabel": ["Dates","Dates","Dates","Dates"],
        "ylabel": [
            "Liquid Rate, sm3/day",
            "Liquid Volume, th. sm3",
            "Gas Rate, th. sm3/day",
            "Surface Gas Volume, mln. sm3",
            "Liquid Rate, sm3/day",
            "Liquid Volume, th. sm3",
            "Pressure, Bars"
            ],
        "legends": [True,True,True,False],
        "xticks": [None,None,None,None],
        "yticks": [None,None,None,None,None,None,None],
        "grid": [True,True,True,True],
        #
        "sublines": [[1,1],[1,1],[1,1],[0,0]],
        "xaxes": [[1],[1],[1],[1],[1],[1],[]],
        "yaxes": [[3],[10],[4],[11],[5],[12],[]],
        "drawstyles": [["default"]]*7, 
        "linestyles": [["-"],["--"],["-"],["--"],["-"],["--"],[]],
        "colors": [["g"],["g"],["b"],["b"],["r"],["r"],[]],
    }

    def __init__(self,workdir,wnames=None,wproddir=None,wcompdir=None,wtrackdir=None,wlogdir=None,wnamefstr=None,**kwargs):

        self.workdir    = workdir   # working directory to save and retrieve saved data

        self.wnames     = wnames    # well names

        self.wproddir   = wproddir  # production files directory
        self.wcompdir   = wcompdir  # completion files directory
        self.wtrackdir  = wtrackdir # trajectory files directory
        self.wlogdir    = wlogdir   # wellogging files directory

        self.wprod      = dataset(headers=self.headers_wprod)
        self.wcomp      = dataset(headers=self.headers_wcomp)
        self.wtrack     = dataset(headers=self.headers_wtrack)
        self.wlog       = dataset(headers=self.headers_wlog)

        self.production = dataset(headers=self.headers_production)
        self.prodshut   = dataset(headers=self.headers_production)
        self.completion = dataset(headers=self.headers_completion)
        self.track      = dataset(headers=self.headers_track)
        self.logging    = dataset(headers=self.headers_logging)
        self.schedule   = dataset(headers=self.headers_schedule)

        self.wnamefstr  = wnamefstr

        self.schedule.templates = (self.welltemp0,)

    def production_run(self):

        path = os.path.join(self.workdir,self.filename_wprod)

        prod = dataset(filepath=path,skiplines=1)

        prod.texttocolumn(0,deliminator="\t",maxsplit=7)
        prod.get_columns(headers=self.headers_wprod,inplace=True)
        prod.sort(header_indices=[1],inplace=True)

        prod.astype(header=self.headers_wprod[1],datestring=True)
        prod.astype(header=self.headers_wprod[2],dtype=np.int64)
        prod.astype(header=self.headers_wprod[3],dtype=np.float64)
        prod.astype(header=self.headers_wprod[4],dtype=np.float64)
        prod.astype(header=self.headers_wprod[5],dtype=np.float64)
        prod.astype(header=self.headers_wprod[6],dtype=np.float64)

        vdate1 = np.vectorize(lambda x: x.day!=calendar.monthrange(x.year,x.month)[1])

        if any(vdate1(prod.running[1])):
            for index in np.where(vdate1(prod.running[1]))[0]:
                well = prod.running[0][index]
                date = prod.running[1][index]
                warnings.warn("{:%d %b %Y} {} date is not the last day of month.".format(date,well))

        vdate2 = np.vectorize(lambda x,y: x.day<y)

        if any(vdate2(prod.running[1],prod.running[2])):
            for index in np.where(vdate2(prod.running[1],prod.running[2]))[0]:
                well = prod.running[0][index]
                date = prod.running[1][index]
                warnings.warn("{:%d %b %Y} {} active days is greater than the days in the month.".format(date,well))

        if any(prod.running[3]<0):
            for index in np.where(prod.running[3]<0)[0]:
                well = prod.running[0][index]
                date = prod.running[1][index]
                warnings.warn("{:%d %b %Y} {} oil production has negative entry.".format(date,well))

        if any(prod.running[4]<0):
            for index in np.where(prod.running[4]<0)[0]:
                well = prod.running[0][index]
                date = prod.running[1][index]
                warnings.warn("{:%d %b %Y} {} water production has negative entry.".format(date,well))

        if any(prod.running[5]<0):
            for index in np.where(prod.running[5]<0)[0]:
                well = prod.running[0][index]
                date = prod.running[1][index]
                warnings.warn("{:%d %b %Y} {} gas production has negative entry.".format(date,well))

        if any(prod.running[6]<0):
            for index in np.where(prod.running[6]<0)[0]:
                well = prod.running[0][index]
                date = prod.running[1][index]
                warnings.warn("{:%d %b %Y} {} water injection has negative entry.".format(date,well))

        roil = prod.running[3]
        rwater = prod.running[4]+prod.running[6]
        rgas = prod.running[5]

        toil = np.cumsum(roil)
        twater = np.cumsum(rwater)
        tgas = np.cumsum(rgas)

        rprod = prod.running[3]+prod.running[4]+prod.running[5]
        rinj = prod.running[6]

        rtot = rprod+rinj

        optype = np.empty(prod.running[2].shape,dtype=object)

        optype[rprod>0] = "production"
        optype[rinj>0] = "injection"

        if any(rtot==0):
            for index in np.where(rtot==0)[0]:
                well = prod.running[0][index]
                date = prod.running[1][index]
                warnings.warn("{:%d %b %Y} {} has zero production and injection.".format(date,well))

        if any(np.logical_and(rprod!=0,rinj!=0)):
            for index in np.where(np.logical_and(rprod!=0,rinj!=0))[0]:
                well = prod.running[0][index]
                date = prod.running[1][index]
                warnings.warn("{:%d %b %Y} {} has both production and injection data.".format(date,well))

        # "WELL","DATE","DAYS","OPTYPE","ROIL","RWATER","RGAS","TOIL","TWATER","TGAS"
        #     0      1      2        3      4        5      6      7        8      9

        if self.wnamefstr is not None:
            vname = np.vectorize(lambda x: self.wnamefstr.format(re.sub("[^0-9]","",x).zfill(3)))

            prod.set_column(vname(prod.running[0]),header_index=0)

        def shifting(x):
            date = x+relativedelta(months=-1)
            days = calendar.monthrange(date.year,date.month)[1]
            return datetime(date.year,date.month,days)

        vdate3 = np.vectorize(lambda x: shifting(x))

        prod.set_column(vdate3(prod.running[1]),header_index=1)

        prod.set_column(roil,header_new="ROIL")
        prod.set_column(rwater,header_new="RWATER")
        prod.set_column(rgas,header_new="RGAS")

        # prod.set_column(toil,header_new="TOIL")
        # prod.set_column(twater,header_new="TWATER")
        # prod.set_column(tgas,header_new="TGAS")

        prod.set_column(optype,header_new="OPTYPE")

        prod.get_columns(headers=self.headers_production[:7],inplace=True)
        
        path = os.path.join(self.workdir,self.filename_production)

        fstring = "{:6s}\t{:%Y-%m-%d}\t{:2d}\t{:10s}\t{:.1f}\t{:.1f}\t{:.1f}\t{:.1f}\t{:.1f}\t{:.1f}\n"

        prod.write(filepath=path,fstring=fstring)

    def production_getfromWdir(self,wellname=None):

        path = os.path.join(self.workdir,self.filename_production)

        self.production = dataset(filepath=path,skiplines=1)

        self.production.texttocolumn(0,deliminator="\t")

        self.production.astype(header=self.headers_production[1],datestring=True)
        self.production.astype(header=self.headers_production[2],dtype=np.int64)
        self.production.astype(header=self.headers_production[4],dtype=np.float64)
        self.production.astype(header=self.headers_production[5],dtype=np.float64)
        self.production.astype(header=self.headers_production[6],dtype=np.float64)

        if wellname is not None:
            self.production.filter(0,keywords=[wellname],inplace=True)

    def completion_collect(self,wellname=None):

        for wellname in self.wnames:

            print(wellname)

            wellindex = int(re.sub("[^0-9]","",wellname))

            folder1 = "GD-{}".format(str(wellindex).zfill(3))

            filename = "GD-{}.xlsx".format(str(wellindex).zfill(3))

            filepath = os.path.join(self.wcompdir,folder1,filename)
            
            comps = dataset(filepath=filepath,sheetname=folder1,headerline=1,skiplines=2,min_row=2,min_col=2)

            comps.get_columns(headers=self.headers_wcomp,inplace=True)

            comps.astype(header=self.headers_wcomp[2],dtype=np.float64)
            comps.astype(header=self.headers_wcomp[3],dtype=np.float64)

            if np.any(comps.running[0]!=wellname):
                warnings.warn("{} has name conflict in completion directory.".format(wellname))

            if np.any(comps.running[1]==None) or np.any(np.char.strip(comps.running[1].astype(str))==""):
                warnings.warn("{} does not have proper layer name in completion directory.".format(wellname))

            if np.any(comps.running[2]<0):
                warnings.warn("{} top level depths must be positive in completion directory.".format(wellname))

            if np.any(comps.running[3]<0):
                warnings.warn("{} bottom level depths must be positive in completion directory.".format(wellname))

            if np.any(comps.running[2]-comps.running[3]>0):
                warnings.warn("{} top level must be smaller than bottom levels in completion directory.".format(wellname))

            if any([not isinstance(value,datetime) for value in comps.running[4].tolist()]):
                warnings.warn("{} start date is not set properly in completion directory.".format(wellname))

            indices = [not isinstance(value,datetime) for value in comps.running[5].tolist()]

            if any(indices) and np.any(comps.running[5][indices]!="ACTIVE"):
                warnings.warn("{} stoped date is not set properly in completion directory.".format(wellname))

            comps.running[5][indices] = datetime.now()

            if any([(s2-s1).days<0 for s1,s2 in zip(comps.running[4].tolist(),comps.running[5].tolist())]):
                warnings.warn("{} start date is after or equal to stop date in completion directory.".format(wellname))

            self.wcomp.set_rows(comps.get_rows())

        path = os.path.join(self.workdir,self.filename_wcomp)

        fstring = "{:6s}\t{}\t{:.1f}\t{:.1f}\t{:%Y-%m-%d}\t{:%Y-%m-%d}\n"

        self.wcomp.write(filepath=path,fstring=fstring)

    def completion_run(self):

        path = os.path.join(self.workdir,self.filename_wcomp)

        comp1 = dataset(filepath=path,skiplines=1)
        comp2 = dataset(filepath=path,skiplines=1)

        comp1.texttocolumn(0,deliminator="\t")
        comp2.texttocolumn(0,deliminator="\t")

        headers_wcomp1 = self.headers_wcomp[:4]+(self.headers_wcomp[4],)
        headers_wcomp2 = self.headers_wcomp[:4]+(self.headers_wcomp[5],)

        comp1.get_columns(headers=headers_wcomp1,inplace=True)
        comp2.get_columns(headers=headers_wcomp2,inplace=True)

        comp1.astype(header=headers_wcomp1[2],dtype=np.float64)
        comp1.astype(header=headers_wcomp1[3],dtype=np.float64)
        comp1.astype(header=headers_wcomp1[4],datestring=True)

        comp2.astype(header=headers_wcomp2[2],dtype=np.float64)
        comp2.astype(header=headers_wcomp2[3],dtype=np.float64)
        comp2.astype(header=headers_wcomp2[4],datestring=True)

        col_perf = np.empty(comp1.running[0].size,dtype=object)
        col_perf[:] = "PERF"

        col_diam = np.empty(comp1.running[0].size,dtype=object)
        col_diam[:] = "0.14"

        comp1.set_column(col_perf,header_new="EVENT")
        comp1.set_column(col_diam,header_new="DIAM")

        col_plug = np.empty(comp2.running[0].size,dtype=object)
        col_plug[:] = "PLUG"

        col_none = np.empty(comp2.running[0].size,dtype=object)
        col_none[:] = ""

        comp2.set_column(col_plug,header_new="EVENT")
        comp2.set_column(col_none,header_new="DIAM")

        comp1.set_rows(comp2.get_rows())

        comp1._headers[0] = self.headers_completion[0]
        comp1._headers[2] = self.headers_completion[3]
        comp1._headers[3] = self.headers_completion[4]
        comp1._headers[4] = self.headers_completion[1]

        comp1.get_columns(headers=self.headers_completion,inplace=True)

        comp1.sort(header_indices=[1],inplace=True)

        path = os.path.join(self.workdir,self.filename_completion)

        fstring = "{:6s}\t{:%Y-%m-%d}\t{:4s}\t{:.1f}\t{:.1f}\t{:4s}\n"

        comp1.write(filepath=path,fstring=fstring)

    def completion_getfromWdir(self,wellname=None):

        path = os.path.join(self.workdir,self.filename_completion)

        self.completion = dataset(filepath=path,skiplines=1)

        self.completion.texttocolumn(0,deliminator="\t",maxsplit=6)

        self.completion.astype(header=self.headers_completion[1],datestring=True)
        self.completion.astype(header=self.headers_completion[3],dtype=np.float64)
        self.completion.astype(header=self.headers_completion[4],dtype=np.float64)

        self.completion.sort(header_indices=[1],inplace=True)

        self.compupdate = dataset(headers=self.headers_compupdate)

        for wname in self.wnames:

            print(wname)

            self.completion.filter(0,keywords=[wname],inplace=False)

            update_dates = np.unique(self.completion.running[1])
            update_counts = np.zeros(update_dates.size,dtype=int)
            update_wells = np.empty(update_dates.size,dtype=object)

            update_wells[:] = wname

            update_indices = np.insert(
                np.cumsum(np.sum(self.completion.running[1]==update_dates.reshape((-1,1)),axis=1)),0,0)

            open_intervals = np.empty((0,2))

            for index,date in enumerate(update_dates):

                compevents = self.completion.running[2][update_indices[index]:update_indices[index+1]]
                compuppers = self.completion.running[3][update_indices[index]:update_indices[index+1]]
                complowers = self.completion.running[4][update_indices[index]:update_indices[index+1]]

                perfevents = compevents=="PERF"

                perfintervals = np.array([compuppers[perfevents],complowers[perfevents]]).T

                open_intervals = np.concatenate((open_intervals,perfintervals),axis=0)

                plugevents = compevents=="PLUG"

                pluguppermatch = np.any(open_intervals[:,0]==compuppers[plugevents].reshape((-1,1)),axis=0)
                pluglowermatch = np.any(open_intervals[:,1]==complowers[plugevents].reshape((-1,1)),axis=0)

                plugmatch = np.where(np.logical_and(pluguppermatch,pluglowermatch))[0]

                open_intervals = np.delete(open_intervals,plugmatch,0)

                update_counts[index] = open_intervals.shape[0]

            rows = np.array([update_wells,update_dates,update_counts]).T.tolist()

            self.compupdate.set_rows(rows)

        self.compupdate.astype(header_index=2,dtype=int)

        if wellname is not None:
            self.completion.filter(0,keywords=[wellname],inplace=True)
            self.compupdate.filter(0,keywords=[wellname],inplace=True)

    def trajectory_collect(self,wellname=None):

        wellnumber = int(re.sub("[^0-9]","",wellname))

        folder1 = "GD-{}".format(str(wellnumber).zfill(3))
        folder2 = "6.GD-{} Deviation".format(str(wellnumber).zfill(3))

        filename = "Qum_Adasi-{}.txt".format(wellnumber)
        
        filepath = os.path.join(self.dirtraj,folder1,folder2,filename)

        traj = dataset(filepath=filepath,skiplines=1,comment="#")

        traj.texttocolumn(0,deliminator=None,maxsplit=None)

        traj.get_columns(headers=headers_traj)

        traj.astype(header=headers_traj[0],dtype=np.float64)
        traj.astype(header=headers_traj[1],dtype=np.float64)
        traj.astype(header=headers_traj[2],dtype=np.float64)
        traj.astype(header=headers_traj[3],dtype=np.float64)

        return traj

    def trajectory_getfromWdir(self,wellname=None):

        pass

    def logging_collect(self,wellname=None):

        pass

    def logging_getfromWdir(self,wellname=None):

        pass

    def schedule_run(self,wellname=None,flagReturn=False,flagShow=False):

        warnNOPROD = "{} has completion but no production data."
        warnNOCOMP = "{} has production but no completion data."

        warnCROSS = "{} production has been defined before completion."

        warnWPGPF = "{:%Y-%m-%d}: {} first perf and last plug dates do not fit production days."
        warnWPERF = "{:%Y-%m-%d}: {} first perf date does not fit production days."
        warnWPLUG = "{:%Y-%m-%d}: {} last plug date does not fit production days."
        warnWEFAC = "{:%Y-%m-%d}: {} efficiency is more than unit [{:2d} out of {:2d} days]."

        self.production_getfromWdir()
        self.completion_getfromWdir()

        prodwellnames = np.unique(self.production.running[0])
        compwellnames = np.unique(self.completion.running[0])

        for wellname in np.setdiff1d(prodwellnames,compwellnames):
            warnings.warn(warnNOCOMP.format(wellname))

        for wellname in np.setdiff1d(compwellnames,prodwellnames):
            warnings.warn(warnNOPROD.format(wellname))

        self.production.filter(0,keywords=[wellname],inplace=False)
        self.completion.filter(0,keywords=[wellname],inplace=False)
        self.compupdate.filter(0,keywords=[wellname],inplace=False)

        try:
            datemin = self.production.running[1].min()
        except ValueError:
            datemin = datetime(3000,1,1)

        date = datemin+relativedelta(months=1)

        days = calendar.monthrange(date.year,date.month)[1]

        date = datetime(date.year,date.month,days)

        if self.compupdate.running[1].min()>=date:
            warnings.warn(warnCROSS.format(wellname))

        compdata = zip(
            self.completion.running[1],
            self.completion.running[2],
            self.completion.running[3],
            self.completion.running[4],
            )

        for index,(compdate,compevent,comptop,compbottom) in enumerate(compdata):

            if index==0:
                self.schedule.set_rows([compdate,"WELSPECS",self.schedule_wspec.format(wellname)])

            if compevent == "PERF":
                bottom = compbottom
                perfs = "OPEN"
            elif compevent == "PLUG":
                bottom = "1*"
                perfs = "SHUT"

            self.schedule.set_rows([compdate,"COMPDATMD",self.schedule_compdat.format(wellname,comptop,bottom,perfs)])
            self.schedule.set_rows([compdate,"COMPORD",self.schedule_compord.format(wellname)])

        flagNoPrevProd = True

        print("{} check is in progress ...".format(wellname))

        opdata = zip(
            self.production.running[1],
            self.production.running[2],
            self.production.running[3],
            self.production.running[4],
            self.production.running[5],
            self.production.running[6],
            )

        shutdates = []

        for index,(date,days,optype,oil,water,gas) in enumerate(opdata):

            prodmonthSTARTday = date+relativedelta(days=1)

            prodmonthdaycount = calendar.monthrange(prodmonthSTARTday.year,prodmonthSTARTday.month)[1]

            prodmonthENDday = datetime(prodmonthSTARTday.year,prodmonthSTARTday.month,prodmonthdaycount)

            if np.sum(self.compupdate.running[1]<prodmonthSTARTday)==0:
                compSTARTindex = 0
            else:
                compSTARTindex = np.sum(self.compupdate.running[1]<prodmonthSTARTday)-1

            compENDindex = np.sum(self.compupdate.running[1]<=prodmonthENDday)

            compupdatecounts = self.compupdate.running[2][compSTARTindex:compENDindex]

            compupdatedates = self.compupdate.running[1][compSTARTindex:compENDindex]

            perfdates = compupdatedates[compupdatecounts!=0]
            plugdates = compupdatedates[compupdatecounts==0]

            try:
                flagNoPostProd = True if self.production.running[1][index+1]-relativedelta(months=1)>prodmonthENDday else False
            except IndexError:
                flagNoPostProd = True

            if np.sum(self.compupdate.running[1]<prodmonthSTARTday)==0:
                flagCompShutSTART = True
            else:
                flagCompShutSTART = compupdatecounts[0]==0

            flagCompShutEND = compupdatecounts[-1]==0

            flagPlugPerf = any([compopencount==0 for compopencount in compupdatecounts[1:-1]])

            if flagCompShutSTART and flagCompShutEND:
                compday = plugdates[-1].day-perfdates[0].day
                prodeff = days/compday
                if optype == "production":
                    self.schedule.set_rows([perfdates[0],"WCONHIST",self.schedule_prodhist.format(wellname,oil,water,gas)])
                elif optype == "injection":
                    self.schedule.set_rows([perfdates[0],"WCONINJH",self.schedule_injhist.format(wellname,water)])
                self.schedule.set_rows([perfdates[0],"WEFAC",self.schedule_wefac.format(wellname,prodeff)])
                self.production.running[1][index] = perfdates[0]
                self.schedule.set_rows([plugdates[-1],"WELOPEN",self.schedule_wopen.format(wellname)])
                shutdates.append(plugdates[-1])
                flagNoPrevProd = True
                if flagShow:
                    print("{:%d %b %Y} Peforated and Plugged: OPEN ({:%d %b %Y}) and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,perfdates[0],plugdates[-1],prodeff))

            elif flagCompShutSTART:
                compday = prodmonthENDday.day-perfdates[0].day
                prodeff = days/compday
                if optype == "production":
                    self.schedule.set_rows([perfdates[0],"WCONHIST",self.schedule_prodhist.format(wellname,oil,water,gas)])
                elif optype == "injection":
                    self.schedule.set_rows([perfdates[0],"WCONINJH",self.schedule_injhist.format(wellname,water)])
                self.schedule.set_rows([perfdates[0],"WEFAC",self.schedule_wefac.format(wellname,prodeff)])
                self.production.running[1][index] = perfdates[0]
                if flagNoPostProd:
                    self.schedule.set_rows([prodmonthENDday,"WELOPEN",self.schedule_wopen.format(wellname)])
                    shutdates.append(prodmonthENDday)
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} Peforated and Open: OPEN ({:%d %b %Y}) and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,perfdates[0],prodmonthENDday,prodeff))
                else:                  
                    flagNoPrevProd = False
                    if flagShow:
                        print("{:%d %b %Y} Peforated and Open: OPEN ({:%d %b %Y}) and CONT WEFAC ({:.3f})".format(prodmonthENDday,perfdates[0],prodeff))

            elif flagCompShutEND:
                for plugdate in plugdates:
                    if plugdate.day>=days: break
                if not plugdate.day>=days:
                    warnings.warn(warnWPLUG.format(prodmonthENDday,wellname))
                compday = plugdate.day
                prodeff = days/compday
                if optype == "production":
                    self.schedule.set_rows([date,"WCONHIST",self.schedule_prodhist.format(wellname,oil,water,gas)])
                elif optype == "injection":
                    self.schedule.set_rows([date,"WCONINJH",self.schedule_injhist.format(wellname,water)])
                self.schedule.set_rows([date,"WEFAC",self.schedule_wefac.format(wellname,prodeff)])
                self.schedule.set_rows([plugdate,"WELOPEN",self.schedule_wopen.format(wellname)])
                shutdates.append(plugdate)
                flagNoPrevProd = True
                if flagShow:
                    print("{:%d %b %Y} Open and Plugged: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,plugdate,prodeff))

            elif flagPlugPerf:
                if flagNoPrevProd and flagNoPostProd:
                    # shift the start day to the first perf day
                    # shut the well at the last plug day
                    if not plugdates[-1].day-perfdates[1].day>=days:
                        warnings.warn(warnWPGPF.format(prodmonthENDday,wellname))
                    compday = plugdates[-1].day-perfdates[1].day
                    prodeff = days/compday
                    if optype == "production":
                        self.schedule.set_rows([perfdates[1],"WCONHIST",self.schedule_prodhist.format(wellname,oil,water,gas)])
                    elif optype == "injection":
                        self.schedule.set_rows([perfdates[1],"WCONINJH",self.schedule_injhist.format(wellname,water)])
                    self.schedule.set_rows([perfdates[1],"WEFAC",self.schedule_wefac.format(wellname,prodeff)])
                    self.production.running[1][index] = perfdates[1]
                    self.schedule.set_rows([plugdates[-1],"WELOPEN",self.schedule_wopen.format(wellname)])
                    shutdates.append(plugdates[-1])
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} Plugged and Perforated: OPEN ({:%d %b %Y}) and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,perfdates[1],plugdates[-1],prodeff))
                elif flagNoPrevProd and not flagNoPostProd:
                    # shift the start day to the proper perf day
                    for perfdate in np.flip(perfdates[1:]):
                        if prodmonthENDday.day-perfdate.day>=days: break
                    if not prodmonthENDday.day-perfdate.day>=days:
                        warnings.warn(warnWPERF.format(prodmonthENDday,wellname))
                    compday = prodmonthENDday.day-perfdate.day
                    prodeff = days/compday
                    if optype == "production":
                        self.schedule.set_rows([perfdate,"WCONHIST",self.schedule_prodhist.format(wellname,oil,water,gas)])
                    elif optype == "injection":
                        self.schedule.set_rows([perfdate,"WCONINJH",self.schedule_injhist.format(wellname,water)])
                    self.schedule.set_rows([perfdate,"WEFAC",self.schedule_wefac.format(wellname,prodeff)])
                    self.production.running[1][index] = perfdate
                    flagNoPrevProd = False
                    if flagShow:
                        print("{:%d %b %Y} Plugged and Perforated: OPEN ({:%d %b %Y}) and CONT WEFAC ({:.3f})".format(prodmonthENDday,perfdate,prodeff))
                elif not flagNoPrevProd and flagNoPostProd:
                    # try shut the well at the proper plug day if not successful shut it at the end of month
                    for plugdate in plugdates:
                        if plugdate.day>=days: break
                    if not plugdate.day>=days:
                        plugdate = prodmonthENDday
                    compday = plugdate.day
                    prodeff = days/compday
                    if optype == "production":
                        self.schedule.set_rows([date,"WCONHIST",self.schedule_prodhist.format(wellname,oil,water,gas)])
                    elif optype == "injection":
                        self.schedule.set_rows([date,"WCONINJH",self.schedule_injhist.format(wellname,water)])
                    self.schedule.set_rows([date,"WEFAC",self.schedule_wefac.format(wellname,prodeff)])
                    self.schedule.set_rows([plugdate,"WELOPEN",self.schedule_wopen.format(wellname)])
                    shutdates.append(plugdate)
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} Plugged and Perforated: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,plugdate,prodeff))
                elif not flagNoPrevProd and not flagNoPostProd:
                    # try shut the well if not successful do nothing
                    for plugdate in plugdates:
                        if plugdate.day>=days: break
                    if not plugdate.day>=days:
                        compday = prodmonthdaycount
                        prodeff = days/compday
                        flagNoPrevProd = False
                        if flagShow:
                            print("{:%d %b %Y} Plugged and Perforated: CONT and CONT WEFAC ({:.3f})".format(prodmonthENDday,prodeff))
                    else:
                        compday = plugdate.day
                        prodeff = days/compday
                        self.schedule.set_rows([plugdate,"WELOPEN",self.schedule_wopen.format(wellname)])
                        shutdates.append(plugdate)
                        flagNoPrevProd = True
                        if flagShow:
                            print("{:%d %b %Y} Plugged and Perforated: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,plugdate,prodeff))
                    if optype == "production":
                        self.schedule.set_rows([date,"WCONHIST",self.schedule_prodhist.format(wellname,oil,water,gas)])
                    elif optype == "injection":
                        self.schedule.set_rows([date,"WCONINJH",self.schedule_injhist.format(wellname,water)])
                    self.schedule.set_rows([date,"WEFAC",self.schedule_wefac.format(wellname,prodeff)])

            else:
                compday = prodmonthdaycount
                prodeff = days/compday
                if optype == "production":
                    self.schedule.set_rows([date,"WCONHIST",self.schedule_prodhist.format(wellname,oil,water,gas)])
                elif optype == "injection":
                    self.schedule.set_rows([date,"WCONINJH",self.schedule_injhist.format(wellname,water)])
                self.schedule.set_rows([date,"WEFAC",self.schedule_wefac.format(wellname,prodeff)])
                if flagNoPostProd:
                    self.schedule.set_rows([prodmonthENDday,"WELOPEN",self.schedule_wopen.format(wellname)])
                    shutdates.append(prodmonthENDday)
                    flagNoPrevProd = True
                    if flagShow:
                        print("{:%d %b %Y} No completion events: CONT and SHUT ({:%d %b %Y}) WEFAC ({:.3f})".format(prodmonthENDday,prodmonthENDday,prodeff))
                else:
                    flagNoPrevProd = False
                    if flagShow:
                        print("{:%d %b %Y} No completion events: CONT and CONT WEFAC ({:.3f})".format(prodmonthENDday,prodeff))

            if prodeff>1:
                warnings.warn(warnWEFAC.format(prodmonthENDday,wellname,days,compday))

        shutdates = np.array(shutdates,dtype=object)
        shutwells = np.empty(shutdates.shape,dtype=object)

        shutwells[:] = wellname

        shutdays = np.zeros(shutdates.shape,dtype=int)

        shutoptype = np.empty(shutdates.shape,dtype=object)

        shutoptype[:] = "shut"

        shutroil = np.zeros(shutdates.shape,dtype=int)
        shutrwater = np.zeros(shutdates.shape,dtype=int)
        shutrgas = np.zeros(shutdates.shape,dtype=int)

        rows = np.array([shutwells,shutdates,shutdays,shutoptype,shutroil,shutrwater,shutrgas]).T.tolist()

        self.prodshut.set_rows(rows)

        if flagShow:
            print("{} check is complete.".format(wellname))

        path = os.path.join(self.workdir,self.filename_schedule)

        with open(path,"w",encoding='utf-8') as wfile:

            welspec = self.schedule.running[1]=="WELSPECS"
            compdat = self.schedule.running[1]=="COMPDATMD"
            compord = self.schedule.running[1]=="COMPORD"
            prodhst = self.schedule.running[1]=="WCONHIST"
            injdhst = self.schedule.running[1]=="WCONINJH"
            wefffac = self.schedule.running[1]=="WEFAC"
            welopen = self.schedule.running[1]=="WELOPEN"

            for date in np.unique(self.schedule.running[0]):

                currentdate = self.schedule.running[0]==date

                currentcont = self.schedule.running[1][currentdate]

                wfile.write("\n\n")
                wfile.write("DATES\n")
                wfile.write(self.schedule_dates.format(date.strftime("%d %b %Y").upper()))
                wfile.write("\n")
                wfile.write("/\n\n")

                if any(currentcont=="WELSPECS"):
                    indices = np.logical_and(currentdate,welspec)
                    wfile.write("WELSPECS\n")
                    for detail in self.schedule.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="COMPDATMD"):
                    indices = np.logical_and(currentdate,compdat)
                    wfile.write("COMPDATMD\n")
                    for detail in self.schedule.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="COMPORD"):
                    indices = np.logical_and(currentdate,compord)
                    wfile.write("COMPORD\n")
                    for detail in self.schedule.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WCONHIST"):
                    indices = np.logical_and(currentdate,prodhst)
                    wfile.write("WCONHIST\n")
                    for detail in self.schedule.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WCONINJH"):
                    indices = np.logical_and(currentdate,injdhst)
                    wfile.write("WCONINJH\n")
                    for detail in self.schedule.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WEFAC"):
                    indices = np.logical_and(currentdate,wefffac)
                    wfile.write("WEFAC\n")
                    for detail in self.schedule.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

                if any(currentcont=="WELOPEN"):
                    indices = np.logical_and(currentdate,welopen)
                    wfile.write("WELOPEN\n")
                    for detail in self.schedule.running[2][indices]:
                        wfile.write(detail)
                        wfile.write("\n")
                    wfile.write("/\n\n")

    def schedule_getfromWdir(self,wellname=None):

        pass

if __name__ == "__main__":

    import unittest

    from tests import pipes
    from tests import porous_media
    from tests import fractures
    from tests import wells

    unittest.main(pipes)
    unittest.main(porous_media)
    unittest.main(fractures)
    unittest.main(wells)
