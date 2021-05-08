# Standard library imports
import csv

# Third party imports
import numpy as np
##import openpyxl

# Local application imports

"""
This is a module for reading:
- csv reader: see sample.csv
- xlsx reader: see sample.xlsx
and assigning read values and units to the given objects
"""

def setup(filename,sheets,*args):

    _,file_extension = os.path.splitext(filename)

    if file_extension == ".csv":
        readcsv(filename,sheets,*args)
    elif file_extension == ".xlsx":
        readxlsx(filename,sheets,*args)

def readcsv(filename,sheets,*args):

    with open(filename) as csvfile:
        reader = list(csv.reader(csvfile))
        reader = np.array(reader)
        num_row, num_col = reader.shape
        if sheets['dataTypes']=='col':
            tuples = tuple(reader.T)
        elif sheets['dataTypes']=='row':
            tuples = tuple(reader)
        setparent(tuples,*args)
        
def readxlsx(filename,sheets,*args):
    all_tuples = ()
    wb = openpyxl.load_workbook(filename)
    for i,sheetname in enumerate(sheets["names"]):
        sheet = wb[sheetname]
        if sheets['dataTypes'][i]=='col':
            tuples = tuple(sheet.iter_cols(max_col=sheets['num_cols'][i],values_only=True))
        elif sheets['dataTypes'][i]=='row':
            tuples = tuple(sheet.iter_rows(max_col=sheets['num_cols'][i],values_only=True))
        all_tuples = all_tuples+tuples
    setparent(all_tuples,*args)

def setparent(tuples,*args):
    for i in range(len(tuples)):
        for arg in args:
##            print(tuples[i][0])
##            print(arg.__name__)
            if tuples[i][0] == arg.__name__:
                setchild(tuples[i],arg)

def setchild(atuple,obj):
    array = np.array(atuple[3:]).astype('float64')
    setattr(obj,atuple[1]+'_unit',atuple[2])
    setattr(obj,atuple[1],array)

if __name__ == "__main__":

    data = {
        'permeability': np.array([100,50,35,47]),
        'porosity': np.array([11,15,21,6])
        }

    raman = item(data)
    
##    class reservoir: pass
##    class well: pass
##    class time: pass
##
##    sheets = {
##        "num_cols": 5,
##        "dataTypes": "col"
##        }
##
##    setup("sample.csv",sheets,reservoir)
##
##    print(reservoir.porosity)
##    
##    sheets = {
##        "names": ["build_up_test","parameters"],
##        "num_cols": [3,4],
##        "dataTypes": ["col","row"]
##        }
##
##    readxlsx('sample.xlsx',sheets,time,well)

