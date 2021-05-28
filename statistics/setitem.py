import csv

import numpy as np

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

"""binary data"""
"""big data reader"""

def setparent(tuples,*args):
    for i in range(len(tuples)):
        for arg in args:
            if tuples[i][0] == arg.__name__:
                setchild(tuples[i],arg)

def setchild(atuple,obj):
    array = np.array(atuple[3:]).astype('float64')
    setattr(obj,atuple[1]+'_unit',atuple[2])
    setattr(obj,atuple[1],array)

class dataReader(object):
    
    def __init__(self,filename,header_lines=0):
        
        self.filename = filename
        self.header_lines = header_lines
        
        self.get_labels()
        self.format_data()
        
        return
    
    def read_file(self):
        
        with open(self.filename) as f:
            
            data = f.readlines()[self.header_lines:]
        
        return [data[i:i+4] for i in range(0, len(data), 4)]
    
    def get_labels(self):
        
        with open(self.filename) as f:
            
            header = f.readlines()[:self.header_lines]
            
        occurance_count = 0
        self.labels = []
        for line_number in range(len(header)):
            if 'LINE' in header[line_number].strip():
                line1 = header[line_number + 1]
                line2 = header[line_number + 2]
                self.labels.extend((line1 + line2).strip().split())
                occurance_count += 1
            if occurance_count == 4:
                break
        return
    
    def format_sample(self, sample_data):
        
        clean_data = []
        
        for data in sample_data:
            clean_data.extend(data.strip().split())
        
        return clean_data
    
    def format_data(self):
        
        data = self.read_file()
        
        data =  pd.DataFrame([self.format_sample(sample_data) for sample_data in data], columns=self.labels)
        
        #Clean up data using Panda's dataframe functions
        data = data.replace('.', np.nan)
        data = data.apply(pd.to_numeric, errors='ignore')
        
        #Drop the rows where KLH is NaN because these are our training values
        self.data = data.drop(data[data['KLH'].isna()].index)
    
    def get_dataframe(self):
        return self.data

class item():

    def __init__(self):
        
        return

    def readcsv(self,filename,headerlines):

        self.filename = filename
        self.headerlines = headerlines

        with open(self.filename) as csvfile:
            reader = list(csv.reader(csvfile))
            reader = np.array(reader)
            tuples = tuple(reader.T)

    def readbin(self):
        pass

    def readtxt(self):
        pass

    def set_property(self,props,header=None,X=None,Y=None,Z=None,dX=1,dY=1,dZ=1):
        """it creates best x,y,z values for the given property"""

        if props is not None:
            ones = np.ones(props.shape[0])
            if header is not None:
                for j,prop in enumerate(props.T):
                    setattr(self,header[j],prop)
            else:
                for j,prop in enumerate(props.T):
                    setattr(self,'property'+str(j),prop)
        elif X is not None:
            ones = np.ones(X.shape)
        elif Y is not None:
            ones = np.ones(Y.shape)
        elif Z is not None:
            ones = np.ones(Z.shape)
        else:
            return
        
        if X is None:
            try:
                self.x = (np.cumsum(ones,0)-1).ravel()*dX
            except:
                self.x = ones.ravel()
        else:
            self.x = X.ravel()

        if Y is None:
            try:
                self.y = (np.cumsum(ones,1)-1).ravel()*dY
            except:
                self.y = ones.ravel()
        else:
            self.y = Y.ravel()

        if Z is None:
            try:
                self.z = (np.cumsum(ones,2)-1).ravel()*dZ
            except:
                self.z = ones.ravel()
        else:
            self.z = Z.ravel()

if __name__ == "__main__":

    import unittest
    
    from tests import test_setitem
    
    unittest.main(test_setitem)
    
##    unittest.main(module='test_setitem')
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

