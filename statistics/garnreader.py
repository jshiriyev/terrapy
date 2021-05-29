import csv

import numpy as np

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

if __name__ == "__main__":

    pass
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

