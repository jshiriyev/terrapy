import numpy as np

import openpyxl

class lectures():

    def __init__(self,info,hours):

        for i in range(len(info)):
            
            header = info[i][0]
            body = info[i][1:]
            
            setattr(self,header,body)

        self.hours_definition = hours[0]

        self.hours = np.array(hours[1:])
        
        self.hours_tot = self.hours.sum(axis=1)

class instructors():

    def __init__(self,info):

        for i in range(len(info)):

            header = info[i][0]
            body = info[i][1:]

            setattr(self,header,body)

    

if __name__ == "__main__":

    path = "nmdoc.xlsx"

    wb = openpyxl.load_workbook(path)

    sheet1 = wb["lectures"]

    info1 = tuple(sheet1.iter_cols(min_row=2,max_row=5,
                                 min_col=2,max_col=6,values_only=True))
    
    hour1 = tuple(sheet1.iter_rows(min_row=2,max_row=5,
                                 min_col=7,max_col=17,values_only=True))

    courses = lectures(info1,hour1)

    sheet2 = wb["instructors"]

    info2 = tuple(sheet2.iter_cols(min_row=1,max_row=11,
                                 min_col=1,max_col=4,values_only=True))

    teachers = instructors(info2)

    
