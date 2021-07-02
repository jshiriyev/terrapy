"""

The task was to get dates of well from the first input file
(inputfile0) and modify the production data of input file 2
(inputfile1) so that the specified well will have new
production data.

"""

import numpy as np

inputfile0 = 'C:\\Users\\javid.s\\Desktop\\BHR-076.txt'
inputfile1 = 'C:\\Users\\javid.s\\Desktop\\BHR_VI_SCHEDULE.INC'

outputfile = 'C:\\Users\\javid.s\\Desktop\\BHR_VI_SCHEDULE_V2.INC'

def skiptoline(file_read,keyword,file_written=None):
    
    while True:
        
        line = next(file_read)

        if file_written is not None:
            file_written.write(line)
        
        if line.split('/')[0].strip() == keyword:
            break

with open(inputfile0) as readfile:
    
    rdate = np.array([])
    rcopy = np.array([])

    rlines = readfile.readlines()

    for rlinenumber,rline in enumerate(rlines):

        if rlinenumber>0:

            alist = rline.split('\t')

            wellname = alist[0]

            date_list = alist[1].split('-')

            if date_list[1]=='01':
                date_newformat = date_list[2]+' JAN '+date_list[0]
            elif date_list[1]=='02':
                date_newformat = date_list[2]+' FEB '+date_list[0]
            elif date_list[1]=='03':
                date_newformat = date_list[2]+' MAR '+date_list[0]
            elif date_list[1]=='04':
                date_newformat = date_list[2]+' APR '+date_list[0]
            elif date_list[1]=='05':
                date_newformat = date_list[2]+' MAY '+date_list[0]
            elif date_list[1]=='06':
                date_newformat = date_list[2]+' JUN '+date_list[0]
            elif date_list[1]=='07':
                date_newformat = date_list[2]+' JUL '+date_list[0]
            elif date_list[1]=='08':
                date_newformat = date_list[2]+' AUG '+date_list[0]
            elif date_list[1]=='09':
                date_newformat = date_list[2]+' SEP '+date_list[0]
            elif date_list[1]=='10':
                date_newformat = date_list[2]+' OCT '+date_list[0]
            elif date_list[1]=='11':
                date_newformat = date_list[2]+' NOV '+date_list[0]
            elif date_list[1]=='12':
                date_newformat = date_list[2]+' DEC '+date_list[0]

            rdate = np.append(rdate,date_newformat)
                
            days = alist[2]
            condensate = alist[3]
            gas = alist[4]
            water = alist[5]

            if rlinenumber+1==len(rlines):
                copied_line = '\t\''+wellname+'\'\tSTOP\tGRAT\t'+condensate+'\t'+water+'\t'+gas+' /\n'
            else:
                copied_line = '\t\''+wellname+'\'\tOPEN\tGRAT\t'+condensate+'\t'+water+'\t'+gas+' /\n'

            rcopy = np.append(rcopy,copied_line)

        
with open(inputfile1) as rewrittenfile:

    with open(outputfile,"w") as writtenfile:

        for i,date in enumerate(rdate):

            skiptoline(rewrittenfile,'DATES',writtenfile)
            skiptoline(rewrittenfile,rdate[i],writtenfile)
            skiptoline(rewrittenfile,'WCONHIST',writtenfile)
            
            while True:
                
                wline = next(rewrittenfile)
                
                if wline.split('/')[0].strip().split(' ')[0] == '\'BHR_76\'':
                    writtenfile.write(rcopy[i])
                    break
                else:
                    writtenfile.write(wline)
                    
        while True:

            try:
                
                copiedline = next(rewrittenfile)
                writtenfile.write(copiedline)
                
            except:
                
                break
                
