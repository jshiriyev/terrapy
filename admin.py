import numpy as np

import openpyxl

from tkinter import *

class schedule():

    def __init__(self,info):

        for i in range(len(info)):
            
            header = info[i][0]
            body = np.array(info[i][1:])
            
            setattr(self,header,body)

    def set_hours(self,hours):

        self.hours_definition = hours[0]

        self.hours = np.array(hours[1:])
        
        self.hours_tot = self.hours.sum(axis=1)

    def set_fullname(self):

        name = np.char.add(self.first," ")
        
        self.fullname = np.char.add(name,self.last)

def clear(txt):
    txt.delete('1.0',END)

def save():
    pass

if __name__ == "__main__":

    path = "nmdoc.xlsx"

    wb = openpyxl.load_workbook(path)

    sheet1 = wb["lectures"]

    info1 = tuple(sheet1.iter_cols(min_row=2,max_row=5,
                                 min_col=2,max_col=6,values_only=True))

    courses = schedule(info1)
    
    hour1 = tuple(sheet1.iter_rows(min_row=2,max_row=5,
                                 min_col=7,max_col=17,values_only=True))

    courses.set_hours(hour1)

    sheet2 = wb["instructors"]

    info2 = tuple(sheet2.iter_cols(min_row=1,max_row=11,
                                 min_col=1,max_col=4,values_only=True))

    teachers = schedule(info2)

    teachers.set_fullname()

    window = Tk()
    window.title("BHOS-PE Administration")
##    window.geometry("1100x500")
    window.configure(background="white")

    Frame1 = Frame(window,padx=5,pady=5) #,text="Instructors"
    Frame2 = Frame(window,padx=5,pady=5,width=10,height=20) #,text="Fall Semester Schedule"
    Frame3 = Frame(window,padx=5,pady=5,width=10,height=20) #,text="Spring Semester Schedule"
    Frame4 = Frame(window,padx=5,pady=5,width=10,height=20) #,text="Courses"

    Frame1.configure(background="white")
    Frame2.configure(background="white")
    Frame3.configure(background="white")
    Frame4.configure(background="white")

    Frame1.pack(side=LEFT)
    Frame2.pack(side=LEFT)
    Frame3.pack(side=LEFT)
    Frame4.pack(side=LEFT)
##    Frame1.grid(row=0,column=0)
##    Frame2.grid(row=0,column=1)
##    Frame3.grid(row=0,column=2)
##    Frame4.grid(row=0,column=3)
    
##    var1 = StringVar()
##    var1.set("Select...")
##    myopmenu1 = OptionMenu(Frame1,var1,*teachers.get_fullname())
##    myopmenu1.config(width=30)
####    myopmenu1.config(width=len(max(teachers.get_fullname(), key=len)))
##    myopmenu1.grid()

    lb1 = Label(Frame1,text="Instructors")
    lb1.pack()

    listbox1 = Listbox(Frame1,width=30,height=20)
    
    for entry in teachers.fullname:
        listbox1.insert(END, entry)

    listbox1.pack()

    button1 = Button(Frame1,text="Test")
    button1.pack()

    lb2 = Label(Frame2,text="Fall Semester")
    lb2.grid(row=0,column=0,columnspan=2)

    text2 = Text(Frame2,width=30,height=20)
    text2.grid(row=1,column=0,columnspan=2)

    button2_1 = Button(Frame2,text="Clear Screen",command=lambda: clear(text2))
    button2_1.grid(row=2,column=0)

    button2_2 = Button(Frame2,text="Save Courses",command=save)
    button2_2.grid(row=2,column=1)

    lb3 = Label(Frame3,text="Spring Semester")
    lb3.grid(row=0,column=0,columnspan=2)
    
    text3 = Text(Frame3,width=30,height=20)
    text3.grid(row=1,column=0,columnspan=2)

    button3_1 = Button(Frame3,text="Clear Screen",command=lambda: clear(text3))
    button3_1.grid(row=2,column=0)

    button3_2 = Button(Frame3,text="Save Courses",command=save)
    button3_2.grid(row=2,column=1)

    lb4 = Label(Frame4,text="Courses")
    lb4.grid(row=0,column=0,columnspan=2)

    listbox4 = Listbox(Frame4,width=30,height=20)
    
    for entry in courses.code:
        listbox4.insert(END, entry)
        
    listbox4.grid(row=1,column=0,columnspan=2)
    
    button4_1 = Button(Frame4,text="to Spring")
    button4_1.grid(row=2,column=0)

    button4_2 = Button(Frame4,text="to Fall")
    button4_2.grid(row=2,column=1)
    
    window.mainloop()

    
