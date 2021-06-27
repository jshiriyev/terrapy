import numpy as np

import openpyxl

from tkinter import *

class adminGUI():

    def __init__(self,window,path):

        self.root = window

        self.path = path
        
        self.set_input()

        self.set_teachers()
        self.set_courses()
        self.set_hours()

        self.initUI()

    def set_input(self):

        wb = openpyxl.load_workbook(self.path)

        class teachers: pass
        class courses: pass
        class hours: pass

        sheet_teachers = wb["instructors"]
        sheet_lectures = wb["lectures"]

        teachers.tuple = tuple(sheet_teachers.iter_cols(min_row=1,max_row=11,
                                     min_col=1,max_col=4,values_only=True))

        courses.tuple = tuple(sheet_lectures.iter_cols(min_row=2,max_row=5,
                                     min_col=2,max_col=6,values_only=True))
        
        hours.tuple = tuple(sheet_lectures.iter_rows(min_row=2,max_row=5,
                                     min_col=7,max_col=17,values_only=True))

        self.teachers = teachers
        self.courses = courses
        self.hours = hours
        
    def set_teachers(self):
        
        for i in range(len(self.teachers.tuple)):
            
            header = self.teachers.tuple[i][0]
            body = np.array(self.teachers.tuple[i][1:])
            
            setattr(self.teachers,header,body)

        name = np.char.add(self.teachers.first," ")

        self.teachers.fullname = np.char.add(name,self.teachers.last)

    def set_courses(self):
        
        for i in range(len(self.courses.tuple)):
            
            header = self.courses.tuple[i][0]
            body = np.array(self.courses.tuple[i][1:])
            
            setattr(self.courses,header,body)

    def set_hours(self):

        self.hours.definition = self.hours.tuple[0]

        self.hours.nparray = np.array(self.hours.tuple[1:])
        
        self.hours.total = self.hours.nparray.sum(axis=1)

    def initUI(self):

        self.root.title("BHOS-PE Administration")
        
        self.root.configure(background="white")

        menubar = Menu(self.root)
        
        self.root.config(menu=menubar)

        fileMenu = Menu(menubar)
    
        fileMenu.add_command(label="Open")
        fileMenu.add_command(label="Exit")
        
        menubar.add_cascade(label="File", menu=fileMenu)

        self.set_frame_teachers()
        self.set_frame_fall()
        self.set_frame_spring()
        self.set_frame_courses()
        
        self.frame_teachers.pack(side=LEFT)
        self.frame_fall.pack(side=LEFT)
        self.frame_spring.pack(side=LEFT)
        self.frame_courses.pack(side=LEFT)

    def set_frame_teachers(self):

        self.frame_teachers = Frame(self.root,padx=5,pady=5)
        self.frame_teachers.configure(background="white")
        
        self.frame_teachers.label = Label(self.frame_teachers,text="Instructors")
        self.frame_teachers.label.configure(background="white")
        self.frame_teachers.label.pack()

        self.frame_teachers.listbox = Listbox(self.frame_teachers,width=30,height=20)
        
        for entry in self.teachers.fullname:
            self.frame_teachers.listbox.insert(END,entry)

        self.frame_teachers.listbox.pack()

        self.frame_teachers.button = Button(self.frame_teachers,text="Test")
        self.frame_teachers.button.configure(background="white")
        self.frame_teachers.button.pack()

    def set_frame_fall(self):
        
        self.frame_fall = Frame(self.root,padx=5,pady=5,width=10,height=20)
        self.frame_fall.configure(background="white")
        
        self.frame_fall.label = Label(self.frame_fall,text="Fall Semester")
        self.frame_fall.label.configure(background="white")
        self.frame_fall.label.grid(row=0,column=0,columnspan=3)

        self.frame_fall.listbox = Listbox(self.frame_fall,width=40,height=20)
        self.frame_fall.listbox.grid(row=1,column=0,columnspan=3)

        self.frame_fall.button_drop = \
            Button(self.frame_fall,text="Drop Selected",
                   command=lambda: self.move_course(self.frame_fall.listbox,
                                                    self.frame_courses.listbox))
        
        self.frame_fall.button_drop.configure(background="white")
        self.frame_fall.button_drop.grid(row=2,column=0)
        
        self.frame_fall.button_clear = \
            Button(self.frame_fall,text="Drop All",
                   command=lambda: self.move_course(self.frame_fall.listbox,
                                                    self.frame_courses.listbox,
                                                    moveall=True))
        
        self.frame_fall.button_clear.configure(background="white")
        self.frame_fall.button_clear.grid(row=2,column=1)

        self.frame_fall.button_save = \
            Button(self.frame_fall,text="Save Courses",command=self.save)
        
        self.frame_fall.button_save.configure(background="white")
        self.frame_fall.button_save.grid(row=2,column=2)

    def set_frame_spring(self):
        
        self.frame_spring = Frame(self.root,padx=5,pady=5,width=10,height=20)
        self.frame_spring.configure(background="white")
        
        self.frame_spring.label = Label(self.frame_spring,text="Spring Semester")
        self.frame_spring.label.configure(background="white")
        self.frame_spring.label.grid(row=0,column=0,columnspan=3)
        
        self.frame_spring.listbox = Listbox(self.frame_spring,width=40,height=20)
        self.frame_spring.listbox.grid(row=1,column=0,columnspan=3)

        self.frame_spring.button_drop = \
            Button(self.frame_spring,text="Drop Selected",
                   command=lambda: self.move_course(self.frame_spring.listbox,
                                                    self.frame_courses.listbox))
        
        self.frame_spring.button_drop.configure(background="white")
        self.frame_spring.button_drop.grid(row=2,column=0)

        self.frame_spring.button_clear = \
            Button(self.frame_spring,text="Drop All",
                   command=lambda: self.move_course(self.frame_spring.listbox,
                                                    self.frame_courses.listbox,
                                                    moveall=True))

        self.frame_spring.button_clear.configure(background="white")
        self.frame_spring.button_clear.grid(row=2,column=1)

        self.frame_spring.button_save = Button(self.frame_spring,
                                               text="Save Courses",
                                               command=self.save)
        
        self.frame_spring.button_save.configure(background="white")
        self.frame_spring.button_save.grid(row=2,column=2)

    def set_frame_courses(self):
        
        self.frame_courses = Frame(self.root,padx=5,pady=5,width=10,height=20)
        self.frame_courses.configure(background="white")

        self.frame_courses.label = Label(self.frame_courses,text="Courses")
        self.frame_courses.label.configure(background="white")
        self.frame_courses.label.grid(row=0,column=0,columnspan=2)

        self.frame_courses.listbox = Listbox(self.frame_courses,width=40,height=20)
        
        for entry in self.courses.code:
            self.frame_courses.listbox.insert(END,entry)
            
        self.frame_courses.listbox.grid(row=1,column=0,columnspan=2)

        self.frame_courses.button_tofall = \
            Button(self.frame_courses,text="Add to Fall",
                   command=lambda: self.move_course(self.frame_courses.listbox,
                                                    self.frame_fall.listbox))

        self.frame_courses.button_tofall.configure(background="white")
        self.frame_courses.button_tofall.grid(row=2,column=0)

        self.frame_courses.button_tospring = \
            Button(self.frame_courses,text="Add to Spring",
                   command=lambda: self.move_course(self.frame_courses.listbox,
                                                    self.frame_spring.listbox))

        self.frame_courses.button_tospring.configure(background="white")
        self.frame_courses.button_tospring.grid(row=2,column=1)

    def move_course(self,frombox,tobox,moveall=False):

        if moveall:
            for entry in frombox.get(0,END):
                tobox.insert(END,entry)
            frombox.delete(0,END)
        
        elif frombox.curselection():
            tobox.insert(END,frombox.get(frombox.curselection()))
            frombox.delete(frombox.curselection())

    def save(self):
        
        pass

if __name__ == "__main__":

    path = "C:\\Users\\Cavid\\Documents\\nmdoc.xlsx"
    
    window = Tk()

    schedule = adminGUI(window,path)
    
##    window.geometry("1100x500")
    
    window.mainloop()
