import numpy as np

import openpyxl

from tkinter import *
from tkinter import ttk

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

        teachers.tuple = tuple(sheet_teachers.iter_cols(min_row=1,max_row=18,
                                     min_col=1,max_col=4,values_only=True))

        courses.tuple = tuple(sheet_lectures.iter_cols(min_row=2,max_row=47,
                                     min_col=2,max_col=6,values_only=True))
        
        hours.tuple = tuple(sheet_lectures.iter_rows(min_row=2,max_row=47,
                                     min_col=7,max_col=22,values_only=True))

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

        name = np.char.add(self.courses.code,"-")
        
        name = np.char.add(name,self.courses.semester_number.astype(str))

        name = np.char.add(name,"-")
        
        self.courses.description = np.char.add(name,self.courses.name_ENG) 

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

        self.set_notebook()
        
        self.set_frame_courses()

        self.notebook.pack(side=LEFT)
        self.frame_courses.pack(side=LEFT)

    def set_notebook(self):

        self.style = ttk.Style(self.root)

        configN = {"tabmargins":[2,5,2,0],"tabposition":"wn","background":"white"}

        configT = {"padding":[5,1],"background":"white"}

        mapT = {"background":[("selected","grey")],
                    "expand":[("selected",[1,1,1,0])]} 
        
        settings = {"TNotebook":{"configure":configN},
                "TNotebook.Tab":{"configure":configT,"map":mapT}}
        
        self.style.theme_create("yummy",parent="alt",settings=settings)

        self.style.theme_use("yummy")

        self.notebook = ttk.Notebook(self.root,style='lefttab.TNotebook')

        for i,name in enumerate(self.teachers.fullname):

            framename = "frame"+str(i)

            frame = self.set_frame_notebook()

            setattr(self,framename,frame)
            
            getattr(self,framename).pack(fill='both',expand=True)

            self.notebook.add(getattr(self,framename),text=name)

    def set_frame_notebook(self):

        frame = Frame(self.notebook,width=300,height=200)
        frame.configure(background="white")

        frame.label0 = Label(frame,text="Fall Semester")
        frame.label0.configure(background="white")
        frame.label0.grid(row=0,column=0,columnspan=2)

        frame.label1 = Label(frame,text="Spring Semester")
        frame.label1.configure(background="white")
        frame.label1.grid(row=0,column=2,columnspan=2)

        frame.listbox0 = Listbox(frame,width=40,height=20)
        frame.listbox0.grid(row=1,column=0,columnspan=2)
        
        frame.listbox1 = Listbox(frame,width=40,height=20)
        frame.listbox1.grid(row=1,column=2,columnspan=2)

        frame.button_drop0 = Button(frame,text="Drop Selected",
            command=lambda: self.drop_course(frame.listbox0,self.frame_courses.listbox))
        
        frame.button_drop0.configure(background="white")
        frame.button_drop0.grid(row=2,column=0,pady=10)
        
        frame.button_clear0 = Button(frame,text="Drop All",
            command=lambda: self.drop_course(frame.listbox0,self.frame_courses.listbox,
                                                    moveall=True))
        
        frame.button_clear0.configure(background="white")
        frame.button_clear0.grid(row=2,column=1,pady=10)

        frame.button_drop1 = Button(frame,text="Drop Selected",
            command=lambda: self.drop_course(frame.listbox1,self.frame_courses.listbox))
        
        frame.button_drop1.configure(background="white")
        frame.button_drop1.grid(row=2,column=2,pady=10)
        
        frame.button_clear1 = Button(frame,text="Drop All",
            command=lambda: self.drop_course(frame.listbox1,self.frame_courses.listbox,
                                                    moveall=True))
        
        frame.button_clear1.configure(background="white")
        frame.button_clear1.grid(row=2,column=3,pady=10)

        frame.button_test = Button(frame,text="Test Instructor's Schedule",
                                   command=self.test_load)
        
        frame.button_test.configure(background="white")
        frame.button_test.grid(row=3,column=0,columnspan=4,pady=10)

        return frame

    def set_frame_courses(self):
        
        self.frame_courses = Frame(self.root,padx=5,width=10,height=20)
        self.frame_courses.configure(background="white")

        self.frame_courses.label = Label(self.frame_courses,text="Courses")
        self.frame_courses.label.configure(background="white")
        self.frame_courses.label.grid(row=0,column=0,columnspan=3)

        self.frame_courses.listbox = Listbox(self.frame_courses,width=40,height=20)
        
        for entry in self.courses.description:
            self.frame_courses.listbox.insert(END,entry)
            
        self.frame_courses.listbox.grid(row=1,column=0,columnspan=3)

        idx = self.notebook.index(self.notebook.select())

        self.frame_courses.button_tofall = \
            Button(self.frame_courses,text="Add to Fall",
                   command=lambda: self.add_course("Fall"))

        self.frame_courses.button_tofall.configure(background="white")
        self.frame_courses.button_tofall.grid(row=2,column=0,pady=10)

        self.frame_courses.button_tospring = \
            Button(self.frame_courses,text="Add to Spring",
                   command=lambda: self.add_course("Spring"))

        self.frame_courses.button_tospring.configure(background="white")
        self.frame_courses.button_tospring.grid(row=2,column=1,pady=10)

        self.frame_courses.button_extended = Button(self.frame_courses,
                                                    text="Extended View")

        self.frame_courses.button_extended.configure(background="white")
        self.frame_courses.button_extended.grid(row=2,column=2,pady=10)

        self.frame_courses.status_text = StringVar()

        self.frame_courses.status = Label(self.frame_courses,
                                          relief="sunken",
                                          textvariable=self.frame_courses.status_text)
        
        self.frame_courses.status.configure(background="white")
        self.frame_courses.status.grid(row=3,column=0,columnspan=3,pady=10,sticky=EW)

    def drop_course(self,frombox,tobox,moveall=False):

        if moveall:
            for entry in frombox.get(0,END):
                tobox.insert(END,entry)
            frombox.delete(0,END)
        
        elif frombox.curselection():
            tobox.insert(END,frombox.get(frombox.curselection()))
            frombox.delete(frombox.curselection())

    def add_course(self,semester):

        idx = self.notebook.index(self.notebook.select())

        frameID = "frame"+str(idx)

        frombox = self.frame_courses.listbox

        if semester == "Fall":
            tobox = getattr(self,frameID).listbox0
        elif semester == "Spring":
            tobox = getattr(self,frameID).listbox1 

        tobox.insert(END,frombox.get(frombox.curselection()))
        frombox.delete(frombox.curselection())

    def test_load(self):

        idx = self.notebook.index(self.notebook.select())

        frameID = "frame"+str(idx)

        s0 = getattr(self,frameID).listbox0.get(0,END)
        s1 = getattr(self,frameID).listbox1.get(0,END)

        annual_load = np.array([np.array(s0+s1)]).T

        idy = np.argwhere(np.array([self.courses.description])==annual_load)[:,1]
            
        hours = schedule.hours.nparray[idy,:]

        hours_sum = hours.sum(axis=0)

        status = "total hours is "+str(int(hours_sum[8]))

        self.frame_courses.status_text.set(status)

    def save(self):
        
        pass

if __name__ == "__main__":

    path = "C:\\Users\\Cavid\\Documents\\nmdoc.xlsx"
    
    window = Tk()

    schedule = adminGUI(window,path)
    
##    window.geometry("1100x500")
    
    window.mainloop()
