import os

import sqlite3

import numpy as np

import openpyxl

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

class schedule():

    def __init__(self,window):

        self.root = window

        self.init_interface()

    def init_interface(self):

        self.label_firstname = tk.Label(self.root,text="First Name")
        self.label_firstname.grid(row=0,column=0)

        self.label_lastname = tk.Label(self.root,text="Last Name")
        self.label_lastname.grid(row=1,column=0)

        self.entry_firstname = tk.Entry(self.root)
        self.entry_firstname.grid(row=0,column=1)

        self.entry_lastname = tk.Entry(self.root)
        self.entry_lastname.grid(row=1,column=1)

        self.button_enter = tk.Button(self.root,text="Enter",command=self.save_to_database)
        self.button_enter.grid(row=2,column=1)

    def save_to_database(self):

        db_file = r"C:\Users\Cavid\Documents\bhospy\instructors.db"

        self.conn = sqlite3.connect(db_file)

##        sqlite_table = '''CREATE TABLE SqliteDb_developers (
##                          id INTEGER PRIMARY KEY,
##                          name TEXT NOT NULL,
##                          email text NOT NULL UNIQUE);'''
        
        self.cursor = self.conn.cursor()

##        self.cursor.execute(sqlite_table)
##        self.conn.commit()
        
        self.cursor.close()
        self.conn.close()
        
if __name__ == "__main__":
    
    window = tk.Tk()
    
    gui = schedule(window)

    window.mainloop()
