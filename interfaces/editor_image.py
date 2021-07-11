import csv
import datetime
import os
import sys

import matplotlib.pyplot as plt

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

import numpy as np

import openpyxl

from PyQt5 import QtCore
from PyQt5 import QtGui
from PyQt5 import QtWidgets

import sqlite3

import tkinter as tk

from tkinter import ttk
from tkinter import filedialog

"""
data manager - reader
data manager - writer
data visualizer - tables
data visualizer - graphs
"""

class Window(QtWidgets.QMainWindow):

    #itemEdited = QtCore.pyqtSignal(item,column)
    
    def __init__(self,filepath):
        
        super(Window,self).__init__()

        self.resize(1200,600)

        self.filepath = filepath

        self.imageView = ImageViewer(parent=self)
        
        self.centralwidget = QtWidgets.QWidget(self)
        self.verticalLayout = QtWidgets.QVBoxLayout(self.centralwidget)
        self.verticalLayout.addWidget(self.imageView)
        self.setCentralWidget(self.centralwidget)
        
        self.editedFlag = True

        self.show()   

class ImageViewer(QtWidgets.QGraphicsView):

    def __init__(self,parent):
        super(ImageViewer,self).__init__(parent)
        self.zoom = 0
        #print(dir(self))
        #self.pix = QtGui.QPixmap()
        #self.rec = QtCore.QRectF()
        self.scene = QtWidgets.QGraphicsScene()
        self.setScene(self.scene)
        self.empty = True

    def setImage(self,filepath):
        self.scene.clear()
        self.pix = QtGui.QPixmap(filepath)
        self.rec = QtCore.QRectF(self.pix.rect())
        self.scene.setSceneRect(self.rec)
        self.scene.addPixmap(self.pix)
        self.empty = False
        self.fitInView(self.rec,QtCore.Qt.KeepAspectRatio)

    def wheelEvent(self,event):
        if not self.empty:
            if event.angleDelta().y()>0:
                factor = 1.25
                self.zoom += 1
            else:
                factor = 0.8
                self.zoom -= 1
            if self.zoom>0:
                self.scale(factor,factor)
            elif self.zoom == 0:
                self.fitInView(self.rec,QtCore.Qt.KeepAspectRatio)
            else:
                self.zoom = 0
            #print("I am scrolling")

if __name__ == "__main__":

    """image editor"""

    app = QtWidgets.QApplication(sys.argv)
    window = Window(os.curdir)
    sys.exit(app.exec_())
    

