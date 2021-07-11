#!/usr/bin/python3

import os,sys

##sys.path.insert(0,os.getcwd()+'/'+'gui')
##sys.path.insert(0,os.getcwd()+'/'+'src')

from PyQt4 import QtGui, QtCore, QtWidgets

##from saWindow import Ui_mainWindow
##from saWidget import Ui_Form

##from simulated_annealing import annealing

class Window(QtWidgets.QMainWindow,Ui_mainWindow):

    def __init__(self,*args,**kwargs):

        super(Window,self).__init__(*args,**kwargs)

        self.setupUi(self)

        self.sawidget = Widget()
        self.setCentralWidget(self.sawidget)

        self.actionOpen.triggered.connect(self.open)
        self.actionClear.triggered.connect(self.sawidget.clear)
        
        #print(dir(self))

    def open(self):

        self.filePath = QtGui.QFileDialog()
        self.filePath.setDirectory(os.environ["HOME"])
        self.filePath.setWindowTitle("Open Objective Directory")
        self.filePath.setFileMode(QtGui.QFileDialog.Directory)
        self.filePath.fileSelected.connect(self.setObjective)
        self.filePath.exec_()

    def setObjective(self):

        self.sawidget.ui.comboObjective.clear()

        fileDir = self.filePath.selectedFiles()[0]

        for root,dirs,files in os.walk(fileDir):
            for ifile in files:
                if ifile.endswith(".py"):
                    self.sawidget.ui.comboObjective.addItem(ifile)

class Widget(QtWidgets):

    def __init__(self,*args,**kwargs):

        super(Widget,self).__init__(*args,**kwargs)

        self.ui = Ui_Form()
        self.ui.setupUi(self)

        self.ui.inpNofIterations.setValidator(QtGui.QIntValidator())
        self.ui.inpNofModels.setValidator(QtGui.QIntValidator())
        self.ui.inpNofParam.setValidator(QtGui.QIntValidator())

        self.ui.inpLowerBound.setDisabled(True)
        self.ui.inpUpperBound.setDisabled(True)

        self.ui.inpNofParam.textEdited.connect(self.boundValidation)
        
##        self.ui.btnStart.clicked.connect(self.running)

        self.ui.progressBar.setValue(0)
        self.ui.logRunOutput.setStyleSheet("QTextEdit {color:gray}")

    def boundValidation(self):

        nofparam = self.ui.inpNofParam.text()
        
        if not nofparam:
            self.ui.inpLowerBound.setDisabled(True)
            self.ui.inpUpperBound.setDisabled(True)
            return
        else:
            self.ui.inpLowerBound.setDisabled(False)
            self.ui.inpUpperBound.setDisabled(False)

        nofparam = int(nofparam)

        self.ui.inpLowerBound.clear()
        self.ui.inpUpperBound.clear()

        #print(dir(self.ui.inpLowerBound))

        string = "[-+]?\d{1,}\.?\d{,}"  # ? is the shorthand for the quantifier {0,1}
        sregex = "[-+]?\d{1,}\.?\d{,}"  # ? is the shorthand for the quantifier {0,1}

        for i in range(nofparam-1):
            #sregex = sregex+"\,"+string
            sregex = sregex+"[, ]"+string

        self.regex = QtCore.QRegExp(sregex)

        self.ui.inpLowerBound.setValidator(QtGui.QRegExpValidator(self.regex))
        self.ui.inpUpperBound.setValidator(QtGui.QRegExpValidator(self.regex))

    def clear(self):

        self.ui.inpNofIterations.clear()
        self.ui.inpNofModels.clear()
        self.ui.inpNofParam.clear()

        self.ui.inpLowerBound.clear()
        self.ui.inpUpperBound.clear()

        self.ui.inpLowerBound.setDisabled(True)
        self.ui.inpUpperBound.setDisabled(True)

        self.ui.logRunOutput.clear()

    def running(self):

        err1 = ""

        if not self.ui.inpNofIterations.text():
            err1 = "Number of iterations is required!"
        elif not self.ui.inpNofModels.text():
            err1 = "Number of models (population) is required!"
        elif not self.ui.inpNofParam.text():
            err1 = "Number of parameters is required!"
        elif not (self.ui.inpLowerBound.text() and self.ui.inpUpperBound.text()):
            err1 = "Lower and Upper boundaries for the parameters are required!"
        
        if err1:
            msgBox1 = QtGui.QMessageBox()
            msgBox1.setIcon(QtGui.QMessageBox.Warning)
            msgBox1.setWindowTitle("Missing Value!")
            msgBox1.setText(err1)
            msgBox1.exec_()
            return
        
        err2 = ""

        if not self.regex.exactMatch(self.ui.inpLowerBound.text()):
            err2 = "Lower boundary is not matching number of parameters."
        elif not self.regex.exactMatch(self.ui.inpUpperBound.text()):
            err2 = "Upper boundary is not matching number of parameters."

        if err2:
            msgBox2 = QtGui.QMessageBox()
            msgBox2.setIcon(QtGui.QMessageBox.Warning)
            msgBox2.setWindowTitle("Wrong Input!")
            msgBox2.setText(err2)
            msgBox2.exec_()
            return

        self.ui.logRunOutput.clear()

        self.inpFile()
        
        srcCode = annealing()

        srcCode.temperature()
        self.ui.logRunOutput.insertPlainText("temperature profile is constructed ...\n")

        srcCode.iterating()
        self.ui.logRunOutput.insertPlainText("source code run successfully ...\n")

        self.ui.logRunOutput.insertPlainText("displaying results ...\n")
        srcCode.plotError()

    def inpFile(self):

        nofitern = self.ui.inpNofIterations.text()
        nofmodel = self.ui.inpNofModels.text()
        nofparam = self.ui.inpNofParam.text()

        lowerBound = self.ui.inpLowerBound.text()
        upperBound = self.ui.inpUpperBound.text()

        tempType = self.ui.comboTemperature.currentText()

        inpfile = open("sa.inp","w")

        inpfile.write(str(int(nofitern))+'\n')
        inpfile.write(str(int(nofmodel))+'\n')
        inpfile.write(str(int(nofparam))+'\n')

        inpfile.write(str(lowerBound)+'\n')
        inpfile.write(str(upperBound)+'\n')
        
        inpfile.write(tempType+'\n')
        
        inpfile.close()

        self.ui.logRunOutput.insertPlainText("input file is ready ...\n")

class Ui_mainWindow(object):
    def setupUi(self, mainWindow):
        mainWindow.setObjectName(_fromUtf8("mainWindow"))
        mainWindow.resize(300, 650)
        self.centralwidget = QtGui.QWidget(mainWindow)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        mainWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(mainWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 300, 25))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        self.menuFile = QtGui.QMenu(self.menubar)
        self.menuFile.setObjectName(_fromUtf8("menuFile"))
        self.menuHelp = QtGui.QMenu(self.menubar)
        self.menuHelp.setObjectName(_fromUtf8("menuHelp"))
        mainWindow.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(mainWindow)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        mainWindow.setStatusBar(self.statusbar)
        self.actionAbout = QtGui.QAction(mainWindow)
        self.actionAbout.setObjectName(_fromUtf8("actionAbout"))
        self.actionOpen = QtGui.QAction(mainWindow)
        self.actionOpen.setObjectName(_fromUtf8("actionOpen"))
        self.actionExport = QtGui.QAction(mainWindow)
        self.actionExport.setObjectName(_fromUtf8("actionExport"))
        self.actionReset = QtGui.QAction(mainWindow)
        self.actionReset.setObjectName(_fromUtf8("actionReset"))
        self.actionClose = QtGui.QAction(mainWindow)
        self.actionClose.setObjectName(_fromUtf8("actionClose"))
        self.actionClear = QtGui.QAction(mainWindow)
        self.actionClear.setObjectName(_fromUtf8("actionClear"))
        self.menuFile.addAction(self.actionOpen)
        self.menuFile.addSeparator()
        self.menuFile.addAction(self.actionReset)
        self.menuFile.addAction(self.actionClear)
        self.menuFile.addSeparator()
        self.menuFile.addAction(self.actionExport)
        self.menuFile.addSeparator()
        self.menuFile.addAction(self.actionClose)
        self.menuHelp.addAction(self.actionAbout)
        self.menubar.addAction(self.menuFile.menuAction())
        self.menubar.addAction(self.menuHelp.menuAction())

        self.retranslateUi(mainWindow)
        QtCore.QMetaObject.connectSlotsByName(mainWindow)

    def retranslateUi(self, mainWindow):
        mainWindow.setWindowTitle(_translate("mainWindow", "Simulated Annealing", None))
        self.menuFile.setTitle(_translate("mainWindow", "File", None))
        self.menuHelp.setTitle(_translate("mainWindow", "Help", None))
        self.actionAbout.setText(_translate("mainWindow", "About Toolbox", None))
        self.actionOpen.setText(_translate("mainWindow", "Open", None))
        self.actionExport.setText(_translate("mainWindow", "Export...", None))
        self.actionReset.setText(_translate("mainWindow", "Reset Toolbox", None))
        self.actionClose.setText(_translate("mainWindow", "Close", None))
        self.actionClear.setText(_translate("mainWindow", "Clear Fields", None))

class Ui_Form(object):
    def setupUi(self, Form):
        Form.setObjectName(_fromUtf8("Form"))
        Form.setEnabled(True)
        Form.resize(287, 617)
        self.verticalLayout_4 = QtGui.QVBoxLayout(Form)
        self.verticalLayout_4.setObjectName(_fromUtf8("verticalLayout_4"))
        self.LayoutObjective = QtGui.QVBoxLayout()
        self.LayoutObjective.setObjectName(_fromUtf8("LayoutObjective"))
        self.txtObjectiveFunction = QtGui.QLabel(Form)
        self.txtObjectiveFunction.setEnabled(True)
        self.txtObjectiveFunction.setMinimumSize(QtCore.QSize(0, 20))
        self.txtObjectiveFunction.setMaximumSize(QtCore.QSize(16777215, 20))
        self.txtObjectiveFunction.setObjectName(_fromUtf8("txtObjectiveFunction"))
        self.LayoutObjective.addWidget(self.txtObjectiveFunction)
        self.comboObjective = QtGui.QComboBox(Form)
        self.comboObjective.setMinimumSize(QtCore.QSize(170, 30))
        self.comboObjective.setMaximumSize(QtCore.QSize(16777215, 30))
        self.comboObjective.setObjectName(_fromUtf8("comboObjective"))
        self.LayoutObjective.addWidget(self.comboObjective)
        spacerItem = QtGui.QSpacerItem(20, 30, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Maximum)
        self.LayoutObjective.addItem(spacerItem)
        self.verticalLayout_4.addLayout(self.LayoutObjective)
        self.LayoutNumberOfs = QtGui.QGridLayout()
        self.LayoutNumberOfs.setObjectName(_fromUtf8("LayoutNumberOfs"))
        self.txtNofIterations = QtGui.QLabel(Form)
        self.txtNofIterations.setMinimumSize(QtCore.QSize(0, 20))
        self.txtNofIterations.setMaximumSize(QtCore.QSize(16777215, 20))
        self.txtNofIterations.setObjectName(_fromUtf8("txtNofIterations"))
        self.LayoutNumberOfs.addWidget(self.txtNofIterations, 0, 0, 1, 1)
        self.txtNofModels = QtGui.QLabel(Form)
        self.txtNofModels.setMinimumSize(QtCore.QSize(0, 20))
        self.txtNofModels.setMaximumSize(QtCore.QSize(16777215, 20))
        self.txtNofModels.setObjectName(_fromUtf8("txtNofModels"))
        self.LayoutNumberOfs.addWidget(self.txtNofModels, 0, 1, 1, 1)
        self.txtNofParam = QtGui.QLabel(Form)
        self.txtNofParam.setMinimumSize(QtCore.QSize(0, 20))
        self.txtNofParam.setMaximumSize(QtCore.QSize(16777215, 20))
        self.txtNofParam.setObjectName(_fromUtf8("txtNofParam"))
        self.LayoutNumberOfs.addWidget(self.txtNofParam, 0, 2, 1, 1)
        self.inpNofIterations = QtGui.QLineEdit(Form)
        self.inpNofIterations.setMinimumSize(QtCore.QSize(0, 30))
        self.inpNofIterations.setMaximumSize(QtCore.QSize(16777215, 30))
        self.inpNofIterations.setText(_fromUtf8(""))
        self.inpNofIterations.setObjectName(_fromUtf8("inpNofIterations"))
        self.LayoutNumberOfs.addWidget(self.inpNofIterations, 1, 0, 1, 1)
        self.inpNofModels = QtGui.QLineEdit(Form)
        self.inpNofModels.setMinimumSize(QtCore.QSize(0, 30))
        self.inpNofModels.setMaximumSize(QtCore.QSize(16777215, 30))
        self.inpNofModels.setText(_fromUtf8(""))
        self.inpNofModels.setObjectName(_fromUtf8("inpNofModels"))
        self.LayoutNumberOfs.addWidget(self.inpNofModels, 1, 1, 1, 1)
        self.inpNofParam = QtGui.QLineEdit(Form)
        self.inpNofParam.setMinimumSize(QtCore.QSize(0, 30))
        self.inpNofParam.setMaximumSize(QtCore.QSize(16777215, 30))
        self.inpNofParam.setText(_fromUtf8(""))
        self.inpNofParam.setObjectName(_fromUtf8("inpNofParam"))
        self.LayoutNumberOfs.addWidget(self.inpNofParam, 1, 2, 1, 1)
        self.verticalLayout_4.addLayout(self.LayoutNumberOfs)
        self.LayoutBoundsandCooling = QtGui.QVBoxLayout()
        self.LayoutBoundsandCooling.setObjectName(_fromUtf8("LayoutBoundsandCooling"))
        self.txtLowerBound = QtGui.QLabel(Form)
        self.txtLowerBound.setMinimumSize(QtCore.QSize(170, 20))
        self.txtLowerBound.setMaximumSize(QtCore.QSize(16777215, 20))
        self.txtLowerBound.setObjectName(_fromUtf8("txtLowerBound"))
        self.LayoutBoundsandCooling.addWidget(self.txtLowerBound)
        self.inpLowerBound = QtGui.QLineEdit(Form)
        self.inpLowerBound.setMinimumSize(QtCore.QSize(170, 30))
        self.inpLowerBound.setMaximumSize(QtCore.QSize(16777215, 30))
        self.inpLowerBound.setText(_fromUtf8(""))
        self.inpLowerBound.setObjectName(_fromUtf8("inpLowerBound"))
        self.LayoutBoundsandCooling.addWidget(self.inpLowerBound)
        self.txtUpperBound = QtGui.QLabel(Form)
        self.txtUpperBound.setMinimumSize(QtCore.QSize(170, 20))
        self.txtUpperBound.setMaximumSize(QtCore.QSize(16777215, 20))
        self.txtUpperBound.setObjectName(_fromUtf8("txtUpperBound"))
        self.LayoutBoundsandCooling.addWidget(self.txtUpperBound)
        self.inpUpperBound = QtGui.QLineEdit(Form)
        self.inpUpperBound.setMinimumSize(QtCore.QSize(170, 30))
        self.inpUpperBound.setMaximumSize(QtCore.QSize(16777215, 30))
        self.inpUpperBound.setText(_fromUtf8(""))
        self.inpUpperBound.setObjectName(_fromUtf8("inpUpperBound"))
        self.LayoutBoundsandCooling.addWidget(self.inpUpperBound)
        self.txtCoolingCurve = QtGui.QLabel(Form)
        self.txtCoolingCurve.setMinimumSize(QtCore.QSize(170, 20))
        self.txtCoolingCurve.setMaximumSize(QtCore.QSize(16777215, 20))
        self.txtCoolingCurve.setObjectName(_fromUtf8("txtCoolingCurve"))
        self.LayoutBoundsandCooling.addWidget(self.txtCoolingCurve)
        self.comboTemperature = QtGui.QComboBox(Form)
        self.comboTemperature.setMinimumSize(QtCore.QSize(170, 30))
        self.comboTemperature.setMaximumSize(QtCore.QSize(16777215, 30))
        self.comboTemperature.setObjectName(_fromUtf8("comboTemperature"))
        self.comboTemperature.addItem(_fromUtf8(""))
        self.comboTemperature.addItem(_fromUtf8(""))
        self.comboTemperature.addItem(_fromUtf8(""))
        self.comboTemperature.addItem(_fromUtf8(""))
        self.LayoutBoundsandCooling.addWidget(self.comboTemperature)
        spacerItem1 = QtGui.QSpacerItem(20, 30, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Maximum)
        self.LayoutBoundsandCooling.addItem(spacerItem1)
        self.verticalLayout_4.addLayout(self.LayoutBoundsandCooling)
        self.LayoutRunControl = QtGui.QHBoxLayout()
        self.LayoutRunControl.setObjectName(_fromUtf8("LayoutRunControl"))
        self.btnStart = QtGui.QPushButton(Form)
        self.btnStart.setMinimumSize(QtCore.QSize(0, 30))
        self.btnStart.setMaximumSize(QtCore.QSize(16777215, 30))
        self.btnStart.setObjectName(_fromUtf8("btnStart"))
        self.LayoutRunControl.addWidget(self.btnStart)
        self.btnPause = QtGui.QPushButton(Form)
        self.btnPause.setMinimumSize(QtCore.QSize(0, 30))
        self.btnPause.setMaximumSize(QtCore.QSize(16777215, 30))
        self.btnPause.setObjectName(_fromUtf8("btnPause"))
        self.LayoutRunControl.addWidget(self.btnPause)
        self.btnStop = QtGui.QPushButton(Form)
        self.btnStop.setMinimumSize(QtCore.QSize(0, 30))
        self.btnStop.setMaximumSize(QtCore.QSize(16777215, 30))
        self.btnStop.setObjectName(_fromUtf8("btnStop"))
        self.LayoutRunControl.addWidget(self.btnStop)
        self.verticalLayout_4.addLayout(self.LayoutRunControl)
        self.LayoutRunInfo = QtGui.QVBoxLayout()
        self.LayoutRunInfo.setObjectName(_fromUtf8("LayoutRunInfo"))
        self.progressBar = QtGui.QProgressBar(Form)
        self.progressBar.setProperty("value", 24)
        self.progressBar.setObjectName(_fromUtf8("progressBar"))
        self.LayoutRunInfo.addWidget(self.progressBar)
        self.logRunOutput = QtGui.QTextEdit(Form)
        self.logRunOutput.setMinimumSize(QtCore.QSize(170, 200))
        self.logRunOutput.setReadOnly(True)
        self.logRunOutput.setObjectName(_fromUtf8("logRunOutput"))
        self.LayoutRunInfo.addWidget(self.logRunOutput)
        self.verticalLayout_4.addLayout(self.LayoutRunInfo)

        self.retranslateUi(Form)
        QtCore.QMetaObject.connectSlotsByName(Form)

    def retranslateUi(self, Form):
        Form.setWindowTitle(_translate("Form", "Simulated Annealing", None))
        self.txtObjectiveFunction.setText(_translate("Form", "Objective Function", None))
        self.txtNofIterations.setText(_translate("Form", "# Iterations", None))
        self.txtNofModels.setText(_translate("Form", "# Models", None))
        self.txtNofParam.setText(_translate("Form", "# Parameters", None))
        self.txtLowerBound.setText(_translate("Form", "Lower bound", None))
        self.txtUpperBound.setText(_translate("Form", "Upper bound", None))
        self.txtCoolingCurve.setText(_translate("Form", "Cooling curve", None))
        self.comboTemperature.setItemText(0, _translate("Form", "straight", None))
        self.comboTemperature.setItemText(1, _translate("Form", "geometric", None))
        self.comboTemperature.setItemText(2, _translate("Form", "reciprocal", None))
        self.comboTemperature.setItemText(3, _translate("Form", "logarithmic", None))
        self.btnStart.setText(_translate("Form", "Start", None))
        self.btnPause.setText(_translate("Form", "Pause", None))
        self.btnStop.setText(_translate("Form", "Stop", None))

if __name__ == "__main__":

    import sys

    app = QtGui.QApplication(sys.argv)

    win = Window()

    win.show()
    win.raise_()

    sys.exit(app.exec_())
