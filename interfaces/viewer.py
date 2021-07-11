import csv, os, sys

from PyQt5 import QtCore, QtGui, QtWidgets

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

class FileViewer(QtWidgets.QTreeView):

    fileClicked = QtCore.pyqtSignal(str)
    
    def __init__(self,parent):
        super(FileViewer,self).__init__(parent)
        self.curDir = QtCore.QDir.rootPath()
        self.model = QtWidgets.QFileSystemModel()
        self.model.setRootPath(self.curDir)
        #print(dir(self.model))
        self.setModel(self.model)
        self.setSortingEnabled(True)
        self.doubleClicked.connect(self.tree_file_open)
        #print(QtGui.QKeySequence.fromString(str(chara))[0])

    def tree_file_open(self,signal):

        #index = self.currentIndex()
        filepath = self.model.filePath(signal)

        if os.path.isdir(filepath):
            self.curDir = filepath
            self.setRootIndex(self.model.index(self.curDir))
        elif os.path.isfile(filepath):
            self.fileClicked.emit(filepath)
            #self.graphicsView.setImage(filepath)
            #os.startfile(filepath)

    def keyPressEvent(self,event):
        if event.key() == QtCore.Qt.Key_Backspace:
            oldDir = self.curDir
            self.curDir = os.path.dirname(oldDir)
            self.setRootIndex(self.model.index(self.curDir))
            self.scrollTo(self.model.index(oldDir))
        elif event.key() == QtCore.Qt.Key_Return:
            self.tree_file_open(self.currentIndex())
        elif event.key() == QtCore.Qt.Key_Up:
            if self.indexAbove(self.currentIndex()).isValid():
                self.setCurrentIndex(self.indexAbove(self.currentIndex()))
        elif event.key() == QtCore.Qt.Key_Down:
            if self.indexBelow(self.currentIndex()).isValid():
                self.setCurrentIndex(self.indexBelow(self.currentIndex()))
        else:
            self.keyboardSearch(QtGui.QKeySequence(event.key()).toString())

if __name__ == "__main__":

    app = QtWidgets.QApplication(sys.argv)

    window = Window(args.filepath)

    DataViewer(window)

    window.show()
##    win.raise_()

    sys.exit(app.exec_())
