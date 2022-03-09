from stream.dataset import dataset

class collection(plot3D):

   def __init__(self,window,filename):

       super().__init__(window)

       self.filename = filename

       path = os.path.join(os.path.dirname(__file__),"tests",self.filename)

       self.data = dataset(filepath=path,skiplines=1)

       self.data.texttocolumn(0,deliminator="\t")

       self.data.astype(1,dtype=np.float64)
       self.data.astype(2,dtype=np.float64)
       self.data.astype(3,dtype=np.float64)
       self.data.astype(4,dtype=np.float64)

       self.itemnames = np.unique(self.data.running[0])

       self.attrnames = ["data"]

window = tk.Tk()

gui = collection(window,"datatest")
# gui = table(window=window,headers=["First Name","Last Name","Contact"])

gui.set_plot()

window.mainloop()