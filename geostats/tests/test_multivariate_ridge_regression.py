# -*- coding: utf-8 -*-

# First we'll need to install a few missing packages.
# Run the following at the command prompt
# 
# >conda install scikit-learn pandas
# >pip install sklearn-pandas
# 
# Use this data to create a machine learning model with "RidgeRegession"
# that can predict the permeability, i.e. "KLH", from the data.
# Recall that standard permeability prediction models utilize only
# porosity and/or possibly grain size, e.g. Kozeny-Carmen.
# Compare the difference in the "goodness" of the models, by plotting
# the model predicted permeability as a function of the actual permeability.
# A "perfect fit" would yield a straight line with a slope of unity.
# 
# A few requirements:
# 
# Use a "LabelBinarizer" to convert textual data to numbers that can
# have computations performed on them.  
# 
# Fill missing data with the average value from the column.  
# 
# Preprocess the data with "StandardScaler".  
# 
# Use only a subset of the data to build the model.
# Test the model against the entire dataset.
 
from sklearn.preprocessing import LabelBinarizer, StandardScaler
from sklearn.impute import SimpleImputer as Imputer
from sklearn.linear_model import Ridge
from sklearn.model_selection import train_test_split
from sklearn_pandas import DataFrameMapper
from sklearn.pipeline import make_pipeline

from data_reader import dataReader

import numpy as np
import matplotlib.pyplot as plt

gr = dataReader('garn_formation.DAT', header_lines=123)
df = gr.get_dataframe(); df

class MyRidge(Ridge):
    def fit(self,X,y):
        return super().fit(X,np.log(y))
    def predict(self,X):
        return np.exp(super().predict(X))

y = df['KLH'];
X_porosity_only = df['POR'];
Xtrain, Xtest, ytrain, ytest = train_test_split(X_porosity_only,y,random_state=1)

model = MyRidge()

model.fit(Xtrain[:,None],ytrain)

y_porosity_only = model.predict(X_porosity_only[:,None]);

mapper = DataFrameMapper([('WELL',LabelBinarizer()),('FM',LabelBinarizer())],default=Imputer(strategy='mean'))
X_all = mapper.fit_transform(df.drop('KLH',axis=1))

model = make_pipeline(StandardScaler(),MyRidge())

Xtrain, Xtest, ytrain, ytest = train_test_split(X_all,y,random_state=1)
model.fit(Xtrain,ytrain)
y_all = model.predict(X_all)

#get_ipython().run_line_magic('matplotlib', 'inline')

x_perfect = np.linspace(0.05,50000)
y_perfect = x_perfect

plt.loglog(y,y_porosity_only,'r+',y,y_all,'bo',x_perfect,y_perfect,'k-')
plt.grid()
plt.ylabel("Model Perm [md]")
plt.xlabel("Actual Perm [md]")

plt.xlim([1e-2,1e6])
plt.ylim([1e-2,1e6])

plt.legend(["Fit w/ porosity only","Fit w/ full data set","Perfect fit"])

plt.show()
