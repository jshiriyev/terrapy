import numpy as np

from geostatistics import csvreader
from geostatistics import heterogeneity
from geostatistics import correlation

class sample():
    pass

sample = csvreader(sample,"03_univariate&bivariate_data.csv")

print(heterogeneity.dykstraParson(sample))
print(correlation(np.log(sample.rfactor),sample.porosity))
print(correlation(np.log(sample.permeability),sample.porosity))
