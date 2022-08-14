import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
import statsmodels.api as sm

from sklearn.model_selection import train_test_split

data = pd.read_csv("tips.csv")

# I will take you through how to train a machine learning model for the task of waiter tips prediction.

# ################################## Waiter Tips Prediction Model########################################## #


# Before training a waiter tips prediction model, I will do some data transformation by transforming the categorical
# values into numerical values:

data["sex"] = data["sex"].map({"Female": 0, "Male": 1})
data["smoker"] = data["smoker"].map({"No": 0, "Yes": 1})
data["day"] = data["day"].map({"Thur": 0, "Fri": 1, "Sat": 2, "Sun": 3})
data["time"] = data["time"].map({"Lunch": 0, "Dinner": 1})
#print(data.head())
#print(data.tail())
#print(str(data))
# Now I will split the data into training and test sets:

x = np.array(data[["total_bill", "sex", "smoker", "day",
                   "time", "size"]])
y = np.array(data["tip"])

xtrain, xtest, ytrain, ytest = train_test_split(x, y,
                                                test_size=0.2,
                                                random_state=42)

# Now below is how we can train a machine learning model for the task of waiter tips prediction using Python:
from sklearn.linear_model import LinearRegression

model = LinearRegression()
model.fit(xtrain, ytrain)

# Now let’s test the performance of this model by giving inputs to this model according to the features that we have
# used to train this model:

# features = [[total_bill, "sex", "smoker", "day", "time", "size"]]
features = np.array([[2404.40, 1, 0, 4, 1, 1]])
print(model.predict(features))


# ###########################################################################################################################


# Summary So this is how you can predict waiter tips with machine learning using Python. Waiter Tips analysis is one
# of the popular data science case studies where we need to predict the tips given to a waiter for serving the food
# in a restaurant. I hope you liked this article on waiter tips prediction with machine learning using Python.

# ###########################################################################################################################
