import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
import statsmodels.api as sm

data = pd.read_csv("tips.csv")
print(data.head())

#############################################################################

# Let’s have a look at the tips given to the waiters according to:
#
# the total bill paid
# number of people at a table
# and the day of the week:

figure = px.scatter(data_frame=data, x="total_bill",
                    y="tip", size="size", color="day", trendline="ols")
figure.show()

###############################################################################


# now let’s have a look at the tips given to the waiters according to:

# the total bill paid
# the number of people at a table
# and the gender of the person paying the bill:

figure = px.scatter(data_frame=data, x="total_bill",
                    y="tip", size="size", color="sex", trendline="ols")
figure.show()

###############################################################################

# Now let’s have a look at the tips given to the waiters according to:

# the total bill paid
# the number of people at a table
# and the time of the meal:

figure = px.scatter(data_frame=data, x="total_bill",
                    y="tip", size="size", color="time", trendline="ols")
figure.show()

###############################################################################

# Now let’s see the tips given to the waiters according to the days to find out which day the most tips are given to
# the waiters:

figure = px.pie(data,
                values='tip',
                names='day', hole=0.5)
figure.show()

# Note: According to the visualization above, on Saturdays, most tips are given to the waiters


###############################################################################

# Now let’s look at the number of tips given to waiters by gender of the person paying the bill to see who tips
# waiters the most:

figure = px.pie(data,
                values='tip',
                names='sex', hole=0.5)
figure.show()

# Note: According to the visualization above, most tips are given by men.

###############################################################################

#  Now let’s see if a smoker tips more or a non-smoker:

figure = px.pie(data,
                values='tip',
                names='smoker', hole=0.5)
figure.show()

# Note: According to the visualization above, non-smoker tips waiters more than smokers.

###############################################################################


# Now let’s see if most tips are given during lunch or dinner:

figure = px.pie(data,
                values='tip',
                names='time', hole=0.5)
figure.show()

# Note:According to the visualization above, a waiter is tipped more during dinner.


# ##############################################################################################

# So this is how we can analyze all the factors affecting waiter tips. Now in the section below,

# ################################################################################################

