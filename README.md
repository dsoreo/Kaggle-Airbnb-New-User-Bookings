# Kaggle-Airbnb-New-User-Bookings

## 40th place solution - https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings
To run the model run config.R. This file executes the code as follows

1.	Calls format_sessions.R. This file
    a. Converts the data into one hot encoding format.
    b. Uses session data to create new variables used in training.
2.	Calls multi_prob.R to create 30 models. For each model an output file with probability of prediction for each country is written as output file.
    a.	Model m1 with 5 seeds (All years training data, ages imputed with SQL grouping)
    b.	Model m2 with 5 seeds (All years training data, xgb imputed age)
    c.	Model m3 with 5 seeds (2014 training data, SQL imputed age)
    d.	Model m4 with 5 seeds (2014 training data, xgb imputed age)
    e.	Model m5 with 5 seeds (2014 April-June training data, SQL imputed age)
    f.	Model m6 with 5 seeds (2014 April-June training data, XGB imputed age)
3.	Calls Ensemble.R
    a.	This file creates an average of probabilities returned by 30 models above. This file writes submission_file.csv that can be used for submission on Kaggle.

##Required Libraries
xgboost
sqldf
caret
readr

