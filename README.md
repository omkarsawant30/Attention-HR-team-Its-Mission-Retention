# HR---Employee-Resignation-Predictions
To predict which valuable employees will leave the organization next based on various factors.

Why are our best and most experienced employees leaving prematurely? Use this database and try to predict which valuable employees will leave next. Fields in the dataset include:

Employee satisfaction level

Last evaluation

Number of projects

Average monthly hours

Time spent at the company

Whether they have had a work accident

Whether they have had a promotion in the last 5 years

Department

Salary

Whether the employee has left

We have given you two datasets , hr_train.csv and hr_test.csv . You need to use data hr_train to build predictive model for response variable ‘left’. hr_test data contains all other factors except “left”, you need to predict that using the model that you developed and submit your predicted values in a csv file.

If you are using decision trees or random forest here, probability scores can be calculated as:

score=predict(rf_model,newdata= testdata, type="prob")[,1]

score=predict(tree_model,newdata= testdata, type=‘vector’)[,1]
