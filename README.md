<a name="BackToTop"></a>

# Ensemble_Classification_of_Employee_Attrition

>The goal of this project was to predict empoloyees leaving a particular company due to retirement or resignation. I first normalized, standardized, or dummy coded necessary features. Then, I used the ROSE package to create synthetic data to balance the "yes" and "no" attrition levels. My model consisted of three meta-algorithms - bagging, boosting, and stacking, which achieved an accuracy of 87.6%. Afterwards, I predicted incomes using linear regression, and explored other areas of the dataset, including the differences in incomes by gender.


---


## Table of Contents
- [Predicting Attrition Summary](#Predicting_Attrition)
- [More Findings](#More_Findings)
- [Linear Regression for Incomes](#Linear_Regression)
- [References and Resources](#References_and_Resources)


---


<a name="Predicting_Attrition"></a>  

## Predicting Attrition Summary
The dataset consisted of 36 total variables, with a mix of both quantitative and qualitative types. Some initial findings included: no null values, several features of all one level, and correlations up to .95 (monthly income and job level). Job roles produced large differences in attrition rates with the highest being 45% from the sales reps, and lowest at 2% from the director roles.  

730 rows consisted of "no" for attrition and 140 rows of "yes". When I first ran my model, the accuracy for just the "yes" attrition level was less than 50%. I then integrated the ROSE package which allowed me to oversample the "yes" class by generating synthetic data based off feature space similarities. ROSE uses a smoothed-bootstrap approach.  

For my ensemble model, I used hard voting to make the final choice between my three meta-algorithms - bagging, boosting, and stacking. The first algorithm used random forest, followed by C5.0. The third algorithm, stacking, used naive_bayes, knn, rpart, svmRadial, and glmnet from the Caret package. My ensemble model produced an accuracy of 87.6%, with a 88.2% sensitivity and 85% specificity. Some of the top features included JobIsaDirector, JobIsSalesRep, Divorce, JobInvolvementScore, and Overtime. 

This plot illustrates some key relationships regarding attrition and job roles  

![Job Roles and Attrition](https://github.com/MichaelMazel/Ensemble_Classification_of_Employee_Attrition/blob/main/Employee_Attrition_Prediction_files/figure-gfm/unnamed-chunk-55-1.png)


[Back to Top](#BackToTop)


---

<a name="More_Findings"></a>

## More Findings 
Leadership roles were comprised of 91 females and 98 males, with monthly incomes being 11,993 and 12,672 dollars, respectively. There was not enough evidence to suggest male incomes were significantly different than female incomes (p-value .37). I used a two sample t-test, and corresponding graphs can be found below:  

![Female Incomes](https://github.com/MichaelMazel/Ensemble_Classification_of_Employee_Attrition/blob/main/Employee_Attrition_Prediction_files/figure-gfm/unnamed-chunk-53-3.png)  
![Male Incomes](https://github.com/MichaelMazel/Ensemble_Classification_of_Employee_Attrition/blob/main/Employee_Attrition_Prediction_files/figure-gfm/unnamed-chunk-53-4.png)   


[Back to Top](#BackToTop)


---

<a name="Linear_Regression"></a>

## Linear Regression for Incomes Summary

The goal here was to predict monthly incomes with linear regression. I used just the quantitative variables, and to meet the assumptions, I log transformed several features, including the response variable. After the assumptions were improved, I used LASSO for variable selection.  Job Level, Total Working Years, and Job Involvement were selected for the model. These achieved a .873 adjusted r-squared, or in other words, 87.3% of the variation in employee monthly incomes could be explained by these three features.
    

[Back to Top](#BackToTop)


---


<a name="References_and_Resources"></a>

## References and Resources  

Ensembling methods: https://github.com/kmutya/Ensemble-Learning-in-R   
ROSE: https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/   
LASSO: https://rstatisticsblog.com/data-science-in-action/machine-learning/lasso-regression/   

##### Technologies
R Studio  
R version 4.0.3

[Back to Top](#BackToTop)