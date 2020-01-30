# Travel-insurance-prediction

Author: Hungchun Lin, Siyang Liu, Chen Chen
Date: December 2019

## Problem statement
### Background
As the living quality improved, nowadays many people consider traveling as their first choice
to spend their spare time. That should be an unforgettable experience traveling with family
and friends, but we often hear bad news about property loss, injury or even death during
traveling. Although being careful is important to avoid danger from traveling, choosing a
proper destination, safe transportation, reliable agency could also be a good way to
protect ourselves.

So what factors might cause the accidents during traveling?

### Proposed Solution
Apply Bayesian analysis on travel insurance dataset.  

## Dataset
[Travel insurance]
(https://www.kaggle.com/mhdzahier/travel-insurance) 
Dataset from Kaggle, Provided by a third-party insurance servicing company based in Singapore.

## Data preprocessing
1. Check for null values: To many empty value in *Gender*, drop column directly

2. Drop outlier for continuous variables

3. Label encoding: For the taget "Claim", we do label encoding, "Yes" to 1, "No" to 0.

4. For the target "Claim": Two levels, 62399 (98.5%) “No”, 927 (1.5%) “Yes”. They are imbalanced, so we do upsampling but the performance become worse.  

5. For the caregorical features: “Agency”: 15 levels, “Destination”: 147 levels, “Product name”: 26 levels.
  Too many levels in "Agencys", "Destination" and "Product name", then it will generate too many features after one-hot  
  encoding.  
  “Destination”: 147 levels → Top 10 levels + 1 “Others” level  
  ”Agency”: 15 levels → Keep  
  “Product”: Due to the correlation with “Agency” → Drop column  

## Feature Selection
As there are too many categorical variable in our dataset, consider the computing power, we will use lasso to do the feature selection and select the top 15 important features.

![Image description](featureselection-1)


## Run the Robust Logistic regression
We use JAGS to run the robust logistic regression with target "Claim". Firstly, we need to do one-hot encoding on the categorical features, and select out the top 15 columns selected by Lasso. Then we split the data into training(90%) and testing(10%) dataset. We also check the correlation between features, we can see only AgencyC2B and DestinationSingapore has a little bit higher correlation, but due to actual situation, AgencyC2B client's destination are all to Singapore, but Client travel to Singapore are not all from AgencyC2B, we decide to keep this two features.


Finally, we generate the Robust Logistic regression using 4 chains with 3750 iterations.

From the distribution of posterior, we can see, there are 7 features beta are not significant, in other words, the HDI of the distribution contains "0", so these features may have beta equals zero, which means these features may not significant when interpreting the target "Claim". 
From each beta's diagnosis, we can see, beta0 and beta5 are converge very good and have good Effective sample size, which mean these two betas are very stable. 
When looking at beta1 and beta11, we can see these two betas are converge very bad, the trace plots are sticky, the autocorrelation are very high, and the ESS are very low. This should be because of the higher correlation of the two features.
## Prediction
Finally, we want to check the accuracy of the MCMC result. We use the 8 significant variables in MCMC to train the model and compare the result with guessing parameter and without guessing parameter. When we train the model in order to imporve the accuracy, we also do the oversampling to balance our target. 


From the result we can see, the two results are almost the same, as we know when alpha equals to 0, the model is non-robust, just the logistic regression. when alpha equals to 1, the model is a horizontal line with y intercept 1/2. This means our model is very very close to a non-robust model, this should because the value of our guessing parameter is too small, so it is almost has no influence on our model. 

In conclusion, non-robust Logistic regression is good enough to use on our project. In MCMC only 8 of 15 variables are significant. When there are high correlation variables in a dataset, this will impact the convergence of samples in MCMC.

