*FACEBOOK METRIC DATASET PROJECT*

Here we analyzed the UCI facebook metrics dataset of a cosmetic company , Objective is to increase the number of lifetime engaged users 
so that it is beneficial to company's future product plan and overall developement to increse its profit. 



*OUR PROJECT STRUCTURE*

1. Introduction and Data description
2. Objective
3. Data pre-processing
4. EDA(exploratory Data analysis)
5. Changing categorical variables into indicators and scaling
6. Modelling
     
    (i)  Leverage and influencial point removal
    
    (ii) Normality Checking and removal
    
   (iii) Heteroscedasticity Checking and removal
   
   (iv)  Multicollinearity Checking and removal
     
            Multicollinearity removal using diffrent approach
            * Principal component regression
            * Variable elimination
            * Partial least square regression
            * Lasso
            
 
 7. Results 
 8. References
 
 
*RESULTS/Conclusions*
 
 Finally we got three models
 
 (i)firstly using principal component of all variables withMAPE 11.8%
 
 (ii)secondly using partial least square with MAPE 6.17%
 
 (iii)thirdly using Lasso with MAPE 5.71%.
 
 Though the first model’s efficiency is not bad, it took all the regressors (48 regressors arehard to interpret at a time ) into account and then used first 38 components (which explain97.78% of total variability ). This will take too much time and cost for larger number ofobservations and from this model we can’t specify the regressors which have severe effectson the response (Lifetime.engaged.users) .
 
 Second model, with better efficiency thanfirst model, also took all the regressors into account (which will again alert a statisticianregarding time and cost) but then using only first 10 components (which explain 99.37% oftotal variability) it predicted very well. Also, here is no scope for choosing best regressors.
 
 Thirdly with Lasso we overcame all the difficulties. It identified 17 regressors and withthe help of these regressors it predicted very much well than first two models. Clearly,here we can check the effects of these selected 17 regressors and time, cost will be muchless here. So, regarding the response Lifetime.engaged.users few factors are there to be noticed. 
 
 Those are -
 
               (i) Posts in April, May, July induced lesser number of users where Feb, Nov, Dec ensuredhigher number of users. So, the company should post in year-end
                    or in the beginning of the year.
 
              (ii) More users are engaged with post type ‘Photo’
 
             (iii) Inspirational posts gathered more users
 
              (iv) More users can be engaged with the posts in the night and noon
 
               (v) Weekdays have not such heavy effects on the users
 
              (vi) No effects of paid or unpaid posts
 
             (vii) As ‘Total.Reach’, ‘like’, ‘comment’, ‘share’, ‘Consumer’, ‘People.engaged.like’ aredirectly related to clicking or seeing any post by the users,
                    more amount of these variablescertainly will imply more the engaged users.
 

 
*SOURCE/CITATION*
1.  [https://archive.ics.uci.edu/ml/datasets/Facebook+metrics]
(Moro et al., 2016) Moro, S., Rita, P., & Vala, B. (2016). Predicting social media performance metrics and evaluation of the impact on brand building: A data mining approach. Journal of Business Research, 69(9), 3341-3351.
2. James, Gareth, Daniela Witten, Trevor Hastie, and Robert Tibshirani. An Introduction to Statistical Learning: with Applications in R. Springer: 2017.
3. Harrison, D. and D. L. Rubinfeld (1978). Hedonic prices and the demand for clean air. Journal of Environmental Economics and Management, 5, 81-102.
4. introduction to Linear Regression Analysis by Douglas C Montgomery, Elizabeth APeck, G. Geoffrey Vining.
 
 
