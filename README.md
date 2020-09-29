# Kickstarter Project

## Rémi Fauve
## 2020-06-17

### Context
Crowdfunding is a common practice nowadays, but the first website to engage in this business model emerged
less than 20 years ago (ArtistShare in 2001). More crowdfunding sites appeared on the web, the most
famous ones being the American companies IndieGoGo (2008), Kickstarter (2009) or GoFundMe (2010).
Crowdfunding grew quickly into a fully mature form of alternative finance, cumulating over 34 billion USD
raised worldwide in 2015. The popular success of crowdfunding is due to the simplicity for both projects’
creators and backers, in contrast with the administrative burden of traditional investment strategies.

However, the drawback of becoming a common practice is that the supply (the backers) cannot match the
demand (the projects’ creators): not every submitted project can be funded. Projects’ creators being therefore
in competition, they now need to optimize the marketing side of their project to increase their chance of
ending funded. This is where data science can provide quick tips, if not a model to forecast the pledged
money, or even better, the optimal settings for a maximal gain.

In this study, data on all the projects registered on the Kickstarter plateform, up to 2018, is imported and
explored to extract general insights on the success rate or the amount of pledged money depending on the
different variables in the data. Then, different machine learning models are trained and their performance on
the prediction of the pledged money per project are compared.

### Environment and libraries
This study was done as part of the HarvardX online course 
[“Data Science: Capstone” (PH125.9x)](https://www.edx.org/course/data-science-capstone).
The data set used for this project was created by Mickaël Mouillé (License CC BY-BC-SA 4.0), and is
available on Kaggle at this 
[link](https://www.kaggle.com/kemical/kickstarter-projects?select=ks-projects-201801.csv).

The code presented here was executed in the RStudio IDE (Version 1.2.5042) with R version 3.6.1 (2019-07-05).

The required libraries are: 

tidyverse, caret, lubridate, rpart, rpart.plot, kableExtra, doParallel, gridExtra

### Main elements
* Data's integrity had to be checked before cleaning.

* Thorough data exploration revealed interesting patterns, and gave a clearer understanding of this *alternative investment strategy* (**less than 40% of all projects in the last 10 years got funded, and around 14% received no money at all**). 

* The prediction attempts were focused on Residuals' Explanation (evaluated with both RMSE (Root Mean Square Error) and Kolmogorov-Smirnov test value), using algorithms such as Explicit Biaises and Regression Trees.
* Multiple biaises model drastically improved the RMSE compared to the best single biais model.
* A single Regression Tree model is almost as good as the best multiple biaises, but it take much longer to train (30 min on 6 threads).
* The Ensemble model of both the best multiple biaises model and the Regression Tree gave no significant improvement.

* The results on the final test set showed no sign of overfitting.
* Final RMSE : 1.27115 (Ensemble model)

* The mean absolute error with this model is around 10 USD, which tells how precise the predictions of this model is (median pledged money: ~800 USD).
