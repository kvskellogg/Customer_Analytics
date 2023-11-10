# Customer_Analytics


---
title: "Tuango:
Targeting Analysis for Mobile App Push Messaging"

author: 
   - "Shiv Viswanathan"               # Shiv Viswanathan
   - "Section 81"         # Replace with section number
output: pdf_document
---

# Preliminaries

IMPORTANT: Before starting this assignment you must go through logistic regression handout we covered in class to correctly install the `kelloggmktg482` package.


```{r, include=FALSE}
### Determine notebook defaults:
#knitr::opts_chunk$set(echo=TRUE,      # Print all the code in all the chunks
                     # warning=FALSE,  # Don't print warning statements
                      #message=FALSE,  # Don't print other R output messages
                      #comment=NA)     # Helps produce prettier output
```



```{r, echo=FALSE, message = FALSE, warning = FALSE}
### Load packages:

##rm(list = ls())

library(gmodels)
library(modelr)
library(janitor)
library(haven)
library(readxl)
library(knitr)
library(psych)
library(statar)
library(tidyverse)
library(kelloggmktg482)
```


```{r}
#install.packages("remotes")
library(remotes)
#install_version("vip", version="0.3.2", upgrade="never")
#devtools::install_github("blakemcshane/kelloggmktg482", upgrade = "never", force = TRUE)

```


## Read in the data:
```{r}
# use load("filename.Rdata") for .Rdata files

load("tuango.Rdata")

```


```{r}

#Mutute the categoriacal variable to type factor
library(dplyr)

tuango <- tuango %>%
  mutate(sex = factor(sex),
          messages = factor(messages))
  
```


# Assignment questions and answers

## Part 1  (18 points)

# 1. Estimate a logistic regression model using “buyer” as the dependent variable using all relevant predictor variables, namely age, sex, messages, recency, frequency, monetary, and music.

```{r}

## Logistic Regression 
lr <- glm (buyer~age+sex+messages+recency+frequency+monetary+music, family = binomial, data =tuango)


```




#2. Use summary(…) to examine the coefficient estimates, varimpplot(…) to assess variable importance, and pardepplot(…) the effect of each predictor. What variables seem to be practically important? Describe their effects.

```{r}
summary(lr)
```

```{r}
confint(lr)
```


###varimpplot(…) to assess variable importance

```{r}
varimpplot(lr, target="buyer")
```

###pardepplot(…) the effect of each predictor

```{r}
perc.buyer.overall <- mean(tuango$buyer==1)

```


## pardepplot for messages

```{r}
pardepplot(lr, pred.var = "messages", data=tuango, hline = perc.buyer.overall, ylim = c(0,0.2))

```

##  pardepplot for monetary

```{r}
pardepplot(lr, pred.var = "monetary", data=tuango, hline = perc.buyer.overall)

```

##  pardepplot for music

```{r}
pardepplot(lr, pred.var = "music", data=tuango, hline = perc.buyer.overall, ylim = c(0,0.2))

```


##  pardepplot for sex

```{r}

pardepplot(lr, pred.var = "sex", data=tuango, hline = perc.buyer.overall, ylim = c(0,0.25))

```

##  pardepplot for age

```{r}


pardepplot(lr, pred.var = "age", data=tuango, hline = perc.buyer.overall, ylim = c(0,0.25))

```

##  pardepplot for frequency

```{r}
pardepplot(lr, pred.var = "frequency", data=tuango, hline = perc.buyer.overall, ylim = c(0,0.25))
```


##  pardepplot for recency

```{r}
pardepplot(lr, pred.var = "recency", data=tuango, hline = perc.buyer.overall, ylim = c(0,0.25))

```








# What variables seem to be practically important? Describe their effects.

###age : Age <45 seem to have higher probability above the buyer mean to buy karaoke packages

###sexM : Females have higher probability to be buying karaoke packages

### messagesON : Message being ON on mobile have higher probabiity of purchasing karaoke package

### recency : negatively trending, ie. people who purchased a deal recently have a higher probability of buying the karaoke package than someone who did not buy a package for a few months. 


### frequency : more than 3 purchases

### monetary : 0-2000$ increasing probability trend -- above 2000% definitely purchase karaoke package

### music : people who purchased music before, tend to buy karaoke deals  - linear 



# 3. Add the predicted values from the logistic regression to the “tuango” dataframe. Compare the average of the predicted values to the overall response rate (i.e., percentage of buyers). What do you notice? Why is that?



```{r}

##Add the predicted values from the logistic regression to the “tuango” dataframe.

tuango <- tuango %>%
  mutate(pred_prob = predict(lr, type="response"))

```


```{r}

tuango %>%
  select(userid, pred_prob) %>%
  head()

mean.pred.prob = mean(tuango$pred_prob)

##average of the predicted values and percentage of buyers
tuango %>% summarise(mean.pred.prob, perc.buyer.overall)


```

## The average of the predicted value is the same as the overall percentage response rate i.e  0.11952 


#4. Assign each customer to a decile based on his or her predicted probability of purchase. Assign those with the highest predicted probability to decile 1 and those with the lowest predicted probability to decile 10. Generate a table with ten rows and four columns with columns being the decile number; the number of customers in that decile, the number of buyers in that decile, and the response rate in that decile.

```{r}

tuango <- tuango %>%
  mutate(decile = ntile(pred_prob, 10))

tuango %>%
  select(userid, pred_prob, decile) %>%
  head(10)

```


```{r}
# Create a summary table with decile, number of customers, number of buyers, and response rate
summary_table <- tuango %>%
  group_by(decile) %>%
  summarise(
    Count = n(),
    Buyers = sum(buyer),
    ResponseRate = mean(buyer)
  ) %>%
  arrange(desc(decile))  # Sort the table by decile in descending order

print(summary_table)

```


## 5. Use the table created in the prior question to make a barchart that plots the response rate in each decile defined. Comment on your findings.

```{r}

library(ggplot2)
library(scales)

# Create a bar chart
bar_chart <- ggplot(summary_table, aes(x = decile, y = ResponseRate)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Response Rate by Decile",
    x = "Decile",
    y = "Response Rate"
  )

# Display the bar chart
print(bar_chart)





```

#6.Estimate a linear regression model using “ordersize” as the dependent variable using all relevant predictor variables, namely age, sex, messages, recency, frequency, monetary, and music. Estimate this regression using only those customers who purchased the Karaoke deal. Hint: Use filter(buyer==1) in the dplyr package to create a dataframe limited to only those customers who purchased the Karaoke deal and use this dataframe as an input to the linear regression. Consider: why are we limiting this analysis only to these customers?

```{r}

## Linear Regression 

load("tuango.Rdata")

#Mutute the categoriacal variable to type factor

tuango <- tuango %>%
  mutate(sex = factor(sex),
          messages = factor(messages))

customer_purchase_karoake <- tuango %>%
  filter(buyer==1)

```


```{r}
lr_buyer <- lm (ordersize~age+sex+messages+recency+frequency+monetary+music, data = customer_purchase_karoake)
```



#7. Use summary(…) to examine the coefficient estimates, varimpplot(…) to assess variable importance, and pardepplot(…) the effect of each predictor. What variables seem to be practically important? Describe their effects. What does this suggest about our ability to predict the order size of those customers who responded to the deal?


```{r}
summary(lr_buyer)
```

```{r}

varimpplot(lr_buyer, target="ordersize")

```


# What variables seem to be practically important? Describe their effects.

### frequency  - frequency seems to be the biggest variable that has an effect on the order size. 

### recency : recency comes second -- this is different from the overall data set -- this suggests that those who purchased recently have a smaller order size. 





```{r}
confint(lr_buyer)

```
```{r}

##Add the predicted values from the logistic regression to the “tuango” dataframe.

customer_purchase_karoake <- customer_purchase_karoake %>%
  mutate(pred_prob = predict(lr_buyer, type="response"))

```


```{r}

customer_purchase_karoake %>%
  select(userid, pred_prob) %>%
  head()

##average of the predicted values and percentage of buyers
customer_purchase_karoake %>% summarise(mean(customer_purchase_karoake$pred_prob), mean(customer_purchase_karoake$ordersize))



```

###The (mean(customer_purchase_karoake$pred_prob), mean(customer_purchase_karoake$ordersize)) are the same! 4.011



```{r}


pardepplot(lr_buyer, pred.var = "frequency", data=customer_purchase_karoake, hline = perc.buyer.overall, ylim = c(3.5,5))
pardepplot(lr_buyer, pred.var = "music", data=customer_purchase_karoake, hline = perc.buyer.overall, ylim = c(3.5,5))
pardepplot(lr_buyer, pred.var = "recency", data=customer_purchase_karoake, hline = perc.buyer.overall,ylim = c(3.5,5))
pardepplot(lr_buyer, pred.var = "monetary", data=customer_purchase_karoake, hline = perc.buyer.overall,ylim = c(3.5,5))
pardepplot(lr_buyer, pred.var = "sex", data=customer_purchase_karoake, hline = perc.buyer.overall,ylim = c(3.5,5))
pardepplot(lr_buyer, pred.var = "age", data=customer_purchase_karoake, hline = perc.buyer.overall,ylim = c(3.5,5))
pardepplot(lr_buyer, pred.var = "messages", data=customer_purchase_karoake, hline = perc.buyer.overall,ylim = c(3.5,5))


```




## Part II: Profitability Analysis (8 points)

##The following questions will ask you to use your data to forecast the profit and the return on marketing expenditures of offering the deal to the remaining 397,252 potential customers (i.e. 418,160 – 20,908).

## To calculate profit assume the following:
##Price per 30-minute session = 49 RMB
##Marginal cost to offer a deal is = 11 RMB
##Fee on each deal sold = 50% (of sales revenues)

## 1. What is the breakeven response rate?

```{r}

price.per.30.min = 49
marginal.cost.deal = 11
perc.fees = 0.5

profit = price.per.30.min * perc.fees

breakeven.response.rate =  marginal.cost.deal/ profit

print("The Breakeven response rate is")
print(breakeven.response.rate)

```


## 2. What is the projected profit in RMB if you offer the deal to all 397,252 remaining customers?

```{r}

remaining.customers = 397252



projected.profit = perc.buyer.overall * remaining.customers * profit


print("Projected profit in RMB based on percentage of buyer response is")
print(projected.profit)




```

## 3. What is the projected profit if you offer the deal to only to those of the 397,252 remaining customers that targeting model deems profitable?

```{r}


## Its the same as above, as the above, since the percentage of buyer overall is the same as the model predicted probability


remaining.customers = 397252

projected.profit = mean.pred.prob * remaining.customers * profit

print("Projected profit in RMB based on model predicts is")
print(projected.profit)


```


## Part III: Build Targeting Models Redux (4 points)

## 1. Estimate a neural network model using “buyer” as the dependent variable using the same predictor variables as the logistic regression estimated in Part I. Set the size tuning parameter to 5 and the decay tuning parameter to 0.1.

```{r}

#install.packages("nnet")
library(nnet)

nn <- nnet(buyer~age+sex+messages+recency+frequency+monetary+music, data=tuango, size=5, decay=0.1, maxit=1000)




```

## 2. Add the predicted values from the neural network to the “tuango” dataframe and repeat the profitability analysis in Part II but using these predicted values rather than those from the logistic regression. Comment on why your profits are similar to or different from those found in Part II.

```{r}


tuango <- tuango %>%
mutate(pred_nn = predict(nn, type="raw")[,1])


tuango %>%
  select(userid, pred_nn) %>%
  head()

mean.pred.nn = mean(tuango$pred_nn)

##average of the predicted values and percentage of buyers
tuango %>% summarise(mean.pred.nn, perc.buyer.overall)




```

### the predicted probability is slightly higher here than the mean buyer percentage. 


```{r}

remaining.customers = 397252

projected.profit = mean.pred.nn * remaining.customers * profit

print("Projected profit in RMB based on model predicts is")
print(projected.profit)
```



```{r}
library(dplyr)

pardepplot(nn, pred.var = "frequency", data=tuango, hline = perc.buyer.overall)
pardepplot(nn, pred.var = "music", data=tuango, hline = perc.buyer.overall)
pardepplot(nn, pred.var = "recency", data=tuango, hline = perc.buyer.overall)
pardepplot(nn, pred.var = "monetary", data=tuango, hline = perc.buyer.overall)
pardepplot(nn, pred.var = "sex", data=tuango, hline = perc.buyer.overall)
pardepplot(nn, pred.var = "age", data=tuango, hline = perc.buyer.overall)
pardepplot(nn, pred.var = "messages", data=tuango, hline = perc.buyer.overall)
```

```{r}
pardepplot(nn, c("sex","music"), data=tuango, hline = perc.buyer.overall)
```

### the nn model based targeting provides slightly more profit -- in this case a 1.4% increase in profit



### Disclaimer -- ChatGPT is  used for debugging purposes only. All the code here is done by me and I take full responsibility. 








# <TITLE>

The goal of this exercise is to explore NYC data, clean it up, and model it to build data vizualizations to get a few key insights about the data. I have explained the results on my blog on medium. Click link to access it - https://com/exploratory-data-analysis-of-airbnb-nyc-ed1998da1ee4

## Functionality
As I go through this task, I try to answer 3 specific questions
1. Which location has the highest number of listings
2. What room types are most common and their avg price in each neighbourhood
3. What are the common words used to describe the listings

## Scope
This project was created to explore data viz libraries in python. I have limited myself to NYC data.

I have used the following sources as a guide to build this project

## Output
1. Boroughs, Neighbourhoods and Locations where most listings are located
   + Most Airbnb listings are located in Manhattana and in parts of Brooklyn closer to Manhattan.

2. Common listings by room type and their avg price by borough
   + The most common roomtype is an Entire home/apt, also the most expensive. The avg prices in Manhattan are greater than rest of the Boroughs

3. Commonly used words to describe the listings.
   + Brooklyn, Manhattan, Private, Heart, Cozy are a few words used commonly to describe the listings
   + Private, spacious, east, beautiful, large are the top 5 adjectives used to describe the listings

Besides the above, I also tried to answer most commonly reviewed listings and hosts with the most number of listings

## Prerequisites
1. import the following libraries using pip
  + numpy
  + pandas
  + matplotlib
  + seaborn
  + folium
  + nltk
 
2. I have used python 3.7.4
I have the above libraries and python installed in an Anaconda environment.

## Installation
Fork this repository under your own control, then clone or download the resulting repository onto your computer. Then navigate there from the command line using the command line below:

```sh
cd <Your Path>/RESTAURANT-ANALYSIS-PY
```

> NOTE: subsequent usage and testing commands assume you are running them from the repository's root directory.

Use Anaconda to create and activate a new virtual environment, perhaps called "dataviz":

```sh
conda create -n dataviz python=3.7.3 # (first time only)
conda activate yelp
```

From inside the virtual environment, install package dependencies by running the below command.

```sh
pip install -r requirements.txt
```

## Sources
I would like to thanks the authors of the below blog posts.
1. https://towardsdatascience.com/an-extensive-guide-to-exploratory-data-analysis-ddd99a03199e
2. https://towardsdatascience.com/data-exploration-on-airbnb-singapore-01-40698c54cac3

## License 
LICENSE.md

## Contact
Any feedback can be emailed to 
