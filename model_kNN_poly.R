---
title: "HW4 Problem 1"
author: "An-Chi Ho, Sruti Devendran, & Miriam Nielsen"
date: "3/24/2018"
output:
  rmdformats::html_docco:
    highlight: kate
    toc: true
    toc_depth: 3
---
```{r knitr_init, echo=FALSE, cache=FALSE}
# DO NOT edit this block
knitr::opts_chunk$set(
  cache=TRUE,
  comment=NA,
  message=FALSE,
  warning=FALSE,
  fig.width=12,
  fig.height=7
)
```

Package Installation:
```{r}
if(!require(pacman)) install.packages('pacman')
pacman::p_load(dplyr, ggplot2, mapproj, readr, ggthemes, viridis, reshape2)
```

Read in the data:
```{r}
cty <- read_csv('cty.csv') %>%
  select(lon, lat, Uppm)
glimpse(cty)
```


**Question 1. Data Exploration**

(a) Make a plot of Radon concentrations (variable name Uppm) in the United States using the Albers projection. Scale the opacity and color of points to Radon concentrations.
```{r}
ggplot(cty, aes(x=lon, y=lat)) + 
  geom_point(aes(size=Uppm, color=Uppm)) +
  scale_size_continuous(range=c(0.1, 1)) +
  coord_map(projection="albers", parameters = c(25, 50)) +
  scale_color_viridis() +  
  theme_map() + 
  ggtitle("Radon Concentrations across the United States")
```

In this map, we can see that the distribution of sampling is not evenly spread across the country. There are far more samples in the eastern US than western US. This makes visually comparing the values of the two sides of the country difficult. There are several areas (like near the Appalacian Mountain Range, Illinois or the Southwest) that have high Radon concentrations, while other areas (like Northern Wisconsin and Northern Washington) have very low concentrations. 


(b) Summarize the Radon data. Use a qqplot - does it seem to be normally distributed?
```{r}
qqnorm(cty$Uppm)
qqline(cty$Uppm)
```

The Radon data does not seem to be normally distributed - note the divergence from linearity at both ends in the Normal Q-Q Plot. There are also zero values making the distribution even worse.


(c) The radon data has a number of 0 values - likely these are not actually zero but just locations where nothing was reported. Use dplyr :: filter () to create a new data frame with the zero values removed. Now summarize this data. Is it normally distributed?
```{r}
cty_no0 <-cty %>%
 filter(Uppm!=0) 

qqnorm(cty_no0$Uppm) 
qqline(cty_no0$Uppm)

```

While this is slightly better (there are no longer any zero values), this shows the data is still not normally distributed as the low and high ends of the Q-Q plot are quite far from linear.  


(d) Let’s consider taking a log transform. Use dplyr :: mutate() to create a new variable called log_Uppm in your new data frame.
```{r}
ctyNew <- cty_no0 %>%
  mutate(log_Uppm = log(Uppm))

```
 
 
(e) One test (out of many tests) of normality is the Kolmogorov-Smirnov test. The command ks.test(x, ’pnorm’, xm, xs) returns the p-value for a null hypothesis that the data comes from a normal distribution with mean xm and standard deviation xs. Is radon or log radon more approximately normal? Use this for your future analysis.
```{r}
ks.test(ctyNew$Uppm,'pnorm',mean(ctyNew$Uppm), sd(ctyNew$Uppm))
ks.test(ctyNew$log_Uppm,'pnorm',mean(ctyNew$log_Uppm), sd(ctyNew$log_Uppm))

qqnorm(ctyNew$log_Uppm) 
qqline(ctyNew$log_Uppm)

hist(ctyNew$log_Uppm)
hist(ctyNew$Uppm)
```
K-S test is strict that it easily rejects the null hypothesis. The result shows that both radon and log(radon) pass the hypothesis test that these two are not normal-distributed, and log(radon) has even lower p-value than radon.  However, the Q-Q plot of log(radon) shows a large deviation at lower quantiles. We plot the histogram to see which is more normal-distributed. The histogram shows that log(Radon) is negatively skewed and less approximately normal than simply Radon. As a result, we choose Radon as our variable.


(f) Now let us explore how the normal density compares with the kernel density estimate. Plot the kernel density estimate of your variable (either radon or log radon) and the normal distribution estimate of it (see normal-kde.Rmd). Does the normal model look like a good ﬁt?

From the previous question, we decide to choose Radon as our variable. But we plot both here so that we can again see the difference in distribution between log(Radon) and Radon.
```{r}
logRn <- ctyNew$log_Uppm
Rn <- ctyNew$Uppm

plot(density(logRn, bw="nrd"))
x_sorted <- seq(min(logRn), max(logRn), length.out = 250)
p <- dnorm(x_sorted, mean(logRn), sd(logRn)); 
lines(x_sorted, p, col="red", lty=2) 

plot(density(Rn, bw="nrd"))
x_sorted <- seq(min(Rn), max(Rn), length.out = 250)
p <- dnorm(x_sorted, mean(Rn), sd(Rn)); 
lines(x_sorted, p, col="red", lty=2) 
```
The kernal density distribution of Radon is closer to normal distribution than log(Radon). It is more concentrated than the normal distribution but the skewness is small and the magnitude is not largely different. As for log(Radon), it has high negative skewness and is much more centralized than normal distribution. The results support our decision that Radon is the better option for the following analysis.


**Question 2**
Now we will consider ﬁtting a regression surface to the data. For now we will use the subset data frame we created before, with the zero values omitted - in reality we should consider censored regression which we will discuss later in the class.

(a) Implement model 1, a linear model on longitude and latitude, where y is either radon or log radon:
$$y_i = \beta_0 + \beta_1lon_i + \beta_2lat_i + \epsilon_i $$
```{r}
model_1 <- lm(Uppm ~ lon+lat, data = ctyNew)
summary(model_1)
```
$$y_i = 2.0184 + 0.0011lon_i - 0.0038lat_i + \epsilon_i $$
where we assume the $\epsilon_i$ are normally distributed with mean zero.
This linear model performs poorly because the two coefficient both don't pass the 5% significant test and the R^2 is very low.


(b) Now let model 2 be a polynomial model on longitude and latitude. Choose the order of the polynomial as you did in HW2.
```{r}
model_2 <- lm(Uppm ~ poly(lon, 3, raw=TRUE) + poly(lat, 3, raw=TRUE),data=ctyNew)
summary(model_2)
```
Based on the distribution of Uppm across the US (see plots in part a), we chose 3rd order polynomials to fit our data because the observed trend of longitude suggested that we needed an odd numbered polynomial. Our final decision was mainly made through a process of trial and error. We attempted to keep the order low to avoid a potential overfitting problem. We also found that the increase of R^2 is significant from 2nd order to 3rd order, but any increases in R^2 are minimal in orders higher than  3rd.
$$y_i = 26.05 + 1.346lon_i + 0.0140lon_i^2 + 4.805*10^{-5}lon_i^3 + 1.095lat_i \\ - 0.0194lat_i^2 + 8.580*10^{-5}lat_i^3$$
All the coefficients pass the hypothesis test at the 0.1% level and the R^2 is 0.2816.


(c) Fit model 1 and model 2 to your data set. Add a column to your data frame of the residuals from fitting model 1, and a column for the residuals from model 2.
```{r}
ctyNew2 <- ctyNew %>%
  mutate(res_1 = model_1$residuals)%>%
  mutate(res_2 = model_2$residuals)
glimpse(ctyNew2)
```


(d) Plot the residuals from model 1 and from model 2. Use the tidyr or reshape2 package plus the facet_wrap argument in ggplot2 so that the two plots are created together. Use the Albers projection. Do you identify any clusters or spatial patterns that are poorly fit?
```{r}
if (!require("RColorBrewer")) {
install.packages("RColorBrewer")
library(RColorBrewer)
}
blups <- brewer.pal(9,"RdBu")

ctyMelt <- melt(ctyNew2,id.vars = c("lon","lat","Uppm","log_Uppm"))

ggplot(ctyMelt, aes(x=lon, y=lat)) + 
  geom_point(aes(size=value, color=value)) +
  scale_size_continuous(range=c(0.1, 1)) +
  coord_map(projection="albers", parameters = c(25, 50)) +
  #scale_color_viridis(colors=blups) +  
  scale_color_gradientn(colors=blups)+
  theme_map()+
  facet_wrap(~variable) +
  theme_few() +
  ggtitle("Residuals from Model 1 and Model 2")

```
The patterns are similar between the residuals of these models, but the magnitude of model 1 is generally larger than model 2. Compared to Uppm distribution (1(a)), we can see that the high Uppm areas has positive residuals and low Uppm areas has negative residuals. The coastal regions, Great Lake region, and Appalacian Mountain Range have higher magnitude residuals.


3. Now consider a nearest-neighbor regression (see the example codes on Courseworks - you should not need to modify them much). The primary choice you need to make is the number of nearest neighbors to use. For this problem, use 15. (4 pts)
```{r}
knnreg <- function(x_train, y_train, x_test, k, weights=NULL){
  # Implement a K-Nearest-Neighbor Regression
  # The predtion is carried out using a kernel regression approach with random sampling.
  #
  # Inputs:
  #   x_train: training data predictors
  #   y_train: training data observed predictand
  #   x_test: test data predictors
  #   k: the number of nearest-neighbors to use
  #   weights: weights to assign to each observation (default NULL)
  # Returns:
  #   y_test: nearest neighbor estimate of predictand given x_test
  
  # Get the names of y_test
  y_names <- names(y_test)
  
  # Convert all inputs to matrix format
  x_train <- as.matrix(x_train)
  x_test <- as.matrix(x_test)
  y_train <- as.matrix(y_train)
  
  # Make sure the input dimensions are correct
  n_train <- nrow(x_train)
  if (nrow(y_train) != n_train) stop('x_train and y_train have different number of rows')
  n_predictors <- ncol(x_train)
  if (ncol(x_test) != n_predictors) stop('x_train and x_test have different number predictors')
  n_test <- nrow(x_test)
  n_predictand <- ncol(y_train)
  
  # set weights if none are given
  if (is.null(weights)) weights <- rep(1, n_predictors)
  
  # Initialize the y_test matrix
  y_test <- matrix(NA, nrow = n_test, ncol = n_predictand)
  
  # Loop through each test data point:
  for (n in 1:n_test){
    # compute the distance from our test point to each training point
    distance <- matrix(NA, nrow = n_train, ncol = n_predictors)
    for (i in 1:n_predictors){
      distance[, i] <- 100 * weights[i] * (x_train[, i]- x_test[n, i])^2
    }
    distance <- rowSums(distance, na.rm=TRUE)
    distance_rank <- rank(distance)
    
    # get the positions of the neighbors for each prediction vector
    neighbor_idx <- rep(NA, n_test)
    neighbor_idx[rank(distance_rank, ties.method='first')] <- seq(1, n_train)
    neighbor_idx <- neighbor_idx[1:k] # keep only k nearest neighbors!
    
    # We make the prediction by taking the average value of the k nearest
    # neighbors, weighted by the *rank* of their distance from the
    # test point. Here "distance" means Euclidean distance.
    sum_dist_rank <- sum(distance_rank[1:k]^(-1)) # normalizing factor
    dist_prob <- (distance_rank[1:k] ^ (-1)) / sum_dist_rank # sampling probabilities
    sampled_indices <- sample(neighbor_idx, 1000, replace=T, prob=dist_prob) # re-sample the indices
    
    # make the predictions
    y_sampled <- y_train[neighbor_idx]
    if (n_predictand == 1) {
      y_test[n, ] <- mean(y_sampled, na.rm = TRUE)
    } else{
      y_test[n, ] <- colMeans(y_sampled, na.rm = TRUE)
    }
  }
  
  # convert to data frame
  y_test <- data.frame(y_test)
  names(y_test) <- y_names
  return(y_test)
}
#knnreg function ends

   #=====================

K <- 10  #10-fold
num <- 15  #nearsest neighbor
##cv_mse <- function(K,num){
ctyNew_test1 <- ctyNew2 %>% 
  mutate(index = sample(1:n())) %>%
  mutate(fold = (index %% K) + 1)

# for each sub-fold create a data frame of true and predicted y
results_df <- vector('list', length = K) # create an empty list
for (k_i in 1:K){
  x_test <- filter(ctyNew_test1, fold == k_i) %>% select(lon, lat)
  x_train <- filter(ctyNew_test1, fold != k_i) %>% select(lon, lat)
  y_train <- filter(ctyNew_test1, fold != k_i) %>% select(Uppm)
  y_test <- filter(ctyNew_test1, fold == k_i) %>% select(Uppm)
  y_hat <- knnreg(x_train=x_train, y_train=y_train, x_test=x_test, k=num)
  results_df[[k_i]] <- data.frame(y_hat = y_hat$Uppm, y_true = y_test$Uppm) 
}
# convert our list of data frames to a single data frame
results_df <- bind_rows(results_df)
# get MSE
mean_squared_error <- mean((results_df$y_hat - results_df$y_true) ^ 2)
##  return(mean(squared_error))
##}

##cv_mse(10,15)  #10-fold, nearest neighbor = 15

```
The mean square error (MSE) of the above caluclation, where we look at 15 nearest neighbors is 0.0592.


4. In general, Cross-Validation (CV) is used to choose the parameters of a model. We do this by choosing parameter which gives us the “best” (remember we have to define what this means) performance on out-of-sample predictions. The recipe for K-fold CV is to:
(a) Define our statistical model which has one (or more, but here we’ll focus on one) parameters λ which we want to choose
(b) Define J values of the parameter of interest λ to try
(c) Split the data into K chunks of equal size
(d) For each value of λ:
  i. Fork=1,...,K:  
    A. Define the “test” data to be the kth chunk of data, and the “training” data to be all but the “test” data  
    B. Fit the model using the current value of λ on the training data  
    C. Use the model you just fit to make predictions about the test data  
  ii. Now you have predictions for each data point. Use these predictions to calculate some measure of model skill (such as mean squared error). Record this value somewhere
(e) Choose the value of λ which had the best performance (ie lowest MSE). 

As you can see, this is a general procedure which allows us to select model parameters based on their out-of-sample prediction performance. Resources such as James et al. [2013] or Friedman et al. [2001] will go into Cross-Validation in more detail. Use K-fold cross-validation to choose the number of neighbors that returns the lowest test MSE. You can choose K, but 5 or 10 are commonly used. (24 pts)

```{r}
K <- 10   #k-fold
n_nearest_neigh <- 2:20
mse <- rep(NA, length(n_nearest_neigh))

for (i in 1:length(n_nearest_neigh)){
  ctyNew_test1 <- ctyNew2 %>% 
    mutate(index = sample(1:n())) %>%
    mutate(fold = (index %% K) + 1)
  
  # for each sub-fold create a data frame of true and predicted y
  results_df <- vector('list', length = K) # create an empty list
  for (k_i in 1:K){
    x_test <- filter(ctyNew_test1, fold == k_i) %>% select(lon, lat)
    x_train <- filter(ctyNew_test1, fold != k_i) %>% select(lon, lat)
    y_train <- filter(ctyNew_test1, fold != k_i) %>% select(Uppm)
    y_test <- filter(ctyNew_test1, fold == k_i) %>% select(Uppm)
    y_hat <- knnreg(x_train=x_train, y_train=y_train, x_test=x_test, k=n_nearest_neigh[i])
    results_df[[k_i]] <- data.frame(y_hat = y_hat$Uppm, y_true = y_test$Uppm) 
  }
  # convert our list of data frames to a single data frame
  results_df <- bind_rows(results_df)
  # get MSE
  mse[i] <- mean((results_df$y_hat - results_df$y_true) ^ 2)
  
}
plot(n_nearest_neigh, mse)
```
The kNN cross validation is used to find the best lambda or the nearest neighbor, which can be used to build a model to best explain our data. Here we use 10-fold, so the data is randomly split into 10 chunks, and each of them are set as testing data while others are set as training data. The training data is used to build the model and we use this model to predict values for the testing data. Using the predicted values and original values, we can calculate mean square error (MSE) to represent the quality of the chosen $\lambda$. The $\lambda$ yielding the least MSE is chosen as the best parameter (which we are suing as the number of nearest neighbors).
From the plot of the MSE versus the number of nearest neighbors, a value of 4 or 5 gives the lowest MSE. The result differs slightly each run because the kNN CV uses random data, but 4 or 5 always has the smallest MSE (at least during all our runs). We also tested values above 20 and found that the MSE continues increasing. 


5. Create a variable in your data frame for residuals on your best linear or polynomial model, and for your best KNN model. Plot them both. (2 pts)

The best polynomial model is order = (3,3).
The best kNN model is the model with the number of nearest neighbor = 4 or 5 (for the below plots we chose 4).

First, find the residuals of kNN model with spatial information recorded.
```{r}
#get k=4 residual
K <- 10

ctyNew_test1 <- ctyNew2 %>% 
  mutate(index = sample(1:n())) %>%
  mutate(fold = (index %% K) + 1)

# for each sub-fold create a data frame of true and predicted y
results_df <- vector('list', length = K) # create an empty list

for (k_i in 1:K){
  x_test <- filter(ctyNew_test1, fold == k_i) %>% select(lon, lat)
  x_train <- filter(ctyNew_test1, fold != k_i) %>% select(lon, lat)
  y_train <- filter(ctyNew_test1, fold != k_i) %>% select(Uppm)
  y_test <- filter(ctyNew_test1, fold == k_i) %>% select(Uppm)
  lat_test <- filter(ctyNew_test1, fold == k_i) %>% select(lat)
  lon_test <- filter(ctyNew_test1, fold == k_i) %>% select(lon)
  
  y_hat <- knnreg(x_train=x_train, y_train=y_train, x_test=x_test, k=4)
  results_df[[k_i]] <- data.frame(y_hat = y_hat$Uppm, y_true = y_test$Uppm, lon = lon_test$lon, lat = lat_test, resid = y_hat$Uppm - y_test$Uppm) 
}
# convert our list of data frames to a single data frame
results_df <- bind_rows(results_df)


#plot
blups <- brewer.pal(9,"RdBu")

##polynomial
polyPlot <- ggplot(ctyNew2, aes(x=lon, y=lat)) + 
  geom_point(aes(size=res_2, color=res_2)) +
  scale_size_continuous(range=c(0.1, 1)) +
  coord_map(projection="albers", parameters = c(25, 50)) +
  #scale_color_viridis(colors=blups) +  
  scale_color_gradientn(colors=blups)+
  theme_map() + 
  theme_few() +
  ggtitle("Polynomial Residuals")

##kNN, k=4
kNNPlot <- ggplot(results_df, aes(x=lon, y=lat)) + 
  geom_point(aes(size=resid, color=resid)) +
  scale_size_continuous(range=c(0.1, 1)) +
  coord_map(projection="albers", parameters = c(25, 50)) +
  #scale_color_viridis(colors=blups) +  
  scale_color_gradientn(colors=blups)+
  theme_map() +
  theme_few() +
  ggtitle("kNN Residuals")

library(gridExtra)
grid.arrange(polyPlot, kNNPlot, nrow = 1)

```
Comparing the residual plots, we can see that the overall kNN residuals are smaller than the polynomial residuals. Except for a few points, the kNN residuals are in the range [-0.5, 0.5]. It also appears that there is no visible pattern observed in the distribution of residuals in the kNN model. We can conclude that the kNN model performs much better than the polynomial and linear one.

**Conclusion**: The kNN model appears to be the better method to fit for this large and complicated dataset. In this example, it is  clear to see from the residual plot that the kNN model performs better than other models (linear and polynomial) we tested.

