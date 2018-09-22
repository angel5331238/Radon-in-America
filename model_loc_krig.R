---
title: "HW5"
author: "An-Chi Ho (ah3508)"
date: "`r Sys.Date()`"
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
  fig.width=15,
  fig.height=10
)
```

# 1. Data Exploration


Package Installation:
```{r}
if(!require(pacman)) install.packages('pacman')
pacman::p_load(dplyr, ggplot2, mapproj, readr, ggthemes, viridis, reshape2, sp, gstat, locfit, automap,RColorBrewer)
```



1. Read the data in to ﬁle from cty.csv. Here we will not use the log data (just use Uppm).
Read in the data:
```{r}
cty <- read_csv('cty.csv') %>%
  select(lon, lat, Uppm)
glimpse(cty)
```


Remove the zero value in Uppm:
```{r}
cty <- cty %>%
 filter(Uppm!=0) %>%
 distinct()
```



2. An efficient way to create many plots (in ggplot) with the same background information is to save that plot as its own object. Create a map of the United States, add a map projection and a map theme.
```{r}
usa_map <- map_data("state") %>%
  rename(lon = long) %>%
  ggplot(aes( x = lon ,y = lat)) +
  geom_polygon(aes(group = group) ,color = 'black', fill ='white') +
  coord_map(projection="albers", parameters = c(25, 50)) +
  theme_map()

plot(usa_map)
```



3. Superimpose the radon data onto your map of the USA, make the point color depend on the radon value, add an appropriate color scale, and add a title to the plot.
```{r}
usa_map+
  geom_point(data=cty, aes(color=Uppm)) +
#    scale_size_continuous(range=c(0.1, 2)) +
  scale_color_viridis(limits=c(0, 4)) +
  ggtitle("Radon Concentrations across the United States")
```



4. We are going to create several models for radon. Last week in HW4 we looked at Cross-Validation as a measure of model prediction. Here, for simplicity, we will just divide our data into training and test data frames, and then we will train on the training data and evaluate model performance on the test data. First use set.seed plus the last four digits of your UNI to create a reproducible example. The test data is 40% of the entire data set and run it to create
a test and training data set.
```{r}
set.seed(3508)
test_size <- floor(0.4*nrow(cty))
test_idx <- sample(seq_len(nrow(cty)),size = test_size)
train <- cty[-test_idx,] %>% as_tibble()
test <- cty[test_idx,] %>% as_tibble()

```



# 2. Local Regression

The locfit package ﬁts a local regression, weighting data based on distance.

1. The generic way to code a model in locfit is with syntax just like lm:
```{r}
lf_1 <- locfit(Uppm~lon+lat, data=cty)
summary(lf_1)
```
Build and ﬁt a model in locfit using this syntax. Leave all the other parameters to their defaults (do not change them).



2. Use this model to obtain predictions on new data. Calculate the mean squared error of these predictions.
```{r}
lf_train <- locfit(Uppm~lon+lat, data = train)
summary(lf_train)  

y_pred <- predict(lf_train, newdata = test)
mse1 <- mean((y_pred - test$Uppm)^2)
mse1
```
MSE of the model predictions is 0.1444.



3. This is good, but we can change the alpha parameter of our model which governs how many neighbors are used for local regression. Fortunately, locfit has a built-in cross-validation function to help us choose it. Modify this code to get the GCV score as a function of alpha. Plot the GCV score as a function of alpha and choose the best value of alpha. You should note that there isn’t a single perfect value to use, and small changes are OK.
```{r}
alpha_vals <- exp(seq(-5, -2,length.out = 50)) #values to try
gcv_1 <- gcvplot(Uppm~lon+lat, data=cty, alpha = alpha_vals) %>%
  summary() %>%
  as_tibble() %>%
  mutate(alpha = alpha_vals)

ggplot(gcv_1,aes(x = alpha, y = GCV))+
  geom_point()+
  ggtitle('GCV score vs. smoothing parameter alpha')

ggplot(gcv_1[1:10,],aes(x = alpha[1:10], y = GCV[1:10]))+
  geom_point()+
  ggtitle('GCV score vs. smoothing parameter alpha')
```
The best alpha is the one with lowest GCV score. Here the best alpha we get is approximate 0.0086.



4. Now that we have an estimated best value of alpha to use, we can make predictions on our test data (using the predict function as you have done before). Create a data frame called test_df with columns lon, lat, Uppm (from the raw data) and lf_pred, which contains the predictions from your best locﬁt model.
```{r}
lf_best <- locfit(Uppm~lon+lat, data = train, alpha = 0.0097)
summary(lf_best)

y_best <- predict(lf_best, newdata = test)

test_df <- test %>%
  mutate(lf_pred = y_best)

```



5. Calculate the MSE of this best model. Has it gone down?
```{r}
mse_best <- mean((test_df$lf_pred - test_df$Uppm)^2)
mse_best
```
Yes, the MSE is 0.0970, which is lower than before.



# 3. Kriging 

1. You have already created training and test data frames. Create a spatial object from each by assigning coords. Call them test_sp and train_sp.
```{r}
test_sp <- test
coordinates(test_sp) = c("lon","lat")  
train_sp <- train
coordinates(train_sp) = c("lon","lat") 
```



2. The automap package makes it easy to simultaneously fit a variogram and a kriging model to our data. Describe what the model and kappa arguments mean. How can we interpret them?
```{r}
krige_model <- autoKrige(
  formula = Uppm~1,
  input_data = train_sp,
  new_data = test_sp,
  model = c("Sph", "Exp", "Gau"),
  #model = c("Sph", "Exp", "Gau", "Ste"),
 # kappa = c(0.05, seq(0.2,2,0.1),5,10),
  remove_duplicates = TRUE,   #####
  verbose = FALSE
)
```
Notice that 10 duplicate observations in input data (i.e., train data) is removed. Ordinary Kriging is used. We test sphere model, exponential model, and Gaussian model here.

- model: List of models that will be tested during automatic variogram fitting.
- kappa: List of values for the smoothing parameter of the Matern model that will be tested during automatic variogram fitting. Kappa is only used for Matern model and we don't try this model here.
- *Matern* covariance: It is commonly used to define the statistical covariance between measurements made at two points that are d units distant from each other. Since the covariance only depends on distances between points, it is stationary. If the distance is Euclidean distance, the Matérn covariance is also isotropic.



3. Fit the model using autoKrige. Plot the resulting object (just call plot(krige_model)).
```{r}
plot(krige_model)
krige_model$var_model
```
(a) How does the variogram look?

    The variogram fit the exponentail line most. Semi-variance is opposite with correlation, i.e., the spatial correlation is larger when semi-variance is smaller. The range, which is the distance where the line flattens out, is the threshold of spatial correlation. Data with distance closer than the range are spatially autocorrelated, whereas locations farther apart than the range are not correlated to each other. The sill is the maximum value the variogram can achieve. Ideally, the semi-variance at zero distance, the nugget, is 0, but here it isn't. The nugget effect may due to measurement error or variation at distances smaller than the sampling interval.
    
    *reference*: http://pro.arcgis.com/en/pro-app/help/analysis/geostatistical-analyst/understanding-a-semivariogram-the-range-sill-and-nugget.htm
    
(b) What variogram function was used? What is its mathematical parameterization?

    Exponential model is chose to be the variogram function. Nuggget = 0.03, sill = 0.3, range = 4.5.
    
(c) Which areas have the highest predicted radon? The lowest?

    The prediction is highly similar to the raw data. There are several areas having high Radon concentrations, like the Appalacian Mountain Range, Great Lake region, Illinois, and the Southwest. Areas like Northern Wisconsin, Northern Washington, southeastern coast, and northwestern coast have very low concentrations. 
    
(d) Which areas have the highest uncertainty (standard error) in radon concentration? The lowest?

    The regions with fewer observations have higher uncertainty. Therefore, the west side of USA has higher standard error than the east side. Aside from the west-east difference, the spatial pattern of uncertainty is quite even.



4. Create a data frame to hold the predictions from your kriging model. Use full_join from dplyr to merge this prediction with your test_df.
```{r}
krige_df <-
  krige_model$krige_output %>%
  as_data_frame() %>%
  rename(krige_pred = var1.pred) %>%
  select(lon, lat, krige_pred)

test_df2 <- full_join(test_df, krige_df, by = c("lon", "lat")) %>%
  distinct()
```
I face a strange problem here that merging `test_df` and `krige_df` generates a larger matrix. Their size is the same and lon and lat are identical. The resulting `test_df2` has several duplicate data (e.g. #502 and #503) and I have to use `distinct()` to remove them.



5. Calculate the mean squared error on test data of your model. How does it compare to local regression?
```{r}
mse_krige <- mean((test_df2$krige_pred - test_df2$Uppm)^2)
mse_krige

#add residual data in the matrix
test_df2 <- test_df2 %>%
  mutate(lf_res = test_df2$lf_pred - test_df2$Uppm) %>%
mutate(krige_res = test_df2$krige_pred - test_df2$Uppm)
```
The MSE of kriging method is 0.0438, much lower than local regression.



# 4. Comparison 

Now we will compare model projections.

1. Create a map of the predictions of both models (this is easiest if you use the melt function from the reshape2 package plus facet_wrap in ggplot). Use the USA map we made earlier.
```{r}
test_melt <- melt(test_df2,id.vars = c("lon","lat","Uppm","krige_res","lf_res"))

#blups <- brewer.pal(9,"RdBu")

usa_map+
  geom_point(data = test_melt, aes(color=value)) +
 # scale_size_continuous(range=c(0.1, 2)) +
  scale_color_viridis() +  
  #scale_color_gradientn(colors=blups)+
  facet_wrap(~variable) +
  theme_few() +
  ggtitle("Prediction of locfit and autoKrige")

usa_map+
  geom_point(data = test_melt, aes(color=value)) +
  #scale_size_continuous(size=1.5) +
  scale_color_viridis(limits=c(0, 4)) +  
  #scale_color_gradientn(colors=blups)+
  facet_wrap(~variable) +
  theme_few() +
  ggtitle("Prediction of locfit and autoKrige (colorbar adjusted)")

```
If we don't set the color bar range, we'll find that there are some extremely high value exist in the local regression model, like the point at the Gulf of Mexico below Florida. After setting the range, the results of two models are both very similar to the observation.


2. Create a map of the model residuals of both models.
```{r}

test_melt2 <- melt(test_df2,id.vars = c("lon","lat","Uppm","krige_pred","lf_pred"))

blups <- brewer.pal(9,"RdBu")

usa_map+
  geom_point(data = test_melt2, aes(color=value)) +
 # scale_size_continuous(range=c(0.1, 2)) +
  #scale_color_viridis() +  
  scale_color_gradientn(colors=blups)+
  facet_wrap(~variable) +
  theme_few() +
  ggtitle("Residual of locfit and autoKrige")

usa_map+
  geom_point(data = test_melt2, aes(color=value)) +
  #scale_size_continuous() +
  #scale_color_viridis(limits=c(0, 4)) +  
  scale_color_gradientn(colors=blups,limits=c(-1, 1))+
  facet_wrap(~variable) +
  theme_few() +
  ggtitle("Residual of locfit and autoKrige (colorbar adjusted)")

```
Similar issue as the prediction value, the residual of local regression model has extreme large value. After adjusting the color bar range, the residual pattern of the two models are similar, having larger values at sparse observation regions.

3. Which model has the lowest MSE?

As discussed in the previous question, Kriging model has the lower MSE than local regrssion model.


4. Which are some pros and cons of each model?

Kriging model has smaller error than local regression model at spare observation areas. It is non-parametric method so it doesn't have a prescribed trend, making it more realistic since the Radon distribution might not have a deterministic spatial pattern. The performance of Kriging model depends on the autocorrelation between locations. If the spatial correlation is not strong, the result will not be good. Besides, it takes more time and computing memory to find the best variogram and minize the residuals.

Local regression model is much better than simple and multiple regression model because it only uses surrounding data, which have greater correlation. However, it is parametric method, which means it has a deterministic trend that might not exist in reality. It will lead to large error at the place with sparse data point, such as the extreme value at Gulf of Mexico. On the other hand, it is computing-effective and memory-saving than Kriging model, and easy to catch larger scale trend if there is one.


# 5. Reflection
1. About how long did you spend on this homework?

About 8 hours.

2. Which statistical concept do you think you understood the best from this homework? The worst?
  
I understand the two methods lots more through this homework, especially thanks for the conceptual part at the beginnng. I learned how to interpret the Kriging results, like the information of variogram, but I don't know how to use these information to improve the model or something else.

3. Which programming concept introduced in this homework was most confusing?
  
The autoKriging function is a bit confusing because its resouce is less on the internet. And I don't know how to choose the range of alpha to test if you don't provide it.


