---
output:
  pdf_document: default
  html_document: default
---
```{r}
library(tidyverse)
library(gridExtra)
library(modelr)
```

### P1: We would like to build a model for predicting life expectancy. Create a data frame that includes onlycomplete cases (no missing values) and includes columns for country code, year, and the followingresponse + predictors. Visualize life expectancy versus the five candidate predictors, transforming variables as necessary, anddescribe their relationships.
```{r}
file_path <-"D:/Education/MS DS NEU/IDMP/Assignments/HW4/ddf--gapminder--systema_globalis-master/countries-etc-datapoints"
lifeexp_path <- "ddf--datapoints--life_expectancy_years--by--geo--time"
lifeexp <-read_csv(file.path(file_path,paste0(lifeexp_path, ".csv")))
inf_mort_path <- "ddf--datapoints--infant_mortality_rate_per_1000_births--by--geo--time"
inf_mort <-read_csv(file.path(file_path,paste0(inf_mort_path, ".csv")))
murder_path <- "ddf--datapoints--murder_per_100000_people--by--geo--time"
murder_rate <-read_csv(file.path(file_path,paste0(murder_path, ".csv")))
gdp_path <- "ddf--datapoints--gdppercapita_us_inflation_adjusted--by--geo--time"
gdp <-read_csv(file.path(file_path,paste0(gdp_path, ".csv")))
doctors_path <- "ddf--datapoints--medical_doctors_per_1000_people--by--geo--time"
doc_data <-read_csv(file.path(file_path,paste0(doctors_path, ".csv")))
poverty_path <- "ddf--datapoints--poverty_percent_people_below_550_a_day--by--geo--time"
poverty_data <-read_csv(file.path(file_path,paste0(poverty_path, ".csv")))
data_df <- lifeexp%>%
  inner_join(inf_mort)%>%
  inner_join(murder_rate)%>%
  inner_join(gdp)%>%
  inner_join(doc_data)%>%
  inner_join(poverty_data)%>%
  rename(lifeexp=life_expectancy_years,
         inf_mort=infant_mortality_rate_per_1000_births,
         murder_rate=murder_per_100000_people,
         gdp=gdppercapita_us_inflation_adjusted,
         doc_data=medical_doctors_per_1000_people,
         poverty_data=poverty_percent_people_below_550_a_day)
data_df
```
```{r}
ggplot(data_df,aes(x=log2(inf_mort),y=lifeexp))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", color="red")+
  labs(x="Infant mortality rate per 1000 births",y="Life expectancy (in years)",title="Relatiion b/w Infant mortality Graph and life expectancy")+
  theme_minimal()
```
#### There is a negative relationship between Infant mortality rate and Life Expectancy
```{r}
ggplot(data_df,aes(x=log2(murder_rate),y=lifeexp))+
  geom_point()+geom_smooth()+
  geom_smooth(method="lm", color="red")+
  labs(x="Murder rate per 100000 people",y="Life expectancy (in years)",title="Relationship b/w murder rate and life expectancy")+
  theme_minimal()
```
#### We can observe a negative relationship between Murder rate and Life expectancy
```{r}
ggplot(data_df,aes(x=log2(gdp),y=lifeexp))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", color="red")+labs(x="GDP",y="Life expectancy (in years)",title="Relationship b/w Life Expectancy and GDP per capita")+
  theme_minimal()
```
#### We can observer a positive relationship between GDP and Life expectancy
```{r}
ggplot(data_df,aes(x=doc_data,y=lifeexp))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", color="red")+
  labs(x="Doctors per 1000 people",y="Life expectancy (in years)",title="Relationship b/w doctors and Life Expectancy")+
  theme_minimal()
```
```{r}
ggplot(data_df,aes(x=log2(1+poverty_data),y=lifeexp))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", color="red")+
  labs(x="Poverty",y="Life expectancy (in years)",title="Relation b/w Poverty rate and Life Expectancy")+
  theme_minimal()
```
#### We can oobserve a negative relationship between Life Expectancy and poverty data

### P2: Build a linear regression model for life expectancy using a single predictor, justifying your choice basedonly on the visualizations from Problem 1. Then use residual plots to perform model diagnostics.Comment on any outliers or violations of model assumptions you notice in the residual plots. If necessary,fix the issue, re-model the model, and perform model diagnostics again.

```{r}
model1 <-lm(lifeexp~ log2(inf_mort), data=data_df)
summary(model1)
```
```{r}
graph_1 <- data_df%>%
  add_residuals(model1, "resid")%>%
  ggplot(aes(x=log2(inf_mort),y=resid))+
  geom_point()+labs(x="Infant mortality")
graph_2 <- data_df%>%
  add_residuals(model1, "resid")%>%
  ggplot(aes(sample=resid))+
  geom_qq()
gridExtra::grid.arrange(graph_1, graph_2, ncol=2)
```
```{r}
outliers <- data_df%>%
  add_residuals(model1, "resid")%>%
  filter(resid< -10)
outliers
```
```{r}
data_df2 <-anti_join(data_df, outliers, by=c("geo", "time"))
model2 <-lm(lifeexp~ log2(inf_mort), data=data_df2)
summary(model2)
```

```{r}
ggplot(data_df2,aes(x=log2(inf_mort),y=lifeexp))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", color="red")+
  labs(x="Infant mortality rate per 1000 births",y="Life expectancy (years)",title="Relationship b/w Life Expectancy & infant mortality (outliers removed")+
  theme_minimal()
```
#### Observation is same as before but no outliers.


### P3: Use residual plots to determine if any other candidate predictors should be added to your model fromProblem 2. If so, add up to one additional predictor to the model, and then perform model diagnosticson the new model.

```{r}
graph_1 <- data_df2%>%
  add_residuals(model2, "resid")%>%
  ggplot(aes(x=log2(murder_rate),y=resid))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", color="red")+
  labs(x="Murder rate")
graph_2 <- data_df2%>%
  add_residuals(model2, "resid")%>%
  ggplot(aes(x=log2(gdp),y=resid))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", color="red")+
  labs(x="GDP")
graph_3 <- data_df2%>%
  add_residuals(model2, "resid")%>%
  ggplot(aes(x=doc_data,y=resid))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", color="red")+
  labs(x="Doctors")
graph_4 <- data_df2%>%
  add_residuals(model2, "resid")%>%
  ggplot(aes(x=log2(1+poverty_data),y=resid))+
  geom_point()+
  geom_smooth()+
  geom_smooth(method="lm", color="red")+
  labs(x="Poverty rate")
gridExtra::grid.arrange(graph_1, graph_2, graph_3, graph_4)
```
#### Only murder rate showcased a negative relationship while the others were completely random and need not be included in the model. Only murder_rate can be included.

```{r}
model3 <-lm(lifeexp~ log2(inf_mort)+ log2(murder_rate), data=data_df2)
summary(model3)
```
```{r}
graph_1 <- data_df2%>%
  add_residuals(model3, "resid")%>%
  ggplot(aes(x=log2(murder_rate),y=resid))+
  geom_point()+
  labs(x="Murder rate")
graph_2 <- data_df2%>%
  add_residuals(model3, "resid")%>%
  ggplot(aes(sample=resid))+
  geom_qq()
gridExtra::grid.arrange(graph_1, graph_2, ncol=2)
```
#### Both the graph shows no significant abnormalities and can be considered an ideal random scatter plot and it is the same case with the residuals. An outlier is there but since it is not extremely abnormal, we can leave it or keep it.

### P4: Using the full dataset (minus any outliers you removed), perform reproducible 10-fold cross-validation onyour model from Problem 3. Report the cross-validated RMSE, as well as the RMSE of the model fromProblem 3 on the data used to train it. Which RMSE is larger? Is this surprising, and why?

#### Set up seed value and perform k fold cross validation and print final CV value
```{r}
set.seed(2020)
cv_df <-crossv_kfold(data_df2, k=10)
cv_df <-mutate(cv_df,model =map(train,~ lm(lifeexp~ log2(inf_mort)+ log2(murder_rate),data = .)),
              rmse =map2_dbl(model, test,~ rmse(.x, .y)))

mean(cv_df$rmse)
```

```{r}
rmse(model3, data_df2)
```
#### The CV RMSE is slightly larger than the regular RMSE. And its not that surprising, because of the test and training data differences.

### P5: Reproducibly partition the dataset (minus any outliers) into a training, validation, and test set using a50/25/25 split. Keeping any transformations you found to be appropriate in Problem 1, perform stepwisemodel selection to build a predictive model for life expectancy using RMSE as the selection criterion.Show the RMSEs at each step and note which variable is being added/dropped, and then report theRMSE of the selected model on the test set.


#### Divide the Data
```{r}
set.seed(2020)
dfpart <-resample_partition(data_df2, p=c(train=0.5, valid=0.25, test=0.25))
```
#### Step 1:
```{r}
mean_1 <-lm(lifeexp~ log2(inf_mort), data=dfpart$train)
mean_2 <-lm(lifeexp~ log2(murder_rate), data=dfpart$train)
mean_3 <-lm(lifeexp~ log2(gdp), data=dfpart$train)
mean_4 <-lm(lifeexp~doc_data, data=dfpart$train)
mean_5 <-lm(lifeexp~ log2(1+poverty_data), data=dfpart$train)

rmse(mean_1, dfpart$valid)
```

```{r}
rmse(mean_2, dfpart$valid)
```
```{r}
rmse(mean_3, dfpart$valid)
```
```{r}
rmse(mean_4, dfpart$valid)
```
```{r}
rmse(mean_5, dfpart$valid)
```
#### Step 2:
```{r}
mean_12 <-lm(lifeexp~ log2(inf_mort)+ log2(murder_rate), data=dfpart$train)
mean_13 <-lm(lifeexp~ log2(inf_mort)+ log2(gdp), data=dfpart$train)
mean_14 <-lm(lifeexp~ log2(inf_mort)+doc_data, data=dfpart$train)
mean_15 <-lm(lifeexp~ log2(inf_mort)+ log2(1+poverty_data), data=dfpart$train)
rmse(mean_12, dfpart$valid)
```


```{r}
rmse(mean_13, dfpart$valid)
```

```{r}
rmse(mean_14, dfpart$valid)
```
```{r}
rmse(mean_15, dfpart$valid)
```
#### Step 3:
```{r}
mean_123 <-lm(lifeexp~ log2(inf_mort)+ log2(murder_rate)+ log2(gdp),data=dfpart$train)
mean_124 <-lm(lifeexp~ log2(inf_mort)+ log2(murder_rate)+doc_data,data=dfpart$train)
mean_125 <-lm(lifeexp~ log2(inf_mort)+ log2(murder_rate)+ log2(1+poverty_data),data=dfpart$train)
rmse(mean_123, dfpart$valid)
```
```{r}
rmse(mean_124, dfpart$valid)
```
```{r}
rmse(mean_125, dfpart$valid)
```

#### Step 4:
```{r}
mean_1234 <-lm(lifeexp~ log2(inf_mort)+ log2(murder_rate)+log2(gdp)+doc_data,data=dfpart$train)
mean_1235 <-lm(lifeexp~ log2(inf_mort)+ log2(murder_rate)+log2(gdp)+ log2(1+poverty_data),data=dfpart$train)
rmse(mean_1234, dfpart$valid)
```

```{r}
rmse(mean_1235, dfpart$valid)
```
#### Step 5:
```{r}
mean_12354 <-lm(lifeexp~ log2(inf_mort)+ log2(murder_rate)+log2(gdp)+ log2(1+poverty_data)+doc_data,data=dfpart$train)
rmse(mean_12354, dfpart$valid)
```
#### There is no need to add the doctors as the RMSE increased.
```{r}
rmse(mean_1235, dfpart$test)
```

