#Sara Heisel
#Modeling
#15 May 2018

##NOTES from lecture
#exploring relationships between variables in dataset
#testing hypotheses according to data
#new packages GGally- looks at correlations between variables (two columns of data frame) - fxn ggpairs
#ggpairs looks at pairs of data in data frame - using 

#Task 2: Using ggpairs to create a 4x4 summary plot of precip, average temp, size, and cases
##ld.prism.pop %>% ggpairs(columns=C("prcp","avtemp","size","cases"))
##ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))

#Task 3:
##logging data
#using dplyr to log transform data
##mutate(log10speed=log10(speed))
##ggpairs(columns=C("log10speed","dist"))

##Reproducibility and subsampling
#creating dataframe and printing
#x <- tibble(rnorm(10)) %>% print

#look at random sample of data
#sample 5 element of data frame

#x %>% sample_n(5)

#this (above) is random sample, get around this by setting seed
#setting seed ensures that always getting same numbers , this is mostly important for reproducible research
#set.seed(123); x %>% sample_n(5)

#LINEAR MODELING
#library(ggplot2)
#ggplot(cars) + geom_point (aes(speed,dist)) +
 # geom_smooth(aes(speed,dist)), method="lm")
#yesterday we were using the loess (non-parametric), the above implies there might be a linear rel.

#Summary on linear model - estimate, std error and t value- how good is the fit?
#extracting coefficients - how much one variable impacts the other

#LISTS
#Anything extracted from a model can be made into a list - for instance, can easily extract coefficients

#CORRELATIONS COEFFICIENTS
#ggpairs automatically does a correlation test
#Spearman's - cor.test(cars$speed, cars$dist, method="spearman")
#what do values mean with respect to relatedness of variables

#MODELR
#modeling and visualizing data

##STARTING EXERCISE
install.packages("ggplot2")
install.packages("GGally")
library(tidyverse)
library(magrittr)
library(GGally)
library(ggplot2)

####Task 1####
#Task 1: loading data
setwd("C:/Users/workshop/Desktop/R")
ld.prism.pop <- read_csv(file = "pop.ld.prism.csv")
#ignore first row, bc these header names are not working

####Task 2####
####assigning####
#Task 2: Using ggpairs to create a 4x4 summary plot of precip, average temp, size, and cases
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))

####Task 3####
#Task 3: Create two new columns for log10(size) and log10(cases+1) and substitute these for the original size and cases
#supplied when you recreate the ggpairs plot
#using dplyr mutate to log transform data
#add 1 to cases, bc there are zero values, which you cannot take log of

ld.prism.pop %<>% mutate(log10cases=log10(1+cases))
ld.prism.pop %<>% mutate(log10size=log10(size))

##rerunning pairs plot on log transformed data
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10size","log10cases"))

####Task 4####
##control + shift + c comments blocks of code
#Task 4: A simple linear model: ggpairs plot
##ggpairs suggested that prcp and avtemp were pos correlated, now going to look at these in a linear model
#setting seed makes it reproducible
set.seed(222)
#create a new dataframe to be a random sample (n=100 rows) of the full data frame and plot prcp (x) and avtemp (y)
sample.100 <- ld.prism.pop %>% sample_n(100)

myPlot <- ggplot(sample.100) + geom_point(aes(prcp,avtemp)) 


####Task 5####
##Adding linear fit
myPlot_line <- ggplot(sample.100) + geom_point(aes(prcp,avtemp)) +
  geom_smooth(aes(prcp,avtemp),method="lm")

myPlot_line

####Task 6####
#Create a linear model (lm) object with a call like MyModel <- lm(y ~ x, data=myData)
#for the subsetted data, where y=avtemp and x=prcp. View summary with a call along the lines of summary(myModel)

SubsetModel <- lm(avtemp ~ prcp, data=sample.100)
summary(SubsetModel)

####Task 7####
#Can access data from the model we just ran. For instance, can extract the slope and the associated p-value
#summary(myModel)$coefficients[2,1]
#summary(myModel)$coefficients[2,4]
#What is the slope of the line you plotted in Task 5, and is the slope significantly different from 0 (p<0.05)?
#This is asking for the coefficient from the subset model (slope)
summary(SubsetModel)$coefficients[2,1]
#This is looking for the p value from the subset model
summary(SubsetModel)$coefficients[2,4]
# > summary(SubsetModel)$coefficients[2,1]
# [1] 0.0001563006 is the slope
# > summary(SubsetModel)$coefficients[2,4]
# [1] 0.884176 is p-value, which is not significant

####Task 8####
#MODELR package
#This package is designed to help seamlessly integrate modeling into a pipeline of data manipulation and 
#visualization
#Write a single line of code to generate a ggplot of total population size by year
#you should pass the main (large) data frame to a group_by call and then a summarize call, then 
#you can pass this new, unnamed data frame to ggplot using ggplot(.) and specify the aesthetics in a geom_point call

ld.prism.pop %>% group_by(year) %>% summarize(total=sum(size)) %>%
  ggplot(.)+geom_point(aes(x=year,y=total))

#are all states contributing equally to this growth in population in the US? modelr tools can be used to
#investigate this

####Task 9####
#create a nested data frame, which we do first by creating a grouped data frame 
#Create a data frame called "by_state" from the main data frame, that groups by state, and inspect it
by_state <- ld.prism.pop %>% group_by(state)
by_state

####Task 10####
#update the new dataframe so it is nested (simply pass it to nest)
#inspect data frame by typing its name in console to see how things changed
#by state has a list-column called "data". List elements are accessed with [].
#For example, to see the data for GA, 10th state in alphabetized data frame

by_state %<>% nest
by_state


####Task 11####
##this element of the data frame is itself a dataframe. The containing column, which contains several such
#data frames differing in length due to the number of counties per state, is most usefully organized as a list - 
#hence the column-list format. This method of organizing data comes in very useful for exploratory modeling - as
#we'll use soon. First, going to create another function.
by_state$data[[10]]

####Task 12####
#write a function that takes a data frame as its argument and returns a linear model object that 
#predicts size by year
linGrowth_model <- function(df){
  lm(size ~ year, data = df)
}

#purr is installed as part of the tidyverse, need to make sure the library is activated when using 
#fxn in purr we are interested in is called "map". To illustrate we can immediately apply a state-wise
#statistical modeling exercise

#below we are creating an object (models) that uses map function in purrr and applies the data column in the
#by_state grouped data to run through the 

models <- purrr::map(by_state$data, linGrowth_model)
models
##WHAT IS THE MAP FXN IN PURR DOING ABOVE AND WHY DO WE NEED IT?

#calling the fxn purrr::map as above, bc there was a function we used earlier today called map that was
#part of another package. To make sure the map fxn in the purrr package is being called, we can do 
#purrr:map or we can detach("package:maps", unload=TRUE)

####Task 13####
#For data science good practice, it makes sense to put the model object fitted for each state with the appropriate
#state in the original data frame - not in a new data frame, like we just saw, as that requires
#extra coordination and possible mistakes
#Add a column to the by_state dataframe, where each row (state) has its own model object
#using mutate to add a column that contains the results of the model for each row (grouped data) to the table

by_state %<>% mutate(model = map(data, linGrowth_model))
by_state

#Continuing in this format, we can store the residuals for each model (discrepancy between the model prediction and
#actual data). For this we will use the associated function to purrr:map, called map2, which takes 2 arguments
#and creates new data from them 

library(modelr)
#add_residuals of map2
by_state %<>% mutate(resids= map2(data,model,add_residuals))
by_state

####Task 14####
#what is the structure of resids?

####Task 15####
#Write a fxn that accepts an object of the type in the resids list, and returns a sum of the absolute values
#i.e. ignoring signs abs(3) + abs(-2) =5. Use the fxn to add a column called totalResid to by_state that provides
#the total size of residuals summed over counties and years

sum_resids <- function(x){
  sum(abs(x$resid))
}
by_state %<>% mutate(totalResid = map(resids,sum_resids))


#we can obtain and visualize the slope of the population by state

####Task 16#### COMMENT THIS
get_slope <- function(model){
  model$coefficients[2]
}
by_state %<>% mutate(slope = purrr::map(model, get_slope))

slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResid)

####Task 17#### COMMENT THIS
#Add + theme(axis.text.x = element_text(angle = 90, hjust = 1)) to ggplot -  to rotate x axes labels to vertical
#if they are hard to read

#Plot the growth rate (slope value) for all states
slopes %>% ggplot(aes(state,slope))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

####Task 18####
#Plot total residuals for all states
totalResids %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

####Task 19####
#Repeat Tasks 9 and 10 using a different data frame, by_state2
####Task 9(original)####
#create a nested data frame, which we do first by creating a grouped data frame 
#Create a data frame called "by_state" from the main data frame, that groups by state, and inspect it
by_state2 <- ld.prism.pop %>% group_by(state)
by_state2

####Task 10(original)####
#update the new dataframe so it is nested (simply pass it to nest)
#inspect data frame by typing its name in console to see how things changed
#by state has a list-column called "data". List elements are accessed with [].
#For example, to see the data for GA, 10th state in alphabetized data frame

by_state2 %<>% nest
by_state2

####Task 20####
#write a fxn that accepts an element of the by_state2$data list-column and returns the 
#spearman correlation coefficient btwn Lyme disease and precipitation

#
runCor <- function(df){
  suppressWarnings(cor.test(df$cases,df$prcp,method="spearman")$estimate)
}
#adding a spCor column to the by_state2 data
by_state2 %<>% mutate(spCor = purrr::map(data, runCor))
#
spCors <- unnest(by_state2,spCor)
spCors %<>% arrange(desc(spCor))
#this encodes the vector as a factor with unique levels based on state
spCors$state <- factor(spCors$state, levels=unique(spCors$state))
#plots the spearmans rank correlation for each state between lyme disease and precip
ggplot(spCors,aes(state,spCor))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
