##Sara Heisel
##Programming
#14 May 2018

#loading ggplot2
library(ggplot2)
#script to load data
wnv <- read.csv(file = "https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv", head=TRUE)
View(wnv)

#log transforming # of cases
str(wnv)
wnv$Year <- as.factor(wnv$Year) 
logCases <- log(wnv$Total, base=10)
logCases


#Create histogram of total number of cases in each state in each year
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=Total)) +
  labs(x='Total number of wnv cases', y='Count', title='Total # of WNV Cases',
       caption = "Data from:https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv ")



#Plotting histogram for log # of cases
#log transformation
logCases <- log(wnv$Total, base=10)
logCases

#Histogram of data log transformed
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=logCases)) +
  labs(x='Total number of wnv cases (log transformed)', y='Count', title='Total # of WNV Cases (log transformed)',
       caption = "Data from:https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv ")

#log transforming inside of plot function
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=log(Total))) +
  labs(x='Total number of wnv cases', y='Count', title='Total # of WNV Cases',
       caption = "Data from:https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv ")



#making new column for case fatality rate
wnv$CFR <- wnv$Fatal/wnv$Total
#plotting a histogram of CFR
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=CFR)) +
  labs(x='Case Fatality Rate', y='Count', title='Case Fatality Rate of WNV',
       caption = "Data from:https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv ")


#Using operators and function sum to verify that the variable Total is simply the sum of # of febrile cases, neuroinv, and other
#yes, this is true for all entries
#or use sum function

##Used logical and arithmetic operator 
wnv$Total == wnv$Fever + wnv$EncephMen + wnv$Other
#why won't this work with the sum function? - returning FALSE for all

#use sum function to compare Total cases and sum of Fever, Enceph, Other
wnv$rowSums <- rowSums(wnv[,3:5])
wnv$Total == wnv$rowSums


#Annual case count for each state rounded down to nearest dozen
##Come back to this
wnv$RoundedTotal <- round(wnv$Total, digits = )

##Write a fxn to calculate mean and standard error of the neuroinvasive disease rate
mean <- function(x){
  s <- sum(x)
  n <- length(x)
  m <- s/n
  return(m)
}

mean(wnv$EncephMen)

stderror <- function(x){
  stdev <- sd(x)
  n <- length(x)
  sqrt <- sqrt(n)
  se <- stdev/sqrt
  return(se)
}

#Subsetting data to just include CA, CO, NY
#look up indexing in R
#also subset by value of column
wnv$EncephMenRate <- wnv$EncephMen/wnv$Total
subset <- subset(wnv,State %in% c('California','Colorado', 'New York'))
CA <- subset(wnv,State %in% c('California'))
CO <- subset(wnv,State %in% c('Colorado'))
NY <- subset(wnv,State %in% c('New York'))
subset
#Calculating average severe disease rate for 3 states
mean(CA$EncephMenRate)
mean(CO$EncephMenRate)
mean(NY$EncephMenRate)
stderror(CA$EncephMenRate)
stderror(CO$EncephMenRate)

##DID NOT FIGURE OUT LOOPS ##CONTINUE TO WORK ON THIS
#Creating loops to look at total # of reported cases
#subsetting years
subset_year <- subset(wnv,Year %in% c('California','Colorado', 'New York'))
i <- wnv$Year
i
j <- 0
for(i in 1:5){
  j[i+1] <- i*2
  print(j)
}

for(prefix in c('b','c','m','r')){
  word <- paste(prefix, 'at', sep='')
  print(word)
}

stderror(NY$EncephMenRate)

x<-filter(wnv, State == wnv$California)
x <- []
mean(x)

mean(wnv$EncephMen)
stderror(wnv$EncephMen)




#Extract rounding errors then add errors to obtain total errors







