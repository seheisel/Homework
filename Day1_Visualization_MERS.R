##Sara Heisel
##Data Visualization
#14 May 2018
#installing packages and loading libraries
install.packages("lubridate")
library(lubridate)
library(ggplot2)

#setting working directory and loading datafile
setwd("C:\Users\workshop\Desktop\R")
mers <- read.csv('cases.csv')

#inspecting data
View(mers)
head(mers)
class(mers$onset)
str(mers)

#cleaning data
mers$hospitalized[890] <- c('2015-02-20')
#this line below deletes the row 471 from the dataset
mers <- mers[-471,]
#mersomit <- mers[471,]
#mersomit
mers

mers$onset2 <-ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)

#search for earliest onset date and remove NAs
day0 <- min(na.omit(mers$onset2))
day0
mers$epi.day <- as.numeric(mers$onset2 - day0)

#plotting data
ggplot(mers)

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of 
       MERS cases by date of symptom onset', caption="Data from https://
       github.com/rambuat/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill = country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of 
       MERS cases by date of symptom onset', caption="Data from https://
       github.com/rambuat/MERS-Cases/blob/gh-pages/data/cases.csv")


ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill = camel_contact)) +
  labs(x='Epidemic day', y='Case count', title='Global count of 
       MERS cases by date of symptom onset', caption="Data from https://
       github.com/rambuat/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill = country), position="fill") +
  labs(x='Epidemic day', y='Case count', title='Global count of 
       MERS cases by date of symptom onset', caption="Data from https://
       github.com/rambuat/MERS-Cases/blob/gh-pages/data/cases.csv")

#with flipped coordinates
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill = country), position="fill") +
  labs(x='Epidemic day', y='Case count', title='Global count of 
       MERS cases by date of symptom onset', caption="Data from https://
       github.com/rambuat/MERS-Cases/blob/gh-pages/data/cases.csv") + coord_flip()

#using polar coordinates
##good way to look at seasonality ##use
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill = country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of 
       MERS cases by date of symptom onset', caption="Data from https://
       github.com/rambuat/MERS-Cases/blob/gh-pages/data/cases.csv") + coord_polar()

#Univariate plots

#construct data for infectious period
mers$infectious.period <- mers$hospitalized2-mers$onset2
class(mers$infectious.period)

#setting infectious period to be a numeric variable
mers$infectious.period <- as.numeric(mers$infectious.period, units="days")
class(mers$infectious.period)

#histogram of infectious period 
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period)) +
  labs(x='Infecious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#calculating new infectious period that does not have negative values
mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)
class(mers$infectious.period2)

ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infecious period', y='Count', title='Distribution of calculated MERS infectious period',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Frequency of hospital-acquired infections in MERS using density plot
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2)) +
  labs(x='Infecious period', y='Frequency', title='Probability density for MERS infectious period (positive values only',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Area plot for MERS infections, 
ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infecious period', y='Frequency', title='Area plot for MERS infectious period (positive values only',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#making dot and bar plots
ggplot(data=mers) +
  geom_dotplot(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infecious period', y='Frequency', title='Area plot for MERS infectious period (positive values only',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) +
  geom_bar(aes(x=infectious.period2)) +
  labs(x='Infecious period', y='Frequency', title='Area plot for MERS infectious period (positive values only',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#bivariate plots
#plotting infectious period over time
str(mers)
ggplot(data=mers) +
  geom_smooth(aes(x=epi.day, y=infectious.period2)) +
  labs(x='Epidemic Day', y='Infectious Period', title='Change in Infectious Period over MERS epidemic',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#plotting 
#creating new object with only countries with data
##countryomit <- mers[mers$country == c('South Korea','KSA','UAE','Qatar','Jordan'),]
##View(countryomit)

##plotting infectous period over epidemic by country
#using lines for different countries by assigning linetype = country
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2, color=country)) +
  geom_smooth(method="loess") +
  labs(x='Epidemic Day', y='Infectious Period by country', title='Change in Infectious Period over MERS epidemic',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##FACETING
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping=aes(color=country)) +
  facet_wrap(~country) +
  scale_y_continuous(limits=c(0,50))+
  labs(x='Epidemic Day', y='Infectious Period', title='MERS infectious period (positive values only) over time',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=subset(mers,gender %in% c('M','F') & country %in% c('KSA','Oman','Iran','Jordan','Qatar', 'South Korea','UAE'))) +
  geom_point(mapping=aes(x=epi.day, y=infectious.period2, color=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits=c(0,50)) +
  labs(x='Epidemic Day', y='Infectious Period', title='MERS infectious period (positive values only) over time',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
 
##FATALITY RATE####

#Installing plotly from github
#This isn't working below

install.packages("plotly")
library(plotly)
epi.curve <- ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic Day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
  ggplotly(epi.curve)

#MAKE ONE OF CASE FATALITY PLOTS AN INTERACTIVE GRAPH
  
  


