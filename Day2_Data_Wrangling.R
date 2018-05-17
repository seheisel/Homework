#15 May 2018
#Sara heisel
#Data Wrangling

#tibble vignette
#read_csv, reads data in as a tibble
#tibble = type of dataframe, but gives more info on each column of dataframe
#load('get-df.Rda')
#tidy data, gather, select, mutate (dplyr package)
#str_replace_all
#paste(base R)
#FIPS codes, another type of zip code - a label for geographic locations (could maybe use to combine lat lon)
#piping, part of magrittr package, can nest them with string or pipe them through - makes code more fluid
#apply fxns to each row (example in presentation)
#apply sum fxn 
#combining datasets, merge is traditional fxn in base R; dplyr fxn use join today, full_join takes column in common and alignes 
#columns not in common

#loading libraries
install.packages("tidyverse")
install.package("magrittr")
install.package("dplyr")
install.package("stringr")
install.package("GGally")
install.package("maptools")
install.package("ggmap")
install.package("maps")
library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
library(ggplot2)


#loading  data
ld <- read_csv(file = "https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/lyme.csv")
View(ld)
pop <- read_csv(file = "https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/pop.csv")
View(pop)
prism <- read_csv(file = "https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/climate.csv")
View(prism)

#Task 2: 
#two columns have different headers, but combine same data
#three columns break up contiguous set of population data
#first row pertain to entire US rather than State or county
#county code needs to be padded so it all contains 3 digits
#data in area name does not all contain same type of data

#Worked example: Census data pop
#manipulate the pop data frame to tidy data format - use 3 fxns from dplyr pkg: select, gather, mutate

#Task 3: 
#retains columns that start with "pop2"
pop %<>% select(fips, starts_with("pop2"))
#removed NAs from data and gathered all of the population data into 1 column
#one directional arrow says "do na.omit" on top of all of the other things
#%<>% is only used to assign everything to the object on far left
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit
#mutate added a new column variable called year and preserved existing variables
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
#this is changing year to an integer
pop %<>% mutate(year=as.integer(year))
#this gets rid of the first zero in front of the fips
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
#this one makes fips an integer
pop %<>% mutate(fips=as.integer(fips))

#Code is now in tidy format, but these two things could be done
#Q: How would you remove state-wide summaries at this stage? - remove rows of data that have fips codes that end with "00"
#A: To select and drop a variable, select is for columns and filter is for rows
#A: pop <-filter(pop,fips %%100 !=0)
#Q: How would you remove the column str_year at this stage? (retain fips, size, year)
#A: pop %<>% select(fips, size, year)

#Lyme disease data
#Write a code chunk to conver Lyme data to tidy format
#combine a state and county code to create unique fips codes
#gathers all the Case data together in two columns, key= creates columsn where headers should go and value= creates
#column where values should go
ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")
#mutate added a new column variable called year and preserved existing variables
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))
#changes year to an integer
ld %<>% mutate(year=as.integer(year))
#creates state and county columns with names
ld %<>% rename(state=STNAME,county=CTYNAME)

#this builds the data for the fips column
#paste only works on characters, so you have to be using characters,
#but this function automatically converts to a character
#paste0 automatically pastes two characters together with no space
#could use that instead of sep="" as below
fips.builder<-function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    3
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}

#creates a column that is fips, which is created from the previous loop
#why do you have to use "rowwise" here
#have to use with fips builder (passing it "to" columns), want to make sure columns
#are aligned - without would be passing whole vector 
#this grouped the rows together (below need to ungroup, so we can group by columns)
ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE))
#getting rid of these columns
ld %<>% select(-c(STCODE,CTYCODE,str_year))

##Combining datasets
#base R has a merge function that works well combining data frames (merge)
#dplyr package has join functions that operate on tibbles - using this method today

#Task 5: Join the Lyme disease data frame and PRISM data frame together to form
#a new data frame, and in such a way that it retains county-year combinations for which
#we have both disease and climate data
#using inner_join bc this only returns data when there are values for both climate
#and disease
ld.prism <-inner_join(ld,prism)
#Joining, by = c("year", "fips")
#Task 6: Write a line of code to additionally combine the demographic data with the Lyme disease and climate
#data.
#have to join 2 at a time, not three
pop.ld.prism <- inner_join(ld.prism,pop)
#Joining, by = c("year", "fips")

#Obtaining summary information
#use summarize function of dplyr package (base R can perform similar duties with aggregate function)

#Write two lines of code that create two new dataframes
#(1) determine how many cases of Lyme disease were reported each year
#(2) average number of cases in each state - averaged across county and year; what was worst year? which 3 states have 
#been most impacted on average?
#this ungroups everything and then regroups by year
#second line summarizes the total number of cases and then arranges them by total in descending values
cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))
#final table grouped all the cases from a given year together and then presented them in a two-column table that has
#total number of cases for a given year
cases_by_year

#2 (from above)Looking at average number of cases grouped by State (this averages across all counties and years)
cases_by_state <- ld %>% ungroup %>% group_by(state) %>%
  summarize(average=mean(cases)) %>% arrange(desc(average))
cases_by_state
#A: states most impacted: CT, MA, NJ
#A: Worst year: 2009

#Saving dataframes as objects and files
#save combined data frame you created in readiness for subsequent module
#Task 8: save to create an Rda file of the data frame and use write_csv to create a csv file of same name
save(pop.ld.prism,file="pop_ld_prism_data.Rda")
write_csv(pop.ld.prism, "ld_prism_pop.csv")
#load("data.Rda")

#Using FIPS and built-in mapping tools to visualize geographic data
#taking advantage of ggplot to map
#Task 9: 
#get map data for US counties and states
#pulling in base maps for county and states
county_map <- map_data("county")
state_map <- map_data("state")

#Most data operations are done on groups defined by variables. group_by() takes an existing 
#tbl and converts 
#it into a grouped tbl where operations are performed "by group". ungroup() removes grouping.
#From: https://www.rdocumentation.org/packages/dplyr/versions/0.7.3/topics/group_by
#So here we are grouping by fips
ag.fips <- group_by(pop.ld.prism,fips)
ag.fips
#summarizing ag.fips data, creating a new column called all.cases that is the sum of the existing cases column
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
#from the pop.ld.prism dataframe, we are joining the state, county, fips to exisiting ld.16y dataframe
#left_join()
#return all rows from x, and all columns from x and y. Rows in x with no match in y will have NA values in the new columns. 
#If there are multiple matches between x and y, all combinations of the matches are returned.
#https://dplyr.tidyverse.org/reference/join.html
#Joining, by = "fips"
ld.16y<-left_join(select(pop.ld.prism,c(state,county,fips)),ld.16y)
#Retain only unique/distinct rows from an input tbl. 
#This is similar to unique.data.frame(), but considerably faster.
#https://www.rdocumentation.org/packages/dplyr/versions/0.7.3/topics/distinct
ld.16y<-distinct(ld.16y)
ld.16y
#this is saying call the column that is now state, region and call the column that is not county, subregion
##WHY IS THIS %<>% here and not <-
ld.16y
ld.16y %<>% rename(region=state,subregion=county)
#this is just removing county from the subregion column in data frame ld.16y
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
#converts all the text in the region column to lower case
ld.16y$region<-tolower(ld.16y$region)
ld.16y
#same thing as above
#can also do toupper
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y
#this creates the log, but first adds a 1 to the zeros, bc can't take log of zero and puts new values in 
#new column called log10cases
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
#Join the county_map and ld.16y data to create map.ld.16y
#returned this: Joining, by = c("region", "subregion")
map.ld.16y<-left_join(county_map,ld.16y)
#
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))


