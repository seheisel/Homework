##
install.packages("picante")
install.packages("geiger")
install.packages("ape")
library(ape)
library(picante)
library(geiger)
#setwd("~/Desktop/GREVY'S ZEBRA PROJECT/DATA ANALYSIS/R SCRIPTS/CEID course_May2018/Phylogenetic Comparative Methods/Day 1")

ftree <- read.tree("sample_newick_tree.txt")

##with nexus file can include trait data and have all other types of data
##have different commands to read newick vs. NEXUS file
##very often published trees are distributed as NEXUS files



ftree2 <- read.nexus("sample_NEXUS_tree.txt")
plot(ftree)
plot(ftree2)
#trees are phylo objects; 
#plots are the same

str(ftree2)
#structure of the phylo table; edge length: edges: Nnode (number of nodes)
#tip label #list of labels of types - often is species names 
#there are underscores in table, they disappear in tree - underscores rep spaces

#drop.tip
####Tree manipulation####
#how to drop tips from tree
#drop tip is fxn in ape to element species we don't want in tree - drops tip #4
#can also use taxon labels this is A. boreas can reference tip label directly to drop
#(see below)
ftree3 <- drop.tip(ftree, 4)
plot(ftree3)

plot(ftree)
ftree3 <- drop.tip(ftree, "A_boreas")
plot(ftree3)


####Exercise 4####
#dropping tips - getting rid of all Rana genera
plot(ftree)
ftree5 <- drop.tip(ftree, c("R_aurora", "R_cascadae", "R_luteiven"))
plot(ftree5)

#when subtree argument is added, makes notation of where tips were dropped
plot(ftree)
ftree5 <- drop.tip(ftree, c("R_aurora", "R_cascadae", "R_luteiven"), subtree=T)
plot(ftree5)

#also can do by:
#if you want to create a quick chart with node labels
plot(ftree5)
nodelabels()
####Exercise 5 & 6####
#Dropping the outgroup taxa using drop.tip
#outgroup were put in to root the tree
#getting rid of those two tips
#drop the outgroup
plot(ftree)
ftree3 <- drop.tip(ftree, c(21,22))
plot(ftree3)
write.tree(ftree3)
#this was saved using newick format, which has only edge length


#extract clade can be used to create a new tree from a subtree of another tree
##use this to pull out the p and h clades
##use extract clade to pull those clades out and make a new tree out of all 
#this pulls out the root node "29" and then plots that tree
plot(ftree2)
#nodelabels adds the node #s to the tree, so you can visualize which to pull
nodelabels()
#creating a new tree from ftree2 pulling only node 29
#use extract.clade to extract “Pseudacris + Hyla” (node 29) clase from ftree
#export as a NEXUS format text file
ftree3 <- extract.clade(ftree2, 29)
plot(ftree3)
nodelabels()


####NOTES on ASSUMPTIONS####
#more sensible way of pruning tree is to match up to set of data you are 
#interested in analyzing

#lining up data and a tree
#four assumptions of many phylo methods (these notes are in R_phylo_basics_IDEAS word doc):
#examples of how to deal with violations of those assumptions are also given in word doc

####Assumptions of many phylo packages in R####
#data and tree match perfectly (ie., all species in data occur 
#in tree, and the spelling of species labels in data matches 
#that of tip labels)
#tip labels match species names in data perfectly

#no missing data (complete case)

#tree is fully dichotomous

#order of species in data matches that of the tip labels in 
#the tree

####Example of violating assumptions####

#loading sample tree
ctree <- read.tree("canid_tree.txt")
str(ctree)
#number of nodes is far fewer than tip labels, many parts of tree not resolved when you look at tree
#more obvious when looking at tree
#unresolved when nodes are not resolved
plot(ctree)

#can randomly resolve tree using the following command
#tree is not fully resolved
ctree <- multi2di(ctree)
#n of nodes should be one less tahn tip taxa
str(ctree)
#still has zero length branches, this can create problems later, so can 
#create very tiny branches to resolve this later
#edge lengths that got added were all zeros
plot(ctree)

#loading sample data (canid data)
cdata <- read.csv("Canid_traits.csv")
View(cdata)
#lots of missnig trait data; want to line up body mass data with the tree
#so we can do a simple analysis
str(cdata)

#using treedata to get body mass data lined up with tree

#extracting and cleaning body mass data mass
#then bc want to be using it with tree - assign names below
cmass <- cdata$AdultBodyMass_g

#assigning names
names(cmass) <- cdata$Binomial

#eliminating missing data
hist(cmass)
range(cmass)
#there are negative values here, so we can get rid of these negative values
#bc theres are not real mass values
#non-normal distribution - many traits scale wtih body mass, so this should 
#be log transformed or normalized
#take only elements that are greater than zero
cmass <- cmass[cmass > 0]

#log transforming (optional)
##data no longer deviate from normal dist after transformation
hist(cmass)
hist(log10(cmass))
cmass <- log10(cmass)
shapiro.test(cmass)

#aligning the tree with data
#geiger also does a lot of other things
#using treedata
library(geiger)

#use treedata command to line up with data fxn(data,traitdata)
#warning - tips dropped bc some species not found in data, bc we
#got rid of that data due to neg body mass
CMout <- treedata(ctree, cmass)
str(CMout)
#list of data - vector where each element can be something different
#to index the elements use two hard brackets, phylogency is the 1st element in the list
#phylogeny is the first element in the list
plot(CMout[[1]])
hist(CMout[[2]])

#final cleanup
CMtree <- CMout[[1]]
plot(CMtree)

#returns dataframe
CMass2 <- CMout[[2]]
head(CMass2)

#returns vector #need to turn this into a vector
CMass2 <- CMout[[2]][,1] 
#order or data needs to be aligned with tree
head(CMass2)
str(CMtree)

#note that data is not in same order as tip labels
#to fix
#realign data, so it matches tip labels
CMass2 <- CMass2[CMtree$tip.label]

head(CMass2)
str(CMtree)
#can also 

#testing for phylogenetic signal, this 
library(picante)

phylosignal(CMass2, CMtree)
#Here k 

####Exercise 7 & 8####
#Using data and provided tree, create a vector of species that have data
#for age at eye opening and create a tree that matches it
#Fix all 4 of data issues mentioned above in "assumptions"
#see notes on above script to see what each line is doing here

#This pulls only AgeatEyeOpening column from cdata(canid traits)
#at this point - only eye opening data
edat <- cdata$AgeatEyeOpening_d
View(edat)
#This assigns names (genus species (Binomial)) from cdata
names(edat) <- cdata$Binomial
#Now edat is Binomial (genus species) column plus age at eye open. from cdata file
View(edat)
#There are some missing values (represented as -999) in this dataset
#get rid of these by selecting only those greater than 0
edat <- edat[edat > 0]
View(edat)
#above reduced data from 35 to 15 entries

#treedata fxn in package ape - The function returns a list of two elements 
#(phy and data) that are manipulated to include only those species found 
#in both the tree and data supplied by the user
eout <- treedata(ctree,edat)
View(ctree)
View (eout)
#This is assigning the first column of data (genus species) from eout to etree
etree <- eout[[1]]
plot(etree)

#This pulls the first column of data, which is eye opening from eout
##NOT SURE WHY here it is eout[[2]]??
edata2 <- eout[[2]][,1]
edata2

#testing for phylosignal

phylosignal(edata2, etree)
#k=0.58, less phylo signal than with body mass  

##phylogenetic signal: instance where patterns of trait disparities scales with phylogenetic distance
#in a statistical framework: difference in trait values x=delta trait, y=phylo distance
#assume this is a positive correlation = phylo signal
#this is why we have to use phylo comparative methods, but if there is no phylo signal,
#we don't want to use phylo-informed method - this makes it more complicated, but also
#can create more Type II error
##How to test for phylo signal before starting?
#Blomberg's K (use phylo signal command, get K value and p value)
#K = msr of phylo signal that can take a number of values - usually between 0 and 1, bcan
#also be greater than 1
#0= no phylo signal at all; 1 = expected phylo signal per brownian motion; more than 1= 
#greater than expected under brownian motion model (done above)

####R notes####
#vector - list of elements all have to be of same type
#matrix - rows and columns - all data of same type
#dataframe - diff from matrix, rows and columns, can mix data types
#dataset that we've loaded is data frame; 
#can figure out what kinds of objects we have by using str command; tells you vector, dataframe, etc
#and types of data within different types of objects that hold data
#str(cdata)

######################################

####Exercise 9####
#did not do this


#Tree plotting basics
#plotting trait data onto tree - different for continuous and discrete data
#adding a scale bar

plot(ftree)
#branch lengths are in units of sequence divergence
#ape makes it easy to add scale bar
add.scale.bar()
#scale bar that goes all along axis can be done as follows
plot(ftree)
axisPhylo()

#mammal supertree
plot(ctree)
axisPhylo()
#################################
#this creates a phylo tree with body mass on tree in color
####continuous character reconstruction####
#VALUES came from maximum likelihood, think characters are evolving along brownian motion
#tree is not fully resolved, need fully resolved tree to have accurate mapping of traits
#want to pick which model of character evolution fits the traits and figure out what the common 
#ancestors would be
#certainty in ancestral state reconstruction gets much worse in past
#further away from data in the past, the less likely the tree is correct
#also highly sensitive to missing information
#models of evolution - limited options in ape; would test different models of evolution and look at AIC and then
#see which to go forward with for model

#creating a color pallete
mypallete <- c("red","orange","yellow","green","blue","purple")

#creating bins for species traits
#still not sure why doing the + .00001 - min
divider <- (max(CMass2+0.0001)-min(CMass2))/6
#converted species trait values and rounded down
index <- floor((CMass2-min(CMass2))/divider)
#index used to do the tree

#ACE
#ace functions will be messed up if you feed it trees with 0 branch lengths, add small value to each branch of tree
#tree still looks pretty much the same, but converts from 0
CMtree$edge.length <- CMtree$edge.length+0.001
str(CMtree)
#ace = ancestral character reconstruction; feeding in CMass2 and the tree, CI = false 
recon <- ace(CMass2, CMtree, CI = F)
#reconstructed trait values and they appear in same order as tree
str(recon)
#pulling the ancestral traits out
recon.values <- recon$ace
#do rounding trick to convert to bin space
recon.index <- floor((recon.values-min(CMass2))/divider)
str(recon.index)

#creating a plot
#label offset moves tip values out to make room for values
plot(CMtree, label.offset = 0.1, edge.width = 2)
#pch = 21 = circle, 22 = square; cex = size, bg = color, recon.index +1; adds 1 
nodelabels(pch = 21, cex = 2, bg = mypallete[recon.index+1])
#
tiplabels(pch = 21, cex = 2, bg = mypallete[index+1])

#add a legend
#have to make temporary vector called pts - going to feed that into legend
#and trick into making legend of size we want
pts = c(0,1,2,3,4,5)
#topright = position of legend
legend("topright", pch = 21, pt.cex = 2, c("0.00-0.25","0.25-0.50","0.50-0.75","0.75-1.00","1.00-1.26","1.26-1.51"), title = "Log 10 Mass", pt.bg = mypallete[pts+1])

##################################

####Discrete reconstruction####

#getting a continuous character
#getting probabilities for interior nodes and plotting them on a legend

#Activity cycle
AC <- cdata$ActivityCycle
#add names before pulling missing data
names(AC) <- cdata$Binomial
#pull values that are greater than zero, so pulling -999 out
AC <- AC[AC > 0]

#aligning tree and data
#align tree using geiger, resolve all polytomies in tree and add small amount to length of branches
#
out2 <- treedata(ctree, AC)

CAtree <- out2[[1]]
CAtree <- multi2di(CAtree)
#getting rid of zero length branches
CAtree$edge.length <- CAtree$edge.length+0.001
#pull from out2 the first column of the dataframe, then rearrange so species are in same 
#order as tree
CAC <- out2[[2]][,1]
CAC <- CAC[CAtree$tip.label]

#peforming reconstruction
#type d = 
anC <- ace(CAC, CAtree, type = "d")
#for each interior node getting likelihood of ancestral state, pulling out this information
#no binning necessary bc it's already a discrete character
states <- anC$lik.anc

#making the plot
#label offset large, so can get info in
plot(CAtree, edge.width = 2, label.offset = 1)
co <- c("white", "gray", "black")
#if you don't give it any data it labels with tip numbers
#tiplabels() adds to all 
tiplabels(pch = 22, bg = co[as.numeric(CAC)], cex = 2, adj = 1)
nodelabels(pie = states, piecol = c("white", "gray", "black"), cex = 0.5)
axisPhylo()

#adding a legend
pts = c(1,2,3)
legend("topright", pch = 22, pt.cex = 2, c("nocturnal","crepuscular","diurnal"), title = "Activity Cycle", pt.bg = co[pts])

####Mapping trait data onto phylogenies####
#getting a continuous character
#getting probabilities for interior nodes and plotting them on a legend

#Diet Breadth
DB <- cdata$DietBreadth
#add names before pulling missing data
names(DB) <- cdata$Binomial
#pull values that are greater than zero, so pulling -999 out
DB <- DB[DB > 0]

#aligning tree and data
#align tree using geiger, resolve all polytomies in tree and add small amount to length of branches
#
out2 <- treedata(ctree, DB)

CAtree <- out2[[1]]
CAtree <- multi2di(CAtree)
#getting rid of zero length branches
CAtree$edge.length <- CAtree$edge.length+0.001
#pull from out2 the first column of the dataframe, then rearrange so species are in same 
#order as tree
CAC <- out2[[2]][,1]
CAC <- CAC[CAtree$tip.label]

#peforming reconstruction
#type d = 
anC <- ace(CAC, CAtree, type = "d")
#for each interior node getting likelihood of ancestral state, pulling out this information
#no binning necessary bc it's already a discrete character
states <- anC$lik.anc

#making the plot
#label offset large, so can get info in
plot(CAtree, edge.width = 2, label.offset = 1)
co <- c("white", "gray", "black")
#if you don't give it any data it labels with tip numbers
#tiplabels() adds to all 
tiplabels(pch = 22, bg = co[as.numeric(CAC)], cex = 2, adj = 1)
nodelabels(pie = states, piecol = c("white", "gray", "black"), cex = 0.5)
axisPhylo()

##Now work with legend to figure out how to plot different levels of diet breadth - 
#could contain them between 1-4 = vegetation; meat etc - go by definitions
#adding a legend
#diet breadth contains - variables with different non-continuous categorical variables, can rename them
pts = c(1,2,3)
legend("topright", pch = 22, pt.cex = 2, c("no","crepuscular","diurnal"), title = "Diet Breadth", pt.bg = co[pts])