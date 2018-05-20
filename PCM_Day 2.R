#Sara Heisel
#PCM Day 2_CEID course
#17 May 2018
####LECTURE NOTES####
#Simon Joly’s course on Comparative Methods
#good resource
#Graham Slater tutorial is also a good one for macroevol models
#Friday - working on Steven Kembel's workshop on biodiversity in R

install.packages("phytools")
install.packages("brms")
#nlme needs to be loaded before ape if both being used
require(nlme) 
require(ape) 
require(picante) 
require(geiger) 
require(phytools) 
require(mvtnorm) 
require(brms)
#brms is interface to another programming language
#can fit all regression syntax from R and bayesian regression models are run in Stan

##Data is from "Host Longevity and Parasite Species RIchness . .. " Cooper et al 2012

#setting interval length
interval.length <- 1
#fxn seq is from 0 to 100 and then interval.length between them
#rate = rate of evolution - variance term, root = initial y value
times <- seq(0, 100, interval.length); rate <- 0.1
root <- 0
#creatign values for normal deviation
#c = colinaer concatentating --> creating a vecotr, starting with root state - then asking
#for random normal
#arguments for rnorm are
#rnorm arguments = # of random draws, then want 1 less than that to make sure we have 100
#next arg for rnorm is to ask for the mean of the normal distribution
#sd = rate * the interval length
normal.dev <- c(root, rnorm(n=(length(times)-1), mean = 0, sd = sqrt(rate * interval.length)))
#X(t) ∼ N(X(0),σ2t)
#rnorm(100) -- generates 100 random samples from a normal distribution
#can use fxn args to look up what that fxn does
#this generates 1 brownian motion through time
traits <- cumsum(normal.dev);
plot(times, traits, type = "l", xlab = "time", ylab = "trait value", ylim = c(-10,10))

##1000 Brownian motion simulations (root =0 and rate =0.1) over 100 timesteps, plot distribution 
#of trait values at time 100 and calculate mean and variance
#would do this by writing a loop
#can add to the existing plot with the argument lines
#write this with a loop

#might need to create a vector before starting loop to hold data that will be created in loop

t100 <- c(1:100)

for (i in 1:1000) {
  interval.length <- 1
  times <- seq(0, 100, interval.length); rate <- 0.1
  root <- 0
  normal.dev <- c(root, rnorm(n=(length(times)-1), mean = 0, sd = sqrt(rate * interval.length)))
  traits <- cumsum(normal.dev);
  #line below is saying: in t100 object in position i, save the traits100 data
  t100[i]<-traits[100];
  lines(times, traits, type = "l", xlab = "time", ylab = "trait value", ylim = c(-10,10))
}
t100
hist(t100)

#calculating mean and variance of all traits
mean(t100)
var(t100)

  
#could also do the above with a function and call that fxn 1000 times

####non-normal distribution####
#try doing with evol simulations drawn from non-normal distribution
interval.length <- 1

times <- seq(0, 100, interval.length); rate <- 0.1
root <- 0
#need to find non-normal distribution
#would swap out below for non-normal
normal.dev <- c(root, pnorm(n=(length(times)-1), mean = 0, sd = sqrt(rate * interval.length)))
traits <- cumsum(normal.dev);
plot(times, traits, type = "l", xlab = "time", ylab = "trait value", ylim = c(-10,10))


####BM on a phylogenetic tree####
#ultrametric tree - tips all line up at zero
#to get utlrametric tree need to time calibrate, so trees out of mega, etc would not be ultrametric
#to time calibrate need fossils, etc or can massage to do this, by rescal branch lengths, rate smoothing
#is one way to do this
#these are tips of tree
#variance in brownian motion is = rate x time
#variation for given 
#how to model variance between species - what distances do we think about then?
#distance to nearest node, this is relative to point at which two species split
#difference in trait values will come at independent parts of evolution, 
#variance is variance after split
#covariance is amount of shared evolutionary history
#to work with this want to pull out variance covariance matrix (vcv) fxn
phy <- "(((t1:0.15,t2:0.15):0.4,t3:0.55):0.5,(t4:0.25,t5:0.25):0.8);"
phy <- read.tree(text=phy)
plot(phy, label.offset=0.05) 
#add edge label values
edgelabels(c(0.5,0.4,0.15,0.15,0.55,0.8,0.25,0.25),adj=c(0.3,-0.3),frame="none",bg="",cex=0.8) 
axisPhylo() 
# put up a scale bar
#this give us a table of 
#variance is diagonal
#covariance: for instance t1 and t2 covary by .9 
vcv(phy)

#this returns phylogenetic distance among pairs of species
cophenetic(phy)
#divide by 2  you get distance from that species to most recent common ancestor
cophenetic(phy)/2

####Simulating brownian motion on a tree
require(phytools)
set.seed(100)
d <- fastBM(tree=phy, a=root, sig2=rate)
d

#this shows that expected value of normal distribution sp will be closely related, but
#for given trait they don't have to be
phenogram(phy,d,spread.labels=TRUE)
fancyTree(phy,x=d, type = "phenogram")
#blue lines around root state are values that it could take to lead to tips (under
#brownian model)
#density of blue lines is represenative of probability of it being one
#of those values - these are max likelihood 
#can also estimate what parameters based on traits we observe - going backwards to 
#look at what parameters should be

####Challenge #2####
# to calculate probability density - given some value of root and rates
#use dmvnorm  - multivariate distribution - find value of root and rate that
#give us best fitting root and rate 
#essentially need to loop through different possible values of root and diff
#possible values of rate - calculate density and find parameters that maximize that

#searching parameters - values of root and rate
#use dmvnorm to calculate density
#x=traits values
#mean value
#variance
#two loops one testing value of root and one testing value of rate

v <- vcv(phy)
require(mvtnorm)
# we can use dmvnorm to compute the likelihood of getting our data 
#with our actual values
dmvnorm(x=d, mean = rep(root, length(phy$tip.label)), sigma = v * rate, log = T)

#Varying root
####LOOP####

rootvary <- seq(-5,5,by=0.01)
rootvary
for (i in 1:length(rootvary)) {
  density[i] <- c(dmvnorm(x=d, mean = rep(rootvary[i], length(phy$tip.label)), sigma = v * rate, log = T))
}
density
droot
max(density)
#return index or boolean op for max density
length(density)
length(rootvary)

density==max(density)
sum(density==max(density)) #only one
which(density==max(density))
rootvary[density==max(density)]

#Varying rate (sub in rate here and RUN)
rootvary <- seq(-5,5,by=0.01)
rootvary
for (i in 1:length(rootvary)) {
  density[i] <- c(dmvnorm(x=d, mean = rep(rootvary[i], length(phy$tip.label)), sigma = v * rate, log = T))
}
density
droot
max(density)
#return index or boolean op for max density
length(density)
length(rootvary)

density==max(density)
sum(density==max(density)) #only one
which(density==max(density))
rootvary[density==max(density)]

##Can look up nested for loops in R to look at optimal combo of root and rate

####FITTING EVOLUTIONARY MODELS####
##because only fitting 2 variables would use AICc, bc corrects for small sample size
#can access these directly from output
#This is essentially doing what we did above with loops 
require(geiger)
bm <- fitContinuous(phy=phy, dat=d, model="BM")
bm

bm$opt$sigsq #this is rate
bm$opt$z0 #this is root state
bm$opt$lnL #this is log likelihood
bm$opt$k # is the number of free estimated parameters
bm$opt$aicc # aicc value

####Phylogenetic Independent Contrasts####
#looking at evolutionary correlations, when X changes, Y tends to change in predictable way - according to
#tree structure


library(phytools) 
set.seed(999)
## simulate a coalescent shaped tree 
tree<-rcoal(n=100) 
#most speciation is towards tips of tree
plotTree(tree,ftype="off")

#simluate uncorrelated brownian motion
x<-fastBM(tree, a=0, sig2=1) 
y<-fastBM(tree, a=0, sig2=1) 
plot(x,y,pch=20)
fit<-lm(y~x) 
abline(fit)

summary(fit)
#bc above traits were generated independently, y does not depend on x is this case, but plot shows
#strong relationship, so we see a high Type I error rate here - random association between 
#Type I = wolf isn't there, but you say it is
#Type II = wolf is there, but you say it isn't

#Not difficult from phylo relationships to see Type I error

#Joe Felsenstein - developed independent contrasts
#looking at value of trait not just at tips, but at internal nodes - looking at change occuring btwn
#two different species

#Logic . . . Trait A and Trait B - Trait A differs by x, Trait B differs by y
#differences between two different sets of species - how to compare differences? 
#look at differences in trait values - do regression on independent contrasts, which gets at average value
#at each node
#sample size will always be one less than with a normal regression, bc there is always one less node than tips
#EXAMPLE: head size, beak size (on paper)
####Independent contrasts####
#this takes into accoutn independent contrasts (improves Type I error rate by taking indep contrast into account)

ix<-pic(x, tree, scaled=TRUE)
iy<-pic(y, tree, scaled=TRUE)
plot(ix,iy,pch=20)
fit<-lm(iy ~ ix - 1) ## we have to fit the model without an intercept term (this treats th abline(fit)
abline(fit)
summary(fit)

#data is x  <- (iy)^2
iysq <- (iy)^2
iysssum <- sum(iysq)
divy = iysssum/100
div


bmx<- fitContinuous(phy=tree, dat=x, model="BM")
bmx$opt$sigsq

ixsq <- (ix)^2
ixsssum <- sum(ixsq)
divx = ixsssum/100
divx


bmy<- fitContinuous(phy=tree, dat=y, model="BM")
bmy$opt$sigsq

####Challenge####
#Show that the sum of the squared contrasts divided by n gives the ML estimate of σ2
#can get sum of sq with pic function

####Cleaning Cooper data####
#dat <- read.csv("Cooper_2012.csv", as.is=T)
#fritz_tree <- read.nexus("Fritz_2009.tre")[[1]]

# Remove species for which we don't have complete data
#dat <- na.omit(dat)

# Match data to tree names
#species.to.exclude <- fritz_tree$tip.label[!(fritz_tree$tip.label %in% dat$Species_W.R05)]
#tree <- drop.tip(fritz_tree,species.to.exclude)

# Order tree to make it nicer when plotting
#tree <- ladderize(tree, right = FALSE)

# Name the rows of dat with the species codes remove obsolete columns
#rownames(dat) <- dat$Species_W.R93
#dat <- subset(dat, select=-c(Species_W.R05,Species_W.R93))

# Check that the order of the tree and data match
name.check(tree, dat)

# Great! Time for analysis!

#wbcPic <- pic(wbc, tree, scaled=TRUE)
#massPic <- pic(log(mass), tree, scaled=TRUE)


####From Nikki - errors removed ####
# challenge 

dat <- read.csv("Cooper_2012.csv", as.is=T)
View(dat)
fritz_tree <- read.nexus("Fritz_2009.tre")[[1]]

# Remove species for which we don't have complete data
dat <- na.omit(dat)

# Match data to tree names

#add an underscore between genus and species name
dat$Species_W.R05 <- sub(" ", "_", dat$Species_W.R05)

#exclude the species that do not have the same label as the 2005
species.to.exclude <- fritz_tree$tip.label[!(fritz_tree$tip.label %in% dat$Species_W.R05)]
tree2 <- drop.tip(fritz_tree,species.to.exclude)
plot(fritz_tree)

# Order tree to make it nicer when plotting
tree2 <- multi2di(ladderize(tree2, right = FALSE))

# Name the rows of dat with the species codes remove obsolete columns
rownames(dat) <- dat$Species_W.R05
dat <- subset(dat, select=-c(Species_W.R05,Species_W.R93))

# Check that the name of the tree and data match
name.check(tree2, dat)

#checking order that names of species are in
head(rownames(dat))
head(tree2$tip.label)
#not in same order, order all the stuff in dat the same as the ones in tree
dat <- dat[tree2$tip.label,]

# Great! Time for analysis!

#assign the white blood cell data an object to calculate contrasts from
wbc <- dat$WBC
wbcPic <- pic(wbc, tree2, scaled=TRUE)

#calculate the mean
mean(wbcPic)

#assign the body mass data an object to calculate contrasts from
mass <-dat$AdultBodyMass_g
massPic <- pic(log(mass), tree2, scaled=TRUE)

#calculate the mean
mean(massPic)

####NOTES####
#Can do flexible correlation structures by using Pagel's lambda
#create a correlation matrix (same thing can be used for spatial autocorrelation)
#OLS - everything is independent 
#Lambda transformation pulls tips of tree back in time; lambda of 1 is original tree tips
#L 0.5 pull tips back so there's more time for independent evolution
#L 0.1 pulls tips back even further, at lambda of 0 = star phylogeny, all species emerged at one time and 
#evolved completely independently of each other (this is same as ordinary least squares)
#lambda is an extension of brownian movement
#lambda is a measure of phylogenetic signal - adjsuting for amount of phylogenetic signal that we see in our data
#at one extreme - phylo signal is pure brownian
#can measure phylo signal using fit continuous model
##see if there is phylogenetic signal in residuals (do this same thing for spatial auto) - 
#figure o
#if no phylogenetic signal in residuals, should not use phylo informed signal, then should just use OLS regression
#when we allow lambda to be estimated from the data, we let the data tell us whether we should apply phylogenetic methods
####Nikki's code####

vcv(phy)

# Convert the covariance matrix to a correlation matrix
corrmat <- cov2cor(vcv(phy))
View(corrmat)
# Print the matrix, rounding the numbers to three decimals
round(corrmat,3)
corrmat <- vcv(phy,corr=TRUE)
round(corrmat,3)

require(nlme)
wbc.gls <- gls(WBC ~ log(AdultBodyMass_g), data=dat)
summary(wbc.gls)

#make your own tree with the tip labels equivalent to the absolute 
#value of the residuals

#assign an object all of the residual values
resi <- as.numeric(residuals(wbc.gls))

#create temp vector that has ones 
temp <- rep(1,213)

#change those values where the residual is less than 
#zero. This will help us assign a color to the circles
temp[which(resi>0)] <- 2
temp

#create a color palette for the tree
co <- c("purple", "orange")

#plot the tree
plot(tree2, show.tip.label = FALSE)
tiplabels(pch = 21, cex= abs(resi)/2, bg=co[temp])

#add a legend
co4 <- c("purple", "purple", "white", "orange", "orange")
pts = c(1,2,3,4,5)
legend("topleft", pch = 21, pt.cex = abs(resi)/2, c("-4","-2", "0", "2", "4"),
       pt.bg = co4[pts])

####Fit Lambda model####

#pull out WBC data and attach row names
wbc <- dat$WBC
names(wbc) <- rownames(dat)
head(wbc)

#use new wbc vector to run lambda model
lamb <- fitContinuous(phy=tree2, dat=wbc, model="lambda")
lamb
#estimated lambda = .9632

str(wbc)
str(tree2)

#Plotting with lambda as 0.5
#underlying model is Brownian motion, other option is to set lambda (which sets time for independent evolution)
#use rescale fxn to set lambda, different ways to rescale with different evolutionary models
newlamb <- rescale(tree2, model="lambda")
plot(newlamb(0.5))
#use AIC values to determine whether favored over BM model

#do fit continuous - 

#Implementing lambda transformation in PGLS framework - give tree, fixed = FALSE, then you will be estimating the lambda 
#value

pagel.corr <- corPagel(0.3,phy=tree2, fixed=FALSE)
pagel.corr
#here lambda = 0.3
#when fixed = true, would keep parameter at 0.3,but when False it will estimate it

#if you do fixed, it just keeps the estimate at 0.3
#if you have a tree structure you can do this, you don't have to know if the residuals
#are phylogenetically structured

#
pagel.corr <- corPagel(0.3,phy=tree2, fixed=TRUE)
pagel.corr

#often people don't have phylogenies - then peopel might add in taxonomic group as 
#some sort of predictor, what to do here is to see if that corrects for phylogenetic non-independence
#add in "primates", etc - include that as co-predictor and compare to OLS model without order
#included
####Fitting with taxonomic info####
linearorder <- lm(WBC ~ log(AdultBodyMass_g) + Order, data=dat)
summary(linearorder)
plot(linearorder)


tax.gls <- gls(WBC ~ log(AdultBodyMass_g) + Order, data=dat)
summary(tax.gls)

tax.glswo <- gls(WBC ~ log(AdultBodyMass_g), data=dat)
summary(tax.glswo)
#get residuals and calculate lambda


##pgls with and without order
wbc.pgls <- gls(WBC ~ log(AdultBodyMass_g), data=dat, correlation=pagel.corr)
summary(wbc.pgls)

####Comparing residuals####
####code from NIkki#### need to change names to run for my work
#notes on paper
#adding ORDER to gls, does this account for phylo signal?
#to see, look at residuals, is lambda reduced by adding Order? 
#some

#pgls model with 
wbc.pgls2a <- gls(WBC ~ log(AdultBodyMass_g), data = dat, correlation=pagel.corr)
summary(wbc.pgls2a)
#lambda =0.3

#OLS regression without Order
modelOLS <- lm(WBC ~ log(AdultBodyMass_g), data=dat)
summary(modelOLS)
modelOLS

# OLS regression with Order 
modelOLSo <- lm(WBC ~ log(AdultBodyMass_g) + Order, data=dat)
summary(modelOLSo)
modelOLSo


#Residuals from OLS with order added, but not accounting for phylo correlation
resid1 <- resid(modelOLSo)
resid_lambda <- fitContinuous(tree2, resid1, model="lambda")
resid_lambda
#lambda = 0.955, phylo signal not accounted for based on labmda still being high

#Residuals from gls model with correlation structure
resid2 <- as.vector(resid(wbc.pgls2, 'normalized'))
names(resid2) <- rownames(dat)
resid2_lambda <- fitContinuous(tree2, resid2, model="lambda")
resid2_lambda
#lambda should be zero - thought this should be zero
####Question####

####There is one extra challenge here to look at corr str between PSR and Adult Body Mass
#do this one

PSRmodel <- gls(PSR ~ log(AdultBodyMass_g), data = dat, correlation=pagel.corr)
summary(PSRmodel)

####
#call with read.tree
##call out individual trees with [[]]

####When to use PGLS####
#if you have signal in predictor and in response,, can get no signla in residuals
#similarly if no signal in predictor and response, can get signal
#that' why we need to test residuals
#Revell 2010 - paper 

#there are papers about this
#If you want to estimate variation in white blood cell count, 
# if you have thousands of different trees, can run through each of the many trees
#, then get a range of best fit lambda values

####Other models####
####Look at this for my stuff####
####There is apparently a vignette for this####
####Fitting Bayesian models####
####Can fit flexible models####
#on max's github - intro to Stan for ecologists
#Can also treat species as a random effect
#Mixed Model framework - can build up slowly all the different sources of error 
#MCMC glmm (new package)- Bayesian 
#Stan
#one way to accoutn for phylo non-independence - can add in variation between sites, 
#individuals, etc
#great way to fit flexible models
#(1|species) = random effect term
#passing corr_phy = correl matrix
#for all parameters in model, get a distribution of values that value could take
#Bayesian methods - setting priors - can set flat prior or uninformative prior to get around this

Faurby <- read.nexus("Faurby_2015_100trees.nex")
View(Faurby)
plot(Faurby)

