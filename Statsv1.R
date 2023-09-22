library(dplyr)
library(tidyr) # for spliting on the period see below
library(moments) # for calculating moments for skewness etc.
library(reshape2)
library(ggplot2)
library(ggcorrplot)
par(mfrow=c(1, 1)) 

df <- read.csv("M:\\pc\\desktop\\Stats in R\\proportional_species_richness_V3.csv")
#Load in the data set being analysed in this project. 
#This data set has already had some data cleaning done to it, namely the removal of missing values.

#Proj_data = df

names(df)
str(df)
df$period <- as.factor(df$period) 
df$dominantLandClass <- as.factor(df$dominantLandClass)

#My7: Bees, Bryophytes, Butterflies, Isopods, Macromoths, Grasshoppers_._Crickets, Vascular_plants
#Other4: Bird, Carabids, Hoverfliers, Ladybirds

all <- c(2:12)
eco_selected <- c(df$Bees, df$Bryophytes, df$Butterflies, df$Isopods, df$Macromoths, df$Grasshoppers_._Crickets, df$Vascular_plants)
eco_selected <- c(2,4,5,8,10,11,12)
eco_not_selected <- all[!(all%in%eco_selected)]
eco_names <- names(df[,2:12])
eco_selected_names <- names(df)[eco_selected]
eco_selected_names

# calculate the bio div measure over 7 taxinomic groups
bd7 <- rowMeans(df[,eco_selected],na.rm=TRUE) # mean the 7 columns 
# add in the biodiversity measure which is the mean over 7 taxonomic groups
#Proj_data_MA334 = df_Proj
df_Proj <- df%>%mutate(eco_status_7=bd7)
names(df_Proj)


##### EDA

df_means <- aggregate(cbind(Bees, Bryophytes, Butterflies, Isopods, Macromoths, 
                            Grasshoppers_._Crickets, Vascular_plants) ~ period, data = df, mean)
df_change <- df_means %>% 
  gather(key = "variable", value = "value", -period) %>% 
  group_by(variable) %>% 
  summarize(percent_change = ((value[period == "Y00"] - value[period == "Y70"]) / value[period == "Y70"]) * 100)
df_change
(40.9+0.7+14-9.11-34.8+11.6-7.47)/7
#Between the two periods there has been major changes in the level of species richness.
#For my7 there has been variations between -35% for Isopods, and +41% for Bees across all hectads
#the mean change in SRichness across my7 is a small increase of 2.26%

# you could split the data by period and compare these stats before and after 
table <- data.frame()
for(i in eco_selected){
  table <- rbind(table,
                 c(eco_names[i-1],
                   round(mean(df_Proj[,i],na.rm = TRUE),digits = 2),
                   round(sd(df_Proj[,i],na.rm = TRUE),digits = 2),
                   round(skewness(df_Proj[,i],na.rm = TRUE),digits = 2)
                 ))}
colnames(table) <- c("taxi_group","mean","sd","skewness")
table%>%arrange(sd,skewness) # something more could be done here 

mean(as.numeric(table$mean))
# extend data exploration; with correlations between continuous variables
cont_vars <- df_Proj%>%select(c(eco_selected,13,14)) # includes easting and northing 
names(cont_vars)
cormat <- round(x = cor(cont_vars,use="pairwise.complete.obs"), digits = 2)
# melt the correlation matrix
melt(cormat)%>%mutate(R2 = value^2)%>%arrange(value)
melt(cormat)%>%mutate(R2 = value^2)%>%arrange(Var1,value)

corrM <- cor(cormat)
ggcorrplot(corrM, type = "lower", outline.col = "white", lab = TRUE, lab_size = 3, colors = c("#6D9EC1", "white", "#E46726"))

########### Think it is here we stop looking at singluar variables but start analysing all BD7
par(mar = c(1, 1, 1, 1))
mlr <- lm(df_Proj$eco_status_7 ~ df$dominantLandClass)
summary(mlr)
plot(df$dominantLandClass, df_Proj$eco_status_7,col="red", xlab="Land Class", ylab="BD7")
plot(mlr)
#Surprisingly 12 of the land classes appear to have little effect on the bd score
#However, the remaining land classes definitely impact on bd scores, with the overall analysis
#showing that land classification has a strong influence on bd scores. 

# now use the eastings and northings (these may be better used as predictors )
cor(df_Proj$eco_status_7,df_Proj$Easting)
cor(df_Proj$eco_status_7,df_Proj$Northing)
# Create a subset of the data with only the columns you want to use
df_sub <- df_Proj[, c("eco_status_7", "Easting", "Northing")]
# Calculate the correlation matrix for the subset
corr_matrix <- cor(df_sub)
# Create the first ggcorrplot showing correlation between eco_status_7 and Easting/Northing
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, lab_size = 3, colors = c("#6D9EC1", "white", "#E46726")) +
  ggtitle("Correlation Between eco_status_7 and Easting")

#The bd7 has a noticable negative correlation with Northing (-0.34)
#However, there is almost no correlation at all with Easting. 

# doing a linear regression with only Northing as a predictor 
lin_mod <- lm(df_Proj$eco_status_7~df$Northing)
summary(lin_mod)
abline(lin_mod,col="green")
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqline(lin_mod$residuals,col="red")

#After a regression analysis it is clear Northing is a strong influencing factor on bd7
#Additionally, looking at the residuals we can see that while there are some outliers in the dataset
#even these are well within limits and are reasonably close to 0 (0.4 being the furthest away)


# following code splits between the two periods to find the BD7 change
# however it may be better to use period as a predictor 

# box plot comparisons for the two periods ignoring all other varaibles 
eco_status <- df_Proj%>%pull(eco_status_7)
eco_period <- df_Proj%>%pull(period)
plot(eco_status~eco_period)

#comparing boxplots of the bd between the two periods shows there is much greater variance 
#in Y00 with a large number of outliers both above and below the tails of the boxplot. 

df_Proj_period <- df_Proj%>%select(Location,period,eco_status_7)
df_Proj_split <- df_Proj_period%>%pivot_wider(names_from =period,values_from=eco_status_7)
df_Proj_split <- df_Proj_split%>%mutate(BD7_change=Y00-Y70)
head(df_Proj_split)
hist(df_Proj_split$BD7_change)  # the distribution of the BD7 change 
abline(v = mean(df_Proj_split$BD7_change), col = "red")

BD7_change <- df_Proj_split%>%pull(BD7_change)
t.test(BD7_change,mu=0)  # t test with H0: mu=0

#looking at the mean of the bd change between the two periods for bd7 shows
#there has been a very slight increase in bd of 0.01% over the period for my7

# comparing the two distributions of bio div based on 7 and 11 taxonomic groups 
qqplot(df_Proj$eco_status_7,df_Proj$ecologicalStatus)
abline(0,1,col="red")
# both cdfs together  and do a kolmogorov test H0: distributions are the same
BD7_cdf <- ecdf(df_Proj$eco_status_7)
BD11_cdf <- ecdf(df_Proj$ecologicalStatus)
plot(BD11_cdf,col="red")
lines(BD7_cdf,col="green")
ks.test(df_Proj$eco_status_7,df_Proj$ecologicalStatus)

#conducting a ks test on bd7 and bd11 to establish if the distributions are the same
#shows that they do not share the same distribution

# Simple linear regression part of the specified assignment
# regressions of eco_status_7 against ecologicalstatus based on all 11
plot(df_Proj$eco_status_7~df_Proj$ecologicalStatus)
abline(0,1,col="red")
lin_mod <- lm(df_Proj$eco_status_7~df_Proj$ecologicalStatus)
abline(lin_mod,col="green")
summary(lin_mod)
#doing lr on bd7 against bd11 shows that bd11 is a strong indicator of bd7
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
#studdying the residuals we can see that they are very close to 0 (within 0.15) suggesting this is an effective model
qqnorm(residuals(lin_mod))
qqline(residuals(lin_mod),col="red")

# do the same for each period report and differences 
df_Proj_Y70 <- df_Proj%>%filter(period=="Y70")
lin_mod <- lm(df_Proj_Y70$eco_status_7~df_Proj_Y70$ecologicalStatus)
lin_mod$coefficients
summary(lin_mod)
# for later period 
df_Proj_Y00 <- df_Proj%>%filter(period=="Y00")
lin_mod <- lm(df_Proj_Y00$eco_status_7~df_Proj_Y00$ecologicalStatus)
lin_mod$coefficients
summary(lin_mod)
#In Y70 bd11 served as a better indicator of bd7 than in Y00. This suggests that there has been a greater
#level of divergence between the two, as bd11 explains less of the variance in bd7 30 years later.  

# linear regression of BD4 on BD7 
mean_selected <- rowMeans(df[,eco_not_selected ],na.rm=TRUE) # mean the rem 4 columns 
# add in the biodiversity measure which is the mean over 7 taxonomic groups
df_Proj <- df_Proj%>%mutate(eco_status_4=mean_selected)
names(df_Proj)

# regressions of means: eco_status_4 against others not inc eco_status_4 data
plot(df_Proj$eco_status_4~df_Proj$eco_status_7)
abline(0,1,col="red")
lin_mod <- lm(df_Proj$eco_status_4~df_Proj$eco_status_7)
summary(lin_mod)
abline(lin_mod,col="green")
#bd7 is an effective predictor variable for bd4, but it does not explain the majority of the variance for bd4
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
#good model, low variance in residuals (less than |3| at |0.4| being the most)
qqnorm(residuals(lin_mod))
qqline(residuals(lin_mod),col="red")

# now multiple linear regression BD4 against the selected 7 

# Create Training and Test data 
trainingRowIndex <- sample(1:nrow(df_Proj), 0.8*nrow(df_Proj))  # row indices for 80% training data
trainingData <- df_Proj[trainingRowIndex, ]  # model training data
testData  <- df_Proj[-trainingRowIndex, ]%>%na.omit # for test data remove NAs 

# Build the model on training data
lmMod_train <- lm(eco_status_4~.,
                  data=trainingData[c(eco_selected_names,"eco_status_4")],
                  na.action=na.omit,y=TRUE)
summary (lmMod_train)  # model summary
cor(lmMod_train$fitted.values,lmMod_train$y) # cor training data 
Eco_4_Pred <- predict(lmMod_train, testData) # predict to check model on test Data
cor(Eco_4_Pred,testData$eco_status_4) #uses the test data as the predictor for the predicted values for bd4
plot(Eco_4_Pred~testData$eco_status_4) 
abline(0,1,col="red")
summary(lm(Eco_4_Pred~testData$eco_status_4)) #can use bd7 to predict bd4, and it be closely matched to the actual bd4
# mis_fit_to_testData are the residuals for the train model fit to the test data 
mis_fit_to_testData <- testData$eco_status_4-Eco_4_Pred
plot(mis_fit_to_testData~Eco_4_Pred) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction
qqline(mis_fit_to_testData,col="red")
#creates a model to predict the missing bd4 data from the bd7 data

# multiple linear regression BD7 against period, easting and northing 
mult_lin_mod <- lm(eco_status_7~.,
                   data=df_Proj[c("eco_status_7",
                                          "period","Easting","Northing")],
                   na.action = na.omit,y=TRUE)
summary(mult_lin_mod)
plot(mult_lin_mod$fitted.values~mult_lin_mod$y)
abline(0,1,col="red")
plot(jitter(fitted(mult_lin_mod)),residuals(mult_lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(mult_lin_mod))
qqline(residuals(mult_lin_mod),col="red")

# compare the effect of each significant coefficient to that of period
mult_lin_mod$coefficients
as.numeric(mult_lin_mod$coefficients[3])*mean(df_Proj$Easting)
as.numeric(mult_lin_mod$coefficients[4])*mean(df_Proj$Northing)
#DONT REALLY KNOW WHAT THIS MEANS








# The following PCA method is an extension to the set book 
# PCA for visualizing the multi-dimensional spread of biodiversity values #######################

table(df_Proj_Y70$period); table(df_Proj_Y00$period) # check that these separate periods 
table(df_Proj_Y00$Location==df_Proj_Y70$Location) # check that Locations correspond between the two periods

eco_difference <- df_Proj_Y00[,eco_selected ]-df_Proj_Y70[,eco_selected ] # general differences between the two periods 
head(eco_difference)

# see ?prcomp the default here is the mean correct but not to scale 
pr.out=prcomp(na.omit(eco_difference)) # Principal Components 
pr.out$center  # gives the mean corrections the "centers"
pr.out$scale  # not scaled
pr.out$rotation[,1:2] # print out first two principal axes
screeplot(pr.out, type="lines") # plot the variances in decreasing order
plot(pr.out$x[,1],pr.out$x[,2]) # scatter plot for first two principal components
text(pr.out$x[,1],pr.out$x[,2], df_Proj_Y00$dominantLandClass, cex=0.5, pos=4, col="red") # location labels

# label by location 
plot(pr.out$x[,1],pr.out$x[,2]) # scatter plot for first two principal components
text(pr.out$x[,1],pr.out$x[,2], df_Proj_Y00$Location, cex=0.4, pos=4, col="red") # location labels

# label by eco increase 
plot(pr.out$x[,1],pr.out$x[,2]) # scatter plot for first two principal components
BD_inc  <- Proj_data_MA334_Y00$eco_status_7-df_Proj_Y70$eco_status_7 # BD differences between the two periods 
text(pr.out$x[,1],pr.out$x[,2], round(BD_inc,2), cex=0.4, pos=4, col="red") # location labels

# label by a particular taxi group (if any) dominant in the first PC 
eco_selected <- c(2,4,5,8,10,11,12) # an example Bees !
eco_difference <- df_Proj_Y00[,eco_selected ]-df_Proj_Y70[,eco_selected ] # general differences between the two periods 
pr.out=prcomp(na.omit(eco_difference)) # Principal Components 
pr.out$rotation[,1:2] # print out first two principal axes
screeplot(pr.out, type="lines") # plot the variances in decreasing order
plot(pr.out$x[,1],pr.out$x[,2]) # scatter plot for first two principal components
text(pr.out$x[,1],pr.out$x[,2], round(eco_difference$Bees,2), cex=0.5, pos=4, col="red") # location labels

####

Proj_data <-  read.csv("M:\\pc\\desktop\\Stats in R\\proportional_species_richness_V3.csv") # you use V2 or V3
Proj_data$period <- as.factor(Proj_data$period) # must set categorical vars
Proj_data$dominantLandClass <- as.factor(Proj_data$dominantLandClass)

# Proj_data_selected  <- Proj_data%>%filter(dominantLandClass=="7w") # Sea cliffs/hard coast, Wales
Proj_data_selected  <- Proj_data%>%filter(dominantLandClass=="7e") # Sea cliffs/hard coast, England
# Proj_data_selected  <- Proj_data%>%filter(dominantLandClass=="6w") # Complex valley systems/table lands, Wales

#  Proj_data_selected <- Proj_data%>%filter(dominantLandClass=="8e") # Estuarine/soft coast/tidal rivers, England
# Proj_data_selected <- Proj_data%>%filter(dominantLandClass=="12e")  # Large river floodplains, flat plains, margins, E Anglia

# Proj_data_selected <- Proj_data%>%filter(dominantLandClass=="24s")  # Steep valley sides/intermediate mountain tops, W Highlands


BD_measures <- na.omit(Proj_data_selected) # need to remove NAs (or replace them) in a rotation
head(BD_measures[,2:12])
nrow(BD_measures)  # BD_measures still contains the periods in a single variable

# see ?prcomp the default here is the mean correct but not to scale 
pr.out=prcomp(BD_measures[,2:12]) # Principal Components 
pr.out$center  # gives the mean corrections; the "centers"
pr.out$scale  # not scaled
pr.out$rotation[,1:2] # print out first two principal axes
screeplot(pr.out, type="lines") # plot the variances in decreasing order
plot(pr.out$x[,1],pr.out$x[,2],col=BD_measures$period, cex=0.7,pch=16) # scatter plot for first two principal components
text(pr.out$x[,1],pr.out$x[,2], BD_measures$period, cex=0.4, pos=4, col="red") # location labels

pr.out$rotation[,1:2] # the first two principal directions 

# watch out: the signs of the principal axes are not defined !
# to interpret the sign you could look at a dominating taxi group via boxplots:
plot(BD_measures$Macromoths~BD_measures$period) # 7w & 7e :bad news for Isopods/Carabids (and us !)
plot(BD_measures$Isopods~BD_measures$period) # 8e & 12e :bad news for Isopods (and us !)
plot(BD_measures$Bees~BD_measures$period) # bees often increase but see 6w 
plot(BD_measures$Vascular_plants~BD_measures$period) # RE 12e 

