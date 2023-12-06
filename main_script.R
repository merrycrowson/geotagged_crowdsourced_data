#########################################################################################################################################
#### Merry Crowson 23/11/2023
#### Code for the puclication Crowson et al. (2023) "Using geotagged crowdsourced data to assess the diverse sociocultural values #######
####  of conservation areas: England as a case study", Ecology and Society ##############################################################
#########################################################################################################################################


# Install and load packages

#install.packages("ggplot2")
#install.packages("rgdal")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("rsq")
#install.packages("DHARMa")

library(ggplot2)
library(rgdal)
library(dplyr)
library(tidyr)
library(rsq)
library(DHARMa)


#setwd("")

# Load data

my_data <- readRDS("./wiki_flickr_designated_area_data.rds") # The data for this code is available at the University of Reading
# data repository. Please see the data and code availability statement in Crowson et al. 2021 to find the DOI and link to the data


### 1) Test for independence between the Flickr data and the Wikipedia data ----

source("./create_contingency_table.R")# Creates a contingency table m for the Flickr and Wikipedia data

chisq.test(m,correct=F) # Pearson’s chi-squared test for independence

 
# Spearman’s Rank correlation was used to assess the direction and strength of correlation between the Flickr and Wikipedia data.

cor.test(as.numeric(my_data$Fli_T_F),as.numeric(my_data$wiki_T_F), method = "spearman")


### 2) Binomial generalized linear models with a logit link function to model the probability of obtaining a Flickr picture or a Wikipedia page within a given designated area.


## 2.1 Model of Flickr data using using all of the covariates

Flickr.bin.all <- glm(Fli_T_F ~ scale(Area_Km2_log) + scale(biodi_100_comb) +  scale(h_max) + as.factor(coastal_co) + as.factor(riv_T_F) + as.factor(waterbody_T_F)  + scale(log_distance_1) + scale(log_public_transport_1) + scale(pop_density_max) , family = binomial, data = my_data)
summary(Flickr.bin.all)

# Fixed covariates considered for the model were the log of the area of the designated area; species richness of birds, butterflies and vascular plants; 
# the log of the distance to the closest major town or city; the log of the number of public transport links; population density; coastal location (categorical with two levels); 
# maximum height; presence of at least one river (categorical with two levels); and presence of at least one waterbody (categorical with two levels).

## Best model for Flickr data

Flickr.bin_best_model <- step(Flickr.bin.all, direction =  "backward") # We Akaike's Information Criterion (AIC) and a stepwise approach, starting with a ‘maximal’ model including all the fixed covariates and conducting backward model selection (Zuur et al. 2009)
summary(Flickr.bin_best_model)


rsq(Flickr.bin_best_model) # R2

#plot(Flickr.bin_best_model) # Look at residuals to assess model fit


## 2.2 Model of Wikipedia data using using all of the covariates

wiki.bin.all <- glm(wiki_T_F ~ scale(Area_Km2_log) + scale(biodi_100_comb) +  scale(h_max) + as.factor(coastal_co) + as.factor(riv_T_F) + as.factor(waterbody_T_F)  + scale(log_distance_1) + scale(log_public_transport_1) + scale(pop_density_max) , family = binomial, data = my_data)
summary(wiki.bin.all)

# Fixed covariates considered for the model were the log of the area of the designated area; species richness of birds, butterflies and vascular plants; 
# the log of the distance to the closest major town or city; the log of the number of public transport links; population density; coastal location (categorical with two levels); 
# maximum height; presence of at least one river (categorical with two levels); and presence of at least one waterbody (categorical with two levels).

## Best model for Wikipedia data

wiki.bin_best_model <- step(wiki.bin.all, direction =  "backward") # We Akaike's Information Criterion (AIC) and a stepwise approach, starting with a ‘maximal’ model including all the fixed covariates and conducting backward model selection (Zuur et al. 2009)
summary(wiki.bin_best_model)

rsq(wiki.bin_best_model) # R2

#plot(wiki.bin_best_model) # Look at residuals to assess model fit


### 2.3 AUC scores for the best binomial models of the Flickr and Wikipedia data

source("./calculate_AUC_for_Flickr_model.R")
source("./calculate_AUC_for_Wikipedia_model.R")



