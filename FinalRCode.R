## INDIVIDUAL CORRELATIONS
install.packages("dplyr")
library(ggplot2)
library(ggplot2)
library(car)
library(xlsReadWrite)
library(dplyr)
wavebreakers <- read.csv(file="DATA.csv")

# NORMALITY TESTING
#Histogram of the toal.coral.families
hist(wavebreakers$Total.coral.families)
hist(wavebreakers$Total.fish.species)

# LINEARITY TESTING
plot(Total.coral.families ~ Area..m.2., data = wavebreakers)
plot(Total.fish.species ~ Area..m.2., data = wavebreakers)



# REGRESSIONS
# Regression: total fish count vs area

reg_fishiarea <- lm(Total.fish.species ~Area..m.2., data=wavebreakers)
summary(reg_fishiarea)

ggplot(wavebreakers, aes(x=Area..m.2., y=Total.fish.count)) + geom_point() + 
  stat_smooth(method = lm)


# Regression: total fish count vs age
#SIGNIFICANT
reg_fishiage <- lm(Total.fish.species ~ Age, data=wavebreakers)
summary(reg_fishiage)

ggplot(wavebreakers, aes(x=Age, y=Total.fish.count)) + geom_point() + 
  stat_smooth(method = lm)


# Regression: total coral count vs age // we discard this becasue fish count was too disproportionate using 20+
#SIGNIFICANT
reg_coral_age<- lm(Total.coral.count~Age, data=wavebreakers)
summary(reg_coral_age)

ggplot(wavebreakers, aes(x=Age, y=Total.coral.count)) + geom_point() + 
  stat_smooth(method = lm)

# # Regression: total coral count vs area
reg_corarea<- lm(Total.coral.count~Area..m.2., data=wavebreakers)
summary(reg_corarea)

ggplot(wavebreakers, aes(x=Area..m.2., y=Total.coral.count)) + geom_point() + 
  stat_smooth(method = lm)

#Regression: total coral count vs area

# Regression: fish species vs area
reg_fishspecies_area <- lm(Total.fish.species ~ Area..m.2., data=wavebreakers)
summary(reg_fishspecies_area)

ggplot(wavebreakers, aes(x=Area..m.2., y=Total.fish.species)) + geom_point() + 
  stat_smooth(method = lm)

# Regression: coral families vs area

# This creates a simple linear regression model where sales is the outcome variable and podcast is the predictor variable. The data used is a data frame named train.
#model <- lm(sales ~ podcast, data = train)
#Data= wavebreakers is the name of the variable representing the file
reg_coralfamilies_area <- lm(Total.coral.families ~ Area..m.2., data=wavebreakers)
summary(reg_coralfamilies_area)

ggplot(wavebreakers, aes(x=Area..m.2., y=Total.coral.families)) + geom_point() + 
  stat_smooth(method = lm)


# Regression: fish species vs distance from shore
## SIGNIFICANT!!!!
reg_fishspecies_shore <- lm(Total.fish.species ~ Distance.from.shore..m., data=wavebreakers)
summary(reg_fishspecies_shore)

ggplot(wavebreakers, aes(x=Distance.from.shore..m., y=Total.fish.species)) + geom_point() + 
  stat_smooth(method = lm)

# Regression: fish species vs age
## 
reg_fishspecies_shore <- lm(Total.fish.species ~ Age, data=wavebreakers)
summary(reg_fishspecies_shore)

ggplot(wavebreakers, aes(x=Age, y=Total.fish.species)) + geom_point() + 
  stat_smooth(method = lm)

# Regression: coral species vs age
##Modified

reg_fishspecies_shore <- lm(Total.coral.species ~ Age, data=wavebreakers)
summary(reg_fishspecies_shore)

ggplot(wavebreakers, aes(x=Age, y=Total.coral.families)) + geom_point() + 
  stat_smooth(method = lm)

# Regression: coral families vs distance from shore
reg_coralfamilies_shore <- lm(Total.coral.families ~ Distance.from.shore..m., data=wavebreakers)
summary(reg_coralfamilies_shore)

ggplot(wavebreakers, aes(x=Distance.from.shore..m., y=Total.coral.families)) + geom_point() + 
  stat_smooth(method = lm)

# Regression: coral families vs distance frm reef
reg_coralfamilies_reef <- lm(Total.coral.families ~ Distance.from.reef..m., data=wavebreakers)
summary(reg_coralfamilies_reef)

ggplot(wavebreakers, aes(x=Distance.from.reef..m., y=Total.coral.families)) + geom_point() + 
  stat_smooth(method = lm)

# Regression: fish species vs distance from reef
reg_fishspecies_reef <- lm(Total.fish.species ~ Distance.from.reef..m., data=wavebreakers)
summary(reg_fishspecies_reef)

ggplot(wavebreakers, aes(x=Distance.from.reef..m., y=Total.fish.species)) + geom_point() + 
  stat_smooth(method = lm)

#regression: fish species vs depth


reg_fish_depth<- lm(Total.fish.species~ Depth..m., data=wavebreakers )
summary(reg_fish_depth)
ggplot(wavebreakers, aes(x=Depth..m., y=Total.fish.species)) + geom_point() + 
  stat_smooth(method = lm)+ simplify()

#regression: fish species vs depth


reg_coral_depth<- lm(Total.coral.families~ Depth..m., data=wavebreakers )
summary(reg_fish_depth)
ggplot(wavebreakers, aes(x=Depth..m., y=Total.coral.families)) + geom_point() + stat_smooth(method = lm)



# T-TESTS

# t-test: Coral families vs. Side
#SIGNIFICANT
t.test(Total.coral.families ~ Side, data = wavebreakers)
ggplot(wavebreakers, aes(x=Side, y=Total.coral.families)) + geom_boxplot()


# t-test: Fish species vs. Side
#SIGNIFICANT
t.test(Total.fish.species ~ Side, data = wavebreakers)
tapply(wavebreakers$Total.fish.count, wavebreakers$Side, mean)
ggplot(wavebreakers, aes(x=Side, y=Total.fish.species)) +  geom_boxplot()



# ANOVA

install.packages("AICcmodavg")
library(car)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(lme4)

# ANOVA: Fish species vs. Age
## SIGNIFICANT!!!

anova_fishspecies_age <- aov(Total.fish.species ~ Age.group, data = wavebreakers)
summary(anova_fishspecies_age)

tukey.fishspecies_age <- TukeyHSD(anova_fishspecies_age)
tukey.fishspecies_age

ggplot(wavebreakers, aes(x=Age.group, y=Total.fish.species)) + geom_boxplot()

# ANOVA: Coral families vs. Age
anova_coralfamilies_age <- aov(Total.coral.families ~ Age.group, data = wavebreakers)
summary(anova_coralfamilies_age)

tukey.coralfamilies_age <- TukeyHSD(anova_coralfamilies_age)
tukey.coralfamilies_age

ggplot(wavebreakers, aes(x=Age.group, y=Total.coral.families)) + geom_boxplot()


## MODELS



# Install and load necessary packages
install.packages("lme4")
install.packages("glmmTMB")
install.packages("ggplot2")
install.packages("ggeffects")
install.packages("DHARMa")
install.packages("MASS")
install.packages("bbmle")
install.packages("dplyr")
install.packages("Matrix")
install.packages("performance")
library(ggeffects)
library(bbmle)
library(ggplot2)
library(DHARMa)
library(lme4)
library(glmmTMB)
library(ggeffects)
library(AICcmodavg)
library(MASS)
library(magrittr)
library(dplyr)
library(performance)
wavebreakers <- read.csv(file="DATA_6.0.csv")


wavebreakers <- wavebreakers[-(53:115),]



wavebreakers <- wavebreakers %>% mutate(Total.abundance.midpoint = case_when(Total.fish.count < 20 ~ 10,
                                                                             Total.fish.count >= 20 & Total.fish.count < 40 ~ 30,
                                                                             Total.fish.count >= 40 & Total.fish.count < 60 ~ 50,
                                                                             Total.fish.count >= 60 & Total.fish.count < 80 ~ 70,
                                                                             Total.fish.count >= 80 & Total.fish.count < 100 ~ 90,
                                                                             Total.fish.count >= 100 & Total.fish.count < 120 ~ 110,
                                                                             Total.fish.count >= 120 & Total.fish.count < 140 ~ 130))

wavebreakers <- wavebreakers %>% mutate(Coral.abundance.midpoint = case_when(Total.coral.count < 10 ~ 5,
                                                                             Total.coral.count >= 10 & Total.coral.count < 20 ~ 15,
                                                                             Total.coral.count >= 20 & Total.coral.count < 30 ~ 25,
                                                                             Total.coral.count >= 30 & Total.coral.count < 40 ~ 35,
                                                                             Total.coral.count >= 40 & Total.coral.count < 50 ~ 45,
                                                                             Total.coral.count >= 50 & Total.coral.count < 60 ~ 55,
                                                                             Total.coral.count >= 60 & Total.coral.count < 70 ~ 65,
                                                                             Total.coral.count >= 70 & Total.coral.count < 80 ~ 75,
                                                                             #Summary of the                                                                              Total.coral.count >= 80 & Total.coral.count < 90 ~ 85,
                                                                             Total.coral.count >= 90 & Total.coral.count < 100 ~ 95))


# Scaling to be able to use area data:
wavebreakers2<-wavebreakers
str(wavebreakers2)


#wavebreakers2$Area..m.2. <- as.numeric(scale(wavebreakers2$Area..m.2.))
#wavebreakers2$Distance.from.shore..m. <- as.numeric(scale(wavebreakers2$Distance.from.shore..m.))
#wavebreakers2$Distance.from.reef..m. <- as.numeric(scale(wavebreakers2$Distance.from.reef..m.))
#wavebreakers2$Age.group <- as.factor(wavebreakers2$Age.group)
#wavebreakers2$Side <- as.factor(wavebreakers2$Side)
#wavebreakers2$Site <- as.factor(wavebreakers2$Site)

wavebreakers2$Area..m.2. <- scale(wavebreakers2$Area..m.2.)
wavebreakers2$Distance.from.shore..m. <- scale(wavebreakers2$Distance.from.shore..m.)
wavebreakers2$Distance.from.reef..m. <- scale(wavebreakers2$Distance.from.reef..m.)


## For Total Fish ABUNDANCE ----#------#-----#-----#-----#


wavebreakers2$Total.abundance.midpoint <- as.integer(wavebreakers2$Total.abundance.midpoint)
ggplot(data = wavebreakers2) + geom_histogram(aes(Total.abundance.midpoint))

M5<- glmmTMB(Total.abundance.midpoint~Age.group +Area..m.2.+ Distance.from.shore..m. +Depth..m.+ Distance.from.reef..m.+ Side  +Latitude+ (1|Site), data=wavebreakers2,family="poisson"(link= "log") )
AIC(M5)
M5<- glmmTMB(Total.abundance.midpoint~Age.group +Area..m.2.+ Distance.from.shore..m. +Depth..m.+ Distance.from.reef..m.+ Side  +Latitude+ (1|Site), data=wavebreakers2,family="poisson"(link= "identity") )
AIC(M5)

simulationOutput <- simulateResiduals(M5)
plot(simulationOutput, quantreg = FALSE)
testOutliers(simulationOutput)
check_model(M5)


performance::check_collinearity(M5) #works- None of the variables are colinear



#Question: Model with the lowest AIC does not show any significant variable
#There is no point in predicting

summary(M5)#Best fitting model and only one that works

#PREDICTION

# -------Unscaling scaled data------
mean_distance <- attr(wavebreakers2$Distance.from.shore..m., "scaled:center")
sd_distance <- attr(wavebreakers2$Distance.from.shore..m., "scaled:scale")
fishpredict <- ggpredict(M5, terms = "Distance.from.shore..m.")
fishpredict$x <- (fishpredict$x * sd_distance) + mean_distance
# Predicting as a distance from the shore
ggplot(fishpredict, aes(x = x, y = predicted)) +geom_line(color = "blue") +geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +labs(title = "Predicted Fish Abundance(Distance From Shore)",x = "Distance from Shore(m)",y = "Predicted Abundance")

# Distance from reef
mean_distance <- attr(wavebreakers2$Distance.from.reef..m., "scaled:center")
sd_distance <- attr(wavebreakers2$Distance.from.reef..m., "scaled:scale")
fishpredict <- ggpredict(M5, terms = "Distance.from.reef..m.")
fishpredict$x <- (fishpredict$x * sd_distance) + mean_distance

reef_predict <- ggpredict(M5, terms = "Distance.from.reef..m.")
ggplot(reef_predict, aes(x = x, y = predicted)) +geom_line(color = "blue") +geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +labs(title = "Predicted Fish Abundance (Distance from Reef)",x = "Distance from Reef (m)",y = "Predicted Abundance")

#-------Latitude---------------
latitude_predict <- ggpredict(M5, terms = "Latitude")
ggplot(latitude_predict, aes(x = x, y = predicted)) +geom_line(color = "blue") +geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +labs(title = "Predicted Fish Abundance (Latitude)",x = "Latitude",y = "Predicted Abundance")

# ------Generate predictions for the Side variable (Protected vs Exposed)--
mean_distance <- attr(wavebreakers2$Distance.from.shore..m., "scaled:center")
sd_distance <- attr(wavebreakers2$Distance.from.shore..m., "scaled:scale")
fishpredict <- ggpredict(M5, terms = "Side")
fishpredict$x <- (fishpredict$x * sd_distance) + mean_distance
# Boxplot-style visualization for the prediction
ggplot(sidepredictions, aes(x = x, y = predicted)) +geom_boxplot(aes(fill = x), width = 0.6, alpha = 0.7) + geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "blue") + labs(title = "Predicted Fish Abundance by Side",x = "Side (Protected vs Exposed)",y = "Predicted Fish Abundance") +theme_minimal() +scale_fill_manual(values = c("Protected" = "#0073C2", "Exposed" = "#EFC000")) +  theme(legend.position = "none")


#For Coral  ABUNDANCE ----#------#-----#-----#-----#

wavebreakers2$Coral.abundance.midpoint <- as.integer(wavebreakers2$Coral.abundance.midpoint)

#Error with identity
MC3<- glmmTMB(Coral.abundance.midpoint~Age.group +Area..m.2.+ Distance.from.shore..m. +Depth..m.+ Distance.from.reef..m.+ Side  +Latitude+ (1|Site), data=wavebreakers2,family="poisson"(link="identity"))
AIC(MC3)
# Log is way to go
MC3<- glmmTMB(Coral.abundance.midpoint~Age.group +Area..m.2.+ Distance.from.shore..m. +Depth..m.+ Distance.from.reef..m.+ Side  +Latitude+ (1|Site), data=wavebreakers2,family="poisson"(link="log"))
AIC(MC3)
ggplot(data = wavebreakers2) +geom_histogram(aes(Coral.abundance.midpoint))



check_model(MC3)
performance::check_collinearity(MC3)
simulationOutput <- simulateResiduals(MC3)
plot(simulationOutput, quantreg = FALSE)
testOutliers(simulationOutput)

summary(MC3)#correlations found
sidepredictions <- ggpredict(MC3, terms = "Side")
summary(sidepredictions)

#Unscaling scaled data
mean_distance <- attr(wavebreakers2$Distance.from.shore..m., "scaled:center")
sd_distance <- attr(wavebreakers2$Distance.from.shore..m., "scaled:scale")
coralpredictmc3 <- ggpredict(MC3, terms = "Distance.from.shore..m.")
coralpredictmc3$x <- (coralpredictmc3$x * sd_distance) + mean_distance
# Predicting as a distance from the shore
ggplot(coralpredictmc3, aes(x = x, y = predicted)) + geom_line(color = "#E63946", size = 1.2) + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#F1A1A9", alpha = 0.3) + labs(title = "Predicted Coral Abundance (Distance from Shore)", subtitle = "Model predictions with 95% confidence intervals", x = "Distance from Shore (m)", y = "Predicted Coral Abundance") + theme_minimal() + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, color = "gray30", hjust = 0.5), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "gray80", linetype = "dashed")) + scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) + scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))

# Generate predictions for Side (Protected vs Exposed)
sidecoralmc3 <- ggpredict(MC3, terms = "Side")
ggplot(sidecoralmc3, aes(x = x, y = predicted)) + geom_boxplot(aes(fill = x), width = 0.5, alpha = 0.8, outlier.shape = NA, color = "black") + geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#E63946", size = 0.8) + scale_fill_manual(values = c("Protected" = "#0073C2", "Exposed" = "#EFC000"), labels = c("Protected Area", "Exposed Area")) + labs(title = "Predicted Coral Abundance by Side", subtitle = "Comparison between Protected and Exposed Areas", x = "Side (Protected vs Exposed)", y = "Predicted Coral Abundance") + theme_minimal() + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, color = "gray30", hjust = 0.5), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12), legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "gray85", linetype = "dashed")) + scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))


# For total coral FAMILIES- ----#------#-----#-----#-----#

ggplot(data = wavebreakers2) + geom_histogram(aes(Total.coral.families))

FC3<- glmmTMB(Total.coral.families~Age.group +Area..m.2.+ Distance.from.shore..m. +Depth..m.+ Distance.from.reef..m. + Side+Latitude + (1|Site), data=wavebreakers2, family="poisson")# Does not runs
FC3<- glmmTMB(Total.coral.families~Age.group +Area..m.2.+ Distance.from.shore..m. +Depth..m.+ Distance.from.reef..m. + Side + (1|Site), data=wavebreakers2, family="poisson")#runs

### model is rank deficient = too many variables for not enough variation
### trying to simplifying it by dropping some of the variables aka: area


AIC(FC3)
performance::check_collinearity(FC3) #only relevant for FC3
check_model(FC3)
summary(FC3)



# QUESTION: FC4 has a lower AICc, and fits well, however, it is gaussean and I am not sure if we are really suposed to take that one.
simulationOutput <- simulateResiduals(FC3)
plot(simulationOutput, quantreg = FALSE)
testOutliers(simulationOutput)

summary(FC3)   #### nothing significant


# PREDICTIONS -unscaling the distances for better graph-
# Extract the mean and standard deviation for unscaling


ggplot(coralpredict, aes(x = x, y = predicted)) +geom_line(color = "red") + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(title = "Predicted Coral Abundance (glmmTMB Model)",x = "Distance from Shore (m)",  y = "Predicted Coral Abundance") 




# For total fish SPECIES--- Is not working with GLM TMB models
str(wavebreakers2$Total.fish.species)
ggplot(data = wavebreakers2) + geom_histogram(aes(Total.fish.species))

#### Total fish species------#------#-----#
SC3<- glmmTMB(Total.fish.species~Age.group + Distance.from.shore..m. +Area..m.2.+ Distance.from.reef..m. +  Side +Depth..m. +  (1|Site), data=wavebreakers2,family="poisson")

AIC(SC3)
simulationOutput <- simulateResiduals(SC3)
plot(simulationOutput, quantreg = FALSE)
testOutliers(simulationOutput)
performance::check_collinearity(SC3)



summary(SC3) #Only one that gives an AICc does not produce an output

# Boxplot-style visualization for the prediction
sidepredictions <- ggpredict(SC3, terms = "Side")


# Professionalized boxplot-style visualization
ggplot(sidepredictions, aes(x = x, y = predicted, fill = x)) + geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA, color = "black") + geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "purple", size = 0.8) + scale_fill_manual(values = c("Protected" = "#CBA4FF", "Exposed" = "#D5B3FF"), labels = c("Protected Area", "Exposed Area")) + labs(title = "Predicted Fish Species by Side", subtitle = "Comparison of fish species in protected vs exposed areas", x = "Side (Protected vs Exposed)", y = "Predicted Number of Fish Species") + theme_minimal() + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, color = "gray30", hjust = 0.5), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12), panel.grid.major = element_line(color = "gray85", linetype = "dashed"), panel.grid.minor = element_blank(), legend.position = "none") + scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))



#----- BIOMASS AS DEPENDENT----#------#-----#
ggplot(data = wavebreakers2) + geom_histogram(aes(Total.Fish.Biomass))
B1<- glmmTMB(Total.Fish.Biomass~Age.group +Area..m.2.+ Distance.from.shore..m. +Depth..m.+ Distance.from.reef..m.+ Side  + (1|Site), data=wavebreakers2,family="poisson")
summary(B1)

AIC(B1)
simulationOutput <- simulateResiduals(B1)
plot(simulationOutput, quantreg = FALSE)
testOutliers(simulationOutput)
performance::check_model(B1)
performance::check_collinearity(B1)



# -------Unscaling scaled data------
mean_distance <- attr(wavebreakers2$Distance.from.shore..m., "scaled:center")
sd_distance <- attr(wavebreakers2$Distance.from.shore..m., "scaled:scale")
biop <- ggpredict(B1, terms = "Distance.from.shore..m.")
biop$x <- (biop$x * sd_distance) + mean_distance
# Predicting as a distance from the shore
# Professionalized plot for predicted biomass (distance from shore)
ggplot(biop, aes(x = x, y = predicted)) + geom_line(color = "#1B9E77", size = 1.2) + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#66C2A5", alpha = 0.3) + labs(title = "Predicted Biomass (Distance from Shore)", subtitle = "Model predictions with 95% confidence intervals", x = "Distance from Shore (m)", y = "Predicted Biomass") + theme_minimal() + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, color = "gray30", hjust = 0.5), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12), panel.grid.major = element_line(color = "gray85", linetype = "dashed"), panel.grid.minor = element_blank()) + scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) + scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))


# Depth
mean_distance <- attr(wavebreakers2$Distance.from.reef..m., "scaled:center")
sd_distance <- attr(wavebreakers2$Distance.from.reef..m., "scaled:scale")
bio <- ggpredict(B1, terms = "Depth..m.")
bio$x <- (bio$x * sd_distance) + mean_distance
# Professionalized plot for predicted biomass (Depth) with fixed x-axis scale
# Professionalized plot for predicted biomass (Depth)
ggplot(bio, aes(x = x, y = predicted)) + geom_line(color = "#1B9E77", size = 1.2) + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#66C2A5", alpha = 0.3) + labs(title = "Predicted Biomass by Depth", subtitle = "Model predictions with 95% confidence intervals", x = "Depth (m)", y = "Predicted Biomass") + theme_minimal() + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, color = "gray30", hjust = 0.5), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12), panel.grid.major = element_line(color = "gray85", linetype = "dashed"), panel.grid.minor = element_blank()) + scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) + scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))



# ------Generate predictions for the Side variable (Protected vs Exposed)--
b1predict <- ggpredict(B1, terms = "Side")
# Professionalized boxplot for predicted biomass (Side)
ggplot(b1predict, aes(x = x, y = predicted, fill = x)) + geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA, color = "black") + geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#1B9E77", size = 0.8) + scale_fill_manual(values = c("Protected" = "#0073C2", "Exposed" = "#EFC000"), labels = c("Protected Area", "Exposed Area")) + labs(title = "Predicted Biomass by Side", subtitle = "Comparison between protected and exposed areas", x = "Side (Protected vs Exposed)", y = "Predicted Biomass") + theme_minimal() + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, color = "gray30", hjust = 0.5), axis.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 12), panel.grid.major = element_line(color = "gray85", linetype = "dashed"), panel.grid.minor = element_blank(), legend.position = "none") + scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5))


