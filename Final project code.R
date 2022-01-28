setwd("C:/Introduction_R/Project")

# Openthe file with the data set

flies<-read.csv2("Household flies.csv")

# Define strain as factor

flies$Strain <- as.factor(flies$Strain)
View(flies)
str(flies)
head(flies)


#Logistic regression using the glm() function
  #without the interactions
analysis<-glm(Response~Strain + Temp + Conc.Log10 + Time,family=binomial(link=logit),data=flies)
summary(analysis)
drop1(analysis,test="Chisq") 


#Logistic regression using glm() function
#with the interactions

analysis2<-glm(Response~Strain + Temp + Conc.Log10 + Time + Strain:Conc.Log10 + Time:Temp ,family=binomial(link=logit),data=flies)
summary(analysis2)
drop1(analysis2,test="Chisq")



# Calculate the odds from the coefficients
#analysis 1
exp(cbind (OR = coef(analysis), confint(analysis)))
#analysis 2
exp(cbind (OR = coef(analysis2), confint(analysis2)))

### Model control
## AIC
AIC(analysis)
AIC (analysis2)

## R2

## calculateing the overall "Pseudo R-squared" and its p-value
#analysis 1
ll.null <- analysis$null.deviance/-2
ll.proposed <- analysis$deviance/-2

#analysis 2
ll.null2 <- analysis2$null.deviance/-2
ll.proposed2 <- analysis2$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
#analysis 1
(ll.null - ll.proposed) / ll.null

#analysis 2
(ll.null2 - ll.proposed2) / ll.null2

### Plot the predicted data for model control 
library(ggplot2)
library(wesanderson)
library(dplyr)

# Define the invlogit function
invlogit<-function(y){exp(y)/(1+exp(y))}

# Predict the fitted values and their standard errors

prediction.temp<-as.data.frame(predict(analysis2,se.fit=T))
View(prediction.temp)

prediction<-data.frame(fit=prediction.temp$fit,
                       upper=prediction.temp$fit+ 1.96*
                         prediction.temp$se.fit,
                       lower=prediction.temp$fit- 1.96*
                         prediction.temp$se.fit)

prediction_final<-invlogit(prediction)
View(prediction_final)

# Combine the original data set and the predicted values

prediction3<- cbind(flies, prediction_final) 
View(prediction3)

#  Groupe the variables by Strain, TIme, Temp and COnc.Log10 for each replicate

count_table <- prediction3 %>%
  group_by(Strain, Time, Temp, Conc.Log10) %>%
  
#  Calculate their respective frequencies
  
  summarise(freq = sum(Response)/length(Response),  No_reps = length(Response), fit = sum(fit)/length(fit), lower = sum(lower)/length(lower), upper = sum(upper)/length(upper))

#  Order and number the data

count_table1<- arrange(count_table, count_table$fit) 
count_table1$index <- 1:nrow(count_table1)
View(count_table1)

# Plot the data using the ggplot2 package

ggplot(count_table1
       , aes(x = index, y = fit)) + 
  geom_smooth(aes(y = fit), color = "#5B1A18", legend = T) +
  geom_smooth(aes(y = upper), color = "#D67236", linetype = "longdash") +
  geom_smooth(aes(y = lower), color = "#D67236", linetype = "longdash") +
  geom_point(aes(y = freq)) + 
  theme_light(base_size = 10) +
  labs(x = "index", y = "Probability of infection")

### Interaction plots

#Define variable intervals
Strain <- c("H7N1", "H5N7")
Temp <- 0:40
Time <- 0:40
Conc.Log10 <- seq(from = 6.5, to = 9, by = 0.2)


#Construct dataframe for all the variables defined into the intervals

temp <- expand.grid(Strain = Strain, Temp = Temp, Time = Time, Conc.Log10 = Conc.Log10)
View(temp)

#use above dataframe to predict response for all variable combos

expanded.data <- cbind(temp, "pred" = predict(analysis2, temp, type="response"))
View(expanded.data)

# Plotting the data for fixed values of temperature = 25 °C
 # at time = 24
ggplot(subset(expanded.data, Time == 24 & Temp == 25) 
       , aes(x = Conc.Log10, col = Strain)) + 
  scale_color_manual(values = wes_palette("Darjeeling1")) + 
  theme_light(base_size = 12) +
  labs(x = "Log10(ELD50)", y = "Probability of infection")

# at time = 6
ggplot(subset(expanded.data, Time == 6 & Temp == 25) 
       , aes(x = Conc.Log10, col = Strain)) + 
  geom_line(aes(y = pred)) +
  scale_color_manual(values = wes_palette("Darjeeling1")) + 
  theme_light(base_size = 12) +
  labs(x = "Log10(ELD50)", y = "Probability of infection") 

# Plotting the data for fixed values of Time = 12 hours
# at temp = 15

ggplot(subset(expanded.data, Time == 12 & Temp == 15) 
       , aes(x = Conc.Log10, col = Strain)) + 
  geom_line(aes(y = pred)) +
  scale_color_manual(values = wes_palette("Darjeeling1")) + 
  theme_light(base_size = 12) +
  labs(x = "Log10(ELD50)", y = "Probability of infection") 

# Plotting the data for fixed values of Time = 12 hours
# at temp = 35

ggplot(subset(expanded.data, Time == 12 & Temp == 35)
       , aes(x = Conc.Log10, col = Strain)) + 
  geom_line(aes(y = pred)) +
  scale_color_manual(values = wes_palette("Darjeeling1")) + 
  theme_light(base_size = 12) +
  labs(x = "Log10(ELD50)", y = "Probability of infection") 

# fixed value Concentration = 7.3 ELD50
# strain H7N1

ggplot(subset(expanded.data, Strain == 'H7N1' & Conc.Log10 == 7.3 & Temp == c(15, 25, 35)) 
       , aes(y = pred, x = Time, col = as.factor(Temp))) + 
  geom_line(aes(y = pred)) + 
  scale_color_manual(values = wes_palette("GrandBudapest1")) +
  labs(fill = "Temp") +
  theme_light(base_size = 12) +
  labs(x = "Time", y = "Probability of infection") 

# strain H5N7
ggplot(subset(expanded.data, Strain == 'H5N7' & Conc.Log10 == 7.3 & Temp == c(15, 25, 35))
       , aes(y = pred, x = Time, col = as.factor(Temp))) + 
  geom_line(aes(y = pred)) + 
  scale_color_manual(values = wes_palette("GrandBudapest1")) +
  theme_light(base_size = 12) + 
  labs(x = "Time", y = "Probability of infection") 

#comparing for lowest and highest concentrations 
# Conc.Log10 = 6.7, strain = H5N7
ggplot(subset(expanded.data, Strain == 'H5N7' & Conc.Log10 == 6.7 & Temp == c(15, 25, 35)) 
       , aes(y = pred, x = Time, col = as.factor(Temp))) + 
  geom_line(aes(y = pred)) +  
  scale_color_manual(values = wes_palette("GrandBudapest1")) + 
  theme_light(base_size = 12) + 
  labs(x = "Time", y = "Probability of infection")

# Conc.Log10 = 8.5, strain = H7N1
ggplot(subset(expanded.data, Strain == 'H7N1' & Conc.Log10 == 8.5 & Temp == c(15, 25, 35))
       , aes(y = pred, x = Time, col = as.factor(Temp))) + 
  geom_line(aes(y = pred)) +
  scale_color_manual(values = wes_palette("GrandBudapest1")) + 
  theme_light(base_size = 12) +
  labs(x = "Time", y = "Probability of infection") 

### T50 values

## H5N7

# resolve the equation for Conc.Log10 = 6.7

T_50_H5N7_conc6.7 <- function(Temp) {
  (16.273649 - 0.032832*Temp-2.333133*6.7)/
    (0.205617-0.016482*Temp)
}
# plot the corresponding curve
curve(T_50_H5N7_conc6.7, from=15, to=35, xlab='Temperature (°C)',  ylim=c(0,80), ylab='T_50 (h)', col = "#9C964A", main='H5N7')

# resolve the equation for Conc.Log10 = 7.3

T_50_H5N7_conc7.3 <- function(Temp) {
  (16.273649 - 0.032832*Temp-2.333133*7.3)/
    (0.205617-0.016482*Temp)
}
# plot the corresponding curve
curve(T_50_H5N7_conc7.3, from=15, to=35, xlab='Temperature (°C)', col = "#85D4E3", ylab='T_50 (h)', add=TRUE)

# resolve the equation for Conc.Log10 = 7.9

T_50_H5N7_conc7.9 <- function(Temp) {
  (16.273649 - 0.032832*Temp-2.333133*7.9)/
    (0.205617-0.016482*Temp)
}
# plot the corresponding curve
curve(T_50_H5N7_conc7.9, from=15, to=35, xlab='Temperature (°C)', ylab='T_50 (h)', col = "#F4B5BD", add= TRUE)


## H7N1    

# resolve the equation for Conc.Log10 = 7.3

T_50_H7N1_conc7.3 <- function(Temp) {
  (-12.862093*1 + 1.755597*1*7.3 + 16.273649 - 0.032832*Temp-2.333133*7.3)/
    (0.205617-0.016482*Temp)
}
# plot the corresponding curve

curve( T_50_H7N1_conc7.3, from=15, to=35, xlab='Temperature (°C)', ylim=c(0,60), col = "#9C964A", ylab='T_50 (h)', main='H7N1')

# resolve the equation for Conc.Log10 = 7.9

T_50_H7N1_conc7.9 <- function(Temp) {
  (-12.862093*1 + 1.755597*1*7.9 + 16.273649 - 0.032832*Temp-2.333133*7.9)/
    (0.205617-0.016482 *Temp)
}
# plot the corresponding curve

curve( T_50_H7N1_conc7.9, from=15, to=35, xlab='Temperature (°C)', ylim=c(0,100), col ="#85D4E3", ylab='T_50 (h)', main='H7N1 Conc.Log10 7.9', add = TRUE)

# resolve the equation for Conc.Log10 = 8.5

T_50_H7N1_conc8.5 <- function(Temp) {
  (-12.862093*1 + 1.755597*1*8.5 + 16.273649 - 0.032832*Temp-2.333133*8.5)/
    (0.205617-0.016482 *Temp)
}
# plot the corresponding curve

curve( T_50_H7N1_conc8.5, from=15, to=35, xlab='Temperature (°C)', ylim=c(0,100), col = "#F4B5BD", ylab='T_50 (h)', main='H7N1 Conc.Log10 8.5', add = TRUE)
