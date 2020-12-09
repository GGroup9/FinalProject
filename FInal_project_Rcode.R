library(tidyverse)
library(car)
library(MASS)
library(caret)
who <- read.csv(file = "Life Expectancy Data.csv", header=TRUE)
#Looking at the  data
head(who)
attach(who)


# introduce one new additional data point into our dataset
china_new_data_point = data.frame("China", 2015, "Developing", 74.2625,73.75,	294.875	,4.182,	
                                  78.48934709	,80.4375,	65857.9375,	21.80625,	350	,93.6875,	4.918,	93.3125,	0.1,	2345.303158,
                                  321812.0625,	4.6375,	4.025	,0.66025,	11.4375)

names(china_new_data_point) = c("Country","Year","Status","Life.expectancy","Adult.Mortality",
                                "infant.deaths","Alcohol","percentage.expenditure","Hepatitis.B","Measles" , "BMI" ,
                                "under.five.deaths" ,"Polio","Total.expenditure","Diphtheria" , "HIV.AIDS","GDP","Population",
                                "thinness..1.19.years", "thinness.5.9.years","Income.composition.of.resources","Schooling")

who = rbind(china_new_data_point,who)

#remove Country since we are not focusing on specific countries in this research
who <- who[,-c(1)]


# exclude missing values (Remove Rows with Missing Data)
who = who[complete.cases(who),]
# 1289 observations were deleted
# 1650 observations remaining

#fit regression model
model <- lm(Life.expectancy~., data = who)
summary(model)
avPlots(model)

#check pairwise correlations
cor(who[,-c(1,2)])

histogram(who$Life.expectancy)
histogram(who$Adult.Mortality)
histogram(who$infant.deaths)
histogram(who$Alcohol)
histogram(who$percentage.expenditure)
histogram(who$Hepatitis.B)
histogram(who$Measles)
histogram(who$BMI)
histogram(who$under.five.deaths)
histogram(who$Polio)
histogram(who$Total.expenditure)
histogram(who$Diphtheria)
histogram(who$HIV.AIDS)
histogram(who$GDP)
histogram(who$Population)
histogram(who$thinness..1.19.years)
histogram(who$thinness.5.9.years)
histogram(who$Income.composition.of.resources)
histogram(who$Schooling)



#checked the  model
summodel <-  summary(model)
#percentage.expenditure and Hepatitis.B and Measle and Polio and GDP and Population 
#thinness..1.19.years  and thinness.5.9.years larger than 0.05


#Life.expectancy and GDP
gdp <- lm( Life.expectancy ~ GDP , data = who )
summary(gdp)

## standardize data using unit length scaling ##
#unit length scalings
s_Year<-sqrt(sum((who$Year-mean(who$Year))^2))
#s_Status<-sqrt(sum((Status-mean(Status))^2))
s_Adult.Mortality = sqrt(sum((who$Adult.Mortality-mean(who$Adult.Mortality))^2))
s_infant.deaths<-sqrt(sum((who$infant.deaths-mean(who$infant.deaths))^2))
s_Alcohol<-sqrt(sum((who$Alcohol-mean(who$Alcohol))^2))
s_percentage.expenditure<-sqrt(sum((who$percentage.expenditure-mean(who$percentage.expenditure))^2))
s_Hepatitis.B<-sqrt(sum((who$Hepatitis.B-mean(who$Hepatitis.B))^2))
s_Measles<-sqrt(sum((who$Measles-mean(who$Measles))^2))
s_BMI<-sqrt(sum((who$BMI-mean(who$BMI))^2))
s_under.five.deaths<-sqrt(sum((who$under.five.deaths-mean(who$under.five.deaths))^2))
s_Life.expectancy = sqrt(sum((who$Life.expectancy-mean(who$Life.expectancy))^2))

s12  = sqrt(sum((who$Polio - mean(who$Polio))^2))
s13  = sqrt(sum((who$Total.expenditure - mean(who$Total.expenditure))^2))
s14  = sqrt(sum((who$Diphtheria - mean(who$Diphtheria))^2))
s15  = sqrt(sum((who$HIV.AIDS - mean(who$HIV.AIDS))^2))
s16  = sqrt(sum((who$GDP - mean(who$GDP))^2))
s17  = sqrt(sum((who$Population - mean(who$Population))^2))
s18  = sqrt(sum((who$thinness..1.19.years - mean(who$thinness..1.19.years))^2))
s19  = sqrt(sum((who$thinness.5.9.years - mean(who$thinness.5.9.years))^2))
s20  = sqrt(sum((who$Income.composition.of.resources -mean(who$Income.composition.of.resources))^2))
s21  = sqrt(sum((who$Schooling - mean(who$Schooling))^2))

z_Year<-(who$Year-mean(who$Year))/s_Year
#z_Status<-(Status-mean(Status))/s_Status
z_Adult.Mortality<-(who$Adult.Mortality-mean(who$Adult.Mortality))/s_Adult.Mortality
z_infant.deaths<-(who$infant.deaths-mean(who$infant.deaths))/s_infant.deaths
z_Alcohol<-(who$Alcohol-mean(who$Alcohol))/s_Alcohol
z_percentage.expenditure<-(who$percentage.expenditure-mean(who$percentage.expenditure))/s_percentage.expenditure
z_Hepatitis.B<-(who$Hepatitis.B-mean(who$Hepatitis.B))/s_Hepatitis.B
z_Measles<-(who$Measles-mean(who$Measles))/s_Measles
z_BMI<-(who$BMI-mean(who$BMI))/s_BMI
z_under.five.deaths<-(who$under.five.deaths-mean(who$under.five.deaths))/s_under.five.deaths
Life.expectancy_s<-(who$Life.expectancy-mean(who$Life.expectancy))/s_Life.expectancy

z12 = (who$Polio - mean(who$Polio))/s12
z13 = (who$Total.expenditure - mean(who$Total.expenditure))/s13
z14 = (who$Diphtheria - mean(who$Diphtheria))/s14
z15 = (who$HIV.AIDS - mean(who$HIV.AIDS))/s15
z16 = (who$GDP - mean(who$GDP))/s16
z17 = (who$Population - mean(who$Population))/s17
z18 = (who$thinness..1.19.years - mean(who$thinness..1.19.years))/s18
z19 = (who$thinness.5.9.years - mean(who$thinness.5.9.years))/s19
z20 = (who$Income.composition.of.resources - mean(who$Income.composition.of.resources))/s20
z21 = (who$Schooling - mean(who$Schooling))/s21

LifeSRModel<-lm(Life.expectancy_s~z_Year+z_Adult.Mortality+z_infant.deaths+z_Alcohol+z_percentage.expenditure+z_Hepatitis.B+z_Measles+z_BMI+z_under.five.deaths + z12 + z13 + z14 + z15 + z16 + z17 + z18 + z19 + z20 + z21 - 1  )

## variable selection ##
summary(LifeSRModel)
LifeSRModel$coefficients



#Life.expectancy_s and percentage.expenditure
le <- lm( Life.expectancy_s ~ z_percentage.expenditure )
summary(le)

#Life.expectancy_s and Hepatitis.B
lh <- lm( Life.expectancy_s ~ z_Hepatitis.B )
summary(lh)

#Life.expectancy_s and Measles
lmea <- lm( Life.expectancy_s ~ z_Measles )
summary(lmea)

#Life.expectancy_s and Polio
lp <- lm( Life.expectancy_s ~ z12 )
summary(lp)

#Life.expectancy_s and GDP
lg <- lm( Life.expectancy_s ~ z16 )
summary(lg)

#Life.expectancy_s and Population
lpo <- lm( Life.expectancy_s ~ z17)
summary(lpo)

#Life.expectancy_s and thinness..1.19.years
lt1 <- lm( Life.expectancy_s ~ z18 )
summary(lt1)

#Life.expectancy_s and thinness.5.9.years
lt5 <- lm( Life.expectancy_s ~ z19 )
summary(lt5)



#population remove from dataset 'who'
#with standardized data
LifeSRModel<-lm(Life.expectancy_s~z_Year+z_Adult.Mortality+z_infant.deaths+z_Alcohol+z_percentage.expenditure+z_Hepatitis.B+z_Measles+z_BMI+z_under.five.deaths + z12 + z13 + z14 + z15 + z16 + z18 + z19 + z20 + z21 - 1  )
summary(LifeSRModel)

#with original data
modelO <- lm(Life.expectancy~Year+Status+Adult.Mortality+infant.deaths+Alcohol+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + thinness..1.19.years + thinness.5.9.years + Income.composition.of.resources + Schooling, data = who)
summary(modelO)



##stepwise
#use stepwise method to complete variable selection
step(model, direction = 'both')

newmodel = lm(formula = Life.expectancy ~ Year + Status + Adult.Mortality + 
                infant.deaths + Alcohol + percentage.expenditure + BMI + 
                under.five.deaths + Total.expenditure + Diphtheria + HIV.AIDS + 
                thinness.5.9.years + Income.composition.of.resources + Schooling, 
              data = who)

summary(newmodel)



## confidence interval ##
confint( newmodel , level = 0.9 )



## residual analysis ##

#Standardized Residuals vs. Index tut 4
plot( rstandard(newmodel))
#straight line around 0

n <- nrow( who )
plot( newmodel$residuals[1:(n-1)] , newmodel$residuals[2:n])
#straight line, linear relationship

plot(newmodel)



#leverage and influence
2*ncol(who)/nrow(who)  #0.02545455
newmodel_influence <- influence(newmodel)
sort(newmodel_influence$hat, decreasing = TRUE)



#cook's distance
newmodel_cook <- cooks.distance(newmodel)
sort(newmodel_cook, decreasing = TRUE)




#There is potential outlier, so we use robust
#robust
Rmodel = rlm(formula = Life.expectancy ~ Year + Status + Adult.Mortality + 
               infant.deaths + Alcohol + percentage.expenditure + BMI + 
               under.five.deaths + Total.expenditure + Diphtheria + HIV.AIDS + 
               thinness.5.9.years + Income.composition.of.resources + Schooling, 
             data = who, psi = psi.huber)

summary(Rmodel)

plot(Rmodel)




##multicollinearity
#vif
vif(Rmodel)




## model validation ##

#cross validation
set.seed(123)
nsamp = ceiling(0.8*length(who$Life.expectancy))
for (i in 1:5){
  training_samps = sample(c(1:length(who$Life.expectancy)), nsamp)
  training_samps = sort(training_samps)
  train_data = who[training_samps, ]
  test_data = who[-training_samps, ]
  
  
  train_mdl = lm(formula = Life.expectancy ~ Year + Status + Adult.Mortality + 
                   infant.deaths + Alcohol + percentage.expenditure + BMI + 
                   under.five.deaths + Total.expenditure + Diphtheria + HIV.AIDS + 
                   thinness.5.9.years + Income.composition.of.resources + Schooling, 
                 data = train_data)
  
  preds = predict(train_mdl, test_data)
  
  R.sq = R2(preds, test_data$Life.expectancy)
  RMSPE = RMSE(preds, test_data$Life.expectancy)
  MAPE = MAE(preds, test_data$Life.expectancy)
  sd_RMSPE = RMSPE/sd(test_data$Life.expectancy)
  print(c(R.sq,RMSPE,MAPE,sd_RMSPE))
}
