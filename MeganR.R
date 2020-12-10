rm(list = ls())
incd = read.csv("updated incd.csv")
death = read.csv("updated death.csv")

#SLR incd
Political.Party1<-factor(incd$Political.Party.2020, levels=c("R", "D"))

incd_slr1 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$MedianAgebyState, na.action = na.omit)
incd_slr2 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$CountyPopulation, na.action = na.omit)
incd_slr3 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$Recent5.YearTrend.à.inIncidenceRates, na.action = na.omit)
incd_slr4 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$Political.Party.2020)

summary(incd_slr1)
summary(incd_slr2)
summary(incd_slr3)
summary(incd_slr4)

#SLR death
Political.Party1.5<-factor(death$Political.Party.2020, levels=c("R", "D"))

death_slr1 = lm(death$Age.Adjusted.Death.Rate ~
                  death$Median.Age.by.State, na.action = na.omit)
death_slr2 = lm(death$Age.Adjusted.Death.Rate ~
                  death$County.Population, na.action = na.omit)
death_slr3 = lm(death$Age.Adjusted.Death.Rate ~
                  death$X5.Year.Trend, na.action = na.omit)
death_slr4 = lm(death$Age.Adjusted.Death.Rate ~
                  death$Political.Party.2020)

summary(death_slr1)
summary(death_slr2)
summary(death_slr3)
summary(death_slr4)

#MLR
Political.Party1<-factor(incd$Political.Party.2020, levels=c("R", "D"))
incd_mlr = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~ incd$MedianAgebyState +
                incd$CountyPopulation + Political.Party1 +
                incd$Recent5.YearTrend.à.inIncidenceRates, na.action = na.omit)
summary(incd_mlr)

Political.Party1.5<-factor(death$Political.Party.2020, levels=c("R", "D"))
death_mlr = lm(death$Age.Adjusted.Death.Rate ~ death$Median.Age.by.State +
                 death$County.Population + Political.Party1.5 + death$X5.Year.Trend,
               na.action = na.omit)
summary(death_mlr)

#MLR But with Other Political Party Shown in regression 
Political.Party2<-factor(incd$Political.Party.2020, levels=c("D", "R"))
incd_mlr2 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~ incd$MedianAgebyState +
                 incd$CountyPopulation + Political.Party2 +
                 incd$Recent5.YearTrend.à.inIncidenceRates, na.action = na.omit)
summary(incd_mlr2)

Political.Party2.5<-factor(death$Political.Party.2020, levels=c("D", "R"))
death_mlr2 = lm(death$Age.Adjusted.Death.Rate ~ death$Median.Age.by.State +
                  death$County.Population + Political.Party2.5 + death$X5.Year.Trend,
                na.action = na.omit)
summary(death_mlr2)


# STEPWISE SELECTION INCD MLR

#Optimize incd MLR with stepwise selection
#Most sig slr on its own were age and political party (p=2.2 e-16)
#F-stat age = 142.9, F-stat political = 87.85
#Therefore we will go with age as the primary significant predictor

incd_red1 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$MedianAgebyState, na.action = na.omit)

#We then add the second most significant predictor

incd_red2 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$MedianAgebyState + incd$Political.Party.2020,
               na.action = na.omit)
summary(incd_red2)

#next we add county population, because its p value was lower than when recent 5 yr trend
#was added, all predictors are stil sig

incd_red3 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$MedianAgebyState + incd$Political.Party.2020 +
                 incd$CountyPopulation, na.action = na.omit)
summary(incd_red3)

#finally we add 5 yr trend and see if all predictors remain sign

incd_red4 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$MedianAgebyState + incd$Political.Party.2020 +
                 incd$CountyPopulation + incd$Recent5.YearTrend.à.inIncidenceRates,
               na.action = na.omit)
summary(incd_red4)

#Every predictor is still significant

# STEPWISE SELECITON DEATH MLR

#Optimize death MLR with stepwise selection
#Most sig slr on its own were 5 yr trend and political party (p=2.2 e-16)
#F-stat trend = 614.2, F-stat political = 156
#Therefore we will go with trend as the primary significant predictor

death_red1 = lm(death$Age.Adjusted.Death.Rate ~
                  death$X5.Year.Trend, na.action = na.omit)

#We then add the second most significant predictor

death_red2 = lm(death$Age.Adjusted.Death.Rate ~
                  death$X5.Year.Trend + death$Political.Party.2020,
                na.action = na.omit)
summary(death_red2)

#next we add median age, because its p value was lower than when population
#was added, all predictors are stil sig

death_red3 = lm(death$Age.Adjusted.Death.Rate ~
                  death$X5.Year.Trend + death$Political.Party.2020 +
                  death$Median.Age.by.State, na.action = na.omit)
summary(death_red3)

#finally we add county population and see if all predictors remain sig

death_red4 = lm(death$Age.Adjusted.Death.Rate ~
                  death$X5.Year.Trend + death$Political.Party.2020 +
                  death$Median.Age.by.State + death$County.Population,
                na.action = na.omit)
summary(death_red4)

#County population not significant, therefore we remove and stick with death_red3

# PLOTS

#Plots of States 
install.packages('ggplot2')
library(ggplot2)

i_state<- factor(incd$State)
d_state<- factor(death$State)

p<-ggplot()+
  geom_boxplot(data = incd, 
               aes(x=i_state,y=Age.AdjustedIncidenceRate...casesper100.000))

p1<-p+labs(x="State", y="Incidence Rate")

#By political Party

#i) Democrat

data2<-subset(incd, Political.Party.2020 == "D")

ii_state<- factor(data2$State)
dd_state<- factor(data2$State)

p_1<-ggplot()+
  geom_boxplot(data = data2, 
               aes(x=ii_state,y=Age.AdjustedIncidenceRate...casesper100.000)) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

p2<-p_1+labs(x="State", y="Incidence Rate")


#ii) Republican

data3<-subset(incd, Political.Party.2020 == "R")

iii_state<- factor(data3$State)
ddd_state<- factor(data3$State)

p_2<-ggplot()+
  geom_boxplot(data = data3, 
               aes(x=iii_state,y=Age.AdjustedIncidenceRate...casesper100.000)) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

p3<-p_1+labs(x="State", y="Incidence Rate")

#Plotting Democrat vs Republican

mean_d_i<-mean(data2$Age.AdjustedIncidenceRate...casesper100.000, na.rm=TRUE)
mean_r_i<-mean(data3$Age.AdjustedIncidenceRate...casesper100.000, na.rm=TRUE)

boxplot(data2$Age.AdjustedIncidenceRate...casesper100.000, 
        data3$Age.AdjustedIncidenceRate...casesper100.000,
        names = c("Demorcratic States","Republican States"),
        ylab="Incidence rate", las = 1)

data4<-subset(death, Political.Party.2020 == "D")
data5<-subset(death, Political.Party.2020 == "R")

boxplot(data4$Age.Adjusted.Death.Rate, 
        data5$Age.Adjusted.Death.Rate,
        names = c("Demorcratic States","Republican States"),
        ylab="Death Rate", las = 1)

#Looks like Patmeff is huge outlier here, lets remove it and replot

data6<-data5[-c(1),]

boxplot(data4$Age.Adjusted.Death.Rate, 
        data6$Age.Adjusted.Death.Rate,
        names = c("Demorcratic States","Republican States"),
        ylab="Death Rate", las = 1)
