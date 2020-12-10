rm(list = ls())
incd = read.csv("updated incd.csv")
death = read.csv("updated death.csv")

#SLR incd

incd_slr1 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$MedianAgebyState, na.action = na.omit)
incd_slr2 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$CountyPopulation, na.action = na.omit)
incd_slr3 = lm(incd$Age.AdjustedIncidenceRate...casesper100.000 ~
                 incd$Recent5.YearTrend.à.inIncidenceRates, na.action = na.omit)

summary(incd_slr1)
summary(incd_slr2)
summary(incd_slr3)

#SLR death

death_slr1 = lm(death$Age.Adjusted.Death.Rate ~
                  incd$MedianAgebyState, na.action = na.omit)
death_slr2 = lm(death$Age.Adjusted.Death.Rate ~
                  incd$CountyPopulation, na.action = na.omit)
death_slr3 = lm(death$Age.Adjusted.Death.Rate ~
                  incd$Recent5.YearTrend.à.inIncidenceRates, na.action = na.omit)

summary(death_slr1)
summary(death_slr2)
summary(death_slr3)

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

#Optimize MLR with stepwise selection

library(MASS)
incd_red = stepAIC(incd_mlr, direction = "both", trace = TRUE)
summary(incd_red)

death_red = stepAIC(death_mlr, direction = "both", trace = TRUE)
summary(death_red)


#Plots of States
install.packages('ggplot2')
library(ggplot2)

i_state<- factor(incd$State)
d_state<- factor(death$State)

p<-ggplot()+
  geom_boxplot(data = incd,
               aes(x=i_state,y=Age.AdjustedIncidenceRate...casesper100.000)) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

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

#Outlier Analysis for Alabama Death Rate (For Patmeff County)


data_alabama<-death[c(1:68),]

#i) Grubbs Test: Tests the alternative hypothesis that the highest value is outlier

install.packages('outliers')
library(outliers)

outlier_test<-grubbs.test(data_alabama$Age.Adjusted.Death.Rate)

#ii) Rosener's Test: 

install.packages('EnvStats')
library(EnvStats)

outlier_test2<-rosnerTest(data_alabama$Age.Adjusted.Death.Rate, k=1)

#Both Tests identify Patmeff County as outlier 

#iii) Alabama's Death Rate With and Without Patmeff County 

data7<- data_alabama[-c(1),]

mean_alabama_withpat<-mean(data_alabama$Age.Adjusted.Death.Rate)
mean_alabama_withoutpat<-mean(data7$Age.Adjusted.Death.Rate)



