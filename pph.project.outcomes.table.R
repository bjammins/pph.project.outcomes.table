
library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)
library(reshape2)


setwd('L:/PPH Project/data/Final Raw data/')
df <- read.csv("outcome_measures_05_05_2016.csv", head=TRUE)
cat <- read.csv("QBL_and_RA_cats.csv", header = TRUE)
names(cat)[1] <- "Hospital.ID"
## Paste a month to the character and covert the period to a date
df$Period <- as.Date(paste('01', df$Period, sep = "-"), format='%d-%b-%y')

#Remove 2016 period dates
df <- df[df$Period < "2016-01-01", ]

timeser <- df %>% filter(Complete. == TRUE) 

timeser2 <- merge(timeser,cat)
#Create a feature for the period to compare baseline to last three months of data

for (i in 1:nrow(timeser2)){
  if (timeser2$Period[i] < "2014-07-01"){
    timeser2$period.cat[i] <- 1                        
  } else if (timeser2$Period[i] >"2015-09-01") {
    timeser2$period.cat[i] <- 2 
  } else {
    timeser2$period.cat[i] <- 0
  }
} 

#make this data point a factor
timeser2$period.cat <- as.factor(timeser2$period.cat)



mean.calcs<- timeser2 %>% group_by(Hospital.ID, Period, period.cat) %>% summarize(count=n(),sum_Q1=sum(Q1), sum_Q2=sum(Q2), 
                                                                                  sum_Q3=sum(Q3), sum_Q4=sum(Q4), 
                                                                                  sum_Q5=sum(Q5), sum_Q6=sum(Q6), 
                                                                                  sum_Q7=sum(Q7), sum_Q8=sum(Q8),
                                                                                  sum_Q9=sum(Q9), sum_Q12=sum(Q12), 
                                                                                  blood_trans=((sum_Q3+sum_Q4+sum_Q5+sum_Q6)/sum_Q1)*1000,
                                                                                  PRBC=(sum_Q3/sum_Q1)*1000, mass_trans=(sum_Q8/sum_Q1)*1000, 
                                                                                  hyst=(sum_Q9/sum_Q1)*1000, ICU_admits=(sum_Q12/sum_Q1)*1000)


mean_hosp_by_per_cat <- mean.calcs %>% group_by(Hospital.ID, period.cat) %>% summarize(avg_blood_trans=mean(blood_trans), avg_PRBC=mean(PRBC),
                                                                                       avg_mass_trans=mean(mass_trans), avg_hyst=mean(hyst),
                                                                                       avg_ICU_admits=mean(ICU_admits))


##################################################################################                                                                                                                                 sum_Q9=sum(Q9), sum_Q12=sum(Q12), PRBC=(sum_Q3/sum_Q1)*1000, mass_trans=(sum_Q8/sum_Q1)*1000, hyst=(sum_Q9/sum_Q1)*1000, ICU_admits=(sum_Q12/sum_Q1)*1000)
#remove period 0 category ( July 2014 - September 2015)
mean_hosp_by_per_cat <- mean_hosp_by_per_cat[ mean_hosp_by_per_cat$period.cat!="0",]

#count hospital records in the table
hosp.tab <- as.data.frame(table(mean_hosp_by_per_cat$Hospital.ID))

#only get hospitals that submitted data for both periods
hosp.tab <- hosp.tab[hosp.tab$Freq>1,]

#select only those Hospitals that submitted the original data 
timeser.3 <- mean_hosp_by_per_cat[mean_hosp_by_per_cat$Hospital.ID %in% hosp.tab$Var1,]


#need to convert this to a matrix for the function to work

overall.rates.per.1 <- as.matrix(timeser.3[timeser.3$period.cat==1,-c(1,2)])



overall.rates.per.2 <- as.matrix(timeser.3[timeser.3$period.cat==2,-c(1,2)])


#### Create the t-test function for paired



f <- function(x,y){
  test <- t.test(x,y, paired = TRUE)
  data.frame(est   = test$estimate,
             pval = test$p.value)
}

#need to convert to matrix for this to work

sapply(colnames(overall.rates.per.2),function (x) f(overall.rates.per.2[,x], overall.rates.per.1[,x]))


######## Calculate Means
apply(overall.rates.per.1,2,mean)
apply(overall.rates.per.2,2,mean)


#select only those Hospitals that submitted the original data 
timeser.4 <- timeser2[timeser2$Hospital.ID %in% hosp.tab$Var1,]  



overall.rates.QBL <- timeser.4 %>% group_by( Hospital.ID, Period, period.cat,qbl.imp.fid) %>%
  summarize(count=n(),sum_Q1=sum(Q1), sum_Q2=sum(Q2), 
            sum_Q3=sum(Q3), sum_Q4=sum(Q4), 
            sum_Q5=sum(Q5), sum_Q6=sum(Q6), 
            sum_Q7=sum(Q7), sum_Q8=sum(Q8),
            sum_Q9=sum(Q9), sum_Q12=sum(Q12), blood_trans=((sum_Q3+sum_Q4+sum_Q5+sum_Q6)/sum_Q1)*1000,
            PRBC=(sum_Q3/sum_Q1)*1000, mass_trans=(sum_Q8/sum_Q1)*1000, 
            hyst=(sum_Q9/sum_Q1)*1000, ICU_admits=(sum_Q12/sum_Q1)*1000)


mean_QBL_per <- overall.rates.QBL %>% group_by(Hospital.ID, period.cat, qbl.imp.fid) %>% summarize(avg_blood_trans=mean(blood_trans), avg_PRBC=mean(PRBC),
                                                                                                   avg_mass_trans=mean(mass_trans), avg_hyst=mean(hyst),
                                                                                                   avg_ICU_admits=mean(ICU_admits))



#Low QBL Implementation Fidelity Rates

overall.rates.QBL.low.per.1 <- mean_QBL_per %>% filter(period.cat==1, qbl.imp.fid=="low")

overall.rates.QBL.low.per.1 <- as.matrix(overall.rates.QBL.low.per.1[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])

overall.rates.QBL.low.per.2 <- mean_QBL_per %>% filter(period.cat==2, qbl.imp.fid=="low")
overall.rates.QBL.low.per.2 <- as.matrix(overall.rates.QBL.low.per.2[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])
#Medium QBL Implementation Fidelity Rates

overall.rates.QBL.med.per.1 <- mean_QBL_per %>% filter(period.cat==1, qbl.imp.fid=="med")
overall.rates.QBL.med.per.1 <- as.matrix(overall.rates.QBL.med.per.1[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])

overall.rates.QBL.med.per.2 <- mean_QBL_per %>% filter(period.cat==2, qbl.imp.fid=="med")
overall.rates.QBL.med.per.2 <- as.matrix(overall.rates.QBL.med.per.2[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])
#High QBL Implementation Fidelity Rates

overall.rates.QBL.high.per.1 <- mean_QBL_per %>% filter(period.cat==1, qbl.imp.fid=="high")
overall.rates.QBL.high.per.1 <- as.matrix(overall.rates.QBL.high.per.1[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])


overall.rates.QBL.high.per.2 <- mean_QBL_per %>% filter(period.cat==2, qbl.imp.fid=="high")
overall.rates.QBL.high.per.2 <- as.matrix(overall.rates.QBL.high.per.2[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])


#### Create the t-tests

#low QBL implementation Fidelity
sapply(colnames(overall.rates.QBL.low.per.2),function (x) f(overall.rates.QBL.low.per.2[,x], overall.rates.QBL.low.per.1[,x]))

#med QBL implementation Fidelity
sapply(colnames(overall.rates.QBL.med.per.2),function (x) f(overall.rates.QBL.med.per.2[,x], overall.rates.QBL.med.per.1[,x]))

#high QBL implementation Fidelity
sapply(colnames(overall.rates.QBL.high.per.2),function (x) f(overall.rates.QBL.high.per.2[,x], overall.rates.QBL.high.per.1[,x]))



overall.rates.ra <- timeser.4 %>% group_by(Hospital.ID, Period, period.cat,ra.imp.fid) %>%
  summarize(count=n(),sum_Q1=sum(Q1), sum_Q2=sum(Q2), 
            sum_Q3=sum(Q3), sum_Q4=sum(Q4), 
            sum_Q5=sum(Q5), sum_Q6=sum(Q6), 
            sum_Q7=sum(Q7), sum_Q8=sum(Q8),
            sum_Q9=sum(Q9), sum_Q12=sum(Q12), blood_trans=((sum_Q3+sum_Q4+sum_Q5+sum_Q6)/sum_Q1)*1000,
            PRBC=(sum_Q3/sum_Q1)*1000, mass_trans=(sum_Q8/sum_Q1)*1000, 
            hyst=(sum_Q9/sum_Q1)*1000, ICU_admits=(sum_Q12/sum_Q1)*1000)


mean_ra_per <- overall.rates.ra %>% group_by(Hospital.ID, period.cat, ra.imp.fid) %>% summarize(avg_blood_trans=mean(blood_trans), avg_PRBC=mean(PRBC),
                                                                                                avg_mass_trans=mean(mass_trans), avg_hyst=mean(hyst),
                                                                                                avg_ICU_admits=mean(ICU_admits))

#Low RA Implementation Fidelity Rates

overall.rates.ra.low.per.1 <- mean_ra_per %>% filter(period.cat==1, ra.imp.fid=="low")

overall.rates.ra.low.per.1 <- as.matrix(overall.rates.ra.low.per.1[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])

overall.rates.ra.low.per.2 <- mean_ra_per %>% filter(period.cat==2, ra.imp.fid=="low")
overall.rates.ra.low.per.2 <- as.matrix(overall.rates.ra.low.per.2[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])

#Medium QBL Implementation Fidelity Rates

overall.rates.ra.med.per.1 <- mean_ra_per %>% filter(period.cat==1, ra.imp.fid=="med")
overall.rates.ra.med.per.1 <- as.matrix(overall.rates.ra.med.per.1[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])

overall.rates.ra.med.per.2 <- mean_ra_per %>% filter(period.cat==2, ra.imp.fid=="med")
overall.rates.ra.med.per.2 <- as.matrix(overall.rates.ra.med.per.2[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])
#High QBL Implementation Fidelity Rates

overall.rates.ra.high.per.1 <- mean_ra_per %>% filter(period.cat==1, ra.imp.fid=="high")
overall.rates.ra.high.per.1 <- as.matrix(overall.rates.ra.high.per.1[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])

overall.rates.ra.high.per.2 <- mean_ra_per %>% filter(period.cat==2, ra.imp.fid=="high")
overall.rates.ra.high.per.2 <- as.matrix(overall.rates.ra.high.per.2[, c("avg_blood_trans", "avg_PRBC", "avg_mass_trans", "avg_hyst","avg_ICU_admits")])

#### Create the t-tests



#low QBL implementation Fidelity
sapply(colnames(overall.rates.ra.low.per.2),function (x) f(overall.rates.ra.low.per.2[,x], overall.rates.ra.low.per.1[,x]))

#med QBL implementation Fidelity
sapply(colnames(overall.rates.ra.med.per.2),function (x) f(overall.rates.ra.med.per.2[,x], overall.rates.ra.med.per.1[,x]))

#high QBL implementation Fidelity
sapply(colnames(overall.rates.ra.high.per.2),function (x) f(overall.rates.ra.high.per.2[,x], overall.rates.ra.high.per.1[,x]))








###################### Other Calculations


#create the initial calculations.  Note, these are not the final calculations for the paper!
mean.baseline <- timeser2 %>% group_by(Hospital.ID, period.cat) %>% summarize(count=n(),sum_Q1=sum(Q1), sum_Q2=sum(Q2), 
                                                                              sum_Q3=sum(Q3), sum_Q4=sum(Q4), 
                                                                              sum_Q5=sum(Q5), sum_Q6=sum(Q6), 
                                                                              sum_Q7=sum(Q7), sum_Q8=sum(Q8))


#blood products transfused low QBL implementation fidelity

t.test(overall.rates.QBL.low.per.2$blood_trans,overall.rates.QBL.low.per.1$blood_trans, paired=TRUE)

t.test(overall.rates.QBL.med.per.2$blood_trans,overall.rates.QBL.med.per.1$blood_trans, paired=TRUE)

t.test(overall.rates.QBL.high.per.2$blood_trans,overall.rates.QBL.high.per.1$blood_trans, paired=TRUE)

#PRBC
t.test(overall.rates.QBL.low.per.2$PRBC,overall.rates.QBL.low.per.1$PRBC, paired=TRUE)

t.test(overall.rates.QBL.med.per.2$PRBC,overall.rates.QBL.med.per.1$PRBC, paired=TRUE)

t.test(overall.rates.QBL.high.per.2$PRBC,overall.rates.QBL.high.per.1$PRBC, paired=TRUE)

#Massive Trans
t.test(overall.rates.QBL.low.per.2$mass_trans,overall.rates.QBL.low.per.1$mass_trans, paired=TRUE)

t.test(overall.rates.QBL.med.per.2$mass_trans,overall.rates.QBL.med.per.1$mass_trans, paired=TRUE)

t.test(overall.rates.QBL.high.per.2$mass_trans,overall.rates.QBL.high.per.1$mass_trans, paired=TRUE)


# Hysterectomy
t.test(overall.rates.QBL.low.per.2$hyst,overall.rates.QBL.low.per.1$hyst, paired=TRUE)

t.test(overall.rates.QBL.med.per.2$hyst,overall.rates.QBL.med.per.1$hyst, paired=TRUE)

t.test(overall.rates.QBL.high.per.2$hyst,overall.rates.QBL.high.per.1$hyst, paired=TRUE)


# ICU Admits
t.test(overall.rates.QBL.low.per.2$ICU_admits,overall.rates.QBL.low.per.1$ICU_admits, paired=TRUE)

t.test(overall.rates.QBL.med.per.2$ICU_admits,overall.rates.QBL.med.per.1$ICU_admits, paired=TRUE)

t.test(overall.rates.QBL.high.per.2$ICU_admits,overall.rates.QBL.high.per.1$ICU_admits, paired=TRUE)




#blood products transfused low QBL implementation fidelity

t.test(overall.rates.ra.low.per.2$blood_trans,overall.rates.ra.low.per.1$blood_trans, paired=TRUE)

t.test(overall.rates.ra.med.per.2$blood_trans,overall.rates.ra.med.per.1$blood_trans, paired=TRUE)

t.test(overall.rates.ra.high.per.2$blood_trans,overall.rates.ra.high.per.1$blood_trans, paired=TRUE)

#PRBC
t.test(overall.rates.ra.low.per.2$PRBC,overall.rates.ra.low.per.1$PRBC, paired=TRUE)

t.test(overall.rates.ra.med.per.2$PRBC,overall.rates.ra.med.per.1$PRBC, paired=TRUE)

t.test(overall.rates.ra.high.per.2$PRBC,overall.rates.ra.high.per.1$PRBC, paired=TRUE)

#Massive Trans
t.test(overall.rates.ra.low.per.2$mass_trans,overall.rates.ra.low.per.1$mass_trans, paired=TRUE)

t.test(overall.rates.ra.med.per.2$mass_trans,overall.rates.ra.med.per.1$mass_trans, paired=TRUE)

t.test(overall.rates.ra.high.per.2$mass_trans,overall.rates.ra.high.per.1$mass_trans, paired=TRUE)


# Hysterectomy
t.test(overall.rates.ra.low.per.2$hyst,overall.rates.ra.low.per.1$hyst, paired=TRUE)

t.test(overall.rates.ra.med.per.2$hyst,overall.rates.ra.med.per.1$hyst, paired=TRUE)

t.test(overall.rates.ra.high.per.2$hyst,overall.rates.ra.high.per.1$hyst, paired=TRUE)


# ICU Admits
t.test(overall.rates.ra.low.per.2$ICU_admits,overall.rates.ra.low.per.1$ICU_admits, paired=TRUE)

t.test(overall.rates.ra.med.per.2$ICU_admits,overall.rates.ra.med.per.1$ICU_admits, paired=TRUE)

t.test(overall.rates.ra.high.per.2$ICU_admits,overall.rates.ra.high.per.1$ICU_admits, paired=TRUE)


