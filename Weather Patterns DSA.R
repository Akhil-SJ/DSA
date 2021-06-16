### visualize historic peak electric demand
##  weather senesitivity to elctric demand
### peak demand by weather, day, season hour
## variance across year, season patterns winter, summer




## data notes: electric hourly load for 2 years 2016-2017
## weather data for 15 years 2003- 2017
### average wind direction removed to mostly missing data. Could be that NA is 0 and that is the base direction. logically 


## library list

library('ggplot2')
library(zoo)
library(tidyverse)
library(plyr)
library(readxl)
library(data.table)
library(scales)
library(anytime)
library(hrbrthemes)
library(BSDA)
#### import data sets
Electric <- read_excel("Downloads/System Load Analysis.xlsx", 
                       sheet = "Electric Demand", col_types = c("date", 
                                                                "numeric"))
Weather <- read_excel("Downloads/System Load Analysis.xlsx", 
                      sheet = "Weather")


# Time format
Electric$Date <- strptime(Electric$`Eastern Date Hour`,format="%Y-%m-%d %H:%M:%S")
Electric$day <- trunc(Electric$Date,"day")
Electric$day = as.Date(Electric$day)

# electric day

E_Day = as.data.frame(ddply(Electric,.(day),
      summarize,
      aveMW=mean(MW),
      maxMW=max(MW),
      minMW=min(MW)
))




E_Day$weeks <- cut(E_Day[, 'day'], breaks="week")

agg =setDT(E_Day)[ , .(mean_MW = mean(aveMW)), by = weeks]
agg = agg[-1,]


### wwek over week electric demand general

ggplot(agg, aes(as.Date(weeks), mean_MW))  + scale_x_date() +
  ylab("MegaWatts") + xlab("Week") + geom_line()+
  ylim(750,1750)  





### merge data

model = merge(Electric, Weather,  by.x = "Eastern Date Hour" , by.y = "datetime" , sort = TRUE)



### plot charts of time and MW, Dew wind and etc to see trend lines



m_day = as.data.frame(ddply(model,.(day),
                            summarize,
                            aveMW=mean(MW),
                            aveTEMP=mean(tempf),
                            aveDEW=mean(dewpoint),
                            aveWIND=mean(winddir),
                            aveWSpeed=mean(windspeed),
                            aveSKY=mean(skycond)
))


m_day$day = as.Date(m_day$day)
m_day$weeks <- cut(m_day[, 'day'], breaks="week")

### take out wind direction due to missing/incomplete, heres a graph why

m_day$aveWIND[is.na(m_day$aveWIND)] <- 0

ggplot(m_day, aes(as.Date(weeks), aveWIND))  + scale_x_date() +
  ylab("Wind Direction") + xlab("day") + geom_line()

m_day = subset(m_day, select = -c(aveWIND) )

##remove missing

m_day = na.omit(m_day)

### week on week change 

w_1 =setDT(m_day)[ , .(mean_MW = mean(aveMW)), by = weeks]
w_2 =setDT(m_day)[ , .(mean_TEMP = mean(aveTEMP)), by = weeks]
w_3 =setDT(m_day)[ , .(mean_WSpeed = mean(aveWSpeed)), by = weeks]
w_4 =setDT(m_day)[ , .(mean_SKY = mean(aveSKY)), by = weeks]
w_5 =setDT(m_day)[ , .(mean_Dew = mean(aveDEW)), by = weeks]


w_agg = Reduce(function(x, y) merge(x, y, all=TRUE), list(w_1, w_2, w_3,w_4, w_5))

w_agg = w_agg[-1,]





###graph it out by weather


ggplot(w_agg, aes(as.Date(weeks), mean_TEMP))  + scale_x_date() +
  ylab("Fahrenheit") + xlab("day") + geom_line()

ggplot(w_agg, aes(as.Date(weeks), mean_Dew))  + scale_x_date() +
  ylab("Dew Point") + xlab("day") + geom_line()


ggplot(w_agg, aes(as.Date(weeks), mean_WSpeed))  + scale_x_date() +
  ylab("Wind Speed") + xlab("day") + geom_line()

ggplot(w_agg, aes(as.Date(weeks), mean_SKY))  + scale_x_date() +
  ylab("Sky Condition") + xlab("day") + geom_line()




#### weather vs mw
ggplot(w_agg, aes( mean_TEMP , mean_MW))    + geom_smooth() + theme_bw() + ggtitle("Temperature and Electric Demand") +
   ylab("Megawatt (MW)") +
  xlab("Fahrenheit") 

ggplot(w_agg, aes( mean_Dew, mean_MW))   + geom_smooth() + theme_bw() + ggtitle("Dew and Electric Demand") +
  ylab("Megawatt (MW)") +
  xlab("Dew") 
ggplot(w_agg, aes( mean_TEMP , mean_Dew))    + geom_smooth() + theme_bw() + ggtitle("Temperature and Dew") +
  ylab("Dew") +
  xlab("Fahrenheit") 

ggplot(w_agg, aes( mean_WSpeed, mean_MW))   +
  xlab("Wind Speed") + ylab("MW") + geom_line() 

ggplot(w_agg, aes( mean_SKY, mean_MW))  +
  xlab("Sky Condition") + ylab("MW") + geom_line() 




#NOTES:
### weather sensitivity, find elasticity for temp, dew. neg. parabolic trend here
## sky, wind condition seem uncorrelateed
## also price sensetivity by seasons




##add day formating 

model$Date = as.Date(model$Date)

model$weekday <- weekdays(as.Date(model$Date))
model$weekday <- factor(model$weekday,levels = c("Sunday", "Monday", "Tuesday",'Wednesday',"Thursday","Friday","Saturday"))


######### change by season

# 2016
winter16 <- subset(model, Date >= "2015-12-21" & Date < "2016-03-20")
spring16 <- subset(model, Date >= "2016-03-20" & Date < "2016-06-20")
summer16 <- subset(model, Date >= "2016-06-20" & Date < "2016-09-22")
fall16 <- subset(model, Date >= "2016-09-22" & Date < "2016-12-21")

#2017

winter17 <- subset(model, Date >= "2016-12-21" & Date < "2017-03-20")
spring17 <- subset(model, Date >= "2017-03-20" & Date < "2017-06-20")
summer17 <- subset(model, Date >= "2017-06-20" & Date < "2017-09-22")
fall17 <- subset(model, Date >= "2017-09-22" & Date < "2017-12-21")

summ1= rbind(summer16,summer17)
win1 = rbind(winter16,winter17)





##### fit temp elasticty 
w_agg2= na.omit(w_agg)
w_agg2$f2= w_agg2$mean_TEMP^2

quad_temp = lm(w_agg2$mean_MW ~ w_agg2$mean_TEMP + w_agg2$f2)

predict <- predict(quad_temp,list(mean_TEMP=w_agg$mean_TEMP, f2=w_agg$mean_TEMP^2))

plot(w_agg2$mean_TEMP,w_agg2$mean_MW)
lines(w_agg2$mean_TEMP,predict, col = "blue")

summary(quad_temp)

M1 = mean(w_agg2$mean_MW)
T1 = mean(w_agg2$mean_TEMP)

### first dervivitve -47.9 + 1.0076 mean_TEMP

## elasticity dy/dx * x/y using averages

Temp_elastic = (-47.9 + (1.0076* T1)) * (T1/M1)
Temp_elastic

##### fit dew elasticty 
w_agg2$d2= w_agg2$mean_Dew^2

quad_dew = lm(w_agg2$mean_MW ~ w_agg2$mean_Dew + w_agg2$d2)

predict <- predict(quad_dew,list(mean_Dew=w_agg2$mean_Dew, d2=w_agg2$mean_Dew^2))

plot(w_agg2$mean_Dew,w_agg$mean_MW)
lines(w_agg2$mean_Dew,predict, col = "blue")

summary(quad_dew)

M2 = mean(w_agg2$mean_MW)
D1 = mean(w_agg2$mean_Dew)

### first dervivitve -30 + .85 mean_dew

## elasticity dy/dx * x/y using averages

Dew_elastic = (-30.07222 + ((0.42598*2)* D1)) * (D1/M2)
Dew_elastic






   ### explain by days of week, season hour

model$weekday <- weekdays(as.Date(model$day))

weekday = as.data.frame(ddply(model,.(weekday),
                            summarize,
                            aveMW=mean(MW),
                            aveTEMP=mean(tempf),
                            aveDEW=mean(dewpoint),
                            aveWIND=mean(winddir),
                            aveWSpeed=mean(windspeed),
                            aveSKY=mean(skycond)
))





win_16 =setDT(winter16)[ , .(mean_MW = mean(MW)), by = weekday]
spr_16 =setDT(spring16)[ , .(mean_MW = mean(MW)), by = weekday]
sum_16 =setDT(summer16)[ , .(mean_MW = mean(MW)), by = weekday]
fall_16 =setDT(fall16)[ , .(mean_MW = mean(MW)), by = weekday]

win_17 =setDT(winter17)[ , .(mean_MW = mean(MW)), by = weekday]
spr_17 =setDT(spring17)[ , .(mean_MW = mean(MW)), by = weekday]
sum_17 =setDT(summer17)[ , .(mean_MW = mean(MW)), by = weekday]
fall_17 =setDT(fall17)[ , .(mean_MW = mean(MW)), by = weekday]
win = setDT(win1)[ , .(mean_MW = mean(MW)), by = weekday]
summ  =setDT(summ1)[ , .(mean_MW = mean(MW)), by = weekday]



weekday$weekday <- factor(weekday$weekday,levels = c("Sunday", "Monday", "Tuesday",'Wednesday',"Thursday","Friday","Saturday"))

### bar plot of mw by day. no general difference 
p<-ggplot(data=weekday, aes(x= weekday, y=aveMW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
p + ylab("Average MegaWatts") + xlab("Day of Week")


## check day change by 4 seasons



model$weekday <- factor(model$weekday,levels = c("Sunday", "Monday", "Tuesday",'Wednesday',"Thursday","Friday","Saturday"))

## test out some bar charts by season

###################### graph output 1


ggplot(data= win, aes(x= weekday, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Day of Week") + ggtitle("Electric Demand by Day (Winter)") + ylim(0,1500)
ggplot(data= summ, aes(x= weekday, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Day of Week") + ggtitle("Electric Demand by Day (Summer)") + ylim(0,1500)

### compare summer and winter demanda\s
z.test(win$mean_MW, y = summ$mean_MW, alternative = "two.sided", mu = 0, sigma.x = (sd(win$mean_MW)),
       sigma.y = (sd(summ$mean_MW)), conf.level = 0.95)
################################

ggplot(data= win_17, aes(x= weekday, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Day of Week") + ggtitle("Winter 2017")



ggplot(data= spr_16, aes(x= weekday, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Day of Week") + ggtitle("Spring 2016")
ggplot(data= spr_17, aes(x= weekday, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Day of Week") + ggtitle("Spring 2017")


ggplot(data= sum_16, aes(x= weekday, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Day of Week") + ggtitle("Summer 2016")
ggplot(data= sum_17, aes(x= weekday, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Day of Week") + ggtitle("Summer 2017")


ggplot(data= fall_16, aes(x= weekday, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Day of Week") + ggtitle("Fall 2016")
ggplot(data= fall_17, aes(x= weekday, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Day of Week") + ggtitle("Fall 2017")


#### elasticity of each by season


###segement 
w_agg$weeks = as.Date(w_agg$weeks)
# 2016
w16 <- subset(w_agg, weeks >= "2015-12-21" & weeks < "2016-03-20")
spr16 <- subset(w_agg, weeks >= "2016-03-20" & weeks < "2016-06-20")
s16 <- subset(w_agg, weeks >= "2016-06-20" & weeks < "2016-09-22")
f16 <- subset(w_agg, weeks >= "2016-09-22" & weeks < "2016-12-21")

#2017

w17 <- subset(w_agg, weeks >= "2016-12-21" & weeks < "2017-03-20")
spr17 <- subset(w_agg, weeks >= "2017-03-20" & weeks < "2017-06-20")
s17 <- subset(w_agg, weeks >= "2017-06-20" & weeks < "2017-09-22")
f17 <- subset(w_agg, weeks >= "2017-09-22" & weeks < "2017-12-21")


## combine seasons

summer = rbind(s16,s17)
spring = rbind(spr16,spr17)
winter = rbind(w16,w17)
fall = rbind(f16,f17)

##### ############################fit temp elasticty summer winter


lin_temp_summer = lm(summer$mean_MW ~ summer$mean_TEMP)

predict_summer <- predict(lin_temp_summer,list(mean_TEMP=summer$mean_TEMP))

plot(summer$mean_TEMP,summer$mean_MW)
lines(summer$mean_TEMP,predict_summer, col = "blue")

summary(lin_temp_summer)

M1_summer = mean(summer$mean_MW)
T1_summer = mean(summer$mean_TEMP)

### first dervivitve 

## elasticity dy/dx * x/y using averages

Temp_elastic_summer = ((28.187)) * (T1_summer/M1_summer)
Temp_elastic_summer
##########################################

##### fit dew elasticty 


lin_dew_summer = lm(summer$mean_MW ~ summer$mean_Dew)

predict <- predict(lin_dew_summer,list(mean_Dew=w_agg$mean_Dew, d2=w_agg$mean_Dew^2))

plot(summer$mean_Dew,summer$mean_MW)
lines(summer$mean_Dew,predict, col = "blue")

summary(lin_dew_summer)

M2_summer = mean(summer$mean_MW)
D1_summer = mean(summer$mean_Dew)

### first dervivitve -30 + .85 mean_dew

## elasticity dy/dx * x/y using averages

Dew_elastic_summer =  28.424  * (D1_summer/M2_summer)


######winter



lin_temp_winter = lm(winter$mean_MW ~ winter$mean_TEMP)

predict_winter <- predict(quad_temp_winter,list(mean_TEMP=winter$mean_TEMP))

plot(winter$mean_TEMP, winter$mean_MW)
lines(winter$mean_TEMP,predict_winter, col = "blue")

summary(lin_temp_winter)

M1_winter = mean(winter$mean_MW)
T1_winter = mean(winter$mean_TEMP)

### first dervivitve 

## elasticity dy/dx * x/y using averages

Temp_elastic_winter = ((-7.492 )) * (T1_winter/M1_winter)
Temp_elastic_winter

##### fit dew elasticty 


lin_dew_winter= lm(winter$mean_MW ~ winter$mean_Dew)

predict <- predict(lin_dew_winter,list(mean_Dew=winter$mean_Dew))

plot(winter$mean_Dew,winter$mean_MW)
lines(winter$mean_Dew,predict, col = "blue")

summary(lin_dew_winter)

M2_winter = mean(winter$mean_MW)
D1_winter = mean(winter$mean_Dew)

### first dervivitve -30 + .85 mean_dew

## elasticity dy/dx * x/y using averages

Dew_elastic_winter =  -6.532  * (D1_winter/M2_winter)
Dew_elastic_winter


###########################################








#formating

model$hour = format(strptime(model$`Eastern Date Hour`,"%Y-%m-%d %H:%M:%S"),'%H:%M')


## gerenal hour by hour summary





# 2016
hourW16 <- subset(model, Date >= "2015-12-21" & Date < "2016-03-20")
hourSP16 <- subset(model, Date >= "2016-03-20" & Date < "2016-06-20")
hourS16 <- subset(model, Date >= "2016-06-20" & Date < "2016-09-22")
hourF16 <- subset(model, Date >= "2016-09-22" & Date < "2016-12-21")

#2017

hourW17 <- subset(model, Date >= "2016-12-21" & Date < "2017-03-20")
hourSP17 <- subset(model, Date >= "2017-03-20" & Date < "2017-06-20")
hourS17 <- subset(model, Date >= "2017-06-20" & Date < "2017-09-22")
hourF17 <- subset(model, Date >= "2017-09-22" & Date < "2017-12-21")

hW= rbind(hourW16,hourW17)
hS= rbind(hourS16,hourS17)

hour_W16 =setDT(hourW16)[ , .(mean_MW = mean(MW)), by = hour]
hour_SP16 =setDT(hourSP16)[ , .(mean_MW = mean(MW)), by = hour]
hour_S16 =setDT(hourS16)[ , .(mean_MW = mean(MW)), by = hour]
hour_F16 =setDT(hourF16)[ , .(mean_MW = mean(MW)), by = hour]

hour_W17 =setDT(hourW17)[ , .(mean_MW = mean(MW)), by = hour]
hour_SP17 =setDT(hourSP17)[ , .(mean_MW = mean(MW)), by = hour]
hour_S17 =setDT(hourS17)[ , .(mean_MW = mean(MW)), by = hour]
hour_F17 =setDT(hourF17)[ , .(mean_MW = mean(MW)), by = hour]

h_W=setDT(hW)[ , .(mean_MW = mean(MW)), by = hour]
h_s =setDT(hS)[ , .(mean_MW = mean(MW)), by = hour]


######################################time bar charts

ggplot(data= h_W, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+ ylim(0,1600) +
  theme_minimal() + ylab("Average MegaWatts") + xlab("Hour (EST)") + ggtitle("Electric Demand by Hour (Winter)") + theme(axis.text.x = element_text(angle = 45))


ggplot(data= h_s, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+ ylim(0,1600) +
  theme_minimal() + ylab("Average MegaWatts") + xlab("Hour (EST)") + ggtitle("Electric Demand by Hour (Summer)") + theme(axis.text.x = element_text(angle = 45))

### compare vairance with f test
var.test(h_W$mean_MW, h_s$mean_MW, alternative = "two.sided")
###################################################


###general trends

ggplot(data= hour_W16, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") +xlab("Hour (EST)") + ggtitle("Winter 2016")
ggplot(data= hour_W17, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Hour (EST)") + ggtitle("Winter 2017")



ggplot(data= hour_SP16, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Hour (EST)") + ggtitle("Spring 2016")
ggplot(data= hour_SP17, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Hour (EST)")+ ggtitle("Spring 2017")


ggplot(data= hour_S16, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Hour (EST)") + ggtitle("Summer 2016")
ggplot(data= hour_S17, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Hour (EST)") + ggtitle("Summer 2017")


ggplot(data= hour_F16, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") +xlab("Hour (EST)") + ggtitle("Fall 2016")


ggplot(data= hour_F7, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Hour (EST)") + ggtitle("Fall 2017") + theme(axis.text.x = element_text(angle = 45))




## overall hour  trend

hour16_17 =setDT(model)[ , .(mean_MW = mean(MW)), by = hour]

ggplot(data= hour16_17, aes(x= hour, y=mean_MW)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + ylab("Average MegaWatts") + xlab("Hour (EST)") + ggtitle("Hours by MW") + theme(axis.text.x = element_text(angle = 45))






###################################################model

###use the 2016/17 data as train test is after


set = merge(Electric, Weather,  by.x = "Eastern Date Hour" , by.y = "datetime" , sort = TRUE)

past = subset(Weather, datetime < "2016-01-01")


## use a regression model here 

### ignore winddir winder spped sky cond

###train/test split on 2017-2017 data

train <-sample_frac(set, 0.8)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-set[-sid,]


###set up data

train$t2 = train$tempf^2
train$d2 = train$dewpoint^2

train = na.omit(train)



train_model= lm(train$MW ~ train$tempf + train$dewpoint + train$t2 + train$d2 )
summary(train_model, robust=TRUE)


### test data

test$t2 = test$tempf^2
test$d2 = test$dewpoint^2
test = na.omit(test)


test$predict = 1731+ (test$tempf *-28.76 ) + (test$t2* .3321) + (test$dewpoint *-6.328 ) + (test$d2* .06838)

RMSE = function(m, o,x){
  sqrt((mean((m - o)^2))/nrow(x))
}

RMSE(test$MW,test$predict,test)
mean(test$MW)
## rsme is relatively small 3.726 by average 1146.68 MW, ie ~.3% R^2 .5702


##################run prediction on rest
past$t2 = past$tempf^2
past$d2 = past$dewpoint^2
past = na.omit(past)

past$predict = 1731+ (past$tempf *-28.76 ) + (past$t2* .3321) + (past$dewpoint *-6.328 ) + (past$d2* .06838)


plot(past$datetime,past$predict)


# Do the calculations
past$Date <- strptime(past$datetime,format="%Y-%m-%d %H:%M:%S")
past$day <- trunc(past$Date,"day")
past$day = as.Date(past$day)

# electric day

past_day = aggregate(past[, 9], list(past$day), mean)



past_day$Group.1


past_day$weeks <- cut(past_day[, 'Group.1'], breaks="week")

agg_past =setDT(past_day)[ , .(mean_MW = mean(predict)), by = weeks]
agg = agg[-1,]


### wwek over week electric demand general

ggplot(agg_past, aes(as.Date(weeks), mean_MW))  + scale_x_date() +
  ylab("Aggregated by Week") + xlab("Week") + geom_line(color="steelblue") + ylim(750,1750)   +
  theme_minimal() + ylab("Predicted MegaWatts") + xlab("Date") + ggtitle("Forecasted Electric Demand") 

  