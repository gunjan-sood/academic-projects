boston=read.csv("e:/5080/Project/311_SHU1.CSV",header=T)

boston$OPEN_DT=as.character(boston$OPEN_DT)

###Question 2

##Day
library(dplyr)
Q21=as.data.frame(count_(boston,c("date"),sort = T))
hist(Q21$n,breaks = 10,probability =T,xlab="Number of Service Requests",ylab="Probability",main = "Probabilty of Service Requests By Day",ylim = c(0,0.0065))
lines(density(Q21$n),col="blue")

library(mixtools)
out=normalmixEM(Q21$n,k=2,fast = FALSE)
summary(out)
curve(dnorm(x,747.165732,133.566575),450,1300,add = T,col="red")
curve(dnorm(x,291.532169,61.412108),0,450,add = T,col="red")

##Hour
Q22=as.data.frame(count_(boston,c("date.hour"),sort = T))
q2.add0=matrix(NA,933,2)
q2.add0[,1]=c(1:933)
q2.add0[,2]=rep(0,933)
colnames(q2.add0)=c("date.hour","n")
Q22.0=rbind(Q22,q2.add0)
hist(Q22.0$n,breaks = 100)
mu=mean(Q22.0$n)
ks.test(Q22.0$n,"pexp",1/24)

Q23=as.data.frame(count_(Q22,c("n"),sort = T))
Q23=rbind(c(0,933),Q23)
plot(Q23$n,Q23$nn,xlab="Observation",ylab="Frequency",main="Frequency of Service Requests By Hour")
curve((1/mu)*exp(-(1/mu)*x)*7800,0,250,add = T,col="red",lwd=2)

mydata=read.csv("e:/5080/Project/311_SHU1.CSV",header=T)
# Question 3(a)
# Most Popular Sourse
table(mydata$Source)

## Question 3(a)
## The most popular way to request a service is Constituent Call. The number is 85,356.


# Question 3(b)
nightdata=subset(mydata,timezone==4)
nightdata.app=subset(nightdata,Source=="Citizens Connect App")
nightdata.call=subset(nightdata,Source=="Constituent Call")
night.callapp=rbind(nightdata.call,nightdata.app)
View(night.callapp)
table(night.callapp$Source)
n=nrow(night.callapp)
p.app=sum(night.callapp$Source=="Citizens Connect App")/n
p.call=sum(night.callapp$Source=="Constituent Call")/n
p.diff=p.app-p.call
z.sta=p.diff/sqrt(0.5*0.5/n)


# Question 3(c)
summary(mydata)
twitter=subset(mydata,Source=="Twitter")
table(twitter$TYPE)
## The type of service requests most often associated with a Twitter request source is parking enforcement. The number is 47.


#Q4ACount the max
library(dplyr)
Q41=as.data.frame(count_(boston,c("timezone"),sort = T))
Q4=as.data.frame(count_(boston,c("timezone","TYPE"),sort = T))
Q4.a=as.data.frame(Q4 %>% group_by(timezone) %>% filter(n == max(n)))
#For time zone 1 it's street light outages, for time zone 2, 3 and 4,
#it's Parking enforcement

#Q4b
boston$date=as.factor(boston$date)
boston$timezone=as.factor(boston$timezone)
Q4.b=as.data.frame(Q4 %>% group_by(timezone) %>% arrange(desc(n)) %>% slice(1:3))
Q4.light=boston[boston$TYPE=="Street Light Outages",]
Q4.b.light=as.data.frame(count_(Q4.light,c("date","timezone"),sort = T))%>% arrange(desc(date))
anova(lm(n~timezone,Q4.b.light))
#Reject the null hypthesis that the probable for each timezone is the same
light=aov(n~timezone,data = Q4.b.light)
TukeyHSD(light)
##Under level 5%, 2&1 are different, 4 and 2 are differnet.


Q4.parking=boston[boston$TYPE=="Parking Enforcement",]
Q4.b.parking=as.data.frame(count_(Q4.parking,c("date","timezone"),sort = T))%>% arrange(desc(date))
anova(lm(n~timezone,Q4.b.parking))
#Reject the null hypthesis that the probable for each timezone is the same
parking=aov(n~timezone,data = Q4.b.parking)
TukeyHSD(parking)
##under level 5%, 3 and 2 are the same.


Q4.cleaning=boston[boston$TYPE=="Requests for Street Cleaning",]
Q4.b.cleaning=as.data.frame(count_(Q4.cleaning,c("date","timezone"),sort = T))%>% arrange(desc(date))
anova(lm(n~timezone,Q4.b.cleaning))
#Reject the null hypthesis that the probable for each timezone is the same
cleaning=aov(n~timezone,data = Q4.b.cleaning)
TukeyHSD(cleaning)
##under level 5%, all are different.



Q4.pickup=boston[boston$TYPE=="Schedule a Bulk Item Pickup",]
Q4.b.pickup=as.data.frame(count_(Q4.pickup,c("date","timezone"),sort = T))%>% arrange(desc(date))
anova(lm(n~timezone,Q4.b.pickup))
#Reject the null hypthesis that the probable for each timezone is the same
pickup=aov(n~timezone,data = Q4.b.pickup)
TukeyHSD(pickup)
##under level 5%, 4 and 1, 3 and 2 are the same.




##Q4c
Q4.neighbor=as.data.frame(count_(boston,"neighborhood",sort = T))
Q4.neighbor1=as.data.frame(count_(boston,c("neighborhood","TYPE"),sort = T))
write.csv(Q4.neighbor1,"e:/5080/Project/neighbor.csv")

Q4.c1=as.data.frame(Q4.neighbor1 %>% group_by(TYPE) %>% arrange(desc(n)) %>% slice(1:3))



Q4.c=as.data.frame(Q4.neighbor %>% arrange(desc(n)) %>% slice(1:10))
library(MASS)
Q4.independence=table(boston$neighborhood,boston$timezone)
chisq.test(Q4.independence) 
#Reject the null hypothesis

##Q4d
Q4.department=as.data.frame(count_(boston,"Department",sort = T))
Q4.d=as.data.frame(Q4.department %>% arrange(desc(n)) %>% slice(1:10))
##Top ten departments

###Q4e
Q4.weekday=as.data.frame(count_(boston,c("weekday","weeknum"),sort = T))
Q4.weekday1=as.data.frame(count_(boston,c("weekday","TYPE"),sort = T))
#Most recurrent type
Q4.e=as.data.frame(Q4.weekday1 %>% group_by(weekday) %>% filter(n == max(n)))
#Top 3 request
Q4.e3=as.data.frame(Q4.weekday1 %>% group_by(weekday) %>% arrange(desc(n)) %>% slice(1:3))
##Number of requests for weekday
Q4.num=as.data.frame(count_(boston,"weekday",sort = T))
#ANOVA TEST????
Q4.weekday$weekday=as.factor(Q4.weekday$weekday)
anova(lm(n~weekday,data = Q4.weekday))

weekday=aov(n~weekday,data = Q4.weekday)
TukeyHSD(weekday)
plot(TukeyHSD(weekday),col="red")
plot(TukeyHSD(weekday))
##reject the null hypothesis, not evenly distributed


##School holiday
Q4.holiday=as.data.frame(count_(boston,c("holiday","TYPE"),sort = T))
Q4.holiday1=as.data.frame(count_(boston,c("holiday"),sort = T))
x=Q4.holiday[Q4.holiday$holiday=="N",]
y=Q4.holiday[Q4.holiday$holiday=="Y",]
t.test(x$n,y$n)
anova(lm(n~holiday,data = Q4.holiday))
x=t(as.matrix(Q4.holiday1$n))
prop.test(x)
##reject the null hypothesis, holiday and regular days are different.

## Question 5(a)
mydata.open=subset(mydata,CASE_STATUS=="Open")
mydata.closed=subset(mydata,CASE_STATUS=="Closed")
# Requests of Open Status
barplot(table(mydata.open$TYPE),main = " Requests of Open Status",xlab="Type of Requests",xaxt="n" )
# Requests of Closed Status
barplot(table(mydata.closed$TYPE),xaxt="n",main = " Requests of Closed Status" )
# Percentage of Open Requests
100*sum(mydata$CASE_STATUS=="Open")/nrow(mydata)

# Question 5(b)
table(mydata.open$TYPE)>1500

# Question 5(c)
location0=subset(mydata,neighborhood=="Allston" )
summary(location0)
location1=subset(mydata,neighborhood=="Allston / Brighton" )
summary(location1)
location2=subset(mydata,neighborhood=="Back Bay" )
summary(location2)
location3=subset(mydata,neighborhood=="Beacon Hill")
summary(location3)
location4=subset(mydata,neighborhood=="Boston" )
summary(location4)
location5=subset(mydata,neighborhood=="Brighton" )
summary(location5)
location6=subset(mydata,neighborhood=="Charlestown" )
summary(location6)
location7=subset(mydata,neighborhood=="Chestnut Hill" )
summary(location7)
location8=subset(mydata,neighborhood=="Dorchester" )
summary(location8)
location9=subset(mydata,neighborhood=="Downtown / Financial District" )
summary(location9)
location10=subset(mydata,neighborhood=="East Boston" )
summary(location10)
location11=subset(mydata,neighborhood=="Fenway / Kenmore / Audubon Circle / Longwood" )
summary(location11)
location12=subset(mydata,neighborhood=="Greater Mattapan" )
summary(location12)
location13=subset(mydata,neighborhood=="Hyde Park" )
summary(location13)
location14=subset(mydata,neighborhood=="Jamaica Plain"  )
summary(location14)
location15=subset(mydata,neighborhood=="Mattapan"  )
summary(location15)
location16=subset(mydata,neighborhood=="Mission Hill"  )
summary(location16)
location17=subset(mydata,neighborhood=="Roslindale"  )
summary(location17)
location17=subset(mydata,neighborhood=="Roxbury"  )
summary(location17)
location18=subset(mydata,neighborhood=="South Boston"  )
summary(location18)
location19=subset(mydata,neighborhood=="South Boston / South Boston Waterfront"  )
summary(location19)
location20=subset(mydata,neighborhood=="South End"  )
summary(location20)
location21=subset(mydata,neighborhood=="West Roxbury"  )
summary(location21)


table(mydata$neighborhood_services_district,mydata$TYPE)[,1]
boston.bike=subset(mydata,TYPE=="Abandoned Bicycle")
bike.neighbour=table(boston.bike$neighborhood)
View(bike.neighbour)

# Question 6
tz1=subset(mydata,timezone==1)
tz2=subset(mydata,timezone==2)
tz3=subset(mydata,timezone==3)
tz4=subset(mydata,timezone==4)
x=c(1,2,3,4)
y=c(nrow(tz1),nrow(tz2),nrow(tz3),nrow(tz4))
plot(x,y,type="l",xlab="Timezone",ylab="Number of Requests",main="Number of Requests in Each Timezone",xaxt="n")
axis(1, at=1:4, labels=c("0:00-5:59","6:00-11:59","12:00-17:59","18:00-23:59"))

# Question 7
call=subset(mydata,Source=="Constituent Call")
tz1.call=subset(call,timezone==1)
tz2.call=subset(call,timezone==2)
tz3.call=subset(call,timezone==3)
tz4.call=subset(call,timezone==4)
x=c(1,2,3,4)
y=c(nrow(tz1.call),nrow(tz2.call),nrow(tz3.call),nrow(tz4.call))
plot(x,y,type="l",xlab="Timezone",ylab="Number of Requests",main="Number of Requests by Call",xaxt="n")
axis(1, at=1:4, labels=c("0:00-5:59","6:00-11:59","12:00-17:59","18:00-23:59"))
tz1.boston=subset(mydata,timezone==1)
tz2.boston=subset(mydata,timezone==2)
tz3.boston=subset(mydata,timezone==3)
tz4.boston=subset(mydata,timezone==4)
x=c(1,2,3,4)
y=c(nrow(tz1.boston),nrow(tz2.boston),nrow(tz3.boston),nrow(tz4.boston))
plot(x,y,type="l",xlab="Timezone",ylab="Number of Requests",main="Total Number of Requests of Each Timezone",xaxt="n")
axis(1, at=1:4, labels=c("0:00-5:59","6:00-11:59","12:00-17:59","18:00-23:59"))
barplot(table(tz2.call$TYPE))
barplot(table(tz3.call$TYPE))
table(tz3.call$TYPE)>1300
which.max(table(tz2.call$TYPE))
which.max(table(tz3.call$TYPE))

boston.bulk=subset(mydata,TYPE=="Schedule a Bulk Item Pickup")
boston.missed=subset(mydata,TYPE=="Missed Trash/Recycling/Yard Waste/Bulk Item")
table(boston.bulk$Source)
table(boston.missed$Source)

# Question 7.2
# In boston, there are four main issues need to be considered and dealt with, "Street Light Outages", ??????Parking Enforcement??????, ??????Requests for Street Cleaning?????? and ??????Schedule a Bulk Item Pickup". 
boston.parking=subset(mydata,TYPE=="Parking Enforcement")
temp=table(boston.parking$Department)
View(temp)

# The top one problem is Parking Enforcement since the number of which is always the largest in each day. 
boston.street=subset(mydata,TYPE="Requests for Street Cleaning")
which.max(table(boston.street$neighborhood))


# The number of requests for street cleaning greater 10,000 by neighbourhood are listed in table"1". The Mayor might consider to increase the number of cleaners in these regions.  Meanwhile, the above 8 regions are also the top eight neighborhoods with the highest number of requests, which indicates these areas should be more concerned. Besides, the number of source is listed in table$$. 
temp=table(mydata$Source)
View(temp)

# Street Light Outages by neighborhood
boston.outage=subset(mydata,TYPE=="Street Light Outages")
temp=table(boston.outage$neighborhood)
View(temp)
