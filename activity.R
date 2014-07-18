##Code by Lakshmi Muralidharan
##July 17 2014

##Unzip the datafile and read the .csv file

unzip(zipfile = "activity.zip")
data1<-read.csv("activity.csv")

##Remove the NAs from the data frame

activity_naomit<-na.omit(data1)

##Determine the total sum of total number of steps each day and make a plot

activitysum<-aggregate.data.frame(x = activity_naomit$steps, by = list(activity_naomit$date),FUN = sum)
activitysum$Group.1<-as.Date(activitysum$Group.1)
activitysum$x<-as.numeric(activitysum$x)
png(filename = "figures/plot1.png")
hist(activitysum$x,xlab = "Steps per day",main = "Sum of Steps per day")
dev.off()

##Calculate the mean and median of number of steps taken per day

stepsmean<-mean(activitysum$x)
stepsmedian<-median(activitysum$x)

##Make a time series plot of the 5 minute interval and the average number of steps taken, averaged across all days

activityintervalmean<-aggregate.data.frame(x = activity_naomit$steps, by = list(activity_naomit$interval),FUN = mean)

##plot it as a time series plot

png(filename = "figures/plot2.png")
plot(x = activityintervalmean$Group.1,y = activityintervalmean$x, type = 'l', xlab = "Interval", ylab= "Average number of steps", main = "Time series plot of Steps vs Interval")
dev.off()

##Time interval with maximum average number of mean steps
maxmeansteps <- max(activityintervalmean$x)
##maxmeanstepsinterval<-activityintervalmean[maxmeansteps,1]
int_max <- activityintervalmean$Group.1[match(maxmeansteps,activityintervalmean$x)]

##Total number of missing values

missingvalues<-sum(is.na(data1))

##Impute the missing values with the average of the time interval value obtained previously
library(Hmisc)
data2<-data1
data2$nanewval <- rep(activityintervalmean$x, 61)
subset <- subset(data2, is.na(steps)== TRUE)
subset$steps <- impute(subset$steps,subset$nanewval)
data2$steps <- impute(data2$steps, subset$steps)

##Draw a histogram of number of steps taken during each day
factivitysum<-aggregate.data.frame(x = data2$steps, by = list(data2$date),FUN = sum)
factivitysum$Group.1<-as.Date(factivitysum$Group.1)
factivitysum$x<-as.numeric(factivitysum$x)
png(filename = "figures/plot3.png")
hist(factivitysum$x,xlab = "Steps per day",main = "Sum of Steps per day")
dev.off()
##Calculate the mean and median of total number of steps taken per day
fstepsmean<-mean(factivitysum$x)
fstepsmedian<-median(factivitysum$x)

##Seperate the data into weekdays and weekends by forming a new vector called "daylevel"
data2$date<-as.Date(data2$date)
data2$steps<-as.numeric(data2$steps)
data2$dayl = factor(weekdays(data2$date),levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
data2$dayl=as.numeric(data2$dayl)
for(i in 1:17568){
    if(data2$dayl[i]<6){
        data2$daylevel[i]="weekday"}
    else{data2$daylevel[i]="weekend"}
}

weekend<-subset(data2,data2$daylevel=="weekend")
weekday<-subset(data2,data2$daylevel=="weekday")

##Determine the average steps taken per interval for both weekdays and weekends
##Compare the two plots
weekendmean<-aggregate.data.frame(x = weekend$steps, by = list(weekend$interval),FUN = mean)
weekdaymean<-aggregate.data.frame(x = weekday$steps, by = list(weekday$interval),FUN = mean)
png(filename = "figures/plot4.png")
par(mfrow=c(3,1)) 
plot(x = weekdaymean$Group.1, y = weekdaymean$x, type = 'l', xlab = "Interval", ylab= "Average number of steps", main = "Steps vs Interval for weekdays")
plot(x = weekendmean$Group.1, y = weekendmean$x, type = 'l', xlab = "Interval", ylab= "Average number of steps", main = "Steps vs Interval for weekends")
dev.off()