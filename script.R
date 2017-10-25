# Loading and preprocessing the data
d<-read.csv("activity.csv")
d<-mutate(d, date = as.Date(date, format= "%Y-%m-%d"))

# What is mean total number of steps taken per day?
## count na and not na for futher use
d1<-group_by(d,date)
steps_by_day <- summarise(d1, sum=sum(steps, na.rm = TRUE), 
                          mean=mean(steps, na.rm = TRUE), 
                          median=median(steps, na.rm = TRUE),
                          count_na = sum(is.na(steps)),
                          count_not_na = sum(!is.na(steps)))
#plot(steps_by_day$date,steps_by_day$sum, type="h")

# What is the average daily activity pattern?
d2<-group_by(d,interval)
steps_by_interval <- summarise(d2,sum=sum(steps, na.rm = TRUE),
                               mean=mean(steps, na.rm = TRUE), 
                               median=median(steps, na.rm = TRUE),
                               count_na = sum(is.na(steps)),
                               count_not_na = sum(!is.na(steps)))

plot(steps_by_interval$interval,steps_by_interval$mean, type="l")

interval_max<-steps_by_interval[steps_by_interval$mean==max(steps_by_interval$mean),1]
print(interval_max)

# Imputing missing values
count_missing_values <- sum(is.na(d$steps))
count_present_values <- sum(!is.na(d$steps))

## strategy : filling N/A with the mean of interval averaged across all the days
## reason : data is missing for some completes days
## show the plots NA/not NA per day and per interval
steps_by_interval$mean
plot(steps_by_day$date,steps_by_day$count_not_na)
#plot(steps_by_interval$interval,steps_by_interval$count_na)

#plot(steps_by_interval$interval,steps_by_interval$mean)

## create the new dataset without NA
m<-select(steps_by_interval,c(interval,mean))
nd<-full_join(d,m, by = "interval")
nd<-mutate(nd, steps= ifelse(is.na(steps), mean,steps))
#sum(is.na(nd$steps))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
nd1<-group_by(nd,date)

new_steps_by_day <- summarise(nd1, sum=sum(steps, na.rm = TRUE), 
                          mean=mean(steps, na.rm = TRUE), 
                          median=median(steps, na.rm = TRUE),
                          count_na = sum(is.na(steps)),
                          count_not_na = sum(!is.na(steps)))
plot(new_steps_by_day$date,new_steps_by_day$sum, type="h")
nd<-mutate(nd, day = as.POSIXlt(date)$wday)
nd<-mutate(nd, jour = weekdays(date))
nd1<-mutate(nd, day_week= ifelse((day == 0 | day == 6), "Weekend","weekday"))
nd1<-mutate(nd1, day_week= factor(day_week))
nd2<-select(nd1, c(steps, date, interval, day_week))
nd3<-group_by(nd2, day_week)
sum_nd3 <- summarise(nd3, mean_day = mean(steps))
nd4 <- group_by(nd2, interval, day_week)
sum_nd4 <- summarise(nd4, mean_interval = mean(steps))
nd5 <- left_join(sum_nd4, sum_nd3,by="day_week")
nd5<-mutate(nd5, mean_averaged = (mean_interval-mean_day)/mean_day)
plot(nd5$interval, nd5$mean_averaged, type="l")
