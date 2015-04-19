# Load Activity Data
dataset = read.csv("activity.csv")

#Total Number of Steps Taken Per Day
library(sqldf)
steps_day = sqldf("
                    select 
                      date,
                      sum(steps) as steps
                    from 
                      dataset 
                    where 
                      steps <> 'NA' 
                    group by 
                      date;
                  ")

#Make a Histogram for Steps / Day
hist(steps_day$steps, main = "Distribution of Total Steps Per Day", xlab = "Steps", ylab = "Days")

#Get Mean and Median of Steps / Day
steps_mean = mean(steps_day$steps)
steps_median = median(steps_day$steps)

steps_mean
steps_median

#Get behavior each day using average by interval
steps_behavior_daily = sqldf("
                                select 
                                  interval, 
                                  avg(steps) as avg_steps 
                                from 
                                  dataset 
                                where 
                                  steps <> 'NA' 
                                group by 
                                  interval;
                             ")


#Plot the daily average steps per interval
plot(steps_behavior_daily$interval, steps_behavior_daily$avg_steps, type = "l",
     xlab = "Time Interval", ylab = "Average Steps", main = "Average Steps Per Time Interval")

#Get the max number of steps, where the max step occurs
max_steps = max(steps_behavior_daily$avg_steps)
max_steps_at = steps_behavior_daily$interval[steps_behavior_daily$avg_steps == max_steps]

#Get the number of NA's in the dataset
count_NA = nrow(subset(dataset, is.na(steps)))
count_NA

# Account for the NA's. They will be filled in using the average steps per interval from
# "steps_behavior_daily". The dataset will be merged on the "interval" parameter

#Merge on the "interval" parameter
dataset_imputed = merge(dataset, steps_behavior_daily, by = "interval")
#Replace data with the avg_steps where NA's are present
dataset_imputed$steps[is.na(dataset_imputed$steps)]<-dataset_imputed$avg_steps[is.na(dataset_imputed$steps)]

#Get steps per day from the imputed dataset
steps_day_imp = sqldf("
                    select 
                      date,
                      sum(steps) as steps
                    from 
                      dataset_imputed 
                    where 
                      steps <> 'NA' 
                    group by 
                      date;
                  ")

#Make a Histogram for Steps / Day for the imputed dataset
hist(steps_day_imp$steps, main = "Distribution of Total Steps Per Day from Imputed Dataset", 
     xlab = "Steps", ylab = "Days")

#Get Mean and Median of Steps / Day for the imputed dataset
steps_mean_imp = mean(steps_day_imp$steps)
steps_median_imp = median(steps_day_imp$steps)

steps_mean_imp
steps_median_imp

#Get delta's from imputed data and original set
delta_mean = steps_mean_imp - steps_mean
delta_median = steps_median_imp - steps_median

delta_mean
delta_median

#Use weekdays to determine whether it's a weekend or weekday
weekend_array = (weekdays(as.Date(dataset_imputed$date))==("Sunday")) | 
  (weekdays(as.Date(dataset_imputed$date))==("Saturday"))

#Set the factor names
weekend_array = factor(weekend_array, levels = c("TRUE", "FALSE"), labels = c("weekend", "weekday"))

dataset_imputed = cbind(dataset_imputed, weekend_array)
colnames(dataset_imputed)[5] = "day_type"

#Average steps per interval grouped by day type (weekend / weekday)
steps_day_imp = sqldf("
                    select 
                      day_type,
                      interval,
                      avg(steps) as avg_steps
                    from 
                      dataset_imputed 
                    where 
                      steps <> 'NA' 
                    group by 
                      day_type,
                      interval;
                  ")

#Make a panel line plot for average steps per interval, grouped by day type (weekend or weekday)
library(lattice)
xyplot(avg_steps ~ interval|day_type, data = steps_day_imp, type = "l", layout = c(1,2),
        xlab = "Time Interval", ylab = "Average Steps", main = "Average Steps per Time Interval by Time of Week")
