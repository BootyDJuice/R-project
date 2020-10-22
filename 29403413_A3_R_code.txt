#First filtering dataset for year 2009, finding number of people for given period, average number of people for giving period and average number of people at a perticular time
# of the day for a given period

# extracting Mongolite library to import our dataset from Mongo Db
library(mongolite)
dmd = mongo(collection = "overall", db = "PedTraffic")
dataFirstPeriod <- dmd$find()

# Defining columns which stands for the streets name to Find the sum of people and average number for given period of time
dataFirstPeriodCol <- dataFirstPeriod[,3:18]
People_num <- colSums(Filter(is.numeric, dataFirstPeriodCol),na.rm = TRUE)
People_num_sorted <- sort(People_num, decreasing = T)
People_num_ave <- apply(Filter(is.numeric, dataFirstPeriodCol),na.rm = TRUE, 2, median)
People_num_ave_sorted <- sort(People_num_ave, decreasing = T)
final_amount <- data.frame(People_num_sorted)
names(final_amount)[1] = "Overall Number of People(2009)"
View(final_amount)
final_average <- data.frame(People_num_ave_sorted)
names(final_average)[1] = "Average Number(2009)"
View(final_average)

# Plotting average number of people at this locations using boxplot, as we can see from plot 25% number of people falls in a division between 210 and almost 260
# and the mid overall average is at 290 , 75% falls into numbert of 290 and up 600 number of people in average at each hour, at each day of a given period and the max number
# is at 1900
boxplot(final_average$`Average Number`, col="red", ylab = "Numbers Division", main="Average Number Categories")

# Filtering average number of people based on a time periods of a day: Night/Morning
NightMorningHours <- subset(dataFirstPeriod, Hour >= 0 & Hour <= 7)
NightMorning_ped_stat_ave <- apply(Filter(is.numeric, NightMorningHours[3:18]),na.rm = TRUE, 2, median)
FinalNM_ave <- data.frame(sort(NightMorning_ped_stat_ave, decreasing = T)) 
names(FinalNM_ave)[1] = "Night/Morning Average(2009)"
View(FinalNM_ave)

# Filtering average number of people based on a time periods of a day: Mid Day 
MidDayHours <- subset(dataFirstPeriod, Hour >= 8 & Hour <= 15)
Midday_ped_stat_ave <- apply(Filter(is.numeric, MidDayHours[3:18]),na.rm = TRUE, 2, median)
FinalMD_ave <- data.frame(sort(Midday_ped_stat_ave, decreasing = T))
names(FinalMD_ave)[1] = "Midday Average(2009)"
View(FinalMD_ave)

# Filtering average number of people based on a time periods of a day: Evening Hours
EveningHours <- subset(dataFirstPeriod, Hour >= 16 & Hour <= 23)
EveningHours_ped_stat_ave <- apply(Filter(is.numeric, EveningHours[3:18]),na.rm = TRUE, 2, median)
FinalEve_ave <- data.frame(sort(EveningHours_ped_stat_ave, decreasing =  T))
names(FinalEve_ave)[1] = "Evening Average(2009)"
View(FinalEve_ave)

# Representing the difference between amount of people duirng different period of the day ( Night/Morning, Mid Day and evening accordingly)
boxplot(FinalNM_ave$`Night/Morning Average`, 
        FinalMD_ave$`Midday Average`, 
        FinalEve_ave$`Evening Average`, col=heat.colors(3), xlab = "Periods Of the Day. Night/Morning, Mid-Day and evening accodingly", 
        ylab = "Number of People", main = "Average Day Period Distrubution - Year 2009")


#Importing second Dataset From Mongo DB
dmd1=mongo(collection = "overall1", db="PedTraffic2")
dataSecondPeriod <- dmd1$find()

# Defining columns which stands for the streets name to Find the sum of people and average number for the year 2012
dataSecondPeriodCol <- dataSecondPeriod[,3:18]
People_num_sec <- colSums(Filter(is.numeric, dataSecondPeriodCol),na.rm = TRUE)
People_num_sec_sorted <- sort(People_num_sec, decreasing = T)
People_num_sec_ave <- apply(Filter(is.numeric, dataSecondPeriodCol),na.rm = TRUE, 2, median)
People_num_sec_ave_sorted <- sort(People_num_sec_ave, decreasing = T)
final_amount_sec <- data.frame(People_num_sec_sorted)
names(final_amount_sec)[1] = "Overall Number of People(2012"
View(final_amount_sec)
final_average_sec <- data.frame(People_num_sec_ave_sorted)
names(final_average_sec)[1] = "Average Number(2012)"
View(final_average_sec)

# Filtering average number of people based on a time periods of a day: Night/Morning for year 2012
NightMorningHours_sec <- subset(dataSecondPeriod, Hour >= 0 & Hour <= 7)
NightMorning_ped_stat_ave_sec <- apply(Filter(is.numeric, NightMorningHours_sec[3:18]),na.rm = TRUE, 2, median)
FinalNM_ave_sec <- data.frame(sort(NightMorning_ped_stat_ave_sec, decreasing = T)) 
names(FinalNM_ave_sec)[1] = "Night/Morning Average(2012)"
View(FinalNM_ave_sec)

# Filtering average number of people based on a time periods of a day: Mid Day for year 2012
MidDayHours_sec <- subset(dataSecondPeriod, Hour >= 8 & Hour <= 15)
Midday_ped_stat_ave_sec <- apply(Filter(is.numeric, MidDayHours_sec[3:18]),na.rm = TRUE, 2, median)
FinalMD_ave_sec <- data.frame(sort(Midday_ped_stat_ave_sec, decreasing = T))
names(FinalMD_ave_sec)[1] = "Midday Average(2012)"
View(FinalMD_ave_sec)

# Filtering average number of people based on a time periods of a day: Evening Hours for year 2012
EveningHours_sec <- subset(dataSecondPeriod, Hour >= 16 & Hour <= 23)
EveningHours_ped_stat_ave_sec <- apply(Filter(is.numeric, EveningHours_sec[3:18]),na.rm = TRUE, 2, median)
FinalEve_ave_sec <- data.frame(sort(EveningHours_ped_stat_ave_sec, decreasing =  T))
names(FinalEve_ave_sec)[1] = "Evening Average(2012)"
View(FinalEve_ave_sec)

# Representing the difference between amount of people duirng different period of the day ( Night/Morning, Mid Day and evening accordingly)
# for year 2012
boxplot(FinalNM_ave_sec$`Night/Morning Average`, 
        FinalMD_ave_sec$`Midday Average`, 
        FinalEve_ave_sec$`Evening Average`, col=topo.colors(3), xlab = "Periods Of the Day. Night/Morning, Mid-Day and evening accodingly", 
        ylab = "Number of People", main = "Average Day Period Distrubution - Year 2012")


#Importing third dataset from MongoDB for year 2015 (February)
dmd2=mongo(collection = "overall2", db="PedTraffic3")
dataThirdPeriod <- dmd2$find()


#Defining columns which stands for the streets name to Find the sum of people and average number for the year 2015 month February
dataThirdPeriodCol <- dataThirdPeriod[,3:18]
People_num_thi_ave <- apply(Filter(is.numeric, dataThirdPeriodCol),na.rm = TRUE, 2, median)
People_num_thi_ave_sorted <- sort(People_num_thi_ave, decreasing = T)
final_amount_thi <- data.frame(People_num_thi_sorted)
final_average_thi <- data.frame(People_num_thi_ave_sorted)
names(final_average_thi)[1] = "Average Number(Feb 2015)"
View(final_average_thi)

# Filtering average number of people based on a time periods of a day: Night/Morning for year 2015 month February
NightMorningHours_thi <- subset(dataThirdPeriod, Hour >= 0 & Hour <= 7)
NightMorning_ped_stat_ave_thi <- apply(Filter(is.numeric, NightMorningHours_thi[3:18]),na.rm = TRUE, 2, median)
FinalNM_ave_thi <- data.frame(sort(NightMorning_ped_stat_ave_thi, decreasing = T)) 
names(FinalNM_ave_thi)[1] = "Night/Morning Average(Feb 2015)"
View(FinalNM_ave_thi)

# Filtering average number of people based on a time periods of a day: Mid Day for year 2015 month February
MidDayHours_thi <- subset(dataThirdPeriod, Hour >= 8 & Hour <= 15)
Midday_ped_stat_ave_thi <- apply(Filter(is.numeric, MidDayHours_thi[3:18]),na.rm = TRUE, 2, median)
FinalMD_ave_thi <- data.frame(sort(Midday_ped_stat_ave_thi, decreasing = T))
names(FinalMD_ave_thi)[1] = "Midday Average(Feb 2015)"
View(FinalMD_ave_thi)

# Filtering average number of people based on a time periods of a day: Evening Hours for year 2015 month February
EveningHours_thi <- subset(dataThirdPeriod, Hour >= 16 & Hour <= 23)
EveningHours_ped_stat_ave_thi <- apply(Filter(is.numeric, EveningHours_thi[3:18]),na.rm = TRUE, 2, median)
FinalEve_ave_thi <- data.frame(sort(EveningHours_ped_stat_ave_thi, decreasing =  T))
names(FinalEve_ave_thi)[1] = "Evening Average(Feb 2015)"
View(FinalEve_ave_thi)

# Representing the difference between amount of people duirng different period of the day ( Night/Morning, Mid Day and evening accordingly)
# for year 2015 month February
boxplot(FinalNM_ave_thi$`Night/Morning Average`, 
        FinalMD_ave_thi$`Midday Average`, 
        FinalEve_ave_thi$`Evening Average`, col=cm.colors(3), xlab = "Periods Of the Day. Night/Morning, Mid-Day and evening accodingly" , 
        ylab = "Number of People", main = "Average Day Period Distrubution - Year 2015 February")

# Plotting average number for 3 given periods: 2009, 2012 and 2015 to see the difference in number of people and the traffic for this period 
boxplot(final_average$`Average Number(2009)`, 
        final_average_sec$`Average Number(2012)`, 
        final_average_thi$`Average Number(Feb 2015)`, col=heat.colors(3), xlab = "Average per Year: 2009, 2012, 2015 accordingly", 
        ylab = "Number of People", main = "Average Numbers Differences for Pedestrian Traffic")


# ASSIGNMENT 3 Part Starts here
# To make an forecasting based on a season distributiion of people among those periods of time that  we have. firstly it is important to find average number of
# of people per each month

# First we take our original dataset and logically define Winter, Spring and Summer periods (Winter: May - June - July, Early Spring/Spring:
# August- September- Oct, Summer: November-December). 
# Looking simply at Date column we can find the needed number of rows to apply them for different seasons and months
# Winter period average
data_Updated_date <- dataFirstPeriod$Date
View(data_Updated_date)
date_Updated_date_selected <- dataFirstPeriod[c(1:2208), 3:18]
People_num_ave_Winter <- apply(Filter(is.numeric, date_Updated_date_selected),na.rm = TRUE, 2, median)
People_num_ave_sorted_Winter <- sort(People_num_ave_Winter, decreasing = T)
final_average_winter <- data.frame(People_num_ave_sorted_Winter)
names(final_average_winter) <- c("Average Number in Winter")
View(final_average_winter)

# Early Spring/Spring average period
data_Updated_date <- dataFirstPeriod$Date
View(data_Updated_date)
date_Updated_date_selected_second <- dataFirstPeriod[c(2209:4417), 3:18]
View(date_Updated_date_selected_second)
People_num_ave_Spring <- apply(Filter(is.numeric, date_Updated_date_selected_second),na.rm = TRUE, 2, median)
People_num_ave_sorted_Spring <- sort(People_num_ave_Spring, decreasing = T)
final_average_spring <- data.frame(People_num_ave_sorted_Spring)
names(final_average_spring) <- c("Average Number in Spring")
View(final_average_spring)

# Summer average period
data_Updated_date <- dataFirstPeriod$Date
View(data_Updated_date)
date_Updated_date_selected_third <- dataFirstPeriod[c(4418:5880), 3:18]
View(date_Updated_date_selected_third)
People_num_ave_Summer <- apply(Filter(is.numeric, date_Updated_date_selected_third),na.rm = TRUE, 2, median)
People_num_ave_sorted_Summer <- sort(People_num_ave_Summer, decreasing = T)
final_average_summer <- data.frame(People_num_ave_sorted_Summer)
names(final_average_summer) <- c("Average Number in Summer")
View(final_average_summer)

# Creating final dataset with the average pedestrian distribution per each defined season
final_seasonal_dataset <- data.frame(cbind(final_average_winter,final_average_spring,final_average_summer))
names(final_seasonal_dataset) <- c('Average Winter', 'Average Spring', 'Average Summer')
View(final_seasonal_dataset)

# Plotting obtained data in Plots and then combining then into one graphical representation using different configurations for line chart
# Each bar correspondes to each street, it is easier to find differnce between 7 top crowded streets
par(mfrow=c(3,1)) 
barplot(final_seasonal_dataset$"Average Winter", col="aquamarine", main = "Season: Winter", ylab = "Number of people")
barplot(final_seasonal_dataset$"Average Spring", col= "deepskyblue", main = "Season: Spring", ylab = "Number of people")
barplot(final_seasonal_dataset$"Average Summer", col="orange", main = "Season:Summer", xlab = "Top 7 crowded places: Town Hall, Princes Bridge,
         Flinders Str St Under, State Lib, BSM(North, BSM(South), Collins Place(2009)", ylab = " Number of people")

# First we take our original dataset and logically define Winter, Spring,Summer and Autumn periods (Winter: June to August Spring:
# Sept - Nov, Summer: Dec - Feb and Autumn: March-May). 
# Looking simply at Date column we can find the needed number of rows to apply them for different seasons and months
# Winter period average (all for year 2012)
data_Updated_date_sec <- dataSecondPeriod$Date
View(data_Updated_date_sec)
date_Updated_date_selected_sec <- dataSecondPeriod[c(3649:5856), 3:18]
People_num_ave_Winter_sec <- apply(Filter(is.numeric, date_Updated_date_selected_sec),na.rm = TRUE, 2, median)
People_num_ave_sorted_Winter_sec <- sort(People_num_ave_Winter_sec, decreasing = T)
final_average_winter_sec <- data.frame(People_num_ave_sorted_Winter_sec)
names(final_average_winter_sec) <- c("Average Number in Winter")
View(final_average_winter_sec)

# Spring
data_Updated_date_sec <- dataSecondPeriod$Date
View(data_Updated_date_sec)
date_Updated_date_selected_sec <- dataSecondPeriod[c(5857:8040), 3:18]
People_num_ave_Spring_sec <- apply(Filter(is.numeric, date_Updated_date_selected_sec),na.rm = TRUE, 2, median)
People_num_ave_sorted_Spring_sec <- sort(People_num_ave_Spring_sec, decreasing = T)
final_average_spring_sec <- data.frame(People_num_ave_sorted_Spring_sec)
names(final_average_spring_sec) <- c("Average Number in Spring")
View(final_average_spring_sec)

# Summer
data_Updated_date_sec <- dataSecondPeriod$Date
View(data_Updated_date_sec)
date_Updated_date_selected_sec <- dataSecondPeriod[c(1:1433, 8041:8784), 3:18]
People_num_ave_Summer_sec <- apply(Filter(is.numeric, date_Updated_date_selected_sec),na.rm = TRUE, 2, median)
People_num_ave_sorted_Summer_sec <- sort(People_num_ave_Summer_sec, decreasing = T)
final_average_summer_sec <- data.frame(People_num_ave_sorted_Summer_sec)
names(final_average_summer_sec) <- c("Average Number in Summer")
View(final_average_summer_sec)

# Autumn
data_Updated_date_sec <- dataSecondPeriod$Date
View(data_Updated_date_sec)
date_Updated_date_selected_sec <- dataSecondPeriod[c(1434:3648), 3:18]
People_num_ave_Autumn_sec <- apply(Filter(is.numeric, date_Updated_date_selected_sec),na.rm = TRUE, 2, median)
People_num_ave_sorted_Autumn_sec <- sort(People_num_ave_Autumn_sec, decreasing = T)
final_average_autumn_sec <- data.frame(People_num_ave_sorted_Autumn_sec)
names(final_average_autumn_sec) <- c("Average Number in Autumn")
View(final_average_autumn_sec)

# Creating final dataset with the average pedestrian distribution per each defined season of year 2012
final_seasonal_dataset_2012 <- data.frame(cbind(final_average_winter_sec,final_average_spring_sec,final_average_summer_sec,
                                                 final_average_autumn_sec))
names(final_seasonal_dataset_2012) <- c('Average Winter', 'Average Spring', 'Average Summer', 'Average Autumn')
View(final_seasonal_dataset_2012)

# Plotting obtained data in Plots and then combining then into one graphical representation using different configurations for line chart
# Each bar correspondes to each street, it is easier to find differnce between 7 top crowded streets
par(mfrow=c(4,1)) 
plot(final_seasonal_dataset_2012$"Average Winter", col="blue", type = "l", main = "Season: Winter", xlab = "Streets", ylab = "Number of people")
plot(final_seasonal_dataset_2012$"Average Spring", col= "goldenrod3", type = "l", main = "Season: Spring",xlab = "Streets", ylab = "Number of people")
plot(final_seasonal_dataset_2012$"Average Summer",col = "blueviolet", type = "l", main = "Season: Summer",xlab = "Streets", ylab = "Number of people")
plot(final_seasonal_dataset_2012$"Average Autumn", col="yellow", type = "l", main = "Season:Autumn", xlab = "Top 7 crowded places: Town Hall, Melbourne Central,
         Flinders Str St Under, Princess Bridge, BSM(North), BSM(South), Australia On Collins(2012)", ylab = " Number of people")

# First we take our original dataset and logically define for the most populated month of the Winter - Feb)
# Looking simply at Date column we can find the needed number of rows
# February period average (all for year 2015)

data_Updated_date_third <- dataThirdPeriod$Date
View(data_Updated_date_third)
date_Updated_date_selected_third <- dataThirdPeriod[c(1: 672), 3:18]
People_num_ave_Feb_third <- apply(Filter(is.numeric, date_Updated_date_selected_third),na.rm = TRUE, 2, median)
People_num_ave_sorted_Feb_third <- sort(People_num_ave_Feb_third, decreasing = T)
final_average_feb_third <- data.frame(People_num_ave_sorted_Feb_third)
names(final_average_feb_third) <- c("Average Number in February")
View(final_average_feb_third)
plot(final_average_feb_third$`Average Number in February`)

par(mfrow=c(1,1)) 
plot(final_average_feb_third$`Average Number in February`, type = "o", col="green", main = "February 2015", ylab = "Number of people"
, xlab = "Top 7 crowded places: Town Hall, Princess Bridge,
         Flinders Str St Under, Melbourne Central, BSM(South), Collins Place(South), New Quay")

# Forecasting is based on a boxplot representation technique , where average numbers , highest and lowest points are given, which makes it easier to
# predict possible way of fullfilling bussines values that we are aimed at
par(mfrow=c(2,2))
boxplot(final_seasonal_dataset, ylab = "Number of people", xlab = "2009" , col = rainbow(7))
boxplot(final_seasonal_dataset_2012, ylab = "Number of people", xlab = "2012", col = cm.colors(4))
boxplot(final_average_feb_third, ylab = "Numbaer of people", xlab = "Feb 2015", col = "deepskyblue")

















































