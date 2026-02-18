

###Desired Variables###

############################################################################################
## + aggregate data  to days or daily           - Hobo(Excel), Fawn/FC Data Download ^    ##
## + Get the total sum  ^                                                                 ##
##   +(then average the sums of the VWC) for the Hobo/Centech.  ^                         ##
## + fawn sum, each time there is precipitation -   ^?                                    ##
## + total water that passes through the system -   ^                                      ##
## + total water occurs by precipitation        -   ^                                      ##
## * remaining water is irrigation.             -                                         ##
## + try to add growing degree days after       -  GDD = [( T MAX + T MIN) 2] - T BASE ^^^##
############################################################################################


###Load Packages-------------------

library (tidyverse)
library(ggplot2)
library(reshape2)
##### Sensor Station Stats #####
# HoboLink has 3 sensors each 2-4 in dept
# field Climate (Group of sensors every 10 cm)
# Sensor 1 = Surface
# Sensor 2 = Shallow 
# Sensor 3 = Root Zone
# sensor 4 = sub-root Zone
# Sensor 5 = Deep Sensor
# Sensor 6 = Very Deep 

# Fawn Station 2 sets of Rainfall
# + 1. total rainfall that day
# + 2. Max amount of rainfall within a 15 min interval



#####    Load needed Data #####
  
FC_Stations <- read.csv('C:/Users/gabe2/Downloads/Data_summary/Daily/csv/FC_Daily/All_station_data_daily.csv')
HoboL_Station <- read.csv ('C:/Users/gabe2/Downloads/Data_summary/Daily/csv/HoboLink_Data_summary_Daily.csv')
Fawn_Station <- read.csv ('C:/Users/gabe2/Downloads/Data_summary/Daily/CSV/FAWN_report_Precipitation.csv')
FC_1 <- read.csv ('C:/Users/gabe2/Downloads/Data_summary/Daily/csv/FC_Daily/01208985_station_data (1).csv')
FC_2 <- read.csv ('C:/Users/gabe2/Downloads/Data_summary/Daily/csv/FC_Daily/01208991_station_data (2).csv')
FC_3 <- read.csv ('C:/Users/gabe2/Downloads/Data_summary/Daily/csv/FC_Daily/01208996_station_data (3).csv')
FC_4 <- read.csv ('C:/Users/gabe2/Downloads/Data_summary/Daily/csv/FC_Daily/012089A5_station_data (4).csv')
#FC_Stations is FC_1, _2, _3, _4, stations combined


#######  Renaming the HoboL_stations (columns) #####
colnames(HoboL_Station)[colnames(HoboL_Station) == "Average.of.Water.Content..RXW.SMD.20691707.20683640.1...m.3.m.3..Hemp_TREC"] <- "Hobo_sen1"
colnames(HoboL_Station)[colnames(HoboL_Station) == "Average.of.Water.Content..RXW.SMD.20691707.20691921.1...m.3.m.3..Hemp_TREC"] <- "Hobo_sen2"
colnames(HoboL_Station)[colnames(HoboL_Station) == "Average.of.Water.Content..RXW.SMD.20691707.20691926.1...m.3.m.3..Hemp_TREC"] <- "Hobo_sen3"


#calculating the max number of rows
max_rows<- max(nrow(FC_Stations), nrow(HoboL_Station))
FC_1_mrows <- max(nrow(FC_1))


#type conversion to numeric
FC_1$Precipitation..mm.<- as.numeric(FC_1$Precipitation..mm.)
FC_2$Precipitation..mm.<- as.numeric(FC_2$Precipitation..mm.)
FC_3$Precipitation..mm.<- as.numeric(FC_3$Precipitation..mm.)
FC_4$Precipitation..mm.<- as.numeric(FC_4$Precipitation..mm.)
FC_Stations$Precipitation..mm.<-as.numeric(FC_Stations$Precipitation..mm.)
FC_Stations$EAG.Soil.moisture.1....<-as.numeric(FC_Stations$EAG.Soil.moisture.1....)
FC_Stations$EAG.Soil.moisture.2....<-as.numeric(FC_Stations$EAG.Soil.moisture.2....)
FC_Stations$EAG.Soil.moisture.3....<-as.numeric(FC_Stations$EAG.Soil.moisture.3....)
FC_Stations$EAG.Soil.moisture.4....<-as.numeric(FC_Stations$EAG.Soil.moisture.4....)
FC_Stations$EAG.Soil.moisture.5....<-as.numeric(FC_Stations$EAG.Soil.moisture.5....)
FC_Stations$EAG.Soil.moisture.6....<-as.numeric(FC_Stations$EAG.Soil.moisture.6....)
HoboL_Station$Hobo_sen1<-as.numeric(HoboL_Station$Hobo_sen1)
HoboL_Station$Hobo_sen2<-as.numeric(HoboL_Station$Hobo_sen2)  
HoboL_Station$Hobo_sen3<-as.numeric(HoboL_Station$Hobo_sen3)
Fawn_Station$X2m.Rain.tot..in.<-as.numeric(Fawn_Station$X2m.Rain.tot..in.)
Fawn_Station$X2m.Rain.max.over.15min..in.<-as.numeric(Fawn_Station$X2m.Rain.max.over.15min..in.)

#Negative sensor glitch removal
HoboL_Station_c <- HoboL_Station %>% 
  filter(HoboL_Station$Hobo_sen3 < 0) %>% 
  mutate(Hobo_sen3 = NA)

HoboL_Station_k <- HoboL_Station %>%
  filter(Hobo_sen3 >= 0) %>%
  mutate(Hobo_sen3 = ifelse(Hobo_sen3 < 0, 0, Hobo_sen3))

HoboL_Station_p <- rbind(HoboL_Station_k, HoboL_Station_c)


#####Calculating the average VWC #####

#isolating the selected columns
HoboL_just_Stations <- HoboL_Station_p [, c("Hobo_sen1","Hobo_sen2", "Hobo_sen3")]
FC_Stations_SM <- FC_Stations[, c("EAG.Soil.moisture.1....", "EAG.Soil.moisture.2....",
                                  "EAG.Soil.moisture.3....", "EAG.Soil.moisture.4....",
                                  "EAG.Soil.moisture.5....", "EAG.Soil.moisture.6....")]
fc_rl <- FC_Stations[, c("EAG.Soil.moisture.1....", "EAG.Soil.moisture.2....",
                                 "EAG.Soil.moisture.3....")]
#Calculating the sum
sum_HoboL <- colSums(HoboL_just_Stations, na.rm = TRUE)
sum_FC_stations_SM <- colSums(FC_Stations_SM, na.rm = TRUE) 
#print( )

#Calculating the Average VWC 
avg_HoboL <- colMeans(HoboL_just_Stations, na.rm = TRUE)
avg_FC_Stations <- colMeans(FC_Stations_SM, na.rm = TRUE)
#print( )

#Calculating the Average Sums (of sensors at root level)
fc_rl_sum <- colSums(fc_rl, na.rm = TRUE)
#root level average sum
RL_avg_sum <- mean(c(sum_HoboL, fc_rl_sum)) 



###### Precipitation Calculations   #####

##### Data columns precipitation Rainfall separation 
FC_1_prec <- FC_1[c("Precipitation..mm.")]
FC_2_prec <- FC_2[c("Precipitation..mm.")]
FC_3_prec <- FC_3[c("Precipitation..mm.")]
FC_4_prec <- FC_4[c("Precipitation..mm.")]
FC_all_prec <- FC_Stations[c("Precipitation..mm.")]


#Adjusting Date formats
Fawn_Station$Period <- as.Date(Fawn_Station$Period, format = "%d %B %Y" )


#total daily
Fawn_rf_daily_tot <- Fawn_Station[c( "Period", "X2m.Rain.tot..in.")]

#(FAWN) max in 15 min
Fn_rf_15min_max<- Fawn_Station[c("X2m.Rain.max.over.15min..in.")]


Fawn_rf_daily_tot$X2m.Rain.tot..in. <- as.numeric(Fawn_rf_daily_tot$X2m.Rain.tot..in.)

# Total Sum
FC_prec_sum <- colSums(FC_all_prec, na.rm = TRUE)
Fawn_rf_sum <- sum(Fawn_rf_daily_tot$X2m.Rain.tot..in., na.rm = TRUE) 

print(FC_prec_sum)

#Average Daily Sum
FN_prec_avg_Daily <- mean(Fawn_rf_daily_tot$X2m.Rain.tot..in., na.rm = TRUE)

print(FN_prec_avg_Daily)







######Growing Degree Days####### 
# GDD = [(T_Max + T_Min)2] - T_Base

FC_tot_AirTemp <- FC_Stations[, c("X","Station_ID","Air.temperature..high.precision...C.", "X.1", "X.2")]
#Converted base temp  to celcius
base_hemp_temp <- 3.4
#renaming temp columns
FC_tot_AirTemp <- FC_tot_AirTemp %>%
  rename( Avg_temp = Air.temperature..high.precision...C.,
          Max_temp = X.1,
          Min_temp = X.2
  )

# Replacing non-Numeric Values
FC_tot_AirTemp$Avg_temp<- as.numeric(FC_tot_AirTemp$Avg_temp)
FC_tot_AirTemp$Max_temp<- as.numeric(FC_tot_AirTemp$Max_temp)
FC_tot_AirTemp$Min_temp<- as.numeric(FC_tot_AirTemp$Min_temp)

#Calculate the GDD
FC_tot_AirTemp$DailyGDD <- pmax(FC_tot_AirTemp$Avg_temp - base_hemp_temp, 0) 
# 34 degrees F is the base temperature for hemp growth and emergence (May vary based on variety)


#Here is the code to get the daily avg or mean if not provided
#FC_tot_AirTemp$Daily_Mean <- [(FC_tot_AirTemp$Max_temp + FC_tot_AirTemp$Min_temp) / 2] - base_hemp_temp


 #Growing Degree Days
######Irrigation v. Precipitation ####



# Determine the minimum number of non-NA values among the columns
min_rows <- min(
  sum(!is.na(FC_1_prec$Precipitation..mm.)),
  sum(!is.na(FC_2_prec$Precipitation..mm.)),
  sum(!is.na(FC_3_prec$Precipitation..mm.)),
  sum(!is.na(FC_4_prec$Precipitation..mm.))
)



# Remove NA values from the 'Precipitation..mm.' 
FC_1_rainfall <- na.omit(FC_1_prec$Precipitation..mm.)[1:min_rows]
FC_2_rainfall <- na.omit(FC_2_prec$Precipitation..mm.)[1:min_rows]
FC_3_rainfall <- na.omit(FC_3_prec$Precipitation..mm.)[1:min_rows]
FC_4_rainfall <- na.omit(FC_4_prec$Precipitation..mm.)[1:min_rows]


All_Station_precip <- data.frame(
  FC_1_rainfall = FC_1_rainfall,
  FC_2_rainfall = FC_2_rainfall,
  FC_3_rainfall = FC_3_rainfall,
  FC_4_rainfall = FC_4_rainfall
)

# Get the number of rows needed to match the length of Fawn_rf_daily_tot
additional_rows_needed <- nrow(Fawn_rf_daily_tot) - nrow(All_Station_precip)


if (additional_rows_needed > 0) {
  # Replicate the last row of All_Station_precip to fill the additional rows
  last_row <- All_Station_precip[nrow(All_Station_precip), ]
  additional_rows <- lapply(1:additional_rows_needed, function(x) last_row)
  
  # Append the additional rows to All_Station_precip
  All_Station_precip_aligned <- rbind(All_Station_precip, do.call(rbind, additional_rows))
} else {
  # If no additional rows are needed, use All_Station_precip as it is
  All_Station_precip_aligned <- All_Station_precip
}

# Merge All_Station_precip_aligned with Fawn_rf_daily_tot
FC_Fawn_rf <- cbind(All_Station_precip_aligned, Fawn_rf_daily_tot)

# Print the merged data frame




# Reorder the columns using the pipe operator
FC_Fawn_rf <- FC_Fawn_rf %>%
  select(Period, `X2m.Rain.tot..in.`, everything())

  
FC_Fawn_rf <- FC_Fawn_rf %>%
  arrange(desc(Period), desc(`X2m.Rain.tot..in.`))


# Print the reordered dataframe
print(FC_Fawn_rf)

FC_Fawn_rf_mm <- FC_Fawn_rf %>%
  mutate(`X2m.Rain.tot..mm.` = `X2m.Rain.tot..in.` * 25.4) %>%
  select(-`X2m.Rain.tot..in.`) 


FC_Fawn_rf_in <- FC_Fawn_rf_mm %>%
  mutate_if(is.numeric, function(x) ifelse(!is.na(x), round(x / 25.4, 2), x))

FCF_rf_mm_na <- FC_Fawn_rf_mm %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, NA, .)))

print(FC_Fawn_rf_mm)
print(FC_Fawn_rf_in)
print(FCF_rf_mm_na)


#filtering for greater than and less than 
#Using the NA that replaced the 0 values then filtering data Based on Fawn data
# Create a new column 'Water_Type' with three options: 'Rainfall', 'Irrigation', and 'No Water'
FCF_rf_mm_na$Water_Type <- ifelse(!is.na(FCF_rf_mm_na$X2m.Rain.tot..mm.), 'Rainfall',   # Option 1: Non-NA value in 'X2m.Rain.tot..mm.'
                                  ifelse(rowSums(is.na(FCF_rf_mm_na[, c('FC_1_rainfall', 'FC_2_rainfall', 'FC_3_rainfall', 'FC_4_rainfall')])) == ncol(FCF_rf_mm_na[, c('FC_1_rainfall', 'FC_2_rainfall', 'FC_3_rainfall', 'FC_4_rainfall')]) & is.na(FCF_rf_mm_na$X2m.Rain.tot..mm.), 'No Water',  # Option 3: NA values in all specified columns and 'X2m.Rain.tot..mm.'
                                         'Irrigation'  # Option 2: Non-NA value in any of the specified columns
                                  )
)


# Print the data frame to verify the new column
print(FCF_rf_mm_na)

# Print the updated dataframe



#####Fert and no_fert days#####   
#identify the stations in different Blocks


FC_Fawn_rf$Period <- as.Date(FC_Fawn_rf$Period)
#Identifyng the dates of the of fertilizer application 
blk1_fert_dates<- c ("2023-05-24","2023-06-21","2023-07-19","2023-05-08",
                    "2023-06-07", "2023-06-08", "2023-07-05","2023-07-06",
                    "2023-07-07")

blk10_fert_dates<- c ("2023-09-07","2023-08-04","2023-07-05")
# combing Blk10 & Blk1
all_fert_dates<- c(blk1_fert_dates, blk10_fert_dates)

#filtering the fert dates from 
FC_Fawn_fert_Days <- FCF_rf_mm_na %>% 
  filter( Period %in% as.Date(all_fert_dates))

FC_Fawn_nofert_Days <- FCF_rf_mm_na %>%
    filter( !Period %in% as.Date(all_fert_dates))

# Create a new column 'Fertilization' with values 'Fertilization' or 'No Fertilization'
FCF_rf_mm_na$Fertilization <- ifelse(FCF_rf_mm_na$Period %in% all_fert_dates, 'Fertilization', 'No Fertilization')

  
####Fertilizer applications####
###Block 1###		
##Timing study##	
#1st Application (2 wk delay)	5/24/2023	
#2nd Application	6/21/2023	
#3rd Application	7/19/2023	

##Rate study##
#Preplant Application	5/8/2023
#1st Post Application	6/7 - 6/8	
#2nd Post Application	7/5 - 7/7	

###Block 10###
#Source		
#Preplant Application    (Polymer coated urea & urea)	7/5/2023	
#1st Post Application	8/4/2023
#2nd Post Application	9/7/2023

#CRF	
#Fertilizer application	7/5/2023
#######calculate how much it will take for the fertilizer to run off ####

##### Planting and Harvest Dates #####

# Planting Dates
planting_dates <- c("2023-05-09", "2023-07-12", "2023-07-11", "2023-06-06")
#harvest Dates
harvest_dates <- c("2023-09-05", "2023-10-04", "2023-08-09", "2023-10-11", "2023-10-13", "2023-09-13", "2023-10-19")

#Rainfall - Planting Days
plating_dates_rf <- FC_Fawn_rf_mm %>%
  filter( Period %in% as.Date(planting_dates))
#Rainfall - Harvest Days
harvest_dates_rf <- FC_Fawn_rf_mm %>%
  filter( Period %in% as.Date(harvest_dates))

#print to verify
print(plating_dates_rf)
print(harvest_dates_rf)



######visualizations#####

#Soil moisture Viz----

#VWC Hobo & FC Viz----

#temp viz----
#removing the Station_ID string var
FC_tot_AirTemp <- FC_tot_AirTemp[FC_tot_AirTemp$Station_ID != "Station_ID", ]

#Plotting
ggplot(FC_tot_AirTemp, aes(x = DailyGDD, y=, Color=Station_ID))+
  geom_histogram(aes(color= Station_ID), size = 3) + facet_wrap("Station_ID")
labs(title = "DailyGDD")


ggplot(FC_tot_AirTemp, aes(x = X, y=DailyGDD, Color=Station_ID))+
  geom_line(aes(color= Station_ID), size = 3) 
labs(title = "DailyGDD", x="Date", y="DailyGDD", color= "Station_ID")


#FC_Fawn Data Viz----
#fawn station = rainfall
FCF_rf_mm_na_point <- ggplot(FCF_rf_mm_na, aes(x = Period, y = X2m.Rain.tot..mm.)) + 
  geom_line() + 
  geom_point(aes(color = Fertilization), size = 3) 
labs(title = "Rainfall")

print(FCF_rf_mm_na_point)

#Water and fertilizer days---------
FCF_rf_mm_na_plot <- ggplot(FCF_rf_mm_na, aes(x = Period, y = Water_Type )) +      
  geom_point(aes(color = Water_Type), size = 3) + facet_grid("Fertilization") # Add points with color distinction based on 'Fertilization'
labs(title = "water_type Scatter Plot", x = "Date", y = "Value")

print(FCF_rf_mm_na_plot)

# Plotting all precip Stations with water type -------
# Melt the data set to long format
FCF_rf_mm_na_long <- reshape2::melt(FCF_rf_mm_na, id.vars = c("Period", "Water_Type", "Fertilization"))

# Define the FC station names
fc_stations <- c("FC_1_rainfall", "FC_2_rainfall", "FC_3_rainfall", "FC_4_rainfall")

# Check if the "Variable" column contains any FC station names
has_fc_data <- grepl(paste(fc_stations, collapse = "|"), FCF_rf_mm_na_long$Variable)

# Assign "FC" to rows where the "Variable" column contains FC station names, otherwise assign "FAWN"
FCF_rf_mm_na_long$Station_Type <- ifelse(has_fc_data, "FC", "FAWN")

# Plot using ggplot
ggplot(FCF_rf_mm_na_long, aes(x = Period, y = value, color = Water_Type)) +
  geom_line() +
  geom_point(aes(color = variable), size = 3)
  labs(title = "Plot with Datetime on X-axis and Multiple Columns on Y-axis",
       x = "Date/time",
       y = "Precipitation(mm)",
       color = "Water type")


FCF_rf_mm_na_plot <- ggplot(FCF_rf_mm_na, aes(x = Period, y = Water_Type )) +      
  geom_point(aes(color = Water_Type), size = 3) + facet_grid("Fertilization") # Add points with color distinction based on 'Fertilization'
  labs(title = "water_type Scatter Plot", x = "Date", y = "Value")

print(FCF_rf_mm_na_plot) 
###DON'T FORGET THE STORY
#####