
############################################ DATA PREPARATION ######################

#### 1) Make a grouped bar chart depicting the hour-wise trip request made at city and airport respectively. Aggregated the data for all 5 days on the same axis of 24 hours.

uber_request_data <- read.csv("Uber request data.csv", stringsAsFactors = F)

PickUp_Point <- uber_request_data$Pickup.point
Hours <- substr(as.factor(uber_request_data$Request.time), 1, 2)
ggplot(uber_request_data, aes(x = Hours, fill = PickUp_Point)) +  geom_bar(position = "dodge")

#### 2) 2.	In the bar chart above (in pt.1), see 5 major time blocks based on the frequency of requests made at the city and airport. We now divide the request-time into 5 time-slots described below. An additional column “Time_Slot” is added which takes these 5 categorical values depending on the request time

for(i in 1:nrow(uber_request_data)){
  
  if(uber_request_data$Request.time[i] >= '00:00:00' & uber_request_data$Request.time[i] < '04:00:00'){
    uber_request_data$Time.Slot[i] = "Pre_Morning"  
  }
  
  if(uber_request_data$Request.time[i] >= '04:00:00' & uber_request_data$Request.time[i] < '10:00:00'){
    uber_request_data$Time.Slot[i] = "Morning_Rush"
  }
  
  if(uber_request_data$Request.time[i] >= '10:00:00' & uber_request_data$Request.time[i] < '17:00:00'){
    uber_request_data$Time.Slot[i] = "Day_Time"
  }
  
  if(uber_request_data$Request.time[i] >= '17:00:00' & uber_request_data$Request.time[i] < '22:00:00'){
    uber_request_data$Time.Slot[i] = "Evening_Rush"
  }
  
  if(uber_request_data$Request.time[i] >= '22:00:00' ){
    uber_request_data$Time.Slot[i] = "Late_Night"
  }
  
}

ggplot(uber_request_data, aes(x = Time.Slot)) +  geom_bar()

# plot shows high level of demand in the Evening and Morning rush hours

summary(factor(uber_request_data$Time.Slot))

######################################## PROBLEM IDENTIFICATION #################

#### 1) Stacked bar chart where each bar represents a time slot and the y-axis shows the frequency of requests. Different proportions of bars should represent the completed, cancelled and no cars available out of the total customer requests. (Hint: ggplot)

ggplot(uber_request_data, aes(x = Time.Slot, fill = Status)) +  geom_bar()

#### 2) Visually identify the 2 most pressing problems for Uber, out of the 15 possible scenarios (5 slots * 3 trip status).

# Problem 1) "Cancelled cabs" in the Morning Rush time 
# Problem 2) "No cars available" in Evening Rush time 

########################### PROBLEM 1 #######################################
# "Cancelled cabs" in the Morning Rush time 

#### For the time slot when problem 1 exists, plot a stacked bar chart to find out if the problem is more severe for pick-up requests made at the airport or the city.


MorningRush_data <- data.frame(subset(uber_request_data, uber_request_data$Time.Slot == "Morning_Rush"))

problem1_data <- data.frame(subset(uber_request_data, uber_request_data$Time.Slot == "Morning_Rush" & uber_request_data$Status == 'Cancelled'))
Hours_MorningRush <- substr(as.factor(problem1_data$Request.time), 1 ,2)
ggplot(problem1_data, aes(x = Hours_MorningRush, fill = Pickup.point))  + geom_bar()

# Plot shows that the problem is more severe in City pickups

length(which(uber_request_data$Time.Slot=='Morning_Rush'))
length(problem1_data$Status)
length(which(problem1_data$Pickup.point=='Airport'))
length(which(problem1_data$Pickup.point=='City'))

# In Morning Rush time slot, this issue exists 935 times out of a total of 2394 requests made. That's about 39% requests in the morning rush resulting in Cancelled cabs.
# Out of these 935 cancelled requests, 27 were made at airport and 908 in the city


####  Also find the percentage breakup for the total number of issues in this time slot based on the pick-up point?

airport_morningRush <- subset(MorningRush_data, MorningRush_data$Pickup.point == 'Airport')
city_morningRush <- subset(MorningRush_data, MorningRush_data$Pickup.point == 'City')


f1 <- function(x) {
  (length(x)/nrow(airport_morningRush))*100
}
airport_aggregate_p1 <- aggregate(airport_morningRush$Pickup.point, by = list(airport_morningRush$Status), FUN = f1)
names(airport_aggregate_p1) <- c("Status", "Airport")


f2 <- function(x) {
  (length(x)/nrow(city_morningRush))*100
}
city_aggregate_p1 <- aggregate(city_morningRush$Pickup.point, by = list(city_morningRush$Status), FUN = f2)
names(city_aggregate_p1) <- c("Status", "City")


# Percentage breakup of Status in Morning Rush based on pick-up points:

# Status              Airport         City    
# Cancelled:         5.314961 %    48.14422 %   
# No Cars Available: 9.448819 %    23.48887 %
# Total               14.762 %     71.632 %


#### SUPPLY-DEMAND ANALYSIS

# For Morning Rush data
nrow(MorningRush_data)
# Demand = total requests made (including completed, cancelled and no cars available cases) = 2394
length(which(MorningRush_data$Status == 'Trip Completed'))
# Supply = completed requests = 968


# For airport
length(which(MorningRush_data$Pickup.point == 'Airport'))
# demand = 508
length(which(MorningRush_data$Status == 'Trip Completed' & MorningRush_data$Pickup.point == 'Airport'))
# supply = 433

# For City
length(which(MorningRush_data$Pickup.point == 'City'))
# demand = 1886
length(which(MorningRush_data$Status == 'Trip Completed' & MorningRush_data$Pickup.point == 'City'))
# supply = 535



########################### PROBLEM 2 #######################################
# "No cars available" in Evening Rush time 

#### For the time slot when problem 2 exists (Evening Rush), plotted a stacked bar chart to find out if the problem is more severe for pick-up requests made at the airport or the city.

EveningRush_data <- data.frame(subset(uber_request_data, uber_request_data$Time.Slot == "Evening_Rush"))

problem2_data <- data.frame(subset(uber_request_data, uber_request_data$Time.Slot == "Evening_Rush" & uber_request_data$Status == 'No Cars Available'))
Hours_EveningRush <- substr(as.factor(problem2_data$Request.time), 1 ,2)
ggplot(problem2_data, aes(x = Hours_EveningRush, fill = Pickup.point))  + geom_bar()

# Plot shows that the problem is more severe in Airport pickups

length(which(uber_request_data$Time.Slot=='Evening_Rush'))
length(problem2_data$Status)
length(which(problem2_data$Pickup.point=='Airport'))
length(which(problem2_data$Pickup.point=='City'))

# In Evening Rush time slot, this issue exists in 1438 cases out of a total of 2416 requests made. That's about 59.5% requests in the evening rush resulting in No cars available.
# Out of these 1438 cases, 1372 were made at airport and 66 in the city


####  Also find the percentage breakup for the total number of issues in this time slot based on the pick-up point?

airport_eveningRush <- subset(EveningRush_data, EveningRush_data$Pickup.point == 'Airport')
city_eveningRush <- subset(EveningRush_data, EveningRush_data$Pickup.point == 'City')

f1 <- function(x) {
  (length(x)/nrow(airport_eveningRush))*100
}
airport_aggregate_p2 <- aggregate(airport_eveningRush$Pickup.point, by = list(airport_eveningRush$Status), FUN = f1)
names(airport_aggregate_p2) <- c("Status", "Airport")

f2 <- function(x) {
  (length(x)/nrow(city_eveningRush))*100
}
city_aggregate_p2 <- aggregate(city_eveningRush$Pickup.point, by = list(city_eveningRush$Status), FUN = f2)
names(city_aggregate_p2) <- c("Status", "City")


# Percentage breakup of issues in Evening Rush based on pick-up points:

# Status             Airport           City
# Cancelled:         5.734191 %     10.90909 %    
# No Cars Available: 73.526259 %    12.00000 % 
# Total:             79.26 %          22.90 %


#### SUPPLY-DEMAND ANALYSIS

# For Evening Rush data

nrow(EveningRush_data)
# Demand = total requests made (including completed, cancelled and no cars available cases) 2416
length(which(EveningRush_data$Status == 'Trip Completed'))
# Supply = completed requests 811


# For airport
length(which(EveningRush_data$Pickup.point == 'Airport'))
# demand = 1866
length(which(EveningRush_data$Status == 'Trip Completed' & EveningRush_data$Pickup.point == 'Airport'))
# supply = 387

# For City
length(which(EveningRush_data$Pickup.point == 'City'))
# demand = 550
length(which(EveningRush_data$Status == 'Trip Completed' & EveningRush_data$Pickup.point == 'City'))
# supply = 424




# Uber data (Overall)
# Demand = total requests made (including completed, cancelled and no cars available cases) 6766
# Supply = completed requests 2852

# For airport (Overall)
# demand = 3251
# supply = 1340

# For City (Overall)
# demand = 3515
# supply = 1512
