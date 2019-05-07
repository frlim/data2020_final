# EXPLORATORY DATA ANALYSIS

# Load pacakages
library(dplyr)
library(tidyverse)
library(sf)
library(maps)
library(rgeos)
library(tools)
library(usmap)
library(ggplot2)
library(ggrepel)
library(urbnmapr)
library(scales)
library(urbnthemes)


# Load data
data <- read.csv("~/Documents/DATA2020/Final Project/completeMerged.csv")
# Remove extra variables
data <- data %>%
  select(-c(X.1, X))

########################

## CREATE IS.BLACK VARIABLE
data$is.black <- data$raceethnicity

# Rename levels
levels(data$is.black) <- c("Not black", "Black", "Not black", "Not black", "Not black", "Not black")

# Convert is.black to a binary value of 0 (non-black) and 1 (black) to a new variable is.black.binary
data$is.black.binary <- ifelse(data$is.black=='Not black', 0, 1)

# Create bar graph of black and non-black victims
ggplot(data, aes(x = is.black, fill = is.black)) +
  geom_bar()

########################

## GEOGRAPHIC VISUALIZATIONS
# Map latitude/longitude of each incident excluding Hawaii and Alashka
new.data <- data[data$state != 'HI',]
new.data <- new.data[new.data$state != 'AK',]

# create dataframe of latitude/longitude points
points <- new.data %>%
  select(c(latitude, longitude, is.black))

# define usa
usa = st_as_sf(map('usa', plot = FALSE, fill = TRUE))

# create us map
ggplot() +
  geom_sf(data=usa) +
  geom_point(data = points, aes(x=longitude, y=latitude), size=2, shape=23, fill = 'darkred')

# Plot black vs non-black killings
# create us map of each police shooting
ggplot() +
  geom_sf(data=usa) +
  geom_point(data = points, aes(x=longitude, y=latitude, color = is.black), size=2, shape=16, alpha = 0.8)

#########################

## CHOROPETH MAPS
# read original acs dataset
acs <- read.csv("~/Documents/DATA2020/Final Project/acs2015_census_tract_data.csv")

# CREATE MAP OF KILLINGS EACH STATE
# create dataframe of number of killings in each state
num_killed_state <- data %>%
  group_by(State) %>%
  summarize(state.counts = n())
# write to csv
write.csv(num_killed_state, file = "~/Documents/DATA2020/Final Project/num_killed_state.csv")

# rename State column so undercase
names(num_killed_state)[1] <- 'state'

# plot number of killings in each state
plot_usmap(data = num_killed_state, values = "state.counts", lines = "white") + 
  scale_fill_continuous(name = "Number of Police Killings", label = scales::comma) + 
  theme(legend.position = "right")

# sort to get top 5 states with highest number of killings
sorted_num_killed_state <- num_killed_state %>%
  arrange(desc(state.counts))
head(sorted_num_killed_state)

# CREATE MAP OF BLACK POPULATION EACH STATE
# Add number of black people in ACS
acs <- acs %>%
  mutate(num_black = TotalPop * Black * 0.01)

# Total number of black people
tot_black <- acs %>%
  group_by(State) %>%
  summarize(black_pop = sum(num_black, na.rm = TRUE))

# create state population variable
state_pop <- acs %>%
  group_by(State) %>%
  summarize(pop = sum(TotalPop, na.rm = TRUE))

# add state population to tot_black
tot_black$state_pop <- state_pop$pop

# create perc.black for each state
tot_black <- tot_black %>%
  mutate(perc.black = (black_pop / state_pop) * 100)
# write to csv
write.csv(tot_black, file = "~/Documents/DATA2020/Final Project/state_pop.csv")

# rename state so it is lowercase
names(tot_black)[1] <- 'state'

# plot choropeth of number of black residents
plot_usmap(data = tot_black, values = "black_pop", lines = "white") + 
  scale_fill_continuous(name = "Number of African American Residents", label = scales::comma) + 
  theme(legend.position = "right")

# dataframe of top 5 states with largest black population
top5_black_state <- tot_black %>%
  arrange(desc(black_pop))
head(top5_black_state)

# plot choropeth of black percentage
plot_usmap(data = tot_black, values = "perc.black", lines = "white") + 
  scale_fill_continuous(name = "% of Population that is Black", label = scales::comma) + 
  theme(legend.position = "right")

# dataframe of top 5 states with largest black percentage
top5_black_perc <- tot_black %>%
  arrange(desc(perc.black))
head(top5_black_perc)

########################

## POPULATION AND POLICE KILLINGS

# rename State in state_pop so it is lowercase
names(state_pop)[1] <- 'state'

# merge state_pop and num_killed_state dataframes
merged_pop_killings <- merge(state_pop, num_killed_state, by = c("state"))

# plot killings and population for each state
ggplot(data = merged_pop_killings, aes(x=state.counts, y=pop)) +
  geom_jitter(alpha = 0.6, color = 'darkred') +
  geom_text_repel(mapping = aes(x = state.counts, y = pop, label = state)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'red', alpha = 0.6)  

# merge num_killed_state and tot_black
merged_black_killings <- merge(tot_black, num_killed_state, by = c('state'))

# plot killings and black population for each state
ggplot(data = merged_black_killings, aes(x=state.counts, y=black_pop)) +
  geom_jitter(alpha = 0.6, color = 'darkred') +
  geom_text_repel(mapping = aes(x = state.counts, y = black_pop, label = state)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'red', alpha = 0.6)

########################

## CALIFORNIA

# Remove "county" from counties to allow for merge
counties$new_county_name <- substr(counties$county_name, 1, nchar(counties$county_name) - 7)

# Create dataframe of just county in California for number of police killings
cali.data <- data %>%
  filter(state == "CA") %>%
  group_by(County) %>%
  summarize(county.counts = n())

# Rename County in cali.data
names(cali.data)[1] <- "new_county_name"

# Change to character
cali.data[] <- lapply(cali.data, as.character)

# Change counts to integer
cali.data$county.counts <- as.numeric(as.character(cali.data$county.counts))

# Just get California counties
county.cali <- counties %>%
  filter(state_abbv == "CA")

# Get all counties in California
all.counties <- unique(county.cali$new_county_name)

# If county not in all.counties, add it to cali.data
kill.counties <- unique(cali.data$new_county_name)

for (i in all.counties) {
  if (!(i %in% kill.counties)) {
    newRow <- data.frame(new_county_name=i, county.counts = 0)
    cali.data <- rbind(cali.data,newRow)
  } 
}

# dataframe for number of deaths in each county in california
cali.deaths <- left_join(cali.data, county.cali, by = "new_county_name")

# plot map for each county in california
cali.deaths %>%
  ggplot(aes(long, lat, group = group, fill = county.counts)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "Number of Police Killings")

# dataframe of top 5 deadliest counties in california
top5_cali_county <- cali.data %>%
  arrange(desc(county.counts))
head(top5_cali_county)

# Each county's total population
cali.county.pop <- acs %>%
  filter(State == 'California') %>%
  group_by(County) %>%
  summarize(county_pop = sum(TotalPop, na.rm = TRUE))

# merge to create map
cali.pop.merged <- left_join(cali.county.pop, county.cali, by = "new_county_name")

# map
cali.pop.merged %>%
  ggplot(aes(long, lat, group = group, fill = county_pop)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "Population")

# Each county's total black population
cali.black.pop <- acs %>%
  filter(State == 'California') %>%
  group_by(County) %>%
  summarize(county_black_pop = sum(num_black, na.rm = TRUE))

# Change name of county column
names(cali.black.pop)[1] <- "new_county_name"

# merge to create map
black.pop.merged <- left_join(cali.black.pop, county.cali, by = "new_county_name")

# map
black.pop.merged %>%
  ggplot(aes(long, lat, group = group, fill = county_black_pop)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "Number of Black Residents")

# Linear trend of population and killings
# merge cali.county.pop and cali.data
merged_cali_counties <- merge(cali.county.pop, cali.data, by = c("new_county_name"))

# plot killings and population for each state
ggplot(data = merged_cali_counties, aes(x=county.counts, y=county_pop)) +
  geom_jitter(alpha = 0.6, color = 'darkred') +
  geom_smooth(method = 'lm', se = FALSE, color = 'red', alpha = 0.6)  

# Percentage of police shootings based on county population
# rename cali.county.pop column
names(cali.county.pop)[1] <- "new_county_name"

# merge cali.county.pop and cali.data
merged.county.pop.killings <- left_join(cali.data, cali.county.pop, by = c('new_county_name'))

# add column for number of killings / state population
merged.county.pop.killings <- merged.county.pop.killings %>%
  mutate(ratio = (county.counts / county_pop)*100)

# merge to create map
killing.ratio.merged <- left_join(merged.county.pop.killings, county.cali, by='new_county_name')

# plot map for each county in california
killing.ratio.merged %>%
  ggplot(aes(long, lat, group = group, fill = ratio)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "Ratio of Police Killings and County Population")

# Percentage of black residents in each county

# merge cali.county.pop and cali.black.pop
cali.full.pop <- left_join(cali.county.pop, cali.black.pop, by = 'new_county_name')

# create column for black perc
cali.full.pop <- cali.full.pop %>%
  mutate(black.perc = (county_black_pop / county_pop)*100)

# merge to plot
cali.black.merged <- left_join(cali.full.pop, county.cali, by='new_county_name')

# plot map for each county in california
cali.black.merged %>%
  ggplot(aes(long, lat, group = group, fill = black.perc)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "% of Population that is Black")

########################

## TEXAS

# Create dataframe of just county in California for number of police killings
tx.data <- data %>%
  filter(state == "TX") %>%
  group_by(County) %>%
  summarize(county.counts = n())

# Rename County in tx.data
names(tx.data)[1] <- "new_county_name"

# Change to character
tx.data[] <- lapply(tx.data, as.character)

# Change counts to integer
tx.data$county.counts <- as.numeric(as.character(tx.data$county.counts))

# Just get Texas counties
county.tx <- counties %>%
  filter(state_abbv == "TX")

# Get all counties in Texas
all.counties <- unique(county.tx$new_county_name)

# If county not in all.counties, add it to cali.data
kill.counties <- unique(tx.data$new_county_name)

for (i in all.counties) {
  if (!(i %in% kill.counties)) {
    newRow <- data.frame(new_county_name=i, county.counts = 0)
    tx.data <- rbind(tx.data,newRow)
  } 
}

# dataframe for number of deaths in each county in california
tx.deaths <- left_join(tx.data, county.tx, by = "new_county_name")

# plot map for each county in california
tx.deaths %>%
  ggplot(aes(long, lat, group = group, fill = county.counts)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "Number of Police Killings")

# Each county's total population
tx.county.pop <- acs %>%
  filter(State == 'Texas') %>%
  group_by(County) %>%
  summarize(county_pop = sum(TotalPop, na.rm = TRUE))

# Rename County column
names(tx.county.pop)[1] <- 'new_county_name'

# merge to create map
tx.pop.merged <- left_join(tx.county.pop, county.tx, by = "new_county_name")

# map
tx.pop.merged %>%
  ggplot(aes(long, lat, group = group, fill = county_pop)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "Population")

# Each county's total black population
tx.black.pop <- acs %>%
  filter(State == 'Texas') %>%
  group_by(County) %>%
  summarize(county_black_pop = sum(num_black, na.rm = TRUE))

# Change name of county column
names(tx.black.pop)[1] <- "new_county_name"

# merge to create map
black.pop.merged <- left_join(tx.black.pop, county.tx, by = "new_county_name")

# map
black.pop.merged %>%
  ggplot(aes(long, lat, group = group, fill = county_black_pop)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "Number of Black Residents")

# Linear trend of population and killings
# merge cali.county.pop and cali.data
merged_tx_counties <- merge(tx.county.pop, tx.data, by = c("new_county_name"))

# plot killings and population for each state
ggplot(data = merged_tx_counties, aes(x=county.counts, y=county_pop)) +
  geom_jitter(alpha = 0.6, color = 'darkred') +
  geom_smooth(method = 'lm', se = FALSE, color = 'red', alpha = 0.6)  

# Percentage of police shootings based on county population

# merge cali.county.pop and cali.data
merged.county.pop.killings <- left_join(tx.data, tx.county.pop, by = c('new_county_name'))

# add column for number of killings / state population
merged.county.pop.killings <- merged.county.pop.killings %>%
  mutate(ratio = (county.counts / county_pop)*100)

# merge to create map
killing.ratio.merged <- left_join(merged.county.pop.killings, county.tx, by='new_county_name')

# plot map for each county in california
killing.ratio.merged %>%
  ggplot(aes(long, lat, group = group, fill = ratio)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "Ratio of Police Killings and County Population")

# Percentage of black residents in each county

# merge cali.county.pop and cali.black.pop
tx.full.pop <- left_join(tx.county.pop, tx.black.pop, by = 'new_county_name')

# create column for black perc
tx.full.pop <- tx.full.pop %>%
  mutate(black.perc = (county_black_pop / county_pop)*100)

# merge to plot
tx.black.merged <- left_join(tx.full.pop, county.tx, by='new_county_name')

# plot map for each county in california
tx.black.merged %>%
  ggplot(aes(long, lat, group = group, fill = black.perc)) +
  geom_polygon(color = "#000000", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn() +
  labs(fill = "% of Population that is Black")


########################

## RATIO

# merge state population (state_pop_ and number of killings per state (num_killed_state)
state.ratio <- left_join(state_pop, num_killed_state, by = 'state')

# create ratio variable
state.ratio <- state.ratio %>%
  mutate(ratio = (state.counts / pop)*100)

# map
plot_usmap(data = state.ratio, values = "ratio", lines = "white") + 
  scale_fill_continuous(name = "Rate of Police Killings") + 
  theme(legend.position = "right")

########################

## ARMED

# CREATE NEW ARMED VARIABLE
data$was.armed <- data$armed
# Rename levels
levels(data$was.armed) <- c("Other", "Yes", "Yes", "No", "Yes", "Yes", "Other", "Yes")

# Plot bar graph
ggplot(data, aes(x = is.black, fill = was.armed)) +
  geom_bar(position = 'fill')

# Chi squared test of independence
chisq_test1 <- chisq.test(data$is.black, data$was.armed)
chisq_test1
# p.value = 0.4888

# CREATE ARMED VARIABLE
data$had.firearm <- data$armed
levels(data$had.firearm) <- c("No", "Yes", "No", "No", "No", "No", "No", "No")

# Plot bar graph
ggplot(data, aes(x = is.black, fill = had.firearm)) +
  geom_bar(position = 'fill')

# Chi squared test of independence
chisq_test2 <- chisq.test(data$is.black, data$had.firearm)
chisq_test2
# p.value = 1

########################

## AGE

summary(data$age)

# Plot boxplot
ggplot(data, aes(x = is.black, y = age)) +
  geom_boxplot(aes(fill = is.black))

# Two sample t-test
t.test(age ~ is.black, data = data, var.equal = FALSE)
# p.value = 1.468e-05

########################

## INCOME
# h_income plot
ggplot(data, aes(x = is.black, y = h_income, fill = is.black)) +
  geom_boxplot()

# Two sample t-test
t.test(h_income ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.01131

# county_income plot
ggplot(data, aes(x = is.black, y = county_income, fill = is.black)) +
  geom_boxplot()

# two sample t-test
t.test(county_income ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.09629

########################

## POVERTY

# plot poverty
ggplot(data, aes(x = is.black, y = Poverty, fill = is.black)) +
  geom_boxplot()

# two sample t-test
t.test(Poverty ~ is.black, data = data, var.equal = FALSE)
# p.value = 3.396e-06

# plot child poverty
ggplot(data, aes(x = is.black, y = ChildPoverty, fill = is.black)) +
  geom_boxplot()

# two sample t-test
t.test(ChildPoverty ~ is.black, data = data, var.equal = FALSE)
# p.value = 3.302e-05

########################

## UNEMPLOYMENT

# plot 
ggplot(data, aes(x = is.black, y = urate, fill = is.black)) +
  geom_boxplot()

# two sample t-test
t.test(urate ~ is.black, data = data, var.equal = FALSE)
# p.value = 1.517e-05

########################

## COLLEGE

# plot
ggplot(data, aes(x = is.black, y = college, fill = is.black)) +
  geom_boxplot()

# two sample t-test
t.test(college ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.8633

########################

## NEIGHBORHOOD

# add column for percentage of nonblack residents
data <- data %>%
  mutate(nonblack_perc = 100 - Black)

# plot black perc
ggplot(data, aes(x = is.black, y = Black, fill = is.black)) +
  geom_boxplot()

# two sample t test
t.test(Black ~ is.black, data = data, var.equal = FALSE)
# p.value < 2.2e-16

# plot nonblack perc
ggplot(data, aes(x = is.black, y = nonblack_perc, fill = is.black)) +
  geom_boxplot()

# two sample t test
t.test(nonblack_perc ~ is.black, data = data, var.equal = FALSE)
# p.value < 2.2e-16

########################

## Cause of Death

summary(data$cause)

# Create new variable was.shot
data$was.shot <- data$cause

levels(data$was.shot)

# Rename levels
levels(data$was.shot) <- c("No", "Yes", "No", "No", "No")

# graph
ggplot(data, aes(x = is.black, fill = was.shot)) +
  geom_bar(position = 'fill')

# chi-squared test
chisq_test <- chisq.test(data$is.black, data$was.shot)
chisq_test
# p.value = 0.007481

########################

## GENDER

# Gender Percentage of neighborhood
# Tract-level male/female percentage
data <- data%>%
  mutate(perc.men = (Men / TotalPop) * 100)

# Boxplot
ggplot(data, aes(x=is.black, y=perc.men, fill=is.black)) +
  geom_boxplot()

# T-test
t.test(perc.men ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.465

# Gender Proportion

# Graph gender and is.black
ggplot(data, aes(x = is.black, fill = gender)) +
  geom_bar(position = 'fill')

# Chi-squared test of independence
chisq_test <- chisq.test(data$is.black, data$gender)
chisq_test
# p.value = 0.9201

########################

## HYPOTHESIS TESTING OF OTHER VARIABLES

# County bucket
# convert county bucket to categorical
data$county_bucket <- as.factor(data$county_bucket)
levels(data$county_bucket)
ggplot(data, aes(x = county_bucket, fill = is.black)) +
  geom_bar(position = 'fill')

# chi-squared test of independence
chisq_test2 <- chisq.test(data$is.black, data$county_bucket)
chisq_test2
# p.value = 0.001278

# Nat_bucket
ggplot(data, aes(x = nat_bucket, fill = is.black)) +
  geom_bar(position = 'fill')

# chi-squared test of independence
chisq_test3 <- chisq.test(data$is.black, data$nat_bucket)
chisq_test3
# p.value = 0.009235

# Citizen
# create variable for citizen percentage
data <- data %>%
  mutate(perc.citizen = (Citizen/TotalPop) * 100)

# boxplot
ggplot(data, aes(x=is.black, y=perc.citizen, fill = is.black)) +
  geom_boxplot()

# t.test
t.test(perc.citizen ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.9282

# Professional
t.test(Professional ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.1088

# Service
t.test(Service ~ is.black, data = data, var.equal = FALSE)
# p.value = 2.423e-05

# graph
ggplot(data, aes(x=is.black, y=Service, fill = is.black)) +
  geom_boxplot()

# Office
t.test(Office ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.9677

# Construction
t.test(Construction~ is.black, data = data, var.equal = FALSE)
# p.value = 0.001527

# graph
ggplot(data, aes(x=is.black, y=Construction, fill=is.black)) +
  geom_boxplot()

# Production
t.test(Production ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.6056

# Drive
t.test(Drive ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.001029

# plot
ggplot(data, aes(x=is.black, y=Drive, fill=is.black)) +
  geom_boxplot()

# Carpool
t.test(Carpool ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.08435

# Transit
t.test(Transit ~ is.black, data = data, var.equal = FALSE)
# p.value = 2.686e-06

# graph
ggplot(data, aes(x=is.black, y=Transit, fill=is.black)) +
  geom_boxplot()

# Walk
t.test(Walk ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.06871

# OtherTransp
t.test(OtherTransp ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.6625

# WorkAtHome
t.test(WorkAtHome ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.1447

# MeanCommute
t.test(MeanCommute ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.07022

# PrivateWork
t.test(PrivateWork ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.01122

# graph
ggplot(data, aes(x=is.black, y=PrivateWork, fill=is.black)) +
  geom_boxplot()

# PublicWork
t.test(PublicWork ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.504

# SelfEmployed
t.test(SelfEmployed ~ is.black, data = data, var.equal = FALSE)
# p.value = 1.935e-05

# graph
ggplot(data, aes(x=is.black, y=SelfEmployed, fill=is.black)) +
  geom_boxplot()

# FamilyWork
t.test(FamilyWork ~ is.black, data = data, var.equal = FALSE)
# p.value = 0.3186

########################

## SCATTER PLOT OF CONTINUOUS VARIABLES

# only get continuous variables and is.black
con_var <- data %>%
  select(c(h_income, urate, Black, Poverty, Service, Construction, Drive, Transit,
           PrivateWork, SelfEmployed, is.black))

my_cols <- c("red", "blue")

pairs(con_var[,1:10], pch = 20,
      col = my_cols[con_var$is.black],
      lower.panel=NULL)

########################

## REGION
# create function to convert to region
state_to_region <- function(vector) {
  for (i in 1:length(vector)) {
    if (vector[i] %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island",
                         "Vermont", "New York", "New Jersey", "Pennsylvania")) {
      vector[i] = 'Northeast'
    }
    else if (vector[i] %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa",
                              "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota",
                              "South Dakota")) {
      vector[i] = 'Midwest'
    }
    else if (vector[i] %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina",
                              "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky",
                              "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas",
                              "District of Columbia")) {
      vector[i] = 'South'
    }
    else if (vector[i] %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico",
                              "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington")) {
      vector[i] = 'West'
    }
  }
  return(vector)
}

# convert column to vector
state.vectors <- as.vector(data$State)

# convert to region
results <- state_to_region(state.vectors)

# add to dataframe
data$region <- results

# convert to factor
data$region <- as.factor(data$region)

########################

## WRITE TO CSV
write.csv(data, file = "~/Documents/DATA2020/Final Project/all_variables.csv")

########################

## ONLY SELECT SIGNIFICANT VARIABLES

sig.data <- data %>%
  select(c(age, state, State, County, latitude, longitude, region, h_income, county_bucket, nat_bucket, urate,
           TotalPop, Black, nonblack_perc, Poverty, ChildPoverty, Service, Construction, Drive, Transit, 
           PrivateWork, SelfEmployed, was.shot, is.black.binary))

names(sig.data)[2] <- "state_abbrev"

# write to csv
write.csv(sig.data, file = "~/Documents/DATA2020/Final Project/sig_variables.csv")
