#Tumblr Test 

install.packages("tidyverse")
install.packages("lubridate")
install.packages("gmodels")
library(gmodels)
library(tidyverse)
library(lubridate)


user_account = tbl_df(read.csv(file = "~/R/Tumblr_Take_Home/Tumblr Take Home/user_act.csv"))
user_registration = tbl_df(read.csv("~/R/Tumblr_Take_Home/Tumblr Take Home/user_regi.csv"))

#user_account = tbl_df(read.csv(file = "~/Documents/Tumblr Take Home/user_act.csv"))
#user_registration = tbl_df(read.csv("~/Documents/Tumblr Take Home/user_regi.csv"))

#Conversion of Unix timestamps to more regular ones
user_account$ts <- as_datetime(as.POSIXct(user_account$ts, origin = "1970-01-01"))
user_registration$regi_ts <- as_datetime(as.POSIXct(user_registration$regi_ts, origin = "1970-01-01"))

#Hour of day users registered
user_registration = user_registration %>%
  mutate(hours = hour(regi_ts)) 

#Hour of day users registered plot
ggplot(user_registration, aes(hours)) + geom_bar()

#Hour of day user accounts were checked
user_account = user_account %>%
  mutate(hours = hour(ts))

#Hour of day user accounts were checked plot
ggplot(user_account, aes(hours)) + geom_bar()

#Day users were registered
user_registration = user_registration %>%
  mutate(day = date(regi_ts)) 

#Day users were registered plot
ggplot(user_registration, aes(day)) + geom_bar()

#Day user accounts were checked
user_account = user_account %>%
  mutate(day = date(ts))

#plot
ggplot(user_account, aes(day)) + geom_bar()

#Quick View of table structure
glimpse(user_registration)
glimpse(user_account)

#Countries with the most users
top_10 = user_registration %>%
  filter(!is.na(regi_geo), regi_geo != "") %>%
  group_by(regi_geo) %>%
  summarise(count = n()) %>%
  mutate(rank = rank(-count)) %>%
  arrange(rank) %>% 
  head(select(regi_geo, count, rank), n = 10) 

#Plot for country percentages
user_registration %>%
  filter(regi_geo != "") %>%
  group_by(regi_geo) %>%
  summarise(prop = n()/nrow(user_registration) * 100) %>%
  arrange(desc(prop)) %>%
  head(10) %>%
  ggplot(aes(reorder(regi_geo, -prop), prop)) + geom_bar(stat = "Identity") + labs(x = "country", y = "Percentage")

nrow(top_10)

top_10 = top_10 %>% droplevels() 

#List of top 10 Countries
top_10

lut = c("US" = "United States", "BR" = "Brazil", "CA" = "Canada", "DE" = "Germany", "GB" = "United Kingdom",
        "ID" = "Indonesia", "IT" = "Italy", "KR" = "Republic of Korea", "MX" = "Mexico", "TR" = "Turkey")

top_10$regi_geo = as.character(top_10$regi_geo)

#Add real names of Countries to top 10
top_10$c_names = lut[top_10$regi_geo]

#Top 10 Countries for new registrations on Tumblr graph
ggplot(top_10, aes(reorder(c_names, -count), count)) + geom_bar(stat = "Identity") 

#List of top10 countries
top_10_list = as.character(top_10$regi_geo)


#Device Usage Worldwide
#Proportion for device usage worldwide
prop.table(table(user_registration$regi_device)) * 100

#... and graph
user_registration %>%
  group_by(regi_device) %>%
  summarise(count = n()) %>%
  ggplot(aes(reorder(regi_device, -count), count)) + geom_bar(stat = "Identity") + labs(x = "device")

#REGISTRATION BY DEVICE
################################################
#Device registration in America
user_registration %>%
  filter(regi_geo == "US") %>%
  group_by(regi_device) %>%
  summarise(count = n()) %>%
  ggplot(aes(reorder(regi_device, -count), count)) + geom_bar(stat = "Identity") + labs(x = "device")

#Device registration outside America
user_registration %>%
  filter(regi_geo != "US") %>%
  group_by(regi_device) %>%
  summarise(count = n()) %>%
  ggplot(aes(reorder(regi_device, -count), count)) + geom_bar(stat = "Identity")

#Side by side bar chart of US. vs the world
user_registration %>%
  mutate(inUS = ifelse(regi_geo == "US", TRUE, FALSE)) %>%
  ggplot(aes(regi_device, fill = inUS)) + geom_bar(position = "dodge") + labs(x = "device")


table(user_registration$regi_source)

#REGISRATION BY WEB
####################################################################
# Registration Sources

#regi Source worldwide
user_registration %>%
  group_by(regi_source) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n = 10) %>%
  ggplot(aes(reorder(regi_source, -count), count)) + geom_bar(stat = "Identity") + labs(x = "Registration Source")

##This only applies for people who signed up via the web
user_registration %>%
  filter(regi_device == "web", regi_geo == "US") %>%
  group_by(regi_source) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n = 10) %>%
  ggplot(aes(reorder(regi_source, -count), count)) + geom_bar(stat = "Identity") + labs(x = "Registration Source")

#regi Source US
user_registration %>%
  filter(regi_device == "web", regi_geo != "US") %>%
  group_by(regi_source) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n = 10) %>%
  ggplot(aes(reorder(regi_source, -count), count)) + geom_bar(stat = "Identity")

#user registration for inside vs outside the us
user_registration %>%
  mutate(inUS = ifelse(regi_geo == "US", TRUE, FALSE)) %>%
  filter(regi_device == "web") %>%
  group_by(regi_source, inUS) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n = 15) %>%
  ggplot(aes(reorder(regi_source, -count), count, fill = inUS)) + geom_bar(stat = "Identity") + labs(x = "device")

#User Registration for the top 5 vs the rest
user_registration %>%
  mutate(top5 = ifelse(regi_geo %in% c("US", "GB", "BR", "TR", "KR"), TRUE, FALSE)) %>%
  filter(regi_device == "web") %>%
  group_by(regi_source, top5) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(15) %>%
  ggplot(aes(reorder(regi_source, -count), count, fill = top5)) + geom_bar(stat = "Identity") + labs(x = "Web Reg. Source")

#User Registration for the top 5 Countries
user_registration %>%
  filter(regi_geo %in% c("US", "GB", "BR", "TR", "KR")) %>%
  filter(regi_device == "web") %>%
  group_by(regi_source, regi_geo) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(15) %>%
  ggplot(aes(reorder(regi_source, -count), count, fill = regi_geo)) + geom_bar(stat = "Identity")
  

#User registration for the web - breaking down the sources - top 10 countries
user_registration %>%
  filter(regi_device == "web", regi_geo %in% top_10_list) %>%
  group_by(regi_source) %>%
  summarise(count = n()) %>%
  mutate(rank = rank(desc(count))) %>%
  arrange(rank) %>%
  head(10) %>%
  ggplot(aes(reorder(regi_source, -count), count)) + geom_bar(stat = "Identity")


#Top 5 sources
top_10_sources = user_registration %>%
  filter(regi_device == "web", regi_geo %in% top_10_list) %>%
  group_by(regi_source) %>%
  summarise(count = n()) %>%
  mutate(rank = rank(desc(count))) %>%
  arrange(rank) %>%
  head(10) 

top_10_sources = as.character(top_10_sources$regi_source)

#Top 5 populous countries
top_5_countries = c("US", "GB", "BR", "TR", "KR")

#plots a graph of top 5 countries, and top 5 sources 
user_registration %>%
  filter(regi_device == "web", regi_geo %in% top_5_countries, regi_source == top_10_sources) %>%
  group_by(regi_geo, regi_source) %>%
  summarise(count = n()) %>%
  ggplot(aes(reorder(regi_source, -count), count, fill = regi_geo)) + geom_bar(stat = "Identity", position = "dodge") 

############################################
#IS VERFIED?
user_registration %>%
  select(regi_device, is_verified) %>%
  group_by(regi_device) %>%
  summarise(is_verfied_count = sum(is_verified), count = n(), is_verified_percentage = sum(is_verified)/n() * 100) %>%
  filter(count > 10) %>%
  arrange(is_verified_percentage) %>%
  ggplot(aes(reorder(regi_device, -is_verified_percentage), is_verified_percentage)) + geom_bar(stat = "Identity")


#How let's see the breakdown inside of web
user_registration %>%
  filter(regi_device == "web") %>%
  group_by(regi_source) %>%
  summarise(is_verified_count = sum(is_verified), count = n(), is_verified_percentage = sum(is_verified)/n() * 100) %>%
  filter(count > 10) %>%
  arrange(desc(is_verified_percentage)) %>%
  ggplot(aes(reorder(regi_source, -is_verified_percentage), is_verified_percentage)) + geom_bar(stat = "Identity")

##No referrals have higher levels of verified accounts than login.. hmm.. they have more verified accounts than  
#both android and iphone!



##########################################
#USAGE STATS FOR DEVICES
 
#Usage Statistics Worldwide
user_registration %>%
  group_by(regi_device) %>%
  gather(stats, count, 5:12) %>%
  arrange(regi_device) %>%
  ggplot(aes(stats, count, fill = regi_device)) + geom_bar(stat = "Identity", position = "dodge")  

#Usage statistics in the US
user_registration %>%
  filter(regi_geo == "US") %>%
  group_by(regi_device) %>%
  gather(stats, count, 5:12) %>%
  arrange(regi_device) %>%
  ggplot(aes(stats, count, fill = regi_device)) + geom_bar(stat = "Identity", position = "dodge") 

#What is interesting here is that despite iphones having more registrations than android, android use
#is higher for likes and searchces! WHy?


#Usage statistics outside of the US
user_registration %>%
  group_by(regi_device) %>%
  gather(stats, count, 5:12) %>%
  arrange(regi_device) %>%
  mutate(inUS = as.factor(ifelse(regi_geo == "US", "inUS", "outUS"))) %>%
  ggplot(aes(stats, count, fill = regi_device)) + geom_bar(stat = "Identity", position = "dodge") + facet_wrap(~inUS)

#Usage stats for devices - top 10 countries
user_registration %>%
  group_by(regi_device) %>%
  filter(regi_geo %in% top_10_list) %>%
  gather(stats, count, 5:12) %>%
  arrange(regi_device) %>%
  ggplot(aes(stats, count, fill = regi_device)) + geom_bar(stat = "Identity", position = "dodge") 

#Usage statistics faceted by top 10 countries
user_registration %>%
  filter(regi_geo %in% top_10_list) %>%
  group_by(regi_device) %>%
  gather(stats, count, 5:12) %>%
  arrange(regi_device) %>%
  ggplot(aes(stats, count, fill = regi_device)) + geom_bar(stat = "Identity", position = "dodge") + facet_wrap(~regi_geo)

 

##############################################################

#Scatter Plots of all variables with positive correlations

cor(user_registration[,4:12])

#Scatter plot of follows vs pageviews
user_registration %>%
  ggplot(aes(follows, pageviews)) + geom_point()

user_registration %>%
  ggplot(aes(likes, pageviews)) + geom_point()

user_registration %>%
  ggplot(aes(original_posts, pageviews)) + geom_point()

user_registration %>%
  ggplot(aes(searches, pageviews)) + geom_point()

user_registration %>%
  ggplot(aes(follows, likes)) + geom_point()  

user_registration %>%
  ggplot(aes(follows, searches)) + geom_point()

user_registration %>%
  ggplot(aes(follows, unfollows)) + geom_point()

user_registration %>%
  ggplot(aes(received_engagments, pageviews)) + geom_point()


###############################################################################
#USER ACCOUNT ANALYSIS

user_registration %>%
  filter(user_id %in% user_registration_ids)


#No of registered accounts retained
no_of_retained_accounts = user_account %>%
  filter(user_id %in% user_registration$user_id) %>%
  summarise(n())

#No. of accounts registered:
no_of_accounts_registered = user_registration %>%
  summarise(n())

#Percentage of accounts retained:
percentage_retained = no_of_retained_accounts/no_of_accounts_registered *100
percentage_retained
#approx 40%

#List of IDs of accounts that were retained:
retained_user_accounts = user_account %>%
  filter(user_id %in% user_registration$user_id) %>%
  select(user_id)

#Dataframe of accounts that were retained:
retained_user_registrations = user_registration %>%
  mutate(retained = ifelse(user_id %in% retained_user_accounts$user_id, TRUE, FALSE))

#plotting side by side device usage
retained_user_registrations %>%
  ggplot(aes(regi_device, fill = retained)) + geom_bar() + labs(x = "device")

#Check proportions of countries
retained_user_registrations %>%
  filter(regi_geo != "") %>% 
  group_by(regi_geo, retained) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(prop = count/sum(count) * 100) %>%
  head(15) %>%
  ggplot(aes(reorder(regi_geo, -count), count, fill = retained)) + geom_bar(stat = "Identity") + labs(x = "country", y = "count")

#is verified
retained_user_registrations %>%
  ggplot(aes(is_verified, fill = retained)) + geom_bar() 

#IS VERIFIED USERS ARE A LOT MORE LIKELY TO STICK AROUND!


#Looking at user registration - nothing here
retained_user_registrations %>%
  filter(regi_device != "none") %>%
  group_by(regi_source, retained) %>%
  summarise(count = n())%>%
  arrange(desc(count)) %>%
  head(20) %>%
  ggplot(aes(reorder(regi_source, -count), count, fill = retained)) + geom_bar(stat = "Identity", position = "dodge")
 
  
#Recieved engagements across devices
retained_user_registrations %>%
  group_by(regi_device, retained) %>%
  summarise(received_engagements = sum(received_engagments)) %>%
  ggplot(aes(reorder(regi_device, -received_engagements),received_engagements, fill = retained)) + geom_bar(stat = "Identity") + labs(x = "device")


#Recieved engagements across devices
retained_user_registrations %>%
  group_by(regi_device, retained) %>%
  summarise(received_engagements = sum(received_engagments)) %>%
  ggplot(aes(reorder(regi_device, -received_engagements),received_engagements, fill = retained)) + geom_bar(stat = "Identity") + labs(x = "device")

#BOOM! ACCOUNTS THAT HAD MORE RECIEVED ENGAGEMENTS TENDED TO STICK AROUND!

#Recieved engagements across devices
retained_user_registrations %>%
  group_by(regi_device, retained) %>%
  summarise(received_engagements = sum(original_posts)) %>%
  ggplot(aes(reorder(regi_device, -received_engagements),received_engagements, fill = retained)) + geom_bar(stat = "Identity") + labs(x = "device")


#user stuff across devices
retained_user_registrations %>%
  gather(metrics, count, 4:12) %>%
  filter(metrics != "is_verified") %>%
  group_by(metrics, retained) %>%
  summarise(count = sum(count)) %>%
  ggplot(aes(metrics, count, fill = retained)) + geom_bar(stat = "Identity", position = "dodge")
#Almost universally, more engaged were likely to stay, no matter what device


#user stuff across devices
retained_user_registrations %>%
  filter(regi_geo == "BR") %>%
  filter(regi_device == "android") %>%
  gather(metrics, count, 4:12) %>%
  filter(metrics != "is_verified") %>%
  group_by(metrics, retained) %>%
  summarise(count = sum(count)) %>%
  ggplot(aes(metrics, count, fill = retained)) + geom_bar(stat = "Identity", position = "dodge")
#Almost universally, more engaged were likely to stay, no matter what device


#Let's look at how they log in
retained_user_registrations %>%
  ggplot(aes(regi_device, fill = retained)) + geom_bar(position = "dodge") + labs(x = "device")

#APIs have higher retention... i guess...

#Devices Retained
user_account %>%
  filter(user_id %in% user_registration_ids) %>%
  ggplot(aes(device), fill = regi_device) + geom_bar()

#Devices Retained - US vs the world
user_registration %>%
  inner_join(user_account, by = "user_id") %>%
  mutate(inUS = ifelse(regi_geo == "US", TRUE, FALSE)) %>%
  ggplot(aes(device, fill = inUS)) + geom_bar(position = "dodge")
  

#Retained devices were mostly android worldwide? with IOS in second?

retained_user_registrations %>%
  filter(regi_geo != "") %>%
  group_by(regi_geo, retained) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(20) %>%
  ggplot(aes(reorder(regi_geo, -count), count, fill = retained)) + geom_bar(stat = "Identity", position = "dodge") + labs(x = "country")


#Higher Proportions are from the States, GB, BR, etc

#Crosstable of devices registered and devices being used
devices = user_registration %>%
  inner_join(user_account, by = "user_id") %>%
  select(regi_device, device)

switch = CrossTable(devices$regi_device, devices$device)


#Table of Usage metrics among retained users in the US
user_registration %>%
  inner_join(user_account, by = "user_id") %>%
  filter(regi_geo == "US") %>%
  gather(metrics, count, 5:12) %>%
  group_by(metrics, device) %>%
  summarise(count = sum(count)) %>%
  ggplot(aes(metrics, count, fill = device)) + geom_bar(stat = "Identity", position = "dodge")


#Table of Usage metrics among retained users outside the US
user_registration %>%
  inner_join(user_account, by = "user_id") %>%
  filter(regi_geo != "US") %>%
  gather(metrics, count, 5:12) %>%
  group_by(metrics, device) %>%
  summarise(count = sum(count)) %>%
  ggplot(aes(metrics, count, fill = device)) + geom_bar(stat = "Identity", position = "dodge")

#Tried looking at time of day when they registered
retained_user_registrations %>%
  ggplot(aes(day, fill = retained)) + geom_bar(position = "dodge") %>%

#Tried looking at time of day when users were examined
retained_user_registrations %>%
  ggplot(aes(hours, fill = retained)) + geom_bar(position = "dodge")
