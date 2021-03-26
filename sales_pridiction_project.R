
library(tidyverse)
library(caret)
library(lubridate)
library(MASS)
library(earth)
library(pls)


# Import train file
data <- read.csv(file = "Train.csv", na.strings = c("", "NA"))


# Take a look
head(data)
summary(data)
str(data)
View(data)
glimpse(data)


#########################################################
###                                                   ###
###       Problem a: Lets play with the data.         ###
###                                                   ###
#########################################################


############# [SessionId] #############  

# These are only index numbers.



############# [custId] #############   

# How many records do we have?
nrow(data)

# How many customers do we have?
length(unique(data$custId))



############# [visitStartTime] ############# 

# Lets see how it performs. I see you - the outlier flying high!
plot(data$visitStartTime, data$revenue)



############# [date] #############

# Any special dates?
data %>% 
  group_by(date) %>% 
  dplyr::summarise(sum = sum(revenue)) %>% 
  arrange(desc(sum))

# Two bad days..
data %>% 
  group_by(date) %>% 
  dplyr::summarise(sum = sum(revenue)) %>% 
  arrange(sum)

# Lets see the total revenue everyday
revenueEveryDay <- data %>% 
  group_by(date) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(date)

ggplot(revenueEveryDay, aes(as.Date(date), sum)) + 
  geom_line()

# Hmmm. Need to see it closer without that outlier.
revenueEveryDayWithoutTheOutlier <- revenueEveryDay[revenueEveryDay$sum < 15000, ]
ggplot(revenueEveryDayWithoutTheOutlier, aes(as.Date(date), sum)) + 
  geom_line()



############# [channelGrouping] #############

# frequency?
table(data$channelGrouping)
channelGroupings <- sort(table(data$channelGrouping), decreasing = T)
plot(channelGroupings)

# searching does not mean buying
channelGroupingsByRevenue <- data %>% 
  group_by(channelGrouping) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

channelGroupingsByRevenue

par(mfrow = c(1, 2))
pie(channelGroupings, main="channel Groupings", col = rainbow(length(channelGroupings)))
pie(channelGroupingsByRevenue$sum, 
    labels = channelGroupingsByRevenue$channelGrouping, 
    main="channel Groupings By Revenue", 
    col = rainbow(nrow(channelGroupingsByRevenue)))

par(mfrow = c(1, 1))

## Wait! Why there is a group named (Other)? Well..
data[data$channelGrouping == '(Other)', ]



############# [visitStartTime] #############  

# Anything special?
visitTimes <- as.POSIXct(data$visitStartTime, origin = "1970-01-01")
visitTimes
visitMonths <- as.numeric(visitTimes %>% substr(6, 7))
hist(visitMonths)

# Maybe buying after work? 
visitHours <- as.numeric(visitTimes %>% substr(12, 13))

# Or during work?
# I guess I need to convert to local time based on country.
hist(visitHours)



############# [visitNumber] #############  

# visitNumber is important
table(data$visitNumber)
data %>% group_by(visitNumber) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

# As visitNumber increases, the average of revenue kinda increases when visitNumber is below 40, 
visitNumbers <- data %>% 
  group_by(visitNumber) %>% 
  dplyr::summarise(mean = mean(revenue, na.rm = T)) %>% 
  arrange(visitNumber)

plot(visitNumbers)
plot(data$visitNumber, data$revenue)
plot(log(data$visitNumber), data$revenue)
plot(sqrt(data$visitNumber), data$revenue)
plot(log(table(data$visitNumber)))
table(data$visitNumber[data$visitNumber <= 40])
plot(table(data$visitNumber[data$visitNumber <= 40]))

cor.test(sqrt(data$visitNumber), data$revenue)



############# [timeSinceLastVisit] #############

# Switch milisecond to minutes
hasRevenue <- data[data$revenue != 0, ]
minutesSinceLastVisit <- hasRevenue$timeSinceLastVisit / 1000 / 60  
minutesSinceLastVisit

hist(minutesSinceLastVisit)
hist(minutesSinceLastVisit)
plot(minutesSinceLastVisit, hasRevenue$revenue)

# take a closer look
plot(minutesSinceLastVisit[hasRevenue$revenue < 15000], hasRevenue$revenue[hasRevenue$revenue < 15000])
hist(log(hasRevenue$timeSinceLastVisit))
plot(data$timeSinceLastVisit / 1000 / 60 / 60, data$revenue)

hasNoRevenue <- data[data$revenue == 0, ]
minutesSinceLastVisitNoRevenue <- hasNoRevenue$timeSinceLastVisit / 1000 / 60
hist(minutesSinceLastVisitNoRevenue)

cor.test(minutesSinceLastVisit, hasRevenue$revenue, method = "kendall")



############# [browser] #############  

table(data$browser)
data %>% 
  group_by(browser) %>% 
  count() %>% 
  arrange(desc(n))

data %>% 
  group_by(browser) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

browserAggregation <- aggregate(data$revenue, by = list(data$browser), sum, na.rm = T) %>% arrange(desc(x))



############# [operatingSystem] #############  

table(data$operatingSystem)
operatingSystemAggregation <- data %>% 
  group_by(operatingSystem) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

operatingSystemAggregation



############# [isMobile] #############  

table(data$isMobile)
data %>% 
  group_by(isMobile) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))



############# [deviceCategory] #############

table(data$deviceCategory)
data %>% 
  group_by(deviceCategory) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))



############# [continent] #############  

# If continent is missing there is no revenue
table(data$continent)
data %>% 
  group_by(continent) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

aggregate(data$revenue, by = list(data$continent), mean, na.rm = T) %>% arrange(desc(x))



############# [subContinent] ############# 

table(data$subContinent)
data %>% 
  group_by(subContinent) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

subContinentAggregation <- aggregate(data$revenue, by = list(data$subContinent), mean, na.rm = T) %>% 
  arrange(desc(x))

subContinentAggregation



############# [country] #############  

table(data$country)
countryAggregation <- aggregate(data$revenue, by = list(data$country), mean, na.rm = T) %>% 
  arrange(desc(x))
countryAggregation



############# [region] #############  

table(data$region)
aggregate(data$revenue, by = list(data$region), sum, na.rm = T) %>% 
  arrange(desc(x))



############# [metro] #############  

table(data$metro)
data %>% 
  group_by(metro) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

aggregate(data$revenue, by = list(data$metro), mean, na.rm = T) %>% 
  arrange(desc(x))



############# [city] #############  

table(data$city)
data %>% 
  group_by(city) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

aggregate(data$revenue, by = list(data$city), mean, na.rm = T) %>% 
  arrange(desc(x))



############# [networkDomain] #############  

# too many categories..
table(data$networkDomain)
aggregate(data$revenue, by = list(data$networkDomain), mean, na.rm = T) %>% 
  arrange(desc(x))



############# [topLevelDomain] #############  

table(data$topLevelDomain)
topLevelDomainAggregation <- aggregate(data$revenue, by = list(data$topLevelDomain), sum, na.rm = T) %>% 
  arrange(desc(x))

topLevelDomainAggregation
aggregate(data$revenue, by = list(data$topLevelDomain), sum, na.rm = T) %>% 
  arrange(desc(x))

# .ws is the Internet country code top-level domain (ccTLD) for Samoa. It is administered by SamoaNIC, for the Ministry of Foreign Affairs of the Government of Samoa. The .ws domain is an abbreviation for "Western Samoa", which was the nation's official name in the 1970s when two-letter country codes were standardized. 
wsDomain <- data[data$topLevelDomain == 'ws', ]

# Never mind. Only 3 instances.
wsDomain



############# [campaign] #############  

# too many missing values.
table(data$campaign)
data %>% 
  group_by(campaign) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

# It's you again who crush my work! UTF-8 saved it. I'll delete you!
zeroCampaign <- data[data$campaign == '0', ]
zeroCampaign



############# [source] #############  

table(data$source)
nrow(data[is.na(data$source), ])
sourceAggregation <- aggregate(data$revenue, by = list(data$source), sum, na.rm = T) %>% 
  arrange(desc(x))

sourceAggregation



############# [medium] #############

table(data$medium)
data %>%
  group_by(medium) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))



############# [keyword] #############  

#  too many missing values
table(data$keyword)
data %>% 
  group_by(keyword) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

keywordAggregation <- aggregate(data$revenue, by = list(data$keyword), sum, na.rm = T) %>% 
  arrange(desc(x))

length(is.na(data$keyword))
typeof(data$keyword)



############# [isTrueDirect] ############# 

table(data$isTrueDirect)
data %>% 
  group_by(isTrueDirect) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

# You guys again!
data[data$isTrueDirect == '/yt/about/', ]
data[data$isTrueDirect == '', ]



############# [referralPath] ############# 

# too many missing values
table(data$referralPath)
data %>% 
  group_by(referralPath) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>%
  arrange(desc(sum))

referralPathAggregation <- aggregate(data$revenue, by = list(data$referralPath), sum, na.rm = T) %>% 
  arrange(desc(x))



############# [adContent] #############  

# Too many missing values
table(data$adContent)
data %>% 
  group_by(adContent) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

adContentAggregation <- aggregate(data$revenue, by = list(data$adContent), sum, na.rm = T) %>% 
  arrange(desc(x))

adContentAggregation



############# [adwordsClickInfo.page] #############  

# Too many missing values
table(data$adwordsClickInfo.page)
data %>% 
  group_by(adwordsClickInfo.page) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>%
  arrange(desc(sum))

# some NAs contribute to revenue
naRowsAP <- data[is.na(data$adwordsClickInfo.page), ]
No1RowsAP <- data[data$adwordsClickInfo.page == 1, ]
sum(naRowsAP$revenue, na.rm = T)
sum(No1RowsAP$revenue, na.rm = T)
mean(naRowsAP$revenue, na.rm = T)
mean(No1RowsAP$revenue, na.rm = T)



############# [adwordsClickInfo.slot] ############# 

# Too many missing values
table(data$adwordsClickInfo.slot)
data %>% 
  group_by(adwordsClickInfo.slot) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

sum(data$revenue, na.rm = T)

# Hello again?
data[data$adwordsClickInfo.slot == '1', ]



############# [adwordsClickInfo.gclId] #############  

#  Too many categories..
table(data$adwordsClickInfo.gclId)
data %>% 
  group_by(adwordsClickInfo.gclId) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

aggregate(data$revenue, by = list(data$adwordsClickInfo.gclId), sum, na.rm = T) %>%
  arrange(desc(x))



############# [adwordsClickInfo.adNetworkType] #############  

# Too many missing values
table(data$adwordsClickInfo.adNetworkType)
data %>% 
  group_by(adwordsClickInfo.adNetworkType) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

# Nice!
data[data$adwordsClickInfo.adNetworkType == '1', ]



############# [adwordsClickInfo.isVideoAd] #############  

# Too many missing values
table(data$adwordsClickInfo.isVideoAd)
data %>% 
  group_by(adwordsClickInfo.isVideoAd) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

data[data$adwordsClickInfo.isVideoAd == '0', ]
data[data$adwordsClickInfo.isVideoAd == '1', ]
data[data$adwordsClickInfo.isVideoAd == '2', ]



############# [pageviews] #############   

table(data$pageviews)
data %>% 
  group_by(pageviews) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

aggregate(data$revenue, by = list(data$pageviews), sum, na.rm = T) %>% 
  arrange(desc(x))
plot(log(data$pageviews), data$revenue)



############# [bounces] ############# 

# bounces means no revenue.
table(data$bounces)
data %>% 
  group_by(bounces) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))



############# [newVisits] #############   

table(data$newVisits)
data %>% 
  group_by(newVisits) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))



############# [revenue] #############
sum(data$revenue, na.rm = T)
mean(data$revenue, na.rm = T)
mean(data$revenue[data$revenue > 0], na.rm = T)
revenuePerCustomer <- data[data$revenue > 0, ] %>% 
  group_by(custId) %>% 
  dplyr::summarise(sum = sum(revenue, na.rm = T)) %>% 
  arrange(desc(sum))

mean(revenuePerCustomer$sum)
plot(revenuePerCustomer$sum)
hist(revenuePerCustomer$sum)
hist(log(revenuePerCustomer$sum + 1))



#########################################################
###                                                   ###
###           Problem b: Lets slim the data.          ###
###                                                   ###
#########################################################

# Extract variables that will be used
refineData <- function(originData) {
  
  dataTemp <- originData
  
  # Add a new variable: month(extracted from date)
  dataTemp$month <- dataTemp$date %>% 
    as_datetime() %>% 
    month()
  
  # Transform channelGrouping to channelGroups
  dataTemp$channelGroups <- dataTemp$channelGrouping
  for (i in 1 : nrow(dataTemp))
    if (is.na(dataTemp$channelGroups[i])) {
      dataTemp$channelGroups[i] <- 'channelGroups_Others'
    } else if (dataTemp$channelGroups[i] != 'Organic Search' & 
               dataTemp$channelGroups[i] != 'Social' & 
               dataTemp$channelGroups[i] != 'Referral' & 
               dataTemp$channelGroups[i] != 'Direct') 
    {
      dataTemp$channelGroups[i] <- 'channelGroups_Others'
    }
  
  # Add two new variable: visitMonth and visitHour(extracted from visitStartTime)
  if (is.na(dataTemp$visitStartTime[i])) {
    dataTemp$visitMonth[i] <- (runif(1) * 12) %>% ceiling()
    dataTemp$visitHour[i] <- (runif(1) * 24) %>% ceiling()
  } 
  
  visitTime <- dataTemp$visitStartTime %>% as.POSIXct(origin = "1970-01-01")
  dataTemp$visitMonth <- as.numeric(visitTime %>% substr(6, 7))
  dataTemp$visitHour <- as.numeric(visitTime %>% substr(12, 13))
  
  # transform visitNumber into logVisitNumber
  dataTemp$logVisitNumber <- log(dataTemp$visitNumber)
  
  # transform timeSinceLastVisit into hoursSinceLastVisit
  dataTemp$hoursSinceLastVisit <- dataTemp$timeSinceLastVisit / 1000 / 60 / 60
  
  # transform browser into browserCategories
  dataTemp$browserCategories <- dataTemp$browser
  for (i in 1 : nrow(dataTemp))
    if (is.na(dataTemp$browserCategories[i])) {
      dataTemp$browserCategories[i] <- 'browserCategories_Others'
    } else if (dataTemp$browserCategories[i] != 'Chrome' & 
               dataTemp$browserCategories[i] != 'Safari' & 
               dataTemp$browserCategories[i] != 'Firefox' & 
               dataTemp$browserCategories[i] != 'Internet Explorer' & 
               dataTemp$browserCategories[i] != 'Edge' & 
               dataTemp$browserCategories[i] != 'Opera' & 
               dataTemp$browserCategories[i] != 'Safari (in-app)' & 
               dataTemp$browserCategories[i] != 'Amazon Silk' & 
               dataTemp$browserCategories[i] != 'Android Webview') 
    {
      dataTemp$browserCategories[i] <- 'browserCategories_Others_Others'
    }
  
  # transform operatingSystem into operatingSystemCategories
  dataTemp$operatingSystemCategories <- dataTemp$operatingSystem
  for (i in 1 : nrow(dataTemp))
    if (is.na(dataTemp$operatingSystemCategories[i])) {
      dataTemp$operatingSystemCategories[i] <- 'operatingSystemCategories_Others'
    } else if (dataTemp$operatingSystemCategories[i] != 'Macintosh' & 
               dataTemp$operatingSystemCategories[i] != 'Windows' & 
               dataTemp$operatingSystemCategories[i] != 'Chrome OS' & 
               dataTemp$operatingSystemCategories[i] != 'Linux' & 
               dataTemp$operatingSystemCategories[i] != 'Android' & 
               dataTemp$operatingSystemCategories[i] != 'iOS') 
    {
      dataTemp$operatingSystemCategories[i] <- 'operatingSystemCategories_Others'
    }
  
  # Fill the missing values in continent
  for (i in 1 : nrow(dataTemp))
    if (is.na(dataTemp$continent[i])) {
      dataTemp$continent[i] <- 'continent_Others'
    } else if (dataTemp$continent[i] != 'Africa' & 
               dataTemp$continent[i] != 'Americas' & 
               dataTemp$continent[i] != 'Asia' & 
               dataTemp$continent[i] != 'Europe' & 
               dataTemp$continent[i] != 'Oceania') 
    {
      dataTemp$continent[i] <- 'continent_Others'
    }
  
  # transform subContinent
  for (i in 1 : nrow(dataTemp))
    if (is.na(dataTemp$subContinent[i])) {
      dataTemp$subContinent[i] <- 'subContinent_Others'
    } else if (dataTemp$subContinent[i] != 'Western Africa' & 
               dataTemp$subContinent[i] != 'Northern America' & 
               dataTemp$subContinent[i] != 'Eastern Africa' & 
               dataTemp$subContinent[i] != 'Caribbean' & 
               dataTemp$subContinent[i] != 'South America' & 
               dataTemp$subContinent[i] != 'Australasia' & 
               dataTemp$subContinent[i] != 'Eastern Asia' & 
               dataTemp$subContinent[i] != 'Central America' & 
               dataTemp$subContinent[i] != 'Western Asia' & 
               dataTemp$subContinent[i] != 'Southeast Asia' & 
               dataTemp$subContinent[i] != 'Central Asia' & 
               dataTemp$subContinent[i] != 'Western Europe' & 
               dataTemp$subContinent[i] != 'Southern Europe' & 
               dataTemp$subContinent[i] != 'Eastern Europe' & 
               dataTemp$subContinent[i] != 'Northern Europe' & 
               dataTemp$subContinent[i] != 'Southern Asia') 
    {
      dataTemp$subContinent[i] <- 'subContinent_Others'
    }
  
  # transform country
  for (i in 1 : nrow(dataTemp))
    if (is.na(dataTemp$country[i])) {
      dataTemp$country[i] <- 'country_Others'
    } else if (dataTemp$country[i] != 'United States' & 
               dataTemp$country[i] != 'Canada' & 
               dataTemp$country[i] != 'Venezuela' & 
               dataTemp$country[i] != 'Nigeria' & 
               dataTemp$country[i] != 'Japan' & 
               dataTemp$country[i] != 'Kenya' & 
               dataTemp$country[i] != 'Indonesia' & 
               dataTemp$country[i] != 'Puerto Rico' & 
               dataTemp$country[i] != 'Australia' & 
               dataTemp$country[i] != 'Hong Kong' & 
               dataTemp$country[i] != 'Ecuador' & 
               dataTemp$country[i] != 'Israel' & 
               dataTemp$country[i] != 'Mexico' & 
               dataTemp$country[i] != 'United Kingdom' & 
               dataTemp$country[i] != 'Germany' & 
               dataTemp$country[i] != 'Ukraine' & 
               dataTemp$country[i] != 'Switzerland' & 
               dataTemp$country[i] != 'India' & 
               dataTemp$country[i] != 'France' & 
               dataTemp$country[i] != 'Spain' & 
               dataTemp$country[i] != 'United Arab Emirates' & 
               dataTemp$country[i] != 'Thailand' & 
               dataTemp$country[i] != 'South Korea' & 
               dataTemp$country[i] != 'Taiwan' & 
               dataTemp$country[i] != 'Brazil' & 
               dataTemp$country[i] != 'Singapore') 
    {
      dataTemp$country[i] <- 'country_Others'
    }
  
  # transform source
  for (i in 1 : nrow(dataTemp))
    if (is.na(dataTemp$source[i])) {
      dataTemp$source[i] <- 'source_Others'
    } else if (dataTemp$source[i] != 'mall.googleplex.com' & 
               dataTemp$source[i] != '(direct)' & 
               dataTemp$source[i] != 'google' & 
               dataTemp$source[i] != 'mail.google.com' & 
               dataTemp$source[i] != 'dfa' & 
               dataTemp$source[i] != 'sites.google.com' & 
               dataTemp$source[i] != 'gdeals.googleplex.com' & 
               dataTemp$source[i] != 'dealspotr.com' & 
               dataTemp$source[i] != 'groups.google.com' & 
               dataTemp$source[i] != 'yahoo' & 
               dataTemp$source[i] != 'facebook.com' & 
               dataTemp$source[i] != 'l.facebook.com' & 
               dataTemp$source[i] != 'bing' & 
               dataTemp$source[i] != 'mg.mail.yahoo.com' & 
               dataTemp$source[i] != 'google.com' & 
               dataTemp$source[i] != 'youtube.com') 
    {
      dataTemp$source[i] <- 'source_Others'
    }
  
  # Fill the missing values in medium
  for (i in 1 : nrow(originData))
    if (is.na(dataTemp$medium[i])) {
      dataTemp$medium[i] <- 'medium_Others'
    } else if (dataTemp$medium[i] != 'affiliate' & 
               dataTemp$medium[i] != 'cpc' & 
               dataTemp$medium[i] != 'cpm' & 
               dataTemp$medium[i] != 'organic' & 
               dataTemp$medium[i] != 'referral') 
    {
      dataTemp$medium[i] <- 'medium_Others'
    }
  
  # Deal with the missing values in isTrueDirect
  dataTemp$isTrueDirect[dataTemp$isTrueDirect != 1] <- 0
  dataTemp$isTrueDirect[is.na(dataTemp$isTrueDirect)] <- 0
  
  # transform pageviews into logPageviews
  dataTemp$logPageviews <- log(dataTemp$pageviews)
  dataTemp$logPageviews[is.na(dataTemp$logPageviews)] <- 0
  
  # deal with the missing values in bounces
  dataTemp$bounces[is.na(dataTemp$bounces)] <- 0
  
  # deal with the missing values in newVisit
  dataTemp$newVisit[is.na(dataTemp$newVisit)] <- 0

  return (dataTemp)
}


#########################################################
###                                                   ###
###               Problem c: data reborn.             ###
###                                                   ###
#########################################################

# refine the data
dataRefined <- refineData(data)
dataReborn <- dataRefined[, c('custId', 'month', 'visitMonth', 'visitHour', 'channelGroups', 'logVisitNumber', 'hoursSinceLastVisit', 'browserCategories', 'operatingSystemCategories', 'isMobile', 'deviceCategory', 'continent', 'subContinent', 'country', 'source', 'medium', 'isTrueDirect', 'logPageviews', 'bounces', 'newVisit', 'revenue')]

# testing

# deal with the missing values in revenue
dataReborn$revenue[is.na(dataReborn$revenue)] <- 0
str(dataReborn)

# transform into numeric variables
dataNumeric <- dataReborn
dataNumeric$channelGroups <- as.numeric(as.factor(dataNumeric$channelGroups))
dataNumeric$browserCategories <- as.numeric(as.factor(dataNumeric$browserCategories))
dataNumeric$operatingSystemCategories <- as.numeric(as.factor(dataNumeric$operatingSystemCategories))
dataNumeric$deviceCategory <- as.numeric(as.factor(dataNumeric$deviceCategory))
dataNumeric$continent <- as.numeric(as.factor(dataNumeric$continent))
dataNumeric$subContinent <- as.numeric(as.factor(dataNumeric$subContinent))
dataNumeric$country <- as.numeric(as.factor(dataNumeric$country))
dataNumeric$source <- as.numeric(as.factor(dataNumeric$source))
dataNumeric$medium <- as.numeric(as.factor(dataNumeric$medium))
dataNumeric$isTrueDirect <- as.numeric(as.factor(dataNumeric$isTrueDirect))

str(dataNumeric)
summary(dataNumeric)

# check the corelation map
df_cor = cor(dataNumeric)
df_cor

# find and remove other column that absolute correlation value is larger than 0.9
highCor <- findCorrelation(cor(dataNumeric), 0.9)
highCor
highCorNames <- names(dataNumeric)[highCor]
# dataNumeric <- dataNumeric[, -c(5, 10, 2)]

str(dataNumeric)

# remove the outlier rows
plot(dataNumeric$revenue)
dataNumericNoOutlier <- dataNumeric[dataNumeric$revenue < 2500, ]




#########################################################
###                                                   ###
###               Problem d(i): modeling.             ###
###                                                   ###
#########################################################

# create partitions for test
set.seed(123)
trainingSize = round(nrow(dataNumericNoOutlier) * 0.7)
train_ind <- sample(seq_len(nrow(dataNumericNoOutlier)), size = trainingSize)
trainingData <- dataNumericNoOutlier[train_ind, ]
testingData <- dataNumericNoOutlier[-train_ind, ]


# create the prediction function
prediction <- function(fit, data, isPredicting) {
  pred <- predict(fit, data)
  pred[pred < 0] <- 0
  if (!isPredicting) {
    result <- data.frame(obs = log(data$revenue + 1), pred = pred)
    defaultSummary(result)
  } else {
    return(pred)
  }
}

# test some models
fit1 <- lm(log(revenue + 1) ~ ., data = trainingData)
prediction(fit1, testingData, FALSE)
summary(fit1)

fit2 <- lm(log(revenue + 1) ~ isMobile + isTrueDirect + logPageviews + bounces + newVisit, data = trainingData)
prediction(fit2, testingData)

fit3 <- rlm(log(revenue + 1) ~ ., data = trainingData)
prediction(fit3, testingData)

fit4 <- rlm(log(revenue + 1) ~ logVisitNumber + logPageviews + bounces + newVisit, data = trainingData)
prediction(fit4, testingData)


############# OLS version #############

fit <- lm(log(revenue + 1) ~ ., data = dataNumericNoOutlier)
summary(fit)

RSS_fit <- c(crossprod(fit$residuals))
MSE_fit <- RSS_fit / length(fit$residuals)
RMSE_fit <- sqrt(MSE_fit)
RMSE_fit


############# Robust regression version #############

huberFit <- rlm(log(revenue + 1) ~ ., data = dataNumericNoOutlier)
summary(huberFit)

RSS_huberfit <- c(crossprod(huberFit$residuals))
MSE_huberfit <- RSS_huberfit / length(huberFit$residuals)
RMSE_huberfit <- sqrt(MSE_huberfit)
RMSE_huberfit


############# MARS version #############

marsFit <- earth(log(revenue + 1) ~ ., data = dataNumericNoOutlier)

marsFit$residuals

RSS_mars <- c(crossprod(marsFit$residuals))
MSE_mars <- RSS_mars / length(marsFit$residuals)
RMSE_mars <- sqrt(MSE_mars)
RMSE_mars



############# PARTIAL LEAST SQUARES version#############

plsFit <- plsr(log(revenue + 1) ~ .,
               data = dataNumericNoOutlier, 
               10, 
               method = "oscorespls")

summary(plsFit)
plsFit$residuals

RSS_pls <- c(crossprod(plsFit$residuals))
MSE_pls <- RSS_pls / length(plsFit$residuals)
RMSE_pls <- sqrt(MSE_pls)
RMSE_pls

sum(plsFit$residuals ^ 2) / 70063 / 10


# Creating a data frame to compare these 4 models
Model <- c('OSL', 'Huber loss', 'MARS', 'PLS')
Method <- c('lm', 'rlm', 'earth', 'plsr')
Package <- c('stats', 'MASS', 'earth', 'pls')
Hyperparamter <- c(NA, NA, 'degree', NA)
Selection <- c(NA, NA, NA, NA)
Rsquare <- c(0.3807, 0.4173, 0.4537384, 1.027621)
RMSE <- c(RMSE_fit, RMSE_huberfit, RMSE_mars, RMSE_pls)

df <- data.frame(Model, Method, Package, Hyperparamter, Selection, Rsquare, RMSE)
df


#########################################################
###                                                   ###
###                Problem d(iii): output.            ###
###                                                   ###
#########################################################

# deal with test data
dataTest <- read.csv('Test.csv', na.strings = c("", "NA"))

dataTest <- dataTest %>% arrange(dataTest$custId)

summary(dataTest)
testDataRefined <- refineData(dataTest)

testDataReborn <- testDataRefined[, c('custId', 'month', 'visitMonth', 'visitHour', 'channelGroups', 'logVisitNumber', 'hoursSinceLastVisit', 'browserCategories', 'operatingSystemCategories', 'isMobile', 'deviceCategory', 'continent', 'subContinent', 'country', 'source', 'medium', 'isTrueDirect', 'logPageviews', 'bounces', 'newVisit')]

# transform into numeric variables
testDataNumeric <- testDataReborn
testDataNumeric$channelGroups <- as.numeric(as.factor(testDataNumeric$channelGroups))
testDataNumeric$browserCategories <- as.numeric(as.factor(testDataNumeric$browserCategories))
testDataNumeric$operatingSystemCategories <- as.numeric(as.factor(testDataNumeric$operatingSystemCategories))
testDataNumeric$deviceCategory <- as.numeric(as.factor(testDataNumeric$deviceCategory))
testDataNumeric$continent <- as.numeric(as.factor(testDataNumeric$continent))
testDataNumeric$subContinent <- as.numeric(as.factor(testDataNumeric$subContinent))
testDataNumeric$country <- as.numeric(as.factor(testDataNumeric$country))
testDataNumeric$source <- as.numeric(as.factor(testDataNumeric$source))
testDataNumeric$medium <- as.numeric(as.factor(testDataNumeric$medium))
testDataNumeric$isTrueDirect <- as.numeric(as.factor(testDataNumeric$isTrueDirect))

str(testDataNumeric)
summary(testDataNumeric)

# check the corelation
df_cor = cor(testDataNumeric)
df_cor

# find and remove other column that absolute correlation value is larger than 0.9
highCor <- findCorrelation(cor(testDataNumeric), 0.9)
highCor
highCorNames <- names(testDataNumeric)[highCor]
# dataNumeric <- dataNumeric[, -c(5, 10, 2)]

head(testDataNumeric)



# Predict it! 
pred <- prediction(marsFit, testDataNumeric, TRUE)
pred
head(testDataNumeric)

# save pred
n <- nrow(testDataNumeric)
dataOutput <- data.frame(custId = testDataNumeric$custId, predRevenue = -1)
head(dataOutput)
for (i in 1 : n)
  dataOutput$predRevenue[i] <- pred[i]


head(dataOutput)

# Fill in 0 if bounces in the origin data is equal to 1
for(i in 1 : n)
  if(!is.na(dataTest$bounces[i])) {
    dataOutput$predRevenue[i] <- 0
  }

# Sum up the revenue grouped by custId
dataFinalized <- dataOutput %>% group_by(custId) %>% dplyr::summarise(predRevenue = sum(predRevenue))
str(dataFinalized)

# Output file
write.csv(dataFinalized, "output.csv", row.names = FALSE)


