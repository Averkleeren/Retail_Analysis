library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(tmaptools)
library(stringr)

#Location Analysis
dat <- read.csv("Charlotte_middle (6).csv")
dat <- read.csv("Noda_fixed.csv")
dat <- read.csv("dill_fixed.csv")


#Property Type Analytics
dat <- read.csv("Noda_housey.csv")
dat <- read.csv("Noda_town.csv")
dat <- read.csv("Condos.csv")

#Extra Analysis
dat <- read.csv("Myers.csv")
dat <- read.csv("Plaza.csv")

#Timber Analysis
dat <- read.csv("lots.csv")

# Extract the numeric part of the lot size and price information
dat$Square.ft <- as.numeric(gsub("[^0-9\\.]", "", dat$Square.ft))
dat$Price <- as.numeric(gsub("[^0-9\\.]", "", dat$Price))
dat$Sale.Date <- substr(dat$Sale.Date, 6, 15)
dat$Sale.Date <- as.Date(dat$Sale.Date, format = "%m/%d/%Y")
dat$day <- day(dat$Sale.Date)
dat$month <- month(dat$Sale.Date)
dat$year <- year(dat$Sale.Date)
dat$quarter <- ((as.numeric(dat$month)-1)%/%3) +1

#fixing the millions errors
for(i in 1:nrow(dat)){
  if(dat$Price[i] < 1000){
    dat$Price[i] = dat$Price[i] * 1000000
  }
}

#ppa statistic
dat$Ppa <- dat$Price / dat$Square.ft

#Cleaning up the data
#getting rid of none sqfts and dupes
dat <- filter(dat, Price > 1000, Square.ft > 10)
              #, Beds != "--", Beds != "Studio")
dat <- unique(dat)
dat <- filter(dat, Ppa < 1000)

#dat <- filter(dat, Square.ft < 100000)

Cleaned <- dat
Cleaned$Beds <- as.numeric(gsub("[^0-9\\.]", "", Cleaned$Beds))
Cleaned <- filter(Cleaned, Beds < 4)
Cleaned$Beds <- as.character(Cleaned$Beds)
Cleaned <- unique(Cleaned)

extra <- dat
extra$Baths <- as.numeric(gsub("[^0-9\\.]", "", extra$Baths))
extra <- filter(extra, Baths < 4)
extra$Baths <- as.character(extra$Baths)
extra <- unique(extra)

#making new statistics
New_dat <- dat  %>% group_by(Sale.Date) %>% summarize(avg = mean(Ppa), sd = sd(Ppa))
Numbers <- Cleaned  %>% group_by(year, quarter) %>% summarize(avg = mean(Ppa), sd = sd(Ppa))
Bedds <- Cleaned %>%  group_by(Beds, Sale.Date) %>% summarize(avg = mean(Ppa), sd = sd(Ppa))
bats <- extra %>%  group_by(Baths, Sale.Date) %>% summarize(avg = mean(Ppa), sd = sd(Ppa))





#_____________________Overall statistics ____________________

#Bar graph
#facet wrap ppa per quarter
ggplot(data = Numbers, aes(x=quarter, y = avg, fill = year)) +
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=avg-sd, ymax = avg+sd))+
  facet_wrap(~year)+
  xlab("Quarter")+
  ylab('Ppa')
  


#Scatter plot
#Ppa vs time
ggplot(data = New_dat, aes(x = Sale.Date, y = avg))+
  geom_smooth(color = "purple", method = lm)+
  geom_point(color = "red")+
  theme_gray()+
  xlab('Year')+
  ylab('Price Per Square Foot ($)')

regression <- lm(New_dat$avg ~ New_dat$Sale.Date)
market_slope <- coef(regression)[2]
summary(regression)

#density plot
density_obj <- density(dat$Ppa)  # Compute density estimates
plot(density_obj, main = "Density Plot of Ppa", xlab = "Values of Ppa", ylab = "Density")

boxplot(dat$Ppa, main = "Box Plot of ppa", xlab = "ppa", horizontal = TRUE)


#___________________________________________________________

#_____________________Specific statistics ___________________

Similar <- filter(dat, Beds ==3, Baths == 2, )
boxplot(Similar$Ppa, main = "Box Plot of ppa", xlab = "ppa", horizontal = TRUE)






#____________________________________________________________

#________________________Bed Analytics_______________________

#scatter plot of all PPA vs time
ggplot(data = Bedds, aes(x = Sale.Date, y = avg, color = Beds))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_fill_manual(values = c("red", "green", "blue"))+
  theme_gray()+
  xlab('Year')+
  ylab('Price Per Square Foot ($)')



#data of PPA/time vs beds

one <- filter(Bedds, Beds == 1)
model <- lm(one$avg ~ one$Sale.Date)
one_slope <- coef(model)[2]

two <- filter(Bedds, Beds == 2)
model <- lm(two$avg ~ two$Sale.Date)
two_slope <- coef(model)[2]

three <- filter(Bedds, Beds == 3)
model <- lm(three$avg ~ three$Sale.Date)
three_slope <- coef(model)[2]

Bed_slope_vs_beds <- Bedds %>% group_by(Beds) %>% summarize()
Bed_slope_vs_beds$Slopes <- c(one_slope, two_slope, three_slope)


#graph of PPA/time vs beds
ggplot(data = Bed_slope_vs_beds, aes(x=Beds, y = Slopes, fill = Beds)) +
  geom_bar(stat = "identity", color = "black")+
  xlab("Number of Beds")+
  ylab('Change in Ppa Over Time')+
  geom_hline(yintercept = 0, color = "black")


#____________________________________________________________
#Baths

#scatter plot of all PPA vs time
ggplot(data = bats, aes(x = Sale.Date, y = avg, color = Baths))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_fill_manual(values = c("yellow", "cyan", "purple"))+
  theme_gray()+
  xlab('Year')+
  ylab('Price Per Square Foot ($)')



#data of PPA/time vs beds

one <- filter(bats, Baths == 1)
model <- lm(one$avg ~ one$Sale.Date)
one_slope <- coef(model)[2]

two <- filter(bats, Baths == 2)
model <- lm(two$avg ~ two$Sale.Date)
two_slope <- coef(model)[2]

three <- filter(bats, Baths == 3)
model <- lm(three$avg ~ three$Sale.Date)
three_slope <- coef(model)[2]

Bed_slope_vs_beds <- bats %>% group_by(Baths) %>% summarize()
Bed_slope_vs_beds$Slopes <- c(one_slope, two_slope, three_slope)


#graph of PPA/time vs beds
ggplot(data = Bed_slope_vs_beds, aes(x=Baths, y = Slopes, fill = Baths)) +
  geom_bar(stat = "identity", color = "black")+
  xlab("Number of Beds")+
  ylab('Change in Ppa Over Time')+
  geom_hline(yintercept = 0, color = "black")



#_____



#______________________Custom stats__________________________
dat <- filter(dat, Price < 2000000)
#Price Histogram
hist(dat$Price, main = "Property Price Distribution", xlab = "Price", ylab = "Frequency")

#Price overtime
ggplot(data = dat, aes(x = Sale.Date, y = Price))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_gray()+
  xlab('Year')+
  ylab('Price')

regression <- lm(dat$Price ~ dat$Sale.Date)
summary(regression)

#Price vs Square ft
ggplot(data = dat, aes(x = Square.ft, y = Price))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_gray()+
  xlab('Square ft')+
  ylab('Price')



#____________________________________________________________

options(scipen = 999)
