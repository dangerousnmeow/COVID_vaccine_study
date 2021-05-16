library(data.table)
require(devtools)
install_github("Displayr/flipTime")
library(flipTime)
# library(lubridate)

infection <- read.csv(file = "infection.csv",header=T)
infection <- data.table(infection)
infection <- infection[, Date:=as.character(AsDateTime(infection$Date, us.format=F))]
infection <- setNames(infection, c("date", "country", "confirmed", "recovered", "deaths"))
infection <- infection[, increaseRate:= deaths/confirmed]

infection_us <- read.csv(file="infection_us.csv", header=T) 
infection_us <- data.table(infection_us)
infection_us <- infection_us[,list(confirmed=sum(Confirmed,na.rm=T),
                                   deaths=sum(Deaths,na.rm=T)),
                             by=list(Date)]
infection_us <- infection_us[, country:= "United States"]
infection_us <- setNames(infection_us, c("date","confirmed","deaths","country"))

vaccination <- read.csv(file="vaccination.csv", header=T)
vaccination <- data.table(vaccination)
vaccination <- vaccination[, date:=as.character(AsDateTime(vaccination$date, us.format = F))]


population <- read.csv(file="population.csv", header=T)
population <- data.table(population)
population <- population[,c(1,2)]
population <- setNames(population, c("country", "population_2020"))

# Mar24
Mar24_inf <- infection[date == "2021-03-24",]
Mar24_inf[179,2] <- "United States"
Mar24_vac <- vaccination[date == "2021-03-01",]
Mar24 <- merge(Mar24_inf, Mar24_vac, by="country")
write.table(Mar24,file="Mar24.txt",quote=T,col.name=T,row.names=T)

#plots
library(psych)
cormat <- signif(cor(Mar24[,c(3,4,5,6,9,10,11,12,13,14,15,16,17)]),2)
pairs.panels(Mar24[,c(3,4,5,6,9,10,11,12,13,14,15,16,17)],
             method="pearson",density=T,ellipses=T)
# change na to 0 to use heatmap
#cormat[is.na(cormat)] = 0
#col<- colorRampPalette(c("blue", "white", "red"))(20)
#heatmap(cormat, col=col, symm=TRUE)
barplot(c(229825,277402), main="Observed Death Frequency", names.arg = c("Female","Male"))

# paired t-test
oct10_inf <- infection[date == "2020-10-10",]
oct10_inf <- oct10_inf[,c(2,6)]
oct10_inf <- setNames(oct10_inf,c("country","oct10"))
apr10_inf <- infection[date == "2021-04-10",]
apr10_inf <- apr10_inf[,c(2,6)]
apr10_inf <- setNames(apr10_inf,c("country","apr10"))
comb_t_test <- merge(oct10_inf,apr10_inf,by="country")
comb_t_test <- comb_t_test[complete.cases(comb_t_test),]
t.test(comb_t_test$oct10,comb_t_test$apr10,alternative="greater",paired=T)
hist(comb_t_test$oct10,breaks=(0:30)*0.01, main="Oct. 10th, 2020",xlab="Increase Rate")
hist(comb_t_test$apr10,breaks=(0:30)*0.01, main="Apr. 10th, 2021",xlab="Increase Rate")

# comb
vac_new <- vaccination[, country.date:= paste(country,date)]
inf_new <- merge(infection, population, by="country")
inf_new <- inf_new[, confirmed_prop:= confirmed/population_2020]
inf_new <- inf_new[, recovered_prop:= recovered/population_2020]
inf_new <- inf_new[, deaths_prop:= deaths/population_2020]
inf_new <- inf_new[, country.date:= paste(country,date)]
inf_new <- subset(inf_new,select=-(increaseRate))
comb <- merge(inf_new, vac_new, by="country.date")
write.table(comb,file="comb.txt",quote=T,col.name=T,row.name=T)
inf_us <- merge(infection_us, population, by="country")
inf_us <- inf_us[, confirmed_prop:= confirmed/population_2020]
inf_us <- inf_us[, deaths_prop:= deaths/population_2020]
inf_us <- inf_us[, country.date:= paste(country,date)]
comb_us <- merge(inf_us, vac_new, by="country.date")
write.table(comb_us,file="comb_us.txt",quote=T,col.name=T,row.name=T)

library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("MASS")
library(MASS)
data <- read.table("comb.txt",head=TRUE)
data_US <- read.table("comb_us.txt",head=TRUE)
data
data_US
data_cleaned <- data[,c(-1, -11, -12, -13, -17, -22, -25)]
unique(data_cleared$country.x)
# choose 6 countries
data_China <- data_cleaned[which(data_cleaned$country.x == "China"),]
data_China$country.x <- factor(data_China$country.x)

data_UK <- data_cleaned[which(data_cleaned$country.x == "United Kingdom"),]
data_UK
data_Aus <- data_cleaned[which(data_cleaned$country.x == "Australia"),]
data_Aus
data_France <- data_cleaned[which(data_cleaned$country.x == "France"),]
data_France
data_India <- data_cleaned[which(data_cleaned$country.x == "India"),]
data_India
# date range
data_China[order(data_China$date.x),][c(1,nrow(data_China)),2]
data_US[order(data_US$date.x),][c(1,nrow(data_US)),3]
data_UK[order(data_UK$date.x),][c(1,nrow(data_UK)),2]
data_Aus[order(data_Aus$date.x),][c(1,nrow(data_Aus)),2]
data_France[order(data_France$date.x),][c(1,nrow(data_France)),2]
data_India[order(data_India$date.x),][c(1,nrow(data_India)),2]

# Extract information for different proportion information
col = c("country.x", "date.x", "confirmed", "deaths", "population_2020", "confirmed_prop", "deaths_prop", "daily_vaccinations")
China_all = data_China[,col]
China_all$daily_vaccinations_prop = China_all$daily_vaccinations / China_all$population_2020
China_all$confirmed_daily = China_all$confirmed / China_all$daily_vaccinations
China_all$deaths_daily = China_all$deaths / China_all$daily_vaccinations

US_all = data_US[,col]
US_all$daily_vaccinations_prop = US_all$daily_vaccinations / US_all$population_2020
US_all$confirmed_daily = US_all$confirmed / US_all$daily_vaccinations
US_all$deaths_daily = US_all$deaths / US_all$daily_vaccinations

UK_all = data_UK[,col]
UK_all$daily_vaccinations_prop = UK_all$daily_vaccinations / UK_all$population_2020
UK_all$confirmed_daily = UK_all$confirmed / UK_all$daily_vaccinations
UK_all$deaths_daily = UK_all$deaths / UK_all$daily_vaccinations

Aus_all = data_Aus[,col]
Aus_all$daily_vaccinations_prop = Aus_all$daily_vaccinations / Aus_all$population_2020
Aus_all$confirmed_daily = Aus_all$confirmed / Aus_all$daily_vaccinations
Aus_all$deaths_daily = Aus_all$deaths / Aus_all$daily_vaccinations

France_all = data_France[,col]
France_all$daily_vaccinations_prop = France_all$daily_vaccinations / France_all$population_2020
France_all$confirmed_daily = France_all$confirmed / France_all$daily_vaccinations
France_all$deaths_daily = France_all$deaths / France_all$daily_vaccinations

India_all = data_India[,col]
India_all$daily_vaccinations_prop = India_all$daily_vaccinations / India_all$population_2020
India_all$confirmed_daily = India_all$confirmed / India_all$daily_vaccinations
India_all$deaths_daily = India_all$deaths / India_all$daily_vaccinations

ALL = rbind(China_all, US_all, UK_all, Aus_all, France_all, India_all)
ALL$country.x <- factor(ALL$country.x)
ALL <- ALL[which(ALL$confirmed_daily < 50),]
ALL <- ALL[which(ALL$deaths_daily < 50),]
#class(levels(ALL$country.x))
#f<-factor(ALL$country.x, labels=c('United States', 'China', 'United Kingdom', 'Australia', 'France', 'India'), ordered=FALSE)
f <- c("China", "United States", "United Kingdom", "Australia", "France", "India")


boxplot(confirmed_prop ~ country.x, data = ALL, main = 'Confirmed-Case Proportion', xlab = 'Country')
boxplot(deaths_prop ~ country.x, data = ALL, main = 'Death Proportion', xlab = 'Country')
boxplot(daily_vaccinations_prop ~ country.x, data = ALL, main = 'Daily-Vaccination Proportion', xlab = 'Country')
boxplot(confirmed_daily ~ country.x, data = ALL, main = 'Confirmed-Case  to Daily-Vaccination', xlab = 'Country',ylab = 'Ratio(Confirmed/DailyVaccination')
boxplot(deaths_daily ~ country.x, data = ALL, main = 'Deaths to Daily-Vaccination', xlab = 'Country', ylab = 'Ratio(Death/DailyVaccination)')

# Normal distribution???
a = asin(sqrt(China_all$confirmed_prop))
qqnorm(a, main = "daily vaccinations proportion in China")
qqline(a)
shapiro.test(a)
b = log(US_all$daily_vaccinations_prop)
qqnorm(b, main = "daily vaccinations proportion in US")
qqline(b)
shapiro.test(b)
c = log(UK_all$daily_vaccinations_prop)
qqnorm(c, main = "daily vaccinations proportion in UK")
qqline(c)
shapiro.test(c)
d = log(Aus_all$daily_vaccinations_prop)
qqnorm(d, main = "daily vaccinations_prop proportion in Australia")
qqline(d)
shapiro.test(d)
e = log(France_all$daily_vaccinations_prop)
qqnorm(e, main = "daily vaccinations_prop proportion in France")
qqline(e)
shapiro.test(e)
f = log(India_all$daily_vaccinations_prop)
qqnorm(f, main = "daily vaccinations_prop proportion in India")
qqline(f)
shapiro.test(f)

# Wilcoxon rank-sum test
country_list <- list(China_all, US_all, UK_all, Aus_all, France_all, India_all)
i = 4
j = 6
wilcox.test(x = country_list[[i]]$confirmed_prop, y = country_list[[j]]$confirmed_prop, paired = F, alternative = "two.sided")
wilcox.test(x = country_list[[i]]$deaths_prop, y = country_list[[j]]$deaths_prop, paired = F, alternative = "two.sided")
wilcox.test(x = country_list[[i]]$daily_vaccinations_prop, y = country_list[[j]]$daily_vaccinations_prop, paired = F, alternative = "two.sided")
wilcox.test(x = country_list[[i]]$confirmed_daily, y = country_list[[j]]$confirmed_daily, paired = F, alternative = "two.sided")
wilcox.test(x = country_list[[i]]$deaths_daily, y = country_list[[j]]$deaths_daily, paired = F, alternative = "two.sided")


#chi square for association between U.S vaccination and infection cases
observed_table <- matrix(c(3,78899,8,41848,161,116496), nrow = 3, ncol = 2, byrow = T)
rownames(observed_table) <- c('two doses','one dose','not vaccinated')
colnames(observed_table) <- c('confirmed','not infected')
chisq.test(observed_table,correct = F)

#Spearman correlation test between world vaccination cases and infection cases
Mar24 <- read.table(file="Mar24.txt",header=T)
cor.test(Mar24$increaseRate,Mar24$total_vaccinations_per_hundred,method = 'spearman')

