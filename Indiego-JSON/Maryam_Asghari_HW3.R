---
#title: "Hw3"
#author: "Maryam Asghari"
#date: "2/19/2021"
---

# Question 1 

# Set the directory
getwd()
setwd("/Users/maryamasghari/Desktop/Study/Bu M.S/fall 2021/web Analytics/Assignments/Hw3")

#loading required libraries
# Load the package required to read JSON files.
library(rjson)
library(jsonlite)
# Load the package for q-q plot with lower and upper limit - 95% conf int
library(car)
#install.packages("effsize")
library("effsize")


#Create some functions
# Create a function to get the data from json file 
get_data <- function(json_file) {
  raw_data <- jsonlite::stream_in(file(json_file))
  return(raw_data)
}

#Create a function to get the year and month of each json file 

get_date <-  function(json_read){
  date <- json_read[["run_id"]][1]
  s1 = unlist(strsplit(date, split=c('_'), fixed=TRUE))[2]
  s1 = unlist(strsplit(s1, split=c('T'), fixed=TRUE))[1]
  date <-  as.Date(s1,"%Y-%m-%d")
  year <- as.numeric(format(date,'%Y'))
  month <-  month.abb[as.numeric(format(date, "%m"))]
  return(c(year,month))
}

#Create a function to get the frequency of each keyword from each file 
#json files from 2016 and 2017 had different formats
get_freq <- function(json_read,word){
  category <- json_read[["data"]]["category"]
  freq <- length(category[category == word ])
  return (c(word, freq))
}
get_freq2 <- function(data,word){
  category <- data[["data"]]["category_name"]
  freq <- length(category[category == word ])
  return (c(word, freq))
}

#function to add all 4 values to a data frame (same for 2016-2017)

add_data <- function(df,data,date,keyword){
  freq <- get_freq(data,keyword)
  df[nrow(df) + 1,] = c(freq[1],date[1],date[2],freq[2])
  return(df)
}
add_data2 <- function(df,data,date,keyword){
  freq <- get_freq2(data,keyword)
  df[nrow(df) + 1,] = c(freq[1],date[1],date[2],freq[2])
  return(df)
}

#get a data frame with data from all 5 files for each keyword
keyword_df <- function(df,keyword){
  df <- add_data2(df,data1,date1,keyword)
  df <- add_data2(df,data2,date2,keyword)
  df <- add_data(df,data3,date3,keyword)
  df <- add_data(df,data4,date4,keyword)
  df <- add_data(df,data5,date5,keyword)
  df$freq <- as.numeric(df$freq)
  return(df)
}

#function to create 4 plots for each keyword and save a jpg file 

plot_dens_hist <- function(df,keyword){
  jpeg(paste(keyword,".jpg"))
  par(mfrow=c(2,2))
  
  hist(df$freq,col = "skyblue",border = FALSE,main ="Histogram", xlab = "Frequency",
       ylab = "Count")
  
  plot(density(df$freq), frame = FALSE,lwd=2, col = "blue",main = "Density plot" )
  
  plot(density(df$freq,adjust = 10), frame = FALSE,lwd=2, col = "blue",main = "Density plot, adj = 10" ,
       sub = paste(" --- mean = ",mean(df$freq)) )
  
  abline(v=mean(df$freq),lty="dashed")
  
  qqPlot(df$freq , main = " Q-Q plot with lower and upper limit" ,ylab = "Frequency Quantiles")
  
  dev.off()
}

#Reading the Json files and get the required information
#get the data and date of 5 files
data1 <- get_data("Indiegogo_2016-10-14T04_16_17_051Z.json")
date1 <- get_date(data1)

data2 <- get_data("Indiegogo_2017-07-15T21_40_50_545Z.json")
date2 <- get_date(data2)

data3 <- get_data("Indiegogo_2018-08-17T10_40_24_782Z.json")
date3 <- get_date(data3)

data4 <- get_data("Indiegogo_2019-03-15T10_40_03_647Z.json")
date4 <- get_date(data4)

data5 <- get_data("Indiegogo_2020-04-17T10_40_10_294Z.json")
date5 <- get_date(data5)

#create an empty data frame
tmp_df <- data.frame(category=character(),
                     year=character(), 
                     month=character(), 
                     freq=integer(),
                     stringsAsFactors=FALSE) 

#create the data frame for each keyword
df_Edu <- keyword_df(tmp_df ,"Education")
df_ENer <- keyword_df(tmp_df ,"Energy & Green Tech")
df_Heal <- keyword_df(tmp_df ,"Health & Fitness")
df_Fash <- keyword_df(tmp_df ,"Fashion & Wearables")
df_Well <- keyword_df(tmp_df ,"Wellness")

#Density plot for each keyword 

#Education

df_Edu

#4 plot
plot_dens_hist(df_Edu,"Education" )
par(mfrow=c(1,1))
#Density plot
plot(density(df_Edu$freq), frame = FALSE,lwd=2, col = "blue",main = "Density plot" )

#Q-Q plot
qqnorm(df_Edu$freq, pch = 1, frame = FALSE,  xlim = c())
qqline(df_Edu$freq, col = "steelblue", lwd = 2)

#Energy & Green Tech
df_ENer

plot_dens_hist(df_ENer,"Energy & Green Tech" )
par(mfrow=c(1,1))
#Density plot

plot(density(df_ENer$freq), frame = FALSE,lwd=2, col = "blue",main = "Density plot" )


#Q-Q plot

qqnorm(df_ENer$freq, pch = 1, frame = FALSE,  xlim = c())
qqline(df_ENer$freq, col = "steelblue", lwd = 2)

#Health & Fitness

df_Heal

plot_dens_hist(df_Heal,"Health & Fitness")
par(mfrow=c(1,1))

#Density plot

plot(density(df_Heal$freq), frame = FALSE,lwd=2, col = "blue",main = "Density plot" )

#Q-Q plot

qqnorm(df_Heal$freq, pch = 1, frame = FALSE,  xlim = c())
qqline(df_Heal$freq, col = "steelblue", lwd = 2)


#Fashion & Wearables

df_Fash

plot_dens_hist(df_Fash,"Fashion & Wearables")
par(mfrow=c(1,1))

#Density plot
plot(density(df_Fash$freq), frame = FALSE,lwd=2, col = "blue",main = "Density plot" )


#Q-Q plot
qqnorm(df_Fash$freq, pch = 1, frame = FALSE,  xlim = c())
qqline(df_Fash$freq, col = "steelblue", lwd = 2)

#Wellness
df_Well

plot_dens_hist(df_Well,"Wellness")
par(mfrow=c(1,1))
#Density plot

plot(density(df_Well$freq), frame = FALSE,lwd=2, col = "blue",main = "Density plot" )

#Q-Q plot

qqnorm(df_Well$freq, pch = 1, frame = FALSE,  xlim = c())
qqline(df_Well$freq, col = "steelblue", lwd = 2)

#Question 2

#Compare following two categories: “Health & Fitness”, “Fashion & Wearables” on year basis (2018, 2019, 2020).

heal_fash <- merge(df_Heal[3:5,],df_Fash[3:5,],by=c("year","month"))
heal_fash

par(mfrow=c(1,2))

qqnorm(heal_fash$freq.x, pch = 16, frame = FALSE,  xlim = c(),main = "Q-Q Plot-Health")
qqline(heal_fash$freq.x, col = "black", lwd = 2,lty="dashed")

qqnorm(heal_fash$freq.y, pch = 16, frame = FALSE,  xlim = c(),main = "Q-Q Plot-Fashion")
qqline(heal_fash$freq.y, col = "black", lwd = 2,lty="dashed")

par(mfrow=c(1,1))

# One parametric test
#Two sample t-test :

t.test(heal_fash$freq.x , heal_fash$freq.y,alternative="two.sided", conf.level=0.95)

#One-Way ANOVA:
aovres = aov(freq~category,data=mydata)
summary(aovres)

boxplot(freq~category,data=mydata) 


#Two non-parametric tests:

# 1- Kolmogorov-Smirnov (KS-Test)

ks.test(heal_fash$freq.x , heal_fash$freq.y)

# 2- Mann-Whitney-Wilcoxon 

wilcox.test(heal_fash$freq.x , heal_fash$freq.y)

# Use the effect size test, to quantify the magnitude of differences.

#Cohen's d Test 

res1 <- cohen.d(heal_fash$freq.x , heal_fash$freq.y,return.dm=TRUE)
print(res1)

#Cliff delta test
res2 <- cliff.delta(heal_fash$freq.x , heal_fash$freq.y,return.dm=TRUE)
print(res2)

# Question 3: 

#since it didn't mentioned in the question I will give cor for both 3 years and five years. 

#3 years

print("Pearson:")
cor(heal_fash$freq.y, heal_fash$freq.x  ,  method = "pearson")
print("Kendall:")
cor(heal_fash$freq.y, heal_fash$freq.x , method = "kendall")
print("Spearman:")
cor(heal_fash$freq.y, heal_fash$freq.x  , method = "spearman")

#5 years

heal_fash2 <- merge(df_Heal,df_Fash,by=c("year","month"))
heal_fash2


print("Pearson:")
cor(heal_fash2$freq.y, heal_fash2$freq.x  ,  method = "pearson")
print("Kendall:")
cor( heal_fash2$freq.y, heal_fash2$freq.x , method = "kendall")
print("Kendall:")
cor(heal_fash2$freq.y,heal_fash2$freq.x  , method = "spearman")




