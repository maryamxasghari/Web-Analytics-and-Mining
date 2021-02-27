
# ------------------------------------  Homework 2   ------------------------------ #
# Maryam Asghari 

# ------------------------------------  Question 1   ------------------------------ #

#install.packages("easyPubMed")
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("extrafont")
#install.packages("plyr")

library(easyPubMed)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)


#function to get the articles and store them in a dataframe 
get_articles <- function(myQuery){
  myIdList <- get_pubmed_ids(myQuery)
  count <- as.numeric(myIdList$Count)
  if (count <= 5000){
    # Fetch data
    my_abstracts_xml <- fetch_pubmed_data(myIdList, retmax =count )
    # Store Pubmed Records as elements of a list
    all_xml <- articles_to_list(my_abstracts_xml)
    # Perform operation (use lapply here, no further parameters)
    df <- do.call(rbind, lapply(all_xml, article_to_df,max_chars =20, getAuthors = FALSE))
  } else {
    my_abstracts_xml1 <- fetch_pubmed_data(myIdList, retmax =5000, retstart = 0 )
    # Store Pubmed Records as elements of a list
    all_xml1 <- articles_to_list(my_abstracts_xml1)
    # Perform operation (use lapply here, no further parameters)
    df1 <- do.call(rbind, lapply(all_xml1, article_to_df,max_chars =20, getAuthors = FALSE))
    
    my_abstracts_xml2 <- fetch_pubmed_data(myIdList,retstart = nrow(df1), retmax =(count-5000) )
    # Store Pubmed Records as elements of a list
    all_xml2 <- articles_to_list(my_abstracts_xml2)
    # Perform operation (use lapply here, no further parameters)
    df2 <- do.call(rbind, lapply(all_xml2, article_to_df,max_chars =20, getAuthors = FALSE))
    
    df <- rbind(df1 , df2)
  }
  return(df)
}

func1 <- function(keyword){
  df <- data.frame()
  for (i in c(2015:2020)){
    query <- paste(keyword, " AND ", i, "[PDAT]", sep = "")
    df_tmp <- get_articles(query)
    df <- rbind(df ,  df_tmp)
  }
  df <- ddply(df, .(year) ,nrow)
  df_f <- aggregate(df$V1, by=list(Category=df$year), FUN=sum)
  names(df_f)[1] <- "year"
  names(df_f)[2] <- "Freq"
  df_f$group = rep(keyword,nrow(df_f))
  return(df_f)
}

#get the frequency of each year article
df_inf <- func1("Influenza")
df_Obs <- func1("Obesity")
df_can <- func1("Cancer")
df_cov <- func1("Covid-19")

library(tidyverse)
#importing 0s for years before 2017,2018

df4_f <- df_cov %>% add_row(year = c("2017","2018"), Freq = 0,  group = "Covid-19" , .before = 2)

#create one datafrae for all 4 kewords 
df <- do.call("rbind", list(df_inf[2:6,],df_Obs[2:6,],df_can[2:6,],df4_f[1:5,]))
df$year <- rep(2016:2020 , 4)

sum(df$Freq)

#plot the area chart 
p <- ggplot() + geom_area(aes(y = Freq, x = year, fill =group), data = df, stat="identity",colour="black", size=0.3, alpha=.8)
print(p + labs(title= "Frequency of Articles - 2016- 2020", y="Frequency", x = "Years"))

#install.packages("hrbrthemes")
#install.packages("viridis")

library(viridis)
library(hrbrthemes)

# another area chart
ggplot(df, aes(x=year, y=Freq, fill=group)) + geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) + theme_ipsum() + ggtitle("Frequency of Articles - 2016- 2020")


# ------------------------------------  Question 2   ------------------------------ #

# getting articles for other keywords 
df5 <- func1("Depression") 
df6 <- func1("Mental health")
df7 <- func1("Physical activity") 
df8 <- func1("Wearable")


#choosing only data for 2019 and 2020 and create a dataframe 
Y_2019 <- c(df_inf$Freq[5] ,df4_f$Freq[4], df5$Freq[5] ,df6$Freq[5] ,df7$Freq[5],df8$Freq[5])
Y_2020 <- c(df_inf$Freq[6],df4_f$Freq[5], df5$Freq[6] ,df6$Freq[6] ,df7$Freq[6],df8$Freq[6])
words <-  c("Influenza"," Covid-19"," Depression", "Mental health", "Physical activity", "Wearable")

data <- data.frame( words, Y_2019, Y_2020)
data

library(tidyverse)
library(ggalt)
library(hrbrthemes)
blue <- "#0171CE"
red <- "#DE4433"

#Dumbbell- chart 
data$words <- factor(data$words,levels=as.character(data$words))
ggplot(data, aes(x=Y_2019, xend=Y_2020, y=words))+geom_dumbbell(
  data=data, aes(y=words, x=Y_2019, xend=Y_2020),size=1.5,
  color="#b2b2b2") + 
  #add 2019 , 2020 on top of first row with the color as a legend 
  geom_text(data=filter(data, words=="Wearable"),aes(x=Y_2019, y=words, label="2019"),
            color=red, size=3, vjust=-1.5, fontface="bold") + 
  geom_text(data=filter(data, words=="Wearable"),aes(x=Y_2020, y=words, label="2020"),
            color=blue, size=3, vjust=-1.5, fontface="bold") +
  #adding the title and lables to the plot 
  ggtitle("Changes in keywords  - 2019 -> 2020") +xlab("Frequency") + ylab("Keywords") + 
  theme(plot.title = element_text( size=16, face="bold.italic"),
        axis.title.x = element_text( size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold")) + 
  # adding the values to the plot 
  geom_text( aes(x=Y_2019,label=Y_2019), color="black", size=3, vjust=2, fontface="bold")+
  geom_text(aes(x=Y_2020, label=Y_2020), color="black", size=3, vjust=2,fontface="bold")+
  geom_point(size=3, aes(x = Y_2019, color = "2019"))+
  geom_point(size=3, aes(x = Y_2020, color = "2020"))+
  scale_color_manual(name = "Year", values = c("red", "blue") )

library(plotly)
fig <- plot_ly(data, color = I("gray80"))
fig <- fig %>% add_segments(x = ~Y_2019, xend = ~Y_2020, y = ~words, yend = ~words, showlegend = FALSE)
fig <- fig %>% add_markers(x = ~Y_2019, y = ~words, name = "2019", color = I("red"))
fig <- fig %>% add_markers(x = ~Y_2020, y = ~words, name = "2020", color = I("blue"))
fig <- fig %>% layout(
  title = "Fequency of keywords 2019- 2020",
  xaxis = list(title = "Frequency"),
  margin = list(l = 65)
)

fig

# ------------------------------------  Question 3   ------------------------------ #

install.packages("mapproj")
install.packages("ggmap")
install.packages("DeducerSpatial")

require(maps)
require(ggmap)
library(ggplot2)
library(mapproj)

#importing the csv file for number of Covid cases 
getwd()
setwd("/Users/maryamasghari/Desktop/Study/Bu M.S/fall 2021/web Analytics/Assignments/Hw 2")

covid <- read.csv("covid.csv",header=T)
covid

#remove the unknown row 
covid <- covid[1:14,]
# change to lower case 
covid$polyname <- tolower(covid$polyname)
covid

# getting the county data 
data(county.fips)
head(county.fips)

fips <- county.fips

#merge two data frames 
merge(x = covid, y = fips[ , c("polyname", "fips")], by = "polyname", all.x=TRUE)

#choose buckets for color base on the number of cases 
covid$colorBuckets <- as.numeric(cut(covid$cases, c(0, 100, 500, 1000, 2000,5000, 10000)))
covid$colorBuckets

# creating a color pallet form yellow to red 
colfunc <- colorRampPalette(c("yellow", "red"))
colfunc(6)
# "#BF0000" darker red 

colors = c("#FFFF00", "#FFCC00", "#FF9900", "#FF6500" ,"#FF3200", "#BF0000")

#plot the map of counties of MA using the colors 
map('county', 'Massachusetts', col = colors[covid$colorBuckets], fill = TRUE, resolution = 0,lty = 0, projection = "polyconic")
# Add border around each State
map('county', 'Massachusetts', col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.3,projection = "polyconic")
#adding the title
title("Massachusetts Covid Cases")
#adding the legend 
leg.txt <- c("<100", "100-500", "500-1000", "1000-2000", "2000-5000",">5000")
legend("bottomleft", leg.txt, horiz = F, fill = colors)


