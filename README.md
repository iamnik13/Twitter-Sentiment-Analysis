# Twitter-Sentiment-Analysis
---
title: "Twitter Sentiment Analysis"
author: "Nikhil Parihar"
date: "November 11, 2017"
output:
  html_document: default
  html_notebook: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```


```{r , echo = TRUE}
options(jupyter.plot_mimetypes = "image/png")

#The dataset contains 14640 tweets and 15 variables (columns).

data = read.csv('C:/Users/Lenovo/Downloads/Tweets.csv')
dim(data)
str(data)

                    #Proportion of tweets with each sentiment
prop.table(table(data$airline_sentiment)) #We see that most of the tweets contain negative sendiment, as labelled by the curators of the dataset.

```


```{r}
# generate a dataframe for plotting in ggplot2
smallData = as.data.frame(prop.table(table(data$airline_sentiment)))
colnames(smallData) = c('Sentiment', 'Frequency')
smallData

                     #using ggplot and gridExtra library for visualization

library(ggplot2)
library(gridExtra)
```


```{r}
# create blank theme for pie chart, otherwise it looks awful in my opinion
blank_theme = theme_minimal() + theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(size = 14, face = 'bold') )

gbar = ggplot(smallData, aes(x = Sentiment, y = Frequency, fill = Sentiment))
gpie = ggplot(smallData, aes(x = "", y = Frequency, fill = Sentiment))

plot1 = gbar + geom_bar(stat = 'identity') + ggtitle("Overall Sentiment") + 
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1),
        axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -1))

plot2 = gpie + geom_bar(stat = 'identity') + coord_polar("y", start = 0) + blank_theme +
  theme(axis.title.x = element_blank()) + geom_text(aes(y = Frequency/3 + c(0, cumsum(Frequency)[-length(Frequency)]),
                                                        label = round(Frequency, 2)), size = 4) + ggtitle('Overall Sentiment')

grid.arrange(plot1, plot2, ncol = 1, nrow = 2)

```

```{r}
#Proportion of tweets per airline
prop.table(table(data$airline))

# dataframe for plotting in ggplot
smallData = as.data.frame(prop.table(table(data$airline)))
colnames(smallData) = c('airline', 'Frequency')
smallData

gbar = ggplot(smallData, aes(x = airline, y = Frequency, fill = airline))
gbar + geom_bar(stat = 'identity') + scale_fill_brewer() + 
  ggtitle('Percentage of Tweets per Airline') +
  guides(fill = FALSE) + 
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1))
#Most of the tweets are directed towards United Airlines, followed by American and US Airways. Very few tweets are targeted towards Virgin America.

```

```{r}
#Proportion of negative sentiment tweets per airline
prop.table(table(data$airline_sentiment, data$airline))
```

```{r}
# dataframe for ggplot
smallData = as.data.frame(prop.table(table(data$airline_sentiment, data$airline)))
colnames(smallData) = c('Sentiment', 'Airline', 'Percentage_Tweets')

gbar = ggplot(smallData, aes(x = Airline, y = Percentage_Tweets, fill = Sentiment)) + ggtitle('Proportion of Tweets per Airline') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_text(vjust = -1))

plot1 = gbar + geom_bar(stat = 'identity')
plot2 = gbar + geom_bar(stat = 'identity', position = 'fill')

grid.arrange(plot1, plot2, ncol = 1, nrow = 2)
```


```{r}
#Reasons for negative sentiment tweets
# dataframe for ggplot
smallData = as.data.frame(prop.table(table(data$negativereason)))
colnames(smallData) = c('Reason', 'Frequency')
smallData = smallData[-1, ] # remove first raw as it has no reason specified
smallData

g = ggplot(smallData, aes(x = Reason, y = Frequency)) + geom_bar(stat = 'identity', fill = 'red')
g = g + ggtitle('Reasons for Negative Sentiment')
g = g + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_text(vjust = -0.1),
              axis.text.x = element_text(angle = 30, size = 10, vjust = 1))
```

```{r}
#Exploratory data analysis: columns containing NAs (no data)
data = as.data.frame(apply(data, 2, function(x) gsub("^$|^ $", NA, x)))
apply(data, 2, function(x) sum(is.na(x)))

```{r}
#Retweet Analysis
table(data$retweet_count)

as.character(subset(data, retweet_count ==44)$text);
print(" ")
as.character(subset(data, retweet_count ==32)$text);
print(" ")
as.character(subset(data, retweet_count ==31)$text);
print(" ")
as.character(subset(data, retweet_count ==28)$text)
```
```{r}
#Tweet Location Analysis
head(unique(data$tweet_location), 50)

timezone = as.data.frame(prop.table(table(data$user_timezone)))
colnames(timezone) = c('timezone', 'Frequency')
timezone = timezone[order(timezone$Frequency, decreasing = TRUE),]
dim(timezone)
head(timezone, 10)
```

```{r}
#Location of Tweets and its Visualization

location = data$tweet_coord
location = location[complete.cases(location)] # remove NAs
location = as.data.frame(location)
location$count =  1 # add a count column filled with 1s
location$location = as.character(location$location)
#remove duplicate locations and count the times they appeared, write the count in the count column
location = aggregate(count~location, data = location, FUN = sum)
location = location[-5,] # removes row containing coords [0,0] which are probably wrong
coords = strsplit(location$location, ',') 
```

```{r}
# separate lat and long from location
lat = NULL
long = NULL
for (i in 1:length(coords)) {
  lat = c(lat, substring(coords[[i]][1], 2)) # removes first character which is [
  long = c(long, coords[[i]][2]) 
}

location$lat = lat
location$long = long
```

```{r}
# remove 
location$long = substr(location$long, 1, nchar(location$long)-1)

location$lat = as.numeric(location$lat)
location$long = as.numeric(location$long)

head(location)
print('')
dim(location)
```

```{r}
require(maps)
world_map <- map_data("world")
g1 = ggplot()
g1 = g1 + geom_polygon(data=world_map, aes(x=long, y=lat, group = group), colour="black", fill = 'lightblue') + 
  ggtitle("Location of tweets across the World")
g1 = g1 + geom_point(data=location, aes(x=long, y=lat, size = count), color="coral1") + scale_size(name="Total Tweets")
g1 = g1 + ylim(-50, 80)
```

```{r}
states <- map_data("state")
g2 = ggplot()
g2 =g2 + geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="black", fill = 'lightblue') + 
  ggtitle("Location of tweets across the States")
g2 = g2 + geom_point(data=location, aes(x=long, y=lat, size = count), color="coral1") + scale_size(name="Total Tweets")
g2 = g2 + xlim(-125, -65) + ylim(25, 50)
```

```{r}

grid.arrange(g1, g2, ncol=1, nrow = 2)
```
