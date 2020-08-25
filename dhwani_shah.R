#packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)

#working directory
getwd()
setwd("D:/Dhwani R Programming and other languages/Rohit R programming class/Final assignment/YouTube")

#import df
asia <- read.csv("ASIA_trending.csv")
europe <- read.csv("EU_trending.csv")
america<- read.csv("NA_trending.csv")
videos<- read.csv("videos.csv")

#EDA for Asia
head(asia, n=3)
summary(asia)
str(asia)

#Data Cleaning for Asia
asia[, c(1,4,6,10)] <- lapply(asia[, c(1,4,6,10)], as.factor)
asia$comments_disabled <- as.logical(asia$comments_disabled)


#EDA for Europe
head(europe, n=3)
summary(europe)
str(europe)


#Data Cleaning for Europe
europe[, c(1,2,4,6,10)] <- lapply(europe[, c(1,2,4,6,10)], as.factor)

#EDA for America
head(america, n=3)
summary(america)
str(america)

#Data Cleaning for America
summary(america)
america[, c(1,2,4,6,10)] <- lapply(america[, c(1,2,4,6,10)], as.factor)



#bind America, Europe and Asia
countries <- bind_rows(america,europe,asia)
head(countries)
dim(countries)
summary(countries)


#change trending date to ymd format
countries$trending_date <- ydm(countries$trending_date)
str(countries)

#separating the likes and dislikes column for simplicity
countries<- countries %>%
  separate(likes.dislikes,c("likes", "dislikes"))


countries$likes <- as.numeric(countries$likes)
countries$dislikes <- as.numeric(countries$dislikes)


#replacing NA
countries <- countries %>%
  mutate(views = replace_na(views,replace = median(views, na.rm = TRUE)),
         comment_count = replace_na(comment_count,replace = median(comment_count, na.rm = TRUE)))

#EDA for videos
head(videos, n=3)
str(videos)
summary(videos)

videos$category_id <- as.factor(videos$category_id)
videos[, c(1,2,3)] <- lapply(videos[, c(1,2,3)], as.factor)



#change publish time to ymd_hms format

videos$publish_time <- ymd_hms(videos$publish_time)

#separate the data frame to get hour, minute, second separately
videos<- videos %>%
  separate(publish_time,c("Year", "Month", "Day", "Hour" , "Minute" , "Second"))

#Joining it again to get the publish date
videos <- unite(videos, col = "Publish_date" , c("Year" , "Month", "Day"), sep="-")

videos$Publish_date <- as.Date(videos$Publish_date)




#left join countries and videos
youtube<- countries %>%
  left_join(videos, by = "video_id")

#EDA for youtube
head(youtube)
dim(youtube)
summary(youtube)
str(youtube)

View(youtube)

#Final data cleaning
youtube$country <- str_trim(youtube$country, side = "both")
youtube$title <- str_trim(youtube$title, side = "both")
youtube$channel_title <- str_trim(youtube$channel_title, side = "both")










#QUESTIONS AND INTERPRETATION




#1.On an average, for how many days does one video trend in each country?

mean_days <- youtube %>% 
  group_by(title, country) %>%
  summarise(n = length(trending_date)) %>%
  arrange(desc(n)) %>% 
  group_by(country) %>% 
  summarise(days = mean(n))



ggplot(mean_days,aes(x = country,y = days)) + 
  geom_bar(aes(fill =days),stat = "identity",position = "dodge" , col="red")


#Result1: On an average, videos trend for more number of days in the UK compared to other countries.   




#2.Most videos are uploaded at what hour of the day?



peak_hour <- youtube %>%
  group_by(video_id) %>%
  summarise(n = max(Hour)) %>% 
  arrange(desc(n))



peak_hour %>% 
  ggplot(aes(x = n)) +
  geom_bar(fill="steelblue")+
  ggtitle("Category_id chart") +
  xlab("Hour") +
  ylab("Videos") 



#Result2: MOst videos are uploaded in the afternoon between 3pm to 5pm. This may be because most viewers are school going children or younger workforce, who get free after 3pm, and hence most views are fetched at 4pm.  






#3.Which channel title's videos have trended the longest on an average in a particular country?

youtube %>% 
     group_by(video_id, channel_title, country) %>% 
     summarise(n = length(unique(trending_date))) %>% 
     group_by(channel_title, country) %>%
     summarise(m = sum(n), k =mean(n), h =n()) %>% 
     arrange(desc(m))




#Result3: The Tonight Show Starring Jimmy Fallon videos are trending the longest in the UK, altogether. This can be used to stream ads in a country (by showing ads of channels which are trending in that country). This will help youtube make money through ads.





#4. Which types of videos get most number of views on an average?  (finding the category_id)


average_views <-youtube %>% 
  group_by(title, category_id) %>% 
  summarise(n = max(views)) %>% 
  group_by(category_id) %>% 
  summarise(views = mean(n)) %>%
  arrange(desc(views))

average_views

ggplot(average_views,aes(x = category_id,y = views)) + 
  geom_bar(aes(fill = views),stat = "identity",position = "dodge" , col="yellow")


#Result4: 10 (Music) has the most number of views on average compared to any other category_id. This means that music videos caters to a bigger audience compared to other videos.





#5.Which video has trended for the most number of days? 


youtube %>% 
  group_by(video_id, title, country) %>% 
  summarise(n=length(unique(trending_date))) %>%
  group_by(country) %>% 
  arrange(desc(n))
  


#Result5. Justin Timberlakeâ???Ts FULL Pepsi Super Bowl and To Our Daughter have trended for the most number of days in the UK.



#6.Which type of videos have disabled their comments?


youtube %>% 
  filter(comments_disabled == "TRUE") %>% 
  group_by(category_id) %>% 
  summarise(n()) %>% 
  arrange(category_id)


#6. 24 (Entertainment), followed by 25 (News and Politics) has the most number of comments disabled. 24 (News) having comments disabled is quite likely because of sensitive content, but 24(Enitertainment) is surprising.
  


  


#7.Does the number of views affect the number of days it takes for a video to get trending?


youtube$Publish_date <- as.Date(youtube$Publish_date)

youtube <-youtube %>% 
  mutate(diff = trending_date - Publish_date)


youtube %>%
  group_by(title, channel_title) %>% 
  summarise(c= min(diff), d= min(views)) %>% 
  arrange(d)



#Result7: Number of views does not make the video go trending, because even at 223 views, "Laurent Baffie - Best of des vannes is trending.
    

#8. Do the number of likes impact the number of days taken for a video to get trending?

youtube %>%
  group_by(title, channel_title) %>% 
  summarise(c= min(diff), d= min(likes)) %>% 
  arrange(d)

#Result8. Even with 0 likes, some of the videos have trended in the 1st or the 2nd day itself. Which means that number of likes does not decide which video is trending or not.



#9.Which video genres have trended the most. (finding the category_id)

df <- youtube %>% 
  group_by(category_id) %>% 
  summarise(n=length(unique(channel_title))) %>% 
  arrange(desc(n)) 

df

youtube %>% 
  ggplot(aes(x = category_id)) +
  geom_bar(fill="steelblue")+
  ggtitle("Category_id chart") +
  xlab("Category_id") +
  ylab("Videos") 

#Result9: 24(entertainment), 22(people and blogs) and 10(music) are the top 3 trending video types. (This will help the youtube content creators to know which type of videos the viewers typically like)


#10.To check which type of video, with category ID: 10, 22 and 44 is trending in which country


youtube %>% 
  filter(category_id %in% c("10", "22", "24", "25")) %>% 
  ggplot(aes(x= country, fill = category_id)) +
  geom_bar(position="fill")+
  ggtitle("Proportion bar chart: country") +
  xlab("Country") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent)


#10Result: Music videos (10) are more trending in the UK compared to Blogs and entertainment, whereas in other countries, entertainment (24) videos are more trending. This can help in knowing your target audience for a particular type of video



#Does the publish date affect the time taken for a video to get trending?


#11. Which type of videos have been deleted the most?

deleted_videos <- youtube %>% 
  filter(title == "Deleted video")

deleted_videos %>% 
  group_by(category_id) %>% 
  summarise(n=length(unique(Publish_date))) %>% 
  arrange(desc(n))

#11Result: Most number of deleted videos are in category ID 24(Entertainment)



