#d1 <- read.csv("RedditDataApr12.csv")
#d2 <- read.csv("RedditDataApr13.csv")
#d3 <- read.csv("RedditDataApr14.csv")
#d4 <- read.csv("RedditDataApr142.csv")
#d5 <- read.csv("RedditDataApr143.csv")
#d6 <- read.csv("RedditDataApr15.csv")
#d7 <- read.csv("RedditDataApr152.csv")
#d8 <- read.csv("RedditDataApr153.csv")
#d9 <- read.csv("RedditDataApr144.csv")
#d10 <- read.csv("RedditDataApr16.csv")
#d11 <- read.csv("RedditDataApr17.csv")
#d12 <- read.csv("RedditDataApr172.csv")
#d13 <- read.csv("RedditDataApr18.csv")

#reddit <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13)

#reddit <- reddit[,-1]
#reddit <- unique(reddit)
#reddit[,3] <- "Reddit"



d6 <- read.csv("RedditDataApr15.csv")
d7 <- read.csv("RedditDataApr152.csv")
d8 <- read.csv("RedditDataApr153.csv")


data <- rbind(d6,d7,d8)
data <- data[,-1]
write.csv(data,"RedditApril15Complete.csv")


stack <- read.csv("politics stack exchange.csv")
twitter <- read.csv("PoliticsTweets.csv")

###############
#DATA CLEANING#
###############

twitter <- twitter[,2:3]
stack <- stack[,1:2]
twitter <- data.frame(twitter[,2],twitter[,1])
names <- c("time","body")
names(stack) <- names
names(twitter) <- names
reddit <- data
names(reddit) <- names

twitter[,1] <- as.character(twitter[,1])
twitter[,1] <- sub("16","2016",twitter[,1])
twitter[,1] <- as.numeric(as.POSIXct(twitter[,1],format="%m/%d/%Y %H:%M"))
stack[,1] <- as.numeric(as.POSIXct(stack[,1]))

reddit$source <- "Reddit"
twitter$source <- "Twitter"
stack$source <- "Stack"


data <- rbind(reddit,twitter,stack)
data$source <- as.factor(data$source)
