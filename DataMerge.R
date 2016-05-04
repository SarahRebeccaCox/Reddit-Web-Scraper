##############################################################################
# IF YOU WANT TO DELETE DATA FROM A MERGED SET, USE                          #
# data <- data[condition,]. for instance, if you want to keep all but stack, #
# use data <- data[data$source != "Stack",]                                   #
##############################################################################

#load data
reddit <- read.csv("RedditApril15Complete.csv")
stack <- read.csv("politics stack exchange.csv")
twitter <- read.csv("PoliticsTweets.csv")

###############
#DATA CLEANING#
###############

reddit <- reddit[,2:3]  #only keep text and time columns
twitter <- twitter[,2:3] #only keep text and time columns
stack <- stack[,1:2] #only keep text and time columns
twitter <- data.frame(twitter[,2],twitter[,1]) #switch column order to time,text
names <- c("time","body") #define column names
names(stack) <- names #rename stack columns
names(twitter) <- names #rename twitter columns
names(reddit) <- names #rename reddit columns

#converting twitter time to UTC
twitter[,1] <- as.character(twitter[,1]) #convert text to character
twitter[,1] <- sub("16","2016",twitter[,1]) 
twitter[,1] <- as.numeric(as.POSIXct(twitter[,1],format="%m/%d/%Y %H:%M"))

#converting stack time to UTC
stack[,1] <- as.numeric(as.POSIXct(stack[,1]))

#add Source column
reddit$source <- "Reddit"
twitter$source <- "Twitter"
stack$source <- "Stack"

#merge
data <- rbind(reddit,twitter,stack) #rbind is short for rowbind

#ensure that source is a factor var
data$source <- as.factor(data$source)

#only keep uniques
data <- unique(data)