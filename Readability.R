library(koRpus)
setwd("~/Documents/Coursework/NYU - MS/Spring2016/Big Data/Project/Reddit Web Scraper")
data <- read.csv("MergedData.csv")
data <- data[,-1]

data$body <- as.character(data$body)


ll.tagged <- lapply(c(data[4,3]), tokenize, lang="en")

a <- tokenize(data[3,3], format = "obj", lang = "en")
readability(a)

b <- tokenize(data[27045,3], format = "obj", lang = "en")

library(tm)
corp <- Corpus(VectorSource(c(data[3,3],data[4,3])))

data[3,3]

ll.tagged <- lapply(data[1:5,2], tokenize, format = "obj", lang="en")

ll.readability <- lapply(ll.tagged,flesch)  

flesch.kincaid(a)