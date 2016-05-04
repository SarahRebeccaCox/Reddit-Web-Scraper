library(koRpus)
setwd("~/Documents/Coursework/NYU - MS/Spring2016/Big Data/Project/Reddit Web Scraper")
data <- read.csv("MergedData.csv")
data <- data[,-1]
data <- data[,-7217] #check dim before running, last digit should be 9 if this row has already been removed


data$body <- as.character(data$body)


#tokenizing so that koRpus will accept the object
ll.tagged <- lapply(data$body[1:nrow(data)], tokenize, format = "obj", lang="en")

#ll.tagged.2 <- lapply(data$body[1:5], tokenize, format = "obj", lang="en")






newdat <- data.frame(matrix(0,0,9))
for (i in 1:length(ll.tagged)){
	newdat <- rbind(newdat,textFeatures(ll.tagged[[i]]))
}

newdat2 <- cbind(data,newdat)



newdat2$Grade <- 0
######Batch processing, because I can't find exactly the row where it gets an error and it takes awhile to run.
ll.readability.1 <- lapply(ll.tagged[1:1000],flesch)


for (i in 1:1000){
		newdat2$Grade[i] <-slot(ll.readability.1[[i]],"Flesch")$grade.min
}

ll.readability.2 <- lapply(ll.tagged[1001:2000],flesch)

for (i in 1001:2000){
		newdat2$Grade[i] <-slot(ll.readability.2[[i-1000]],"Flesch")$grade.min
}

ll.readability.3 <- lapply(ll.tagged[2001:3000],flesch)

for (i in 2001:3000){
		newdat2$Grade[i] <-slot(ll.readability.3[[i-2000]],"Flesch")$grade.min
}

ll.readability.4 <- lapply(ll.tagged[3001:4000],flesch)

for (i in 3001:4000){
		newdat2$Grade[i] <-slot(ll.readability.4[[i-3000]],"Flesch")$grade.min
}

ll.readability.5 <- lapply(ll.tagged[4001:5000],flesch)

for (i in 4001:5000){
		newdat2$Grade[i] <-slot(ll.readability.5[[i-4000]],"Flesch")$grade.min
}

ll.readability.6 <- lapply(ll.tagged[5001:6000],flesch)

for (i in 5001:6000){
		newdat2$Grade[i] <-slot(ll.readability.6[[i-5000]],"Flesch")$grade.min
}

ll.readability.7 <- lapply(ll.tagged[6001:7000],flesch)

for (i in 6001:7000){
		newdat2$Grade[i] <-slot(ll.readability.7[[i-6000]],"Flesch")$grade.min
}

ll.readability.8 <- lapply(ll.tagged[7001:8000],flesch)

for (i in 7001:8000){
		newdat2$Grade[i] <-slot(ll.readability.8[[i-7000]],"Flesch")$grade.min
}

ll.readability.9 <- lapply(ll.tagged[8001:9000],flesch)

for (i in 8001:9000){
		newdat2$Grade[i] <-slot(ll.readability.9[[i-8000]],"Flesch")$grade.min
}


ll.readability.10 <- lapply(ll.tagged[9001:10000],flesch)

for (i in 9001:10000){
		newdat2$Grade[i] <-slot(ll.readability.10[[i-9000]],"Flesch")$grade.min
}


ll.readability.11 <- lapply(ll.tagged[10001:11000],flesch)

for (i in 10001:11000){
		newdat2$Grade[i] <-slot(ll.readability.11[[i-10000]],"Flesch")$grade.min
}

ll.readability.12 <- lapply(ll.tagged[11001:12000],flesch)

for (i in 11001:12000){
		newdat2$Grade[i] <-slot(ll.readability.12[[i-11000]],"Flesch")$grade.min
}


ll.readability.13 <- lapply(ll.tagged[12001:13000],flesch)

for (i in 12001:13000){
		newdat2$Grade[i] <-slot(ll.readability.13[[i-12000]],"Flesch")$grade.min
}

ll.readability.14 <- lapply(ll.tagged[13001:14000],flesch)

for (i in 13001:14000){
		newdat2$Grade[i] <-slot(ll.readability.14[[i-13000]],"Flesch")$grade.min
}


ll.readability.15 <- lapply(ll.tagged[14001:15000],flesch)

for (i in 14001:15000){
		newdat2$Grade[i] <-slot(ll.readability.15[[i-14000]],"Flesch")$grade.min
}

ll.readability.16 <- lapply(ll.tagged[15001:16000],flesch)

for (i in 15001:16000){
		newdat2$Grade[i] <-slot(ll.readability.16[[i-15000]],"Flesch")$grade.min
}


ll.readability.17 <- lapply(ll.tagged[16001:17000],flesch)

for (i in 16001:17000){
		newdat2$Grade[i] <-slot(ll.readability.17[[i-16000]],"Flesch")$grade.min
}

ll.readability.18 <- lapply(ll.tagged[17001:17909],flesch)

for (i in 17001:17909){
		newdat2$Grade[i] <-slot(ll.readability.18[[i-17000]],"Flesch")$grade.min
}

write.csv(newdat2,"ReadabilityVersion.csv")

