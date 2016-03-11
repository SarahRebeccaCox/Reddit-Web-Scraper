#library(lasso2)
#library(tm)           # Framework for text mining.
#library(SnowballC)    # Provides wordStem() for stemming.
#library(qdap)         # Quantitative discourse analysis of transcripts.
#library(qdapDictionaries)
#library(dplyr)        # Data preparation and pipes %>%.
library(rjson)
library(rvest, warn.conflicts=FALSE)


url <- 'https://www.reddit.com/r/politics/'

library(RSelenium)
checkForServer() # check if Selenium Server package is installed and if not, install now
startServer() # start server
browser <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")


browser$open()
browser$navigate(url)


links <- browser$findElements(using="css selector", value=".may-blank")
urls <- rep(NA, length(links))
for (i in 1:length(links)){
  urls[i] <- unlist(links[[i]]$getElementAttribute('href'))
}
# alternatively, we can also do
urls <- unlist(sapply(links, function(x) x$getElementAttribute('href')))

#only get the comment urls
url.list <- urls[grep("r/politics/comments",urls)]





###############################################
#ATTEMPT TO GET ALL COMMENTS VIA SELECTOR TOOL#
###############################################


browser$navigate(url.list[5])
comments <- browser$findElements(using="css selector", value=".md p")


comment.url <- url.list[5]
comments <- read_html(comment.url) # reading the HTML code
text <- html_nodes(comments, ".md") # identify the CSS selector
textauth <- html_nodes(comments, ".may-blank")
text # content of CSS selector
text2 <- as.character(text)
textauth2 <- as.character(textauth)
#this method will require cleaning out HTML, but I think it's faster

##########################################


#this section will eventually be able to click the "load more comments" button and continue scraping. Ignore for now.


##########################################


###################################
#Getting comments through the JSON#
###################################

#works for one page of comments for now. in the future, will loop through all threads

#convert all comment urls to json
url.to.json <- function(url.list){
  for (i in 1:length(url.list)) {
    url.list[i] <- paste0(url.list[i],".json")
  }
  return(url.list)
}



main.data.generator <- function(url.list,i){
  #select the URL of the thread to scrape from
  url.comment <- as.character(url.list[i])
  
  #convert from JSON to R-usable data
  rawdat <- fromJSON(file=url.comment)
  
  #pulling just want we want
  main.data <- rawdat[[2]]$data$children
  
  return(main.data)
  
  #main.data[[5]]$data$body #comment number 5
  #main.data[[5]]$data$author #author of comment number 5
  #information about the parent post title and url can be found in rawdat[[1]]
}



#add data


#this function puts comments and their authors into a dataframe from a given main.data

comments.to.dataframe <- function(main.data){
  
  #initialize a matrix
  data.list <- matrix(c(main.data[[1]]$data$body,main.data[[1]]$data$author),nrow=1)
  
  #add the other rows
  for (i in 2:(length(main.data)-1)){
    data.list <- rbind(data.list,c(main.data[[i]]$data$body,main.data[[i]]$data$author))
  }

  return(data.list)
}





#let's try something
data.matrix <- matrix(c("a","b"),nrow=1)
for (url in 1:length(url.list)){ #for every thread
  main.data <- main.data.generator(url.list,url) #create main data
  
  if (length(main.data) == 0){
    next
  }
  else{
    data.matrix <- rbind(data.matrix,comments.to.dataframe(main.data))
  }
}

reddit.data <- as.data.frame(data.matrix)
reddit.data <- reddit.data[-1,]
names(reddit.data) <- c("Body","Author")