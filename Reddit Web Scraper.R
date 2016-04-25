#library(lasso2)
#library(tm)           # Framework for text mining.
#library(SnowballC)    # Provides wordStem() for stemming.
#library(qdap)         # Quantitative discourse analysis of transcripts.
#library(qdapDictionaries)
#library(dplyr)        # Data preparation and pipes %>%.
library(rjson)
library(RJSONIO)
library(rvest, warn.conflicts=FALSE)
library(RSelenium)
#library(jsonlite)


###################################
#Getting comments through the JSON#
###################################

#the main URL to pull from
url <- 'https://www.reddit.com/r/politics/'


#open the page
wait_till_page_load<-function(page_load_time_out=60){
  t0<-Sys.time()
  while(browser$executeScript("return document.readyState;")[[1]]!="complete" & (Sys.time()-t0)<=page_load_time_out){
    Sys.sleep(0.5)
  }
  invisible(0)
}

open.page <- function(url){
  checkForServer() # check if Selenium Server package is installed and if not, install now
  startServer(invisible = FALSE, log = FALSE) # start server
  browser <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")
  
  
  browser$open()
  browser$navigate(url)
  #wait_till_page_load(500000000)
  return(browser)
}



#create thread list
create.thread.list <- function(browser){ #works from open browser
  links <- browser$findElements(using="css selector", value=".may-blank")
  urls <- rep(NA, length(links))
  
  urls <- unlist(sapply(links, function(x) x$getElementAttribute('href')))
  
  #only get the comment urls
  url.list <- urls[grep("r/politics/comments",urls)]
  
  for (i in 1:length(url.list)) {
    url.list[i] <- paste0(url.list[i],".json")
  }
  
  return(url.list)
} 



#get the "next" link to crawl later
get.next.link <- function(browser){
  links.next <- browser$findElements(using="css selector", value=".nextprev a")
  urls.next <- rep(NA, length(links.next))
  
  urls.next <- unlist(sapply(links.next, function(x) x$getElementAttribute('href')))
  next.link <- urls.next[grep("?count=25",urls.next)]
  
  return(next.link)
}


#get the "next" link to crawl later AFTER NEXT HAS BEEN CLICKED ONCE
get.next.link2 <- function(browser){
  links.next <- browser$findElements(using="css selector", value="#siteTable .separator+ a")
  urls.next <- rep(NA, length(links.next))
  
  urls.next <- unlist(sapply(links.next, function(x) x$getElementAttribute('href')))
  next.link <- urls.next[grep("?count=",urls.next)]
  
  return(next.link)
}


main.data.generator <- function(url.list,i){
  #select the URL of the thread to scrape from
  url.comment <- as.character(url.list[i]) 
  
  #convert from JSON to R-usable data
  rawdat <- rjson::fromJSON(file=url.comment)
  
  #pulling just want we want
  main.data <- rawdat[[2]]$data$children
  
  return(main.data)
  
  #main.data[[5]]$data$body #comment number 5
  #main.data[[5]]$data$author #author of comment number 5
  #information about the parent post title and url can be found in rawdat[[1]]
}



#this function puts comments and their authors into a matrix from a given main.data
comments.to.dataframe <- function(main.data){
  
  #initialize a matrix
  data.list <- matrix(c(main.data[[1]]$data$created_utc,main.data[[1]]$data$body),nrow=1)
  
  if (length(main.data) != 1){
    for (i in 2:(length(main.data)-1)){
      data.list <- rbind(data.list,c(main.data[[i]]$data$created_utc,main.data[[i]]$data$body))
    }
  }
  return(data.list)
}


automate.scraping <- function(number.of.pages,matrix){
  i <- 1
  while (i<=number.of.pages){
    url.list <- create.thread.list(browser)
    
    for (url in 1:length(url.list)){ #for every thread
      main.data <- main.data.generator(url.list,url) #create main data
      
      if (length(main.data) == 0){
        next
      }
      if (length(main.data) != 0){
        matrix <- rbind(matrix,comments.to.dataframe(main.data))
      }
    }
    
    next.link.2 <- get.next.link2(browser)
    browser$navigate(next.link.2)
    
    print(paste0(as.character(round(100*i/33)),"%"))
    i <- i+1
    
  }
  return(matrix)
}


####################
#USING THIS CRAWLER#
####################

#Steps for when all the functions are defined. 

#STEP 1: PUT IN THE URL AND OPEN THE BROWSER
#the main URL to pull from
url <- 'https://www.reddit.com/r/politics/'
browser <- open.page(url)

#STEP 2: GET URLS TO THREADS ON PAGE
url.list <- create.thread.list(browser)


#STEP 3: GET LINK TO CLICK TO NEXT PAGE
next.link <- get.next.link(browser)

#STEP 4: INITIALIZE DATA MATRIX
data.matrix <- matrix(c("a","b"),nrow=1)

#STEP 5: GATHER COMMENT DATA FROM EACH THREAD
for (url in 1:length(url.list)){ #for every thread
  main.data <- main.data.generator(url.list,url) #create main data
  
  if (length(main.data) == 0){
    next
  }
  else{
    data.matrix <- rbind(data.matrix,comments.to.dataframe(main.data))
  }
}


#STEP 6: NAVIGATE TO THE NEXT PAGE
browser$navigate(next.link) 

#STEP 7: COLLECT COMMENT DATA ON NEW PAGE (by repeating steps 4+5)
url.list <- create.thread.list(browser)

for (url in 1:length(url.list)){ #for every thread
  main.data <- main.data.generator(url.list,url) #create main data
  
  if (length(main.data) == 0){
    next
  }
  else{
    data.matrix <- rbind(data.matrix,comments.to.dataframe(main.data))
  }
}

#STEP 8: GO TO THE NEXT PAGE USING THE SECOND "NEXTLINK" FUNCTION AND GO TO NEXT PAGE
next.link.2 <- get.next.link2(browser)
browser$navigate(next.link.2)


#STEP 9: REPEAT STEPS 7 AND 8 FOR AS LONG AS YOU LIKE

## number.of.pages is the number of additional pages you would like to scroll through and take data from.
## for example, automate.scraping(3) will repeat steps 7 and 8 3 times.
data.matrix <- automate.scraping(33,data.matrix)


#STEP 10: CONVERT YOUR DATA INTO A DATAFRAME
reddit.data <- as.data.frame(data.matrix)
reddit.data <- reddit.data[-1,]
names(reddit.data) <- c("UTC","Body")

#STEP 11: SAVE TO CSV
write.csv(reddit.data,"RedditDataApr25.csv")



#DON'T USE BELOW HERE


###############################################
#ATTEMPT TO GET ALL COMMENTS VIA SELECTOR TOOL#
###############################################

#this method returns more comments (and subcomments!)
#but we can't easily map authors to comments
#and we have to clean out html

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


#this section will eventually be able to click the "load more comments" button and continue scraping, hopefully. Ignore for now.


##########################################


