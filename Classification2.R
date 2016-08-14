dat <- read.csv("FinalClean2.csv")

dat2 <- read.csv("commentNoHashAvg.csv")
#train <- read.csv("Training.csv")
#test <- read.csv("Testing.csv")

set.seed(109)
training.n <- sample(nrow(dat2),0.6*nrow(dat))
training <- dat2[training.n,]
testing <- dat2[-training.n,]

#training <- training[,-1]
#testing <- testing[,-1]
#dat <- dat[,-1]
#colnames(dat)

#syllCt, Flesch.Kincaid, Fucks, charCt, lttrCt, flesch, Linsear.Write are excluded


#cor_relation <- cor(dat[,-c(1:4)])
#cor_relation[abs(cor_relation) < 0.8] <- NA

#for (i in 1:ncol(cor_relation)){
#  print(c(i+4,35-sum(is.na(cor_relation)[,i])))
#}

# Decision tree - not great

##########################################################
#library(party)

formula <- Reddit ~ .

#tree <- ctree(formula, data=train)

#plot(tree)

#table(predict(tree),train$source)
#table(predict(tree))
#table(train$source)
##################################################################

# multinomial regression, because we have continuous predictors
library(nnet)
multi <- multinom(formula, data=training)

sum(predict(multi,newdata=testing)==testing$Reddit)/nrow(testing) #72%, pretty good!

reddit.test <- testing[testing$Reddit=="Reddit",]
twitter.test <- testing[testing$Reddit=="Twitter",]
stack.test <- testing[testing$Reddit=="Stack",]
sum(predict(multi,newdata=reddit.test)==reddit.test$Reddit, na.rm=TRUE)/nrow(reddit.test) #55.7%, pretty good!
sum(predict(multi,newdata=twitter.test)==twitter.test$Reddit, na.rm=TRUE)/nrow(twitter.test) #87.9%, pretty good!
sum(predict(multi,newdata=stack.test)==stack.test$Reddit, na.rm=TRUE)/nrow(stack.test) #2%, eh


summary(multi)

z <- summary(multi)$coefficients/summary(multi)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2



