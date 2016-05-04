dat <- read.csv("FinalClean2.csv")
train <- read.csv("Training.csv")
test <- read.csv("Testing.csv")
train <- train[,-1]
test <- test[,-1]
dat <- dat[,-1]
colnames(dat)

#syllCt, Flesch.Kincaid, Fucks, charCt, lttrCt, flesch, Linsear.Write are excluded


#cor_relation <- cor(dat[,-c(1:4)])
#cor_relation[abs(cor_relation) < 0.8] <- NA

#for (i in 1:ncol(cor_relation)){
#  print(c(i+4,35-sum(is.na(cor_relation)[,i])))
#}

# Decision tree - not great

##########################################################
library(party)

formula <- source ~ uniqWd + complx + sntCt + sntLen +
  FOG + ARI + Coleman.Liau +
  Farr.Jenkins.Paterson + Flesch + FORCAST + 
  LIX + RIX + SMOG +  Wheeler.Smith + 
  WordCount + MisspelledWords + afinn + bing + nrc + anger + 
  anticipation + disgust + fear + joy + sadness + surprise + trust +
  negative + positive

tree <- ctree(formula, data=train)

plot(tree)

table(predict(tree),train$source)
table(predict(tree))
table(train$source)
##################################################################

# multinomial regression, because we have continuous predictors
library(nnet)
multi <- multinom(formula, data=train)

sum(predict(multi,newdata=test)==test$source)/nrow(test) #72%, pretty good!
summary(multi)

z <- summary(multi)$coefficients/summary(multi)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
