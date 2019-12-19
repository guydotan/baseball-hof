setwd("~/Documents/UCLA MAS/2019 Fall/Final")

hof <- read.csv("pitcher_list_v3.csv", stringsAsFactors = F, na.strings = "NULL")

library(nnet)
library(NeuralNetTools)
#library(keras)
#library(tensorflow)
library(dplyr)
library(caret)

hof$inducted <- factor(hof$inducted)
#hof$inducted <- unclass(hof$inducted)

hof <- hof
hof[is.na(hof <- hof)] <- 0
colnames(hof)[colSums(is.na(hof)) > 0]

hof_rel <- hof[hof$max_year < 2015,]
act_plyr <- hof[hof$max_year >= 2015,] #<- hof_rel[,-c(1:5,44)]

# Build your own `normalize()` function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalize the dataset
hof_norm <- as.data.frame(lapply(hof_rel[,-c(1:5,49,71)], scale))
hof_norm <- cbind(hof_norm, inducted = hof_rel[,71])
summary(hof_norm)

# normalize act players
act_plyr_norm <- as.data.frame(lapply(act_plyr[,-c(1:5,49,71)], scale))
act_plyr_norm <- cbind(act_plyr_norm, inducted = act_plyr[,71])
summary(act_plyr_norm)

#hof_norm <- hof_norm[hof_norm$max_year < 2015,]
#act_plyr_norm <- hof_norm[hof_norm$max_year >= 2015,]

# Determine sample size
ind <- sample(2, nrow(hof_norm), replace=TRUE, prob=c(0.7, 0.3))

# Split the `hof` data
hof.training <- hof_norm[ind==1, ]
hof.test <- hof_norm[ind==2, ]


set.seed(10)

neural_net  <- nnet(inducted ~ w.reg + l.reg + sv.reg + ipouts.reg + era.reg +
  h.reg + er.reg + hr.reg + bb.reg + so.reg + asg + 
  cy.young + mvp + gold.glove + post.mvp + w.post + 
  ipouts.post  + careerWAR + era_top10 + era_top5 + 
  w_top10 + w_top5 + sv_top10 + so_top10 + so_top5 +
  war_top10 + war_top5
  , data = hof.training,  size = 30, maxit = 1000, na.action = na.omit)

#neural_net  <- nnet(inducted ~  careerWAR + era.reg + mvp + war_top10 + cy.young
#                    , data = hof.training,  size = 50, na.action = na.omit)

plotnet(neural_net)

preds_nnet <- predict(neural_net, hof.test, type = "class")
table(hof.test$inducted, preds_nnet)
confusionMatrix(hof.test$inducted, factor(preds_nnet))

test_pred <- cbind(hof_rel[ind==2, c(1,2,3,60,71)], pred = preds_nnet)
View(test_pred[order(-as.numeric(test_pred$pred)),], title = 'test results') 

act_nnet <- predict(neural_net, act_plyr_norm)#, type = "class")
table(act_plyr_norm$inducted, act_nnet)
act_test_pred <- cbind(act_plyr[, c(1,2,3,60,71)], pred = round(act_nnet,5))
View(act_test_pred[order(-as.numeric(act_test_pred$pred)),], title = 'test results') 
