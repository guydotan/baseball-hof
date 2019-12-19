setwd("~/Documents/UCLA MAS/2019 Fall/Final")

library(ggplot2)
library(ggrepel)
library(dplyr)

hof <- read.csv("CSVS/pitcher_list_v3.csv", stringsAsFactors = F, na.strings = "NULL")
names(hof)

hof <- hof[!hof$player_id == 'griffcl01',]

hof$inducted <- factor(hof$inducted, levels = c("Y", "N"))

hof[is.na(hof <- hof)] <- 0
colnames(hof)[colSums(is.na(hof)) > 0]

#m2 <- hof[,c(1,2,3,4,5,60,65:71)]

ggplot(hof, aes(x=careerWAR, y=w.reg, color=inducted, 
                       label=ifelse(careerWAR >= 100 | (careerWAR < 30 & inducted == "Y"), 
                                    paste(hof$name_first, hof$name_last) ,""))) +
  xlab("WAR") +
  ylab("Wins") +
  ggtitle("HOFers by Career WAR vs. Regular Season Wins") +
  geom_text_repel(position=position_jitter(width = 0.5), show.legend=F, size = 6) +
  scale_color_manual(values = c("#7CAE00", "#F8766D")) + 
  geom_point(size = 3, alpha = 0.65) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16),
        plot.title=element_text(size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16))


hof$awards <- hof$cy.young+hof$mvp+hof$post.mvp+hof$gold.glove+hof$asg

ggplot(data = hof, aes(x=era.reg, y=so.reg, color=inducted, 
                       label=ifelse( so.reg > 3500 | (so.reg < 500 & inducted == "Y"), 
                                    paste(hof$name_first, hof$name_last) ,""))) +
  xlab("ERA") +
  ylab("Strikeouts") +
  ggtitle("HOFers by Career Strikeouts vs. ERA") +
  geom_text_repel(show.legend = F, size = 6) +
  scale_color_manual(values = c("#7CAE00", "#F8766D")) + 
  geom_point(size = 3, alpha = 0.65) + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16),
        plot.title=element_text(size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16))


d<-hof

d_hof = d[d$inducted == 'Y', ]
d_hof[order(d_hof$asg), c('name_first', 'name_last', 'careerWAR', 'asg')]
d_not = d[d$inducted == 'N', ]
d_not[order(d_not$asg, decreasing=T), c('name_first', 'name_last', 'careerWAR', 'asg')]

# plot(hof$careerWAR, 
#      hof$w.reg, 
#      pch=21, bg=c("red","green3","blue")[unclass(hof$inducted)], 
#      xlab="WAR", 
#      ylab="Wins")


library(nnet)
library(NeuralNetTools)

hof$inducted <- as.factor(hof$inducted)


train_df <- hof[hof$max_year < 2015,]
test_df <-  hof[hof$max_year >= 2015,]

set.seed(1)
neural_net  <- nnet(inducted ~  careerWAR + era.reg + mvp + war_top10 + cy.young
                , data = train_df,  size = 7, na.action = na.omit)

plotnet(neural_net)

preds_nnet <- predict(neural_net, test_df, type = "class")
table(test_df$inducted, preds_nnet)
act_pred <- cbind(test_df[,c(1,2,3,60,71)], pred= preds_nnet)
View(act_pred[order(-as.numeric(act_pred$pred)),])
