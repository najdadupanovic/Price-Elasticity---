library(bayesm)
data(tuna)
tuna$MOVE1


data.frame(colnames(tuna))
logmove1=log(tuna$MOVE1)
logmove2=log(tuna$MOVE2)
logmove3=log(tuna$MOVE3)
logmove4=log(tuna$MOVE4)
logmove5=log(tuna$MOVE5)
logmove6=log(tuna$MOVE6)
logmove7=log(tuna$MOVE7)
tuna
cbind(tuna, logmove1, logmove2, logmove3, logmove4, logmove5, logmove6, logmove7)

#EDA
library(corrplot)
tunacor <- cor(tuna)
tunacor
corrplot(tunacor, method="circle")

#Histograms

par(mfrow=c(3,4))
for (i in 1:(length(tuna))){
  hist(x = tuna[[i]], 
       main = sprintf('Histogram of the variable: %s',
                      colnames(tuna[i])), 
       xlab = colnames(tuna[i]))
}


tuna$totalsales <- tuna$MOVE1+tuna$MOVE2+tuna$MOVE3+tuna$MOVE4+tuna$MOVE5+tuna$MOVE6+tuna$MOVE7
tuna
data.frame(colnames(tuna))
#matrix required to perform geometric means
require(matrixStats)
tunamat <- as.matrix((tuna)[2:8])
#calculating geometric mean
tunamat2 <- exp(rowMeans(log(tunamat)))
tunamat2 <- as.vector(tunamat2)
tunamarketshare <- cbind(tuna,tunamat2)
colnames(tunamarketshare)
#market share elasticities for all the brands
# for the first brand
ms1 = tunamarketshare[, c("WEEK", "MOVE1", "LPRICE1", "LPRICE2", "LPRICE3", "LPRICE4",
                          "LPRICE5", "LPRICE6", "LPRICE7", "tunamat2")]

ms1$logratio <- log(tunamarketshare$MOVE1/tunamarketshare$tunamat2)


lmmarket1 <- lm(ms1$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms1)
lmmarket1
ms1
out = with(ms1, by(ms1, WEEK, function(x) lm(ms1$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms1)))
out
outsum=lapply(out,summary)
coefmat=sapply(outsum,coef)
summary(out)
out$results
coefmat[[2]]
ci=matrix(nrow=length(out), ncol=8)
ci[,1]=coefmat[1]
ci[,2]=coefmat[2]
ci[,3]=coefmat[3]
ci[,4]=coefmat[4]
ci[,5]=coefmat[5]
ci[,6]=coefmat[6]
ci[,7]=coefmat[7]
ci[,8]=coefmat[8]
ci

#for the 2nd brand

ms2 = tunamarketshare[, c("WEEK", "MOVE2", "LPRICE1", "LPRICE2", "LPRICE3", "LPRICE4",
                          "LPRICE5", "LPRICE6", "LPRICE7", "tunamat2")]

ms2$logratio <- log(tunamarketshare$MOVE2/tunamarketshare$tunamat2)


lmmarket2 <- lm(ms2$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms2)
lmmarket2
ms2
out = with(ms2, by(ms2, WEEK, function(x) lm(ms2$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms2)))
out
outsum=lapply(out,summary)
coefmat=sapply(outsum,coef)
summary(out)
out$results
coefmat[[2]]
ci=matrix(nrow=length(out), ncol=8)
ci[,1]=coefmat[1]
ci[,2]=coefmat[2]
ci[,3]=coefmat[3]
ci[,4]=coefmat[4]
ci[,5]=coefmat[5]
ci[,6]=coefmat[6]
ci[,7]=coefmat[7]
ci[,8]=coefmat[8]
ci

#for the 3rd brand

ms3 = tunamarketshare[, c("WEEK","MOVE3", "LPRICE1", "LPRICE2", "LPRICE3", "LPRICE4",
                          "LPRICE5", "LPRICE6", "LPRICE7", "tunamat2")]

ms3$logratio <- log(tunamarketshare$MOVE3/tunamarketshare$tunamat2)


lmmarket3 <- lm(ms3$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms3)
lmmarket3
ms3
out = with(ms3, by(ms3, WEEK, function(x) lm(ms3$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms3)))
out
outsum=lapply(out,summary)
coefmat=sapply(outsum,coef)
summary(out)
out$results
coefmat[[2]]
ci=matrix(nrow=length(out), ncol=8)
ci[,1]=coefmat[1]
ci[,2]=coefmat[2]
ci[,3]=coefmat[3]
ci[,4]=coefmat[4]
ci[,5]=coefmat[5]
ci[,6]=coefmat[6]
ci[,7]=coefmat[7]
ci[,8]=coefmat[8]
ci

#for the 4th brand

ms4 = tunamarketshare[, c("WEEK","MOVE4", "LPRICE1", "LPRICE2", "LPRICE3", "LPRICE4",
                          "LPRICE5", "LPRICE6", "LPRICE7", "tunamat2")]

ms4$logratio <- log(tunamarketshare$MOVE4/tunamarketshare$tunamat2)


lmmarket4 <- lm(ms4$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms4)
lmmarket4
ms4
out = with(ms4, by(ms4, WEEK, function(x) lm(ms4$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms4)))
out
outsum=lapply(out,summary)
coefmat=sapply(outsum,coef)
summary(out)
out$results
coefmat[[2]]
ci=matrix(nrow=length(out), ncol=8)
ci[,1]=coefmat[1]
ci[,2]=coefmat[2]
ci[,3]=coefmat[3]
ci[,4]=coefmat[4]
ci[,5]=coefmat[5]
ci[,6]=coefmat[6]
ci[,7]=coefmat[7]
ci[,8]=coefmat[8]
ci


#for the 5th brand

ms5 = tunamarketshare[, c("WEEK","MOVE5", "LPRICE1", "LPRICE2", "LPRICE3", "LPRICE4",
                          "LPRICE5", "LPRICE6", "LPRICE7", "tunamat2")]

ms5$logratio <- log(tunamarketshare$MOVE5/tunamarketshare$tunamat2)


lmmarket5 <- lm(ms5$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms5)
lmmarket5
ms5
out = with(ms5, by(ms5, WEEK, function(x) lm(ms5$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms5)))
out
outsum=lapply(out,summary)
coefmat=sapply(outsum,coef)
summary(out)
out$results
coefmat[[2]]
ci=matrix(nrow=length(out), ncol=8)
ci[,1]=coefmat[1]
ci[,2]=coefmat[2]
ci[,3]=coefmat[3]
ci[,4]=coefmat[4]
ci[,5]=coefmat[5]
ci[,6]=coefmat[6]
ci[,7]=coefmat[7]
ci[,8]=coefmat[8]
ci


#for the 6th brand

ms6 = tunamarketshare[, c("WEEK", "MOVE6", "LPRICE1", "LPRICE2", "LPRICE3", "LPRICE4",
                          "LPRICE5", "LPRICE6", "LPRICE7", "tunamat2")]

ms6$logratio <- log(tunamarketshare$MOVE6/tunamarketshare$tunamat2)


lmmarket6 <- lm(ms6$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms6)
lmmarket6
ms6
out = with(ms6, by(ms6, WEEK, function(x) lm(ms6$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms6)))
out
outsum=lapply(out,summary)
coefmat=sapply(outsum,coef)
summary(out)
out$results
coefmat[[2]]
ci=matrix(nrow=length(out), ncol=8)
ci[,1]=coefmat[1]
ci[,2]=coefmat[2]
ci[,3]=coefmat[3]
ci[,4]=coefmat[4]
ci[,5]=coefmat[5]
ci[,6]=coefmat[6]
ci[,7]=coefmat[7]
ci[,8]=coefmat[8]
ci


#for the 7th brand

ms7 = tunamarketshare[, c("WEEK","MOVE7", "LPRICE1", "LPRICE2", "LPRICE3", "LPRICE4",
                          "LPRICE5", "LPRICE6", "LPRICE7", "tunamat2")]

ms7$logratio <- log(tunamarketshare$MOVE7/tunamarketshare$tunamat2)


lmmarket7 <- lm(ms7$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms7)
lmmarket7
ms7
out = with(ms7, by(ms7, logratio, function(x) lm(ms7$logratio ~ LPRICE1 + LPRICE2+LPRICE3+LPRICE4+LPRICE5+LPRICE6+LPRICE7, data = ms7)))
out
outsum=lapply(out,summary)
coefmat=sapply(outsum,coef)
summary(out)
out$results
coefmat[[2]]
ci=matrix(nrow=length(out), ncol=8)
ci[,1]=coefmat[1]
ci[,2]=coefmat[2]
ci[,3]=coefmat[3]
ci[,4]=coefmat[4]
ci[,5]=coefmat[5]
ci[,6]=coefmat[6]
ci[,7]=coefmat[7]
ci[,8]=coefmat[8]
ci


# Price Share Elasticities for all brands
model1 <- lm(logmove1 ~ LPRICE1, data = tuna)
model1
require(ggplot2)
ggplot(tuna, aes(LPRICE1, logmove1)) +
  geom_point() +
  stat_smooth(method = lm)

model2 <- lm(logmove2 ~ LPRICE2, data = tuna)
model2
ggplot(tuna, aes(LPRICE2, logmove2)) +
  geom_point() +
  stat_smooth(method = lm)

model3 <- lm(logmove3 ~ LPRICE3, data = tuna)
model3
ggplot(tuna, aes(LPRICE3, logmove3)) +
  geom_point() +
  stat_smooth(method = lm)

model4 <- lm(logmove4 ~ LPRICE4, data = tuna)
model4
ggplot(tuna, aes(LPRICE4, logmove4)) +
  geom_point() +
  stat_smooth(method = lm)

model5 <- lm(logmove5 ~ LPRICE5, data = tuna)
model5
ggplot(tuna, aes(LPRICE5, logmove5)) +
  geom_point() +
  stat_smooth(method = lm)

model6 <- lm(logmove6 ~ LPRICE6, data = tuna)
model6
ggplot(tuna, aes(LPRICE6, logmove6)) +
  geom_point() +
  stat_smooth(method = lm)

model7 <- lm(logmove7 ~ LPRICE7, data = tuna)
model7
ggplot(tuna, aes(LPRICE7, logmove7)) +
  geom_point() +
  stat_smooth(method = lm)

