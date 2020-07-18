#decision tree to improve user booking rate

#split the data into 2 groups training data and testing data
table(total[,c('is_booking','cluster')])
aggregate(.~cluster, data=total[,c('cluster',
                                   'is_booking')], mean, na.action = na.omit)

#lets look at just one cluster group to see how we might target those specific cities
#lets look a cluster 3
total3 = total[total$cluster==3,]
set.seed(999)
s=sample(nrow(total3), nrow(total3)*.75)
total3_t = total3[s,]
total3_v = total3[-s,]
aggregate(.~cluster, data=total3_t[,c('cluster',
                                      'is_booking')], mean, na.action = na.omit)
aggregate(.~cluster, data=total3_v[,c('cluster',
                                      'is_booking')], mean, na.action = na.omit)

table(total3_v$is_booking)
table(total3_t$is_booking)

# grow tree 
set.seed(999)
fit_default = rpart(as.factor(total3_t[,'is_booking']) ~ .,
                    data=total3_t[,depvars],
                    method='class',
                    model=TRUE)
rpart.plot(fit_default)
#with default rpart values we are unable to grow a tree
#because of sample size lets adjust parameters to make it lest strict

# grow tree 
set.seed(999)
fit_dt = rpart(total3_t[,'is_booking'] ~ ., data=total3_t[,depvars],
               control=rpart.control(minsplit=20, cp=.005),#.004 - k means is 6
               method='class', model=TRUE)
printcp(fit_dt) # display the results 
plotcp(fit_dt) # visualize cross-validation results 
summary(fit_dt) # detailed summary of splits

fancyRpartPlot(fit_dt, main='Booking Decision Tree', sub='Cluster 3')
  #Each node shows
  #- the predicted class (0=no booking or 1=booking),
  #- the predicted probability of booking,
  #- the percentage of observations in the node.
  
  pred = predict(fit_dt, total3_v, type = 'class')
  cf=confusionMatrix(as.factor(total3_v[,'is_booking']), pred)
  cat("classifier Accuracy on Valid set:", cf$overall['Accuracy'][[1]],"\n")
  
  #I was curious:
  #pred = predict(fit_dt, total3_t, type = 'class')
  #cf=confusionMatrix(as.factor(total3_t[,'is_booking']), pred)
  #cat("classifier Accuracy on Test set:", cf$overall['Accuracy'][[1]],"\n")
  
  #pred=predict(fit_dt, total, type = 'class')
  #cf=confusionMatrix(as.factor(total[,'is_booking']), predict(fit_dt, total, type = 'class'))
  #cf$overall['Accuracy'][[1]]
  
  #fit_t = rpart(total3[,'is_booking'] ~ ., data=total3[,depvars],
  #               control=rpart.control(minsplit=20, cp=.005),#.004 - k means is 6
  #               method='class', model=TRUE)
  #pred = predict(fit_t, total3, type = 'class')
  #cf=confusionMatrix(as.factor(total3[,'is_booking']), pred)
  #cat("classifier Accuracy on Valid set:", cf$overall['Accuracy'][[1]],"\n")
  