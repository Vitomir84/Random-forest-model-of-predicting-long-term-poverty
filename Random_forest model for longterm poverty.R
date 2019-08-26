
#We are importing our SILC Eurostat database (no real data will be visible about users - they are confidential).
library(haven)
silc_r <- read_sav("R:/3. Projekti/2019 SILC analiza/Baze za rad/silc_r.sav")
View(silc_r)
df <- silc_r
df <- na.omit(df)

str(df)
#We want to create model that predicts longterm poverty (two out of three years ar poverty risk - 60% of median of income)
#We want a little bit to explore our data to get sense of what is going on.
#Create a scatterplot of ZWORK_INT (work intensity) versus ZINCPEN(income from pensions), colored by the long_term poverty column(LONG_POV).

library(ggplot2)

#Firstly, we have to create categorical variables where apropriate.

df$LONG_POV <- as.factor(df$LONG_POV)
df$POL <- as.factor(df$POL)
df$DROPOUT <- as.factor(df$DROPOUT)
df$SAMOZAPOSLENOST_PORODIČNI_RAD <- as.factor(df$SAMOZAPOSLENOST_PORODIČNI_RAD)
df$TENSTA <- as.factor(df$TENSTA)
df$SEV_DEP2015 <- as.factor(df$SEV_DEP2015)
df$LOW_WORK_INT2015 <- as.factor(df$LOW_WORK_INT2015)
df$LOW_WORK_INT2016 <- as.factor(df$LOW_WORK_INT2016)
df$ZDRAVLJE <- as.factor(df$ZDRAVLJE)
df$NEZAPOSLENOST <- as.factor(df$NEZAPOSLENOST)
df$UDOVIŠTVO <- as.factor(df$UDOVIŠTVO)
df$ZDRAV_SPREČENOST_ZA_AKTIVNOSTI <- as.factor(df$ZDRAV_SPREČENOST_ZA_AKTIVNOSTI)
df$OVERCROWDED <- as.factor(df$OVERCROWDED)

#We are checking our database
str(df)
#We are trying to understand a longterm poverty
pl <- ggplot(df,aes(x=ZWORK_INT,y=ZINCPEN,color=LONG_POV)) + geom_point()
pl

#We see that longterm poverty rate is higher if working intensity is lower, but still there are 
#not so small amounts of respondents who are poor even if they have job
ggplot(df, aes(ZWORK_INT), colours()) + geom_histogram(aes(fill=LONG_POV), color="blue", binwidth = 0.3, position = "dodge") + theme_bw() + xlim(-3, 3)

#Create a histogram of income from pensions that should reduce long_term poverty rates.

ggplot(df, aes (ZINCPEN)) + geom_histogram(aes(fill=LONG_POV), color="black", binwidth = 0.5, position = "dodge") + theme_bw() + xlim(-3, 3)

#GOOD or BAD health are not so well connected with long_term poverty as could be expected

ggplot(df, aes (ZDRAVLJE, LONG_POV)) + geom_jitter() + theme_bw()
# load the MASS package 
library(MASS)      
tbl <-  table(df$ZDRAVLJE, df$LONG_POV) 
tbl
chisq.test(tbl)



#creating training and test database in proportion 70/30

library(caTools)

set.seed(101) 

sample = sample.split(df$LONG_POV, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#desicion tree model

#Use the rpart library to build a decision tree to predict whether or not a school is Private. 
#Remember to only build your tree off the training data

library(rpart)
tree <- rpart(LONG_POV ~.,method='class',data = train)

#kreiraj predviđene vrednosti na osnovu desicion tree modela za test bazu

tree.preds <- predict(tree,test)
head(tree.preds)

#Turn these two columns into one column to match the original Yes/No Label for a Private column.

tree.preds <- as.data.frame(tree.preds)

joiner <- function (x){
  if (x>=0.5){
    return(1)
  }else{
    return(0)
  }
}


tree.preds$LONG_POV <- sapply(tree.preds$`1`, joiner)

head(tree.preds)

#Now use table() to create a confusion matrix of your tree model.

tab <- table(tree.preds$LONG_POV, test$LONG_POV)
tab
#accuracy rate
acc <- sum(diag(tab))/sum(tab)
acc
#mislasification rate
mfc <- 1-acc
mfc


#Use the rpart.plot library and the prp() function to plot out your tree model.

install.packages("rpart.plot")
library(rpart.plot)
prp(tree)


#Use the rpart.plot library and the prp() function to plot out your tree model.

install.packages("rpart.plot")
library(rpart.plot)
prp(tree)

#Random forests improve predictive accuracy by generating a large number of bootstrapped trees 
#(based on random samples of variables), classifying a case using each tree in this new "forest", 
#and deciding a final predicted outcome by combining the results across all of the trees 
#(an average in regression, a majority vote in classification).
#We can use the randomForest library to create and build out a Random Forest.

#Call the randomForest package library

install.packages("randomForest")
library(randomForest)

#Now use randomForest() to build out a model to predict Private class. 
#Add importance=TRUE as a parameter in the model. (Use help(randomForest) to find out what this does.

help("randomForest")
forest <- randomForest(LONG_POV ~ ., data=df, importance=TRUE)
forest

#What was your model's confusion matrix on its own training set? Use model$confusion.

forest$confusion

#Grab the feature importance with model$importance. Refer to the reading for more info on what Gini[1] means.[2]

forest$importance
prp(tree)

#MeanDecreasedAccuracy - koliko uključenje prediktora smanjuje grešku klasifikacije

#MeanDecreaseGini - Gini is defined as "inequity" when used in describing a society's distribution of income, 
#or a measure of "node impurity" in tree-based classification. 
#A low Gini (i.e. higher descrease in Gini) means that a particular predictor variable plays a greater role 
#in partitioning the data into the defined classes. It's a hard one to describe without talking about the fact 
#that data in classification trees are split at individual nodes based on values of predictors. 
##############################################################################################
#The highest decreased in gini has the work intensity, meaning that this predictor is the best predictor 
#of the longterm poverty. It seems that tree only identifies this predictor as important
#############################################################################################
#Now use your random forest model to predict on your test set!

p <- predict(forest,test)

tab <- table(p,test$LONG_POV)
tab
#accuracy rate
acc <- sum(diag(tab))/sum(tab)
acc
#mislasification rate
mfc <- 1-acc
mfc

#Random forest model is far better with almost of 90% of accuracy in predictions
