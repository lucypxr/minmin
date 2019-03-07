###
### Important: It's expected that you can figure out what this script does
###  since you are all highly motivated, curious graduate students
###  with lots of initiative.  Also, we have been writing very similar code
###  for an *entire* semester now.
###  

###
### I highly recommend that you close all other applications 
###  when you run the script below.
###

###
### This is the data set with extra data.
###

train = read.csv("train_extra.csv", header = TRUE)


###
### Do some preprocessing.
###
### Important: My preprocessing may yield bad results.
### 
### The purpose here is to run a model to get some results and see
###  how you can overcome the large test data size.
###

train$click_time      = NULL
train$attributed_time = NULL
train$ip              = as.factor(train$ip)
train$app             = as.factor(train$app)
train$device          = as.factor(train$device)
train$os              = as.factor(train$os)
train$channel         = as.factor(train$channel)
train$is_attributed   = as.factor(train$is_attributed)

###
### I like ranger, another random forest library, more than randomForest.
###
### Explore on your own.
###

library(ranger)

set.seed(1000)
system.time({
  rf_mod = ranger(is_attributed ~ ., data = train, num.trees = 500, probability = TRUE)
})

###
### Read the test data.
### This will take 2 minutes.  You can also use fread of data.table.
###


system.time({
test = read.csv("test.csv", header = TRUE)
})

###
### Do the same clean up:
###

test$click_time      = NULL
test$attributed_time = NULL
test$ip              = as.factor(test$ip)
test$app             = as.factor(test$app)
test$device          = as.factor(test$device)
test$os              = as.factor(test$os)
test$channel         = as.factor(test$channel)

###
### Important: All my attempts at prediction over this very large
###  test data using the predict function failed miserably.
### I had to use a trick; see below.  Figure it out.
###

ends = seq(from = 10^6, to = 18 * 10^6, by = 10^6)
ends = c(ends, nrow(test))
starts = seq(from = 1, to = 18 * 10^6 + 1, by = 10^6)

###
### This chuck coming up takes about 50 minutes in my machine.
###

y = rep(NA, nrow(test))

system.time({
for (i in 1:length(ends))
{
  current    = starts[i]:ends[i]
  y[current] = predict(rf_mod, data = test[current,] , verbose = FALSE)$predictions[,2]
}
})

###
###  Submit...takes 2 minutes
###

system.time({
  submission = data.frame(click_id = 0:(nrow(test)-1), is_attributed = y)
  write.csv(submission, "submission_rf.csv", row.names = F)
})

###
### My submission to Kaggle took around 15 minutes.
###





