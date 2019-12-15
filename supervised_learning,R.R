library(jsonlite)
library(e1071)
library(tidyverse)
library(caret)


setwd("D:\\mgr\\dyplom\\R_analysis\\sentyment\\ocena_manualna")
lemmas <- read_json(path = "D:\\mgr\\dyplom\\R_analysis\\wp_par_lemma_flt_002_byPost.txt")
lemmas <- lemmas %>% unlist(recursive = FALSE)
lemmas.joined <- data.frame(doc_id = 1:length(lemmas), text = sapply(lemmas, paste, collapse=" "), stringsAsFactors = FALSE)

manual_sent <- read_json(path = "manual_sent.json", simplifyVector = TRUE) %>% fromJSON()
emo <- manual_sent$emo
emo <- as.factor(emo)
levels(emo)


library(tm)
source <- DataframeSource(as.data.frame(lemmas.joined[1:1000,]))
corpus <- VCorpus(source)
tdm <- DocumentTermMatrix(corpus) 
tdm <- tdm[, apply(tdm, 2, sum) > 4]
tdm <- as.matrix(tdm)
ind <- apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
emo <- emo[ind]
dim(tdm); length(emo)


set.seed(95)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 10)
model <- train(tdm, emo, method = "svmLinear",
               trControl = train.control)
#Support Vector Machines with Linear Kernel 
#
#981 samples
#1339 predictors
#5 classes: '-1', '-2', '0', '1', '2' 
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 10 times) 
#Summary of sample sizes: 882, 884, 882, 885, 883, 881, ... 
#Resampling results:
#  
#  Accuracy   Kappa    
#0.5808446  0.2104012
#
#Tuning parameter 'C' was held constant at a value of 1

#model.svm <- svm(tdm.train, emo.train, type = "C-classification", kernel = "linear", scale = FALSE)


### spłaszczona wersja


library(jsonlite)
library(e1071)
library(tidyverse)
library(caret)
library(tm)



setwd("D:\\mgr\\dyplom\\R_analysis\\sentyment\\ocena_manualna")
lemmas <- read_json(path = "D:\\mgr\\dyplom\\R_analysis\\wp_par_lemma_flt_002_byPost.txt")
lemmas <- lemmas %>% unlist(recursive = FALSE)
lemmas.joined <- data.frame(doc_id = 1:length(lemmas), text = sapply(lemmas, paste, collapse=" "), stringsAsFactors = FALSE)

manual_sent <- read_json(path = "manual_sent.json", simplifyVector = TRUE) %>% fromJSON()
emo <- manual_sent$emo
emo[emo == -2] <- -1
emo[emo == 2] <- 1
emo <- as.factor(emo)
levels(emo)


source <- DataframeSource(as.data.frame(lemmas.joined[1:1000,]))
corpus <- VCorpus(source)
tdm <- DocumentTermMatrix(corpus) 
tdm <- tdm[, apply(tdm, 2, sum) > 4]
tdm <- as.matrix(tdm)
ind <- apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
emo <- emo[ind]
dim(tdm); length(emo)


set.seed(95)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 10)
model <- train(tdm, emo, method = "svmLinear",
               trControl = train.control)
print(model)

#Support Vector Machines with Linear Kernel 
#
#981 samples
#1339 predictors
#3 classes: '-1', '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 10 times) 
#Summary of sample sizes: 883, 883, 882, 884, 883, 882, ... 
#Resampling results:
#  
#  Accuracy   Kappa    
#0.5968155  0.2235993
#
#Tuning parameter 'C' was held constant at a value of 1#




#### Tf_idf
library(jsonlite)
library(e1071)
library(tidyverse)
library(caret)


setwd("D:\\mgr\\dyplom\\R_analysis\\sentyment\\ocena_manualna")
lemmas <- read_json(path = "D:\\mgr\\dyplom\\R_analysis\\wp_par_lemma_flt_002_byPost.txt")
lemmas <- lemmas %>% unlist(recursive = FALSE)
lemmas.joined <- data.frame(doc_id = 1:length(lemmas), text = sapply(lemmas, paste, collapse=" "), stringsAsFactors = FALSE)

manual_sent <- read_json(path = "manual_sent.json", simplifyVector = TRUE) %>% fromJSON()
emo <- manual_sent$emo
emo <- as.factor(emo)
levels(emo)


library(tm)
source <- DataframeSource(as.data.frame(lemmas.joined[1:1000,]))
corpus <- VCorpus(source)
tdm <- DocumentTermMatrix(corpus, control = list(weighting = function(x)
                                                   weightTfIdf(x, normalize = FALSE))) 
tdm <- tdm[, apply(tdm, 2, sum) > 4]
tdm <- as.matrix(tdm)
ind <- apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
emo <- emo[ind]
dim(tdm); length(emo)


set.seed(95)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 10)
model <- train(tdm, emo, method = "svmLinear",
               trControl = train.control)
#
#Support Vector Machines with Linear Kernel 
#
#996 samples
#6724 predictors
#5 classes: '-1', '-2', '0', '1', '2' 
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 10 times) 
#Summary of sample sizes: 897, 897, 897, 897, 894, 894, ... 
#Resampling results:
#  
#  Accuracy   Kappa    
#0.6326308  0.2245865
#
#Tuning parameter 'C' was held constant at a value of 1



######################## tf_idf splaszczone

library(jsonlite)
library(e1071)
library(tidyverse)
library(caret)
library(tm)



setwd("D:\\mgr\\dyplom\\R_analysis\\sentyment\\ocena_manualna")
lemmas <- read_json(path = "D:\\mgr\\dyplom\\R_analysis\\wp_par_lemma_flt_002_byPost.txt")
lemmas <- lemmas %>% unlist(recursive = FALSE)
lemmas.joined <- data.frame(doc_id = 1:length(lemmas), text = sapply(lemmas, paste, collapse=" "), stringsAsFactors = FALSE)

manual_sent <- read_json(path = "manual_sent.json", simplifyVector = TRUE) %>% fromJSON()
emo <- manual_sent$emo
emo[emo == -2] <- -1
emo[emo == 2] <- 1
emo <- as.factor(emo)
levels(emo)


source <- DataframeSource(as.data.frame(lemmas.joined[1:1000,]))
corpus <- VCorpus(source)
tdm <- DocumentTermMatrix(corpus, control = list(weighting = function(x)
  weightTfIdf(x, normalize = FALSE))) 
tdm <- tdm[, apply(tdm, 2, sum) > 4]
tdm <- as.matrix(tdm)
ind <- apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
emo <- emo[ind]
dim(tdm); length(emo)


set.seed(95)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 10)
model <- train(tdm, emo, method = "svmLinear",
               trControl = train.control)
print(model)
#
#Support Vector Machines with Linear Kernel 
#
#996 samples
#6724 predictors
#3 classes: '-1', '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 10 times) 
#Summary of sample sizes: 896, 897, 896, 897, 895, 895, ... 
#Resampling results:
#  
#  Accuracy   Kappa    
#0.6500644  0.2635761
#
#Tuning parameter 'C' was held constant at a value of 1#


############### rf tf_idf


############### rf tf_idf splaszczone
library(jsonlite)
library(e1071)
library(tidyverse)
library(caret)
library(tm)



setwd("D:\\mgr\\dyplom\\R_analysis\\sentyment\\ocena_manualna")
lemmas <- read_json(path = "D:\\mgr\\dyplom\\R_analysis\\wp_par_lemma_flt_002_byPost.txt")
lemmas <- lemmas %>% unlist(recursive = FALSE)
lemmas.joined <- data.frame(doc_id = 1:length(lemmas), text = sapply(lemmas, paste, collapse=" "), stringsAsFactors = FALSE)

manual_sent <- read_json(path = "manual_sent.json", simplifyVector = TRUE) %>% fromJSON()
emo <- manual_sent$emo
emo[emo == -2] <- -1
emo[emo == 2] <- 1
emo <- as.factor(emo)
levels(emo)


source <- DataframeSource(as.data.frame(lemmas.joined[1:1000,]))
corpus <- VCorpus(source)
tdm <- DocumentTermMatrix(corpus, control = list(weighting = function(x)
  weightTfIdf(x, normalize = FALSE))) 
tdm <- tdm[, apply(tdm, 2, sum) > 4]
tdm <- as.matrix(tdm)
ind <- apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
emo <- emo[ind]
dim(tdm); length(emo)


set.seed(95)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 10)
model <- train(tdm, emo, method = "rf",
               trControl = train.control)
print(model)
