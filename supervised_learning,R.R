library(jsonlite)
library(e1071)
library(tidyverse)

CM <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  return(sum(diag(CM)) / sum(CM))
}

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

ss <- sample(1:2, size=nrow(tdm), replace=TRUE, prob=c(0.8,0.2))
dim(tdm); length(emo)
tdm.train <- tdm[ss==1,]
tdm.test <- tdm[ss==2,]
emo.train <- emo[ss==1]
emo.test <- emo[ss==2]

model.svm <- svm(tdm.train, emo.train, type = "C-classification", kernel = "linear", scale = FALSE)


pred <- predict(model.svm, tdm.test)

table(emo.test, pred)
CM(emo.test, pred)

### spłaszczona wersja

library(jsonlite)
library(e1071)
library(tidyverse)

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


library(tm)
source <- DataframeSource(as.data.frame(lemmas.joined[1:1000,]))
corpus <- VCorpus(source)
tdm <- DocumentTermMatrix(corpus) 
tdm <- tdm[, apply(tdm, 2, sum) > 4]
tdm <- as.matrix(tdm)
ind <- apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
emo <- emo[ind]

ss <- sample(1:2, size=nrow(tdm), replace=TRUE, prob=c(0.8,0.2))
dim(tdm); length(emo)
tdm.train <- tdm[ss==1,]
tdm.test <- tdm[ss==2,]
emo.train <- emo[ss==1]
emo.test <- emo[ss==2]

model.svm <- svm(tdm.train, emo.train, type = "C-classification", kernel = "linear", scale = FALSE)


pred <- predict(model.svm, tdm.test)

table(emo.test, pred)
CM(emo.test, pred)
