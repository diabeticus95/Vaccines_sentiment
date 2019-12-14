library(rjson)
library(magritrr)
  data <- fromJSON(file = 'wp_parenting_bs4.txt')
  data.filtered <- fromJSON(file = 'wp_parenting_filtered005.txt')

#data.bit <- list()
#for (i in 1:length(data$wp.pl$threads))
#  data.bit = c(data.bit, data$wp.pl$threads[i])
  
#####DATY#####
####wp.pl####
dates <- unlist(lapply(data$wp.pl$threads, function(i) lapply(i$posts, function(j) j$date)))
dates <- lapply(dates, function(i) as.POSIXct(i, format = "%Y-%m-%dT%H:%M:%S"))
dates.vec <- do.call("c", dates)
hist(dates.vec, "months", freq = T, main = "posty na wp.parenting")
##filtered
dates <- unlist(lapply(data.filtered, function(i) lapply(i$posts, function(j) j$date)))
dates <- lapply(dates, function(i) as.POSIXct(i, format = "%Y-%m-%dT%H:%M:%S"))
dates.vec <- do.call("c", dates)
hist(dates.vec, "months", freq = T, main = "posty na wp.parenting, filtr 002")
####kafeteria####
data <- fromJSON(file = 'kafeteria_all.txt')
dates <- unlist(lapply(data$kafeteria$threads, function(i) lapply(i$posts, function(j) j$date)))
dates <- lapply(dates, function(i) as.POSIXct(i, format = "%Y.%m.%d"))
dates.vec <- do.call("c", dates)
##filtered
data.filtered <- fromJSON(file = 'kafeteria_filtered002.txt')
dates <- unlist(lapply(data.filtered, function(i) lapply(i$posts, function(j) j$date)))
dates <- lapply(dates, function(i) as.POSIXct(i, format = "%Y.%m.%d"))
dates.vec <- do.call("c", dates)
hist(dates.vec, "months", freq = T, main = "posty na kafeterii, filtr 002")

#####wordcloud#####
polish_stopwords <- getURL("https://raw.githubusercontent.com/stopwords-iso/stopwords-pl/master/stopwords-pl.json")
polish_stopwords <- fromJSON(polish_stopwords)
content <- list()
for (thread in data.bit) {
  for (post in thread$posts)
    content <- c(content, post$content)
}

library("SnowballC")
library("RColorBrewer")
library('stringi')
library(quanteda)

cont.vec <- do.call("c", content)
Encoding(cont.vec)  <- "UTF-8"
corp <- corpus(cont.vec)
my_dfm <- dfm(corp)
myStemMat <- dfm(corp, remove =
                   c(polish_stopwords, 'się', 'w', 'z', 'że', 'nie', 'na', 'to', 'jest', 'ma', 'tak', 'było', 'bo', 'po', 'co',
                     'do', 'ze', 'u', 'no', 'a', 'jak', 'ale', 'też', 'się', 'od', 'sie', 'taki', 'ja', 'za', 'i', 'o',
                     'mam', 'tylko', 'dziecko', 'już', 'on', 'mi', 'tym', 'nas', 'moja', 'mnie', 'dla', 'teraz', 'tym',
                     'te', 'tam', 'ok', 'tu', 'jeszcz', 'czi', 'jej', 'je', 'coś', 'by', 'albo', 'bardzo',
                     'wszystko', 'będzie', 'więc', 'was', 'być', 'pewni', 'trochę™', 'może', 'jeszcze', 'chyba', 'sobie',
                     'chce','czy', 'tej', 'juz', 'tego', 'sa', 'ani', 'ten', 'wiem', 'tez', 'wczoraj', 'miałam', 'hej',
                     'pewnie', 'wiec', 'cześć', 'itp', 'potem', 'da', 'nu', 'oby', 'tymi', 'bylo', 'witam', 'dnia', 'nocy',
                     'cos', 'n'),
                  stem = F, remove_punct = TRUE,remove_numbers = T)
textplot_wordcloud(myStemMat, min_count = 100, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"), max_words = 200)