library(dplyr)
library(magrittr)
library(RCurl)
library(tidytext)
library(tidyr)
library(rjson)

sentiment <- read.csv(file = "D:\\mgr\\dyplom\\R_analysis\\slowosiec\\plwordnet_4_0\\słownik_anotacji_emocjonlanej.csv", header = T)

sent <- sentiment %>% 
  select(lemat, stopien_nacechowania) 
names(sent)[2] <- "emo"
sent$emo <- as.character(sent$emo); sent$lemat <- as.character(sent$lemat)
sent$emo[sent$emo == "NULL"] = 0
sent$emo[sent$emo == ""] = 0
sent$emo[sent$emo == "amb"] = 0
sent$emo[sent$emo == "- m"] = -2
sent$emo[sent$emo == "- s "] = -1
sent$emo[sent$emo == "- s"] = -1
sent$emo[sent$emo == "+ m"] = 2
sent$emo[sent$emo == "+ s"] = 1
sent$emo <- as.integer(sent$emo)
sent <- unique(sent)
#wyrzucam duplikaty z różnymi emo na korzyść pozytywniejszych
sent %<>% group_by(lemat) %>%
  summarise(emo = max(emo))

#zrobić analizę bigramową
#pobieram dane - odfiltrowane wątki wp
#data <- fromJSON(file = 'wp_parenting_filtered002.txt')
#getLemma <- function(t,u) {
#  library(httr)
#  library(xml2)
#  p <- list(lpmn="any2txt|wcrft2",text=t,user=u)
#  s <- POST("http://ws.clarin-pl.eu/nlprest2/base/process", body = p, encode = "json", verbose())
#  r <- content(s, "text")
#  r <- gsub('[[:punct:] ]+','',unlist(as_list(xml_find_all(read_xml(r),"//base"))))
#  return(r[r != ""])
#}
src <- fromJSON(file = "D:\\mgr\\dyplom\\R_analysis\\wp_parenting_filtered002.txt")
#posts.lemma <- lapply(src, function(i) lapply(
#  i$posts, function(j) getLemma(j$content, "270628@pw.edu.pl"))
#)
####write(toJSON(posts.lemma), "wp_par_lemma_flt_002_byPost.txt")
dates <- lapply(src, function(i) lapply(
  i$posts, function(j) j$date)
) %>% unlist()
dates <- lapply(dates, function(i) as.POSIXct(i, format = "%Y-%m-%d"))
dates.vec <- do.call("c", dates)

posts.lemma <- fromJSON(file = "D:\\mgr\\dyplom\\R_analysis\\wp_par_lemma_flt_002_byPost.txt")
#do kazdego posta chcialbym dopisac sentyment

#stopw <- getURL("https://raw.githubusercontent.com/stopwords-iso/stopwords-pl/master/stopwords-pl.json")
stopw <- fromJSON(file = "D:\\mgr\\dyplom\\R_analysis\\stopwords.txt")
stopw <- c(stopw, as.character(1:1000))
####write(toJSON(stopw), "stopwords.txt")
keywords <- c("zaszczepić", "szczepienie", "szczepić", "szczepionka", "pneumokok", "wzw", "gruźlica", "Błonica", "tężec",
              "krztusiec", "odra", "świnka", "różyczka","ospa", "meningokok", "grypa", "polio", "NOPach",
              "obowiązkowy", "poliomyelitis", "zakażenie", "chory", "choroba", "zapalenie", "autyzm", "rtęć",
              "Aspergera", "spektrum", "wietrzny")

evalPost <- function(post) {
  post <- paste(post, collapse = " ")
  post <- tibble(text = post)
  post.bi <- post %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stopw) %>%
    filter(word2 %in% keywords) %>%
    group_by(word2) %>%
    inner_join(sent, by = c('word1' = 'lemat')) #%>%
    #count(word2, emo, sort = T) %>%
    #ungroup() %>%
    #mutate(emo = reorder(emo, n))
  return(sum(post.bi$emo))
}

#chciałbym dostać liste wartosci data - sentyment
#wyciągam to z data najpierw
#powinienem to przepisac na wspolnego tibbla, i robic dzialanie naraz na tabeli, bo za wolno dziala. Rozrozniac po numerach
posts.l.u <- unlist(posts.lemma, recursive = F)
sents <- sapply(posts.l.u, function(i) {
  evalPost(i)}
  )
df.b <- data.frame(dates = as.Date(dates.vec), sentiment = sents)
df.b$month <- as.Date(cut(df.b$dates, breaks = "month"))
library(scales)
library(ggplot2)
ggplot(data = df.b,
       aes(month, sentiment)) +
  stat_summary(fun.y = sum, geom = "bar") + 
  labs(title="Analiza bigramów ze słowami kluczowymi w każdym poście") +
  scale_x_date(
    labels = date_format("%Y"),
    breaks = "1 year") # custom x-axis labels


#mniej niż 10% ma w ogóle jakiś sentyment! Spróbuję ze zwykłą analizą sent
evalPostMono <- function(post) {
  post <- tibble(word = post) %>%
    filter(!word %in% stopw) %>%
    inner_join(sent, by = c('word' = 'lemat'))
  return(sum(post$emo))
}
sentsMono <- sapply(posts.l.u, function(i) {
  evalPostMono(i)}
)
df.mono <- data.frame(dates = df.b$dates, sentiment = sentsMono)
df.mono$month <- df.b$month
ggplot(data = df.mono,
  aes(month, sentiment)) +
    stat_summary(fun.y = sum, geom = "bar") + 
    labs(title="Analiza poj. słów w każdym poście") +
    scale_x_date(
      labels = date_format("%Y"),
      breaks = "1 year") 
#kafeteria

  