#idea: stworzyć zespół‚ keywordów które chciałbym widzieć w wątku, ustawić próg występowania aby odsiać bzdury.
#test lematyzera
getLemma <- function(t,u) {
  library(httr)
  library(xml2)
  p <- list(lpmn="any2txt|wcrft2",text=t,user=u)
  s <- POST("http://ws.clarin-pl.eu/nlprest2/base/process", body = p, encode = "json", verbose())
  r <- content(s, "text")
  r <- gsub('[[:punct:] ]+','',unlist(as_list(xml_find_all(read_xml(r),"//base"))))
  return(r[r != ""])
}
#tekst do przetworzenia - suma tekstu dla danego wątku.
library(rjson)
library(magrittr)
####wp_par####
data <- fromJSON(file = 'wp_parenting_bs4.txt')
content.threads <- lapply(data$wp.pl$threads, function(i) paste0(sapply(i$posts, function(j) j$content), collapse = " "))
#threads.lemma <- lapply(content.threads, function(i) getLemma(i, "270628@pw.edu.pl"))
threads.lemma <- fromJSON(file = "lematyzacja\\wp_lemma\\wp_lemma.txt")
#fajnie by byĹ‚o to wszystko jeszcze wystemować
#ale póki co...
keywords <- c("zaszczepić", "szczepienie", "szczepić", "szczepionka", "pneumokok", "wzw", "gruźlica", "Błonica", "tężec",
              "krztusiec", "odra", "świnka", "różyczka","ospa", "meningokok", "grypa", "polio", "NOPach",
              "obowiązkowy", "poliomyelitis", "zakażenie", "chory", "choroba", "zapalenie", "autyzm", "rtęć",
              "Aspergera", "spektrum", "wietrzny")
library(dplyr)
progi <- sapply(threads.lemma, function(i) sum(i %in% keywords)/length(i)) #zawartość keywordów w wątku
#write(toJSON(threads.lemma), "lematyzacja\\wp_lemma\\wp_lemma.txt")

#1 - odłowić thready z progiem 0.05
lemma.filtered <- threads.lemma[progi > 0.02]
write(toJSON(lemma.filtered), "wp_parenting_lemma_filtered002.txt")
data.filtered <- data$wp.pl$threads[progi > 0.02]
write(toJSON(data.filtered), "wp_parenting_filtered005.txt")
#2 - porobić wykresy dat z odłowionymi POSTAMI, nie threadami, zawierajacymi w ogole keywordsy
#3 - zrobic analizę sentymentu powyzej progu 0.01
#4 - powtórzyć dla kafeterii
####kafeteria####


#podzielić na party żeby nie dobić lematyzera
#wstępnie mogę wyrzucić wątki dłuższe niż x
data <- fromJSON(file = 'kafeteria_all.txt')
postNum <- sapply(data$kafeteria$threads, function(i) length(i$posts))
#za długo się lematyzują pozostałe, a i tak takie długie by odpadły przy filtrowaniu
data <- data$kafeteria$threads[postNum < 1000]
data.chunks <- split(data, ceiling(seq_along(data)/150))
####chunk 1, timing test####
chunk <- data.chunks[[3]]
content.threads <- lapply(chunk, function(i) paste0(sapply(i$posts, function(j) j$content), collapse = " "))
ilosc.postow.chunka <- lapply(chunk, function(i) length(i$posts))
#muszą wyodrębnić duze thready i je załatwić oddzielnie
#threads.lemma <- lapply(content.threads, function(i) getLemma(i, "270628@pw.edu.pl"))
#export chunkĂłw
write(toJSON(threads.lemma), "kafeteria_lemma_chunks\\kafeteria_chunk_3.txt")
#agregujÄ™
agr <- lapply(1:3, function(i)
  fromJSON(file = paste0("kafeteria_lemma_chunks\\kafeteria_chunk_", as.character(i), ".txt")))
agr %<>% unlist(recursive = F)
progi <- sapply(agr, function(i) sum(i %in% keywords)/length(i)) #zawartość keywordów w wątku
data.filtered <- data[progi > 0.02]
write(toJSON(data.filtered), "kafeteria_filtered002.txt")






