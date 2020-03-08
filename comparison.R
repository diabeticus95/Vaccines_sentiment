library(jsonlite)
library(dplyr)

manual_sent <- read_json(path = "D:\\mgr\\dyplom\\R_analysis\\sentyment\\ocena_manualna\\manual_sent.json", simplifyVector = TRUE) %>% fromJSON()
bigram_df <- read_json("D:\\mgr\\dyplom\\R_analysis\\sentyment\\unsupervised\\bigram_df.json",simplifyVector = TRUE)
bigram_check <- bigram_df[1:1000,] %>% cbind(manual_sent)
bigram_check$emo <- manual_sent$emo
bigram_check$emo[bigram_check$emo == -2] <- -1
bigram_check$emo[bigram_check$emo == 2] <- 1
bigram_check$emo <- as.factor(bigram_check$emo)

bigram_check$sent <- as.numeric(bigram_check$sent)
bigram_check$sent[bigram_check$sent < 0] <- -1
bigram_check$sent[bigram_check$sent > 0] <- 1
bigram_check$sent <- as.factor(bigram_check$sent)

eff <- bigram_check$emo == bigram_check$sent
bigram_check$hit <- eff
sum(eff)

pred.neg.true<- sum(bigram_check$sent == -1 & bigram_check$emo == -1)
pred.pos.true<- sum(bigram_check$sent == 1 & bigram_check$emo == 1)
pred.neu.true<- sum(bigram_check$sent == 0 & bigram_check$emo == 0)
pred.neg.false<- sum(bigram_check$sent == -1 & bigram_check$emo != -1)
pred.pos.false<- sum(bigram_check$sent == 1 & bigram_check$emo != 1)
pred.neu.false<- sum(bigram_check$sent == 0 & bigram_check$emo != 0)
real.neg<- sum(bigram_check$emo == -1)
real.pos<- sum(bigram_check$emo == 1)
real.neu<- sum(bigram_check$emo == 0)

count<- c(pred.neg.true, pred.neg.false,  pred.pos.true, pred.pos.false, pred.neu.true, pred.neu.false, real.neg, real.pos, real.neu)
polarity <- c(-1, -1, 1, 1, 0, 0, -1, 1, 0)
type <- c('Trafna predykcja', 'Nietrafna predykcja','Trafna predykcja', 'Nietrafna predykcja','Trafna predykcja', 'Nietrafna predykcja',
          'Prawdziwa liczba',      'Prawdziwa liczba',      'Prawdziwa liczba')

df_barplot = data.frame(count, polarity, type)
df_barplot$type = factor(df_barplot$type, levels(df_barplot$type) [c(1,3,2)])

ggplot(df_barplot, aes(x = polarity, y = count, fill = type)) +
  geom_col(position = 'dodge') +
  xlab('Polaryzacja opinii') +
  ylab('Liczba wystąpień') +
  ggtitle("Uczenie bez nadzoru") +
  theme_bw() + 
  theme(text = element_text(size = 20), legend.title = element_blank())
ggsave("D:\\mgr\\dyplom\\R_analysis\\sentyment\\unsupervised\\eff.png", width = 40, height = 20, units = 'cm')


library(ggplot2)
ggplot(bigram_check, aes(x = doc_id)) +
  geom_point(aes(y = sent, color = hit), size = 3) +
  labs(title = "Skuteczność analizy sentymentu bez nadzoru") + xlab("numer postu") + ylab("przewidziany sentyment") +
  scale_colour_manual(name="Czy trafnie przewidziane", labels = c("Nie", "Tak"), values = c("red", "green")) +
  theme_bw() + 
  theme(text = element_text(size = 20))

ggsave("D:\\mgr\\dyplom\\R_analysis\\sentyment\\unsupervised\\eff.png", width = 40, height = 20, units = 'cm')

ggplot(bigram_check, aes(x = doc_id)) +
  geom_point(aes(y = sent, color = hit), size = 3) +
  labs(title = "Unsupervised sentiment analysis efficiency") + xlab("post number") + ylab("predicted sentiment") +
  scale_colour_manual(name="Was prediction correct", labels = c("No", "Yes"), values = c("red", "green")) +
  theme_bw() + 
  theme(text = element_text(size = 20))

ggsave("D:\\mgr\\dyplom\\R_analysis\\sentyment\\unsupervised\\eff_en.png", width = 40, height = 20, units = 'cm')



###Pod nadzorem
manual_sent <- read_json(path = "D:\\mgr\\dyplom\\R_analysis\\sentyment\\ocena_manualna\\manual_sent.json", simplifyVector = TRUE) %>% fromJSON()
supervised_df <- read_json("D:\\mgr\\dyplom\\R_analysis\\sentyment\\supervised\\supervised_df.json",simplifyVector = TRUE)
supervised_df$doc_id = 1:nrow(supervised_df)
supervised_df$hit <- supervised_df$prd == supervised_df$emo.ss....2.


library(ggplot2)
ggplot(supervised_df, aes(x = doc_id)) +
  geom_point(aes(y = prd, color = hit), size = 3) +
  labs(title = "Skuteczność analizy sentymentu pod nadzorem") + xlab("numer postu") + ylab("przewidziany sentyment") +
  scale_colour_manual(name="Czy trafnie przewidziane", labels = c("Nie", "Tak"), values = c("red", "green")) +
  theme_bw() +
  theme(text = element_text(size = 20))
ggsave("D:\\mgr\\dyplom\\R_analysis\\sentyment\\supervised\\eff.png", width = 40, height = 20, units = 'cm')

pred.neg.true<- sum(supervised_df$prd == -1 & supervised_df$hit == TRUE)
pred.pos.true<- sum(supervised_df$prd == 1 & supervised_df$hit == TRUE)
pred.neu.true<- sum(supervised_df$prd == 0 & supervised_df$hit == TRUE)
pred.neg.false<- sum(supervised_df$prd == -1 & supervised_df$hit == FALSE)
pred.pos.false<- sum(supervised_df$prd == 1 & supervised_df$hit ==FALSE)
pred.neu.false<- sum(supervised_df$prd == 0 & supervised_df$hit == FALSE)
real.neg<- sum(supervised_df$emo.ss....2. == -1)
real.pos<- sum(supervised_df$emo.ss....2. == 1)
real.neu<- sum(supervised_df$emo.ss....2. == 0)

count<- c(pred.neg.true, pred.neg.false,  pred.pos.true, pred.pos.false, pred.neu.true, pred.neu.false, real.neg, real.pos, real.neu)
polarity <- c(-1, -1, 1, 1, 0, 0, -1, 1, 0)
type <- c('Trafna predykcja', 'Nietrafna predykcja','Trafna predykcja', 'Nietrafna predykcja','Trafna predykcja', 'Nietrafna predykcja',
          'Prawdziwa liczba',      'Prawdziwa liczba',      'Prawdziwa liczba')

df_barplot = data.frame(count, polarity, type)
df_barplot$type = factor(df_barplot$type, levels(df_barplot$type) [c(2,3,1)])

ggplot(df_barplot, aes(x = polarity, y = count, fill = type)) +
  geom_col(position = 'dodge') +
  xlab('Polaryzacja opinii') +
  ylab('Liczba wystąpień') +
  ggtitle("Uczenie pod nadzorem") +
  theme_bw() + 
  theme(text = element_text(size = 20), legend.title = element_blank())
ggsave("D:\\mgr\\dyplom\\R_analysis\\sentyment\\supervised\\eff.png", width = 40, height = 20, units = 'cm')




##manual
library(jsonlite)
library(dplyr)

manual_sent <- read_json(path = "D:\\mgr\\dyplom\\R_analysis\\sentyment\\ocena_manualna\\manual_sent.json", simplifyVector = TRUE) %>%
  fromJSON(simplifyVector = TRUE)
dates <- read_json("D:\\mgr\\dyplom\\R_analysis\\manual_posts_with_dates.json", simplifyVector = TRUE)[1:1000,]
df <- data.frame(sent = as.numeric(manual_sent$emo), date = dates$dates)
df$date = as.Date(df$date, format = "%Y-%m-%dT%H:%M:%S")

library(lubridate)
df <- df %>% group_by(month = floor_date(date, unit = "month")) %>%
  summarize(avg = mean(sent))

library(ggplot2)
library(scales)

ggplot(df, aes(x = month, y = avg)) + 
  geom_bar(stat = "identity", aes(color = "sentyment"), fill = "#00BA38") + 
  scale_x_date(labels = date_format("%Y"), breaks = "1 year") +
  geom_point(data = data, aes(x = Date, y = -zachorowania, color = "ospa"), size = 3) +
  ggtitle("Uśredniony miesięcznie, ręcznie oceniony sentyment wypowiedzi") + xlab("Rok") + ylab("Średni sentyment wypowiedzi \n Liczba zachorowań na odrę") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(name="", values = c(ospa = "#F8766D", sentyment = "#00BA38"))
  
  
#ggsave("D:\\mgr\\dyplom\\R_analysis\\sentyment\\dates.svg", width = 40, height = 20, units = 'cm')


## zachorowania
data <- read.csv(file = "D:\\mgr\\dyplom\\dane_epidemiologiczne\\agregat\\data.csv", sep = ";")
names(data)[1] = "Year"
data$Month <- as.factor(data$Month); data$Year <- as.factor(data$Year)
data <- data %>% mutate(Date = as.Date(paste(Year, Month, "01", sep = "."), format = "%Y.%m.%d"))
data <- data %>% filter(choroba == "Odra") %>% select(zachorowania, Date)
data <- na.omit(data)

data$zachorowania <- as.numeric(data$zachorowania)
data$zachorowania = 2 * data$zachorowania / max(data$zachorowania)
