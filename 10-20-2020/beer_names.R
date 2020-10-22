library(quanteda)
library(ggplot2)
library(tidytext)
library(fmsb)

df=read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

get_docs = function(dt, col){
  return(dt[, col])}

get_dfm = function(docs){
  text = paste(docs, collapse = ", ")
  tokens = tokens(text, remove_punct = TRUE)
  dfm = dfm(tokens, tolower=TRUE, stem=FALSE)
  return(dfm)}

get_df = function(dfm){
  words = dfm@Dimnames$features
  counts = dfm@x
  frequencies = counts/sum(counts)
  list=list(words, counts, frequencies)
  df=as.data.frame(list)
  colnames(df) = c("Word", "Count", "Frequency")
  return(df)}

get_word_freq = function(df, col){
  docs = get_docs(df, col)
  dfm = get_dfm(docs)
  df = get_df(dfm)
  return(df)}

sents = get_sentiments("nrc") %>% 
  filter(sentiment %in% c("sadness", "trust", "fear", 
                          "anger", "anticipation", "joy"))

get_words = function(df, col){
  text = paste(df[,col], collapse = ", ")
  tokens = tokens(text, remove_punct = TRUE)
  dfm = dfm(tokens, tolower=TRUE, stem=FALSE)
  words = dfm@Dimnames$features
  return(words)}

get_sents = function(df, col, sents){
  words = get_words(df, col)
  words = as.data.frame(words)
  colnames(words)="word"
  words=words[!(words %in% c("stout","porter","belgian","saison","pale","ale","lager","pilsener","pilsner"))]
  word_sents = words %>% inner_join(sents) %>% count(sentiment)
  word_sents[,"freq"] = word_sents[,"n"]/sum(word_sents[,"n"])
  return(word_sents)}

word_sents = get_sents(df, "beer_name", sents)
gold_sents = get_sents(df[df[,"medal"]=="Gold",], "beer_name", sents)
silver_sents = get_sents(df[df[,"medal"]=="Silver",], "beer_name", sents)
bronze_sents = get_sents(df[df[,"medal"]=="Bronze",], "beer_name", sents)

get_radar_data = function(df, type){
  beer_sents=get_sents(df[grepl(beer_type, tolower(df$category)),], "beer_name", sents)[,c(1,3)]
  beer_sents[,"max"] = max(beer_sents[,"freq"])
  beer_sents[,"min"] = 0
  beer_sents=transpose(beer_sents)
  colnames(beer_sents)=beer_sents[1,]
  beer_sents=beer_sents[-1,]
  beer_sents=beer_sents[c(2,3,1),]
  beer_sents = apply(beer_sents, 2, function(x) as.numeric(as.character(x)));
  return(as.data.frame(beer_sents))}

par(mfrow=c(2,2),oma=c(0,0,2,0),mar=c(1,1,5,1), bg="grey85")

for(i in 1:4){
  beer_type=c("stout | porter", "belgian | saison", "india pale ale", "lager | pilsener | pilsner")[i]
  color=c(rgb(65, 21, 8, max=255, alpha=150),
          rgb(203, 130, 18, max=255, alpha=150),
          rgb(242, 187, 29, max=255, alpha=150),
          rgb(247, 232, 99, max=255, alpha=150))[i]
  data=get_radar_data(df, beer_type)
  radarchart(data, pcol=color, 
             pfcol = color, plwd=4,
             title=paste(beer_type))}

title("Sentiment of Beer Names by Style", outer=TRUE, cex.main=2)


