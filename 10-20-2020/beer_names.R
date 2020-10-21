library(quanteda)
library(ggplot2)

df=read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

df[,"num_words"] = sapply(df$beer_name, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
df[,"num_letts"] = sapply(df$beer_name, function(x) str_length(x))

ggplot(df, aes(x=factor(year), y=num_words)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

ggplot(df, aes(x=factor(year), y=num_letts)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

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

select_beer = function(df){
  beer_words = c("ale","stout","lager","porter","red","pale","ipa","brown",
                 "amber","wheat","imperial","pilsner","light","black","dark",
                 "bock","beer","blonde","cream","double","oktoberfest","gold"
                 #,"golden","irish","pils","rye","hefeweizen","white","saison",
                 #"oatmeal","belgian","alaskan","extra","heavy","barrel","smoked",
                 #"special","alt","dunkel","dry","esb","barley","wine",
                 #"barleywine","hop","american","helles","bitter","wee","summer",
                 #"scotch","india","schwarzbier",""
                 )
  df=df[df[,"Word"] %in% beer_words,]
  return(df)}

viz_preprocess = function(df){
  df=select_beer(df)
  df=df[order(-df[,"Count"]),]
  df=transform(df, cum_freq=ave(Frequency, FUN=cumsum))
  df$Word=factor(df$Word, levels=unique(df$Word))
  return(df)}

df_all=viz_preprocess(get_word_freq(df,"beer_name"))
df_gold=viz_preprocess(get_word_freq(df[df["medal"]=="Gold",],"beer_name"))
df_silver=viz_preprocess(get_word_freq(df[df["medal"]=="Silver",],"beer_name"))
df_bronze=viz_preprocess(get_word_freq(df[df["medal"]=="Bronze",],"beer_name"))

ggplot(df_all, aes(x=Word, y=Count)) + 
  geom_col()

ggplot(df_gold, aes(x=Word, y=Count)) + 
  geom_col()

# Number of categories over time
df[,c("category","year")] %>% count(year)

# Beers with 'gold' in name
golden = df[grepl("gold", tolower(df$beer_name)),] %>% count(medal)


