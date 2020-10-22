df[,"num_words"] = sapply(df$beer_name, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
df[,"num_letts"] = sapply(df$beer_name, function(x) str_length(x))

ggplot(df, aes(x=factor(year), y=num_words)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

ggplot(df, aes(x=factor(year), y=num_letts)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)


select_beer = function(df){
  beer_words = c("ale","stout","lager","porter","red","pale","ipa","brown",
                 "amber","wheat","imperial","pilsner","light","black","dark",
                 "bock","beer","blonde","cream","double","oktoberfest","gold",
                 "golden","irish","pils","rye","hefeweizen","white","saison",
                 "oatmeal","belgian","alaskan","extra","heavy","barrel","smoked",
                 "special","alt","dunkel","dry","esb","barley","wine",
                 "barleywine","hop","american","helles","bitter","wee","summer",
                 "scotch","india","schwarzbier",""
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