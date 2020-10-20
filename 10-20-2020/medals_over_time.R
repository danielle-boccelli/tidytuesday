library(ggplot2)
library(tidyr)
library(dplyr)
library(gganimate)
library(gifski)


df=read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

cols=c("medal","state","year")
df=df[cols]

df[,"state"] = toupper(df[,"state"])
df=df %>% 
  complete(year, nesting(state, medal)) %>% 
  count(state, medal, year)

df=spread(df, medal, n)
df[is.na(df)] = 0

df=df[order(df[,"state"], df[,"year"]),]

df=transform(df, 
             cum_Bronze=ave(Bronze,state,FUN=cumsum),
             cum_Silver=ave(Silver,state,FUN=cumsum),
             cum_Gold=ave(Gold,state,FUN=cumsum))
cols=c("state", "year", "cum_Bronze", "cum_Silver", "cum_Gold")
df=df[cols]
colnames(df) = c("state", "year", "bronze", "silver", "gold")
df=gather(df, key="medal", value="count", -c(state, year))

p=ggplot(data=df,
       mapping=aes(x=count,
                   y=state,
                   color=medal)) +
  geom_point(size=4,
             alpha=0.8) +
  scale_color_manual(values=c("wheat4", "gold", "gray87")) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size=12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey25")) +
  transition_states(year,
                    transition_length=2,
                    state_length=2) +
  labs(title = 'Year: {closest_state}') 

anim=animate(p, renderer = gifski_renderer(), height=800, width=400)

anim_save("medals.gif", anim)