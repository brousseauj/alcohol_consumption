url='https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv'
df = read.csv(url)
setwd('/Users/brousju1/Desktop/Fun Data for Fun/alcohol')

library(ggmap)
library(parallel)
library(RColorBrewer)
library(maptools)
library(ggplot2)
library(rworldmap)
library(reshape2)
df$region = df$country
map.world = map_data(map='world')
map.world = merge(df,map.world, by='region',all.y=TRUE )
map.world = map.world[order(map.world$order), ]
#Countries with no data are omitted

#theme for all graphs
mytheme = function(){
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        legend.background = element_rect(fill='black', 
                                         size=0.5, linetype="solid"),
        legend.text = element_text(colour = 'white'),
        legend.position = 'bottom',
        legend.title =
          element_text(color = "White") 
  )
}

#World plot for beer consupmtion
beer_plot=ggplot() + 
  geom_map(data = map.world,map = map.world,colour='white',size=.1, aes(
    map_id = region,
    x=long,
    y=lat,
    fill=beer_servings
  )) + 
  coord_quickmap()+
  scale_fill_continuous(name = "Servings Of Beer\nPer Person, 2010")+
  mytheme()
ggsave(filename = 'beer_plot.png',plot = beer_plot,dpi = 300)

# World plot for wine Consumption
wine_plot=ggplot() + 
  geom_map(data = map.world,map = map.world,colour='white',size=.1, aes(
    map_id = region,
    x=long,
    y=lat,
    fill=wine_servings
  )) + 
  coord_quickmap()+
  scale_fill_continuous(name = "Servings Of Wine\nPer Person, 2010")+
  mytheme()
ggsave(filename = 'wine_plot.png',plot=wine_plot,dpi=300)

#World Plot for spirit consumption
spirit_plot=ggplot() + 
  geom_map(data = map.world,map = map.world,colour='white',size=.1, aes(
    map_id = region,
    x=long,
    y=lat,
    fill=spirit_servings
  )) + 
  coord_quickmap()+
  scale_fill_continuous(name = "Servings Of Spirits\nPer Person, 2010")+
  mytheme()
ggsave('spirit_plot.png',spirit_plot,dpi=300)

#World plot for pure alcohol
pure_alc_plot = ggplot() + 
  geom_map(data = map.world,map = map.world,colour='white',size=.1, aes(
    map_id = region,
    x=long,
    y=lat,
    fill=total_litres_of_pure_alcohol
  )) + 
  coord_quickmap()+
  scale_fill_continuous(name = "Liters of Pure Alcohol\nPer Person, 2010")+
  mytheme()
ggsave('pure_alc_plot.png',pure_alc_plot,dpi=300)

## TOTAL ALCOHOL CONSUMPATION 
map.world$total_alcohol = map.world$beer_servings+
                        map.world$spirit_servings+
                        map.world$wine_servings+
                        map.world$total_litres_of_pure_alcohol

total_alcohol_plot = ggplot()+
  geom_map(data=map.world, map=map.world , colour = 'white', size =.1, aes(
    map_id=region,
    x=long,
    y=lat,
    fill=total_alcohol
  ))+ coord_quickmap() + #scale_fill_continuous(name = 'Total Alcohol Servings \nPer Person, 2010')+
  mytheme()+ scale_fill_gradientn(colours = terrain.colors(10),name ='Total Alcohol Servings \nPer Person, 2010')
ggsave('total_alc_plot.png',total_alcohol_plot,dpi=300)

#Reshape data for bar plots
alf = map.world[2:6]
alf$country=as.character(alf$country)
alf=alf[!is.na(alf$country),]
alf_lf = unique(melt(alf))

library(dplyr)
bar_dat = alf_lf %>% group_by(country) %>%
  mutate(Total = sum(value)) 
bar_dat$variable=factor(bar_dat$variable,levels = c('beer_servings','spirit_servings',
                                                    'wine_servings','total_litres_of_pure_alcohol'),
                        labels = c('Beer','Spirit','Wine','Pure Alcohol'))
beer_bar = bar_dat[bar_dat$variable=='Beer',]
beer_bar = transform(beer_bar,country=reorder(country,value))

wine_bar = bar_dat[bar_dat$variable=='Wine',]
wine_bar = transform(wine_bar,country=reorder(country,value))

spirit_bar = bar_dat[bar_dat$variable=='Spirit',]
spirit_bar = transform(spirit_bar,country=reorder(country,value))

pure_bar = bar_dat[bar_dat$variable=='Pure Alcohol',]
pure_bar = transform(pure_bar,country=reorder(country,value))


bar_dat = transform(bar_dat,country=reorder(country,Total))
bar_dat=bar_dat[bar_dat$Total >quantile(x = bar_dat$Total,probs=1-15/100),]

mytheme_bar = function(){
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(colour="white"),
        axis.text.y=element_text(colour="white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        legend.background = element_rect(fill='black', 
                                         size=0.5, linetype="solid"),
        legend.text = element_text(colour = 'white'),
        legend.position = 'bottom',
        text=element_text(face = 'bold',family = 'Helvetica'),
        plot.title = element_text(
          colour = "white")
)
}


bar_plot_top15=ggplot(data = bar_dat,aes(country, value, fill=variable))+
  geom_bar(stat='identity')+
  coord_flip()+mytheme_bar()+
  labs(y='',x='Country')+
  labs(title='Number of Servings Per Person Per Year, 2010')
ggsave('top15_bar_total.png',plot = bar_plot_top15,dpi = 500)

beer_bar_plot = ggplot(data=beer_bar,aes(country, value, fill=variable))+
  geom_bar(stat='identity',position="dodge")+coord_flip()+mytheme_bar()+
  labs(y='',x='Country')+labs(title='Number of Servings of Beer Per Person Per Year, 2010')
ggsave('beer_plot_top15.png',beer_bar_plot,dpi=500)

wine_bar_plot = ggplot(data=wine_bar,aes(country, value, fill=variable))+
  geom_bar(stat='identity',position="dodge")+coord_flip()+mytheme_bar()+
  labs(y='',x='Country')+scale_fill_brewer(palette="Dark2")

spirit_bar_plot = ggplot(data=spirit_bar,aes(country, value, fill=variable))+
  geom_bar(stat='identity',position="dodge")+coord_flip()+mytheme_bar()+
  labs(y='',x='Country')+scale_fill_brewer(palette="Dark2")

pure_bar_plot = ggplot(data=pure_bar,aes(country, value, fill=variable))+
  geom_bar(stat='identity',position="dodge")+coord_flip()+mytheme_bar()+
  labs(y='',x='Country')+scale_fill_brewer(palette="Dark2")


## add in GDP

gdp = read.csv('gdpData.csv')
gdp$country = gdp$Country.Name
alldat = merge(gdp,alf_lf,by='country')

alldat=alldat %>% group_by(country) %>%
  mutate(Total = sum(value)) 

alldat$variable=factor(alldat$variable,levels = c('beer_servings','spirit_servings',
                                                    'wine_servings','total_litres_of_pure_alcohol'),
                        labels = c('Beer','Spirit','Wine','Pure Alcohol'))

scatter_all=ggplot(alldat,aes(x=log(X2010),y=value,color=variable))+
  geom_point(size=.5)+
  stat_smooth(method = "lm", se = FALSE,color='black')+
  facet_wrap(~variable,scales = 'free')+xlab('log(Gross Domestic Product)')+
  ylab('Number of Servings, Per Person')+theme_dark()+
  theme(legend.position="none",text=element_text(face = 'bold',family = 'Helvetica'))+
  scale_colour_brewer(palette = 'OrRd')
scatter_all

## cor

beer_cor =alldat[alldat$variable=='Beer',]
beer_R = cor(log(beer_cor$X2010),beer_cor$value,use = 'complete.obs')

wine_cor =alldat[alldat$variable=='Wine',]
wine_R = cor(log(wine_cor$X2010),wine_cor$value,use = 'complete.obs')

spirit_cor =alldat[alldat$variable=='Spirit',]
spirit_R = cor(log(spirit_cor$X2010),spirit_cor$value,use = 'complete.obs')

PureAlcohol_cor =alldat[alldat$variable=='Pure Alcohol',]
PureAlcohol_R = cor(log(PureAlcohol_cor$X2010),PureAlcohol_cor$value,use = 'complete.obs')
