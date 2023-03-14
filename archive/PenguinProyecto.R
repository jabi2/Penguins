#setup
library(dplyr)
library(ggplot2)
library(ggthemes)


penguins <- read.csv('penguins.csv')
  #Variable de dataset

#Bar Chart Distribucion pinguinos por isla
theme_set(theme_bw())
ggplot(penguins, aes(x=factor(Island))) +
  geom_bar(col='black',fill='lightblue') +
  labs(title="Pingüinos por isla", x = 'Isla',y="Pingüinos", caption="Proyecto pingüinos")

# Grafica Especies por isla
theme_set(theme_bw())
porEspecie<-penguins %>% group_by(Island,Species) %>% summarise(cuenta=n())
gg2<- ggplot(porEspecie,aes(y=cuenta, x=factor(Species), fill=Species))
gg2<- gg2+geom_bar(stat = "identity")
gg2<- gg2+facet_wrap(facets = ~Island,drop = FALSE, nrow = 1)
gg2<- gg2+labs(title="Distribución de especies por isla", x = 'Isla',y="Pingüinos", caption="Proyecto pingüinos") +
  theme(axis.text.x = element_blank())
plot(gg2)
