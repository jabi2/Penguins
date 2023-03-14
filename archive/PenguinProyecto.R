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
  labs(title="Distribución de pingüinos por isla", x = 'Isla',y="Pingüinos", caption="Proyecto pingüinos")
