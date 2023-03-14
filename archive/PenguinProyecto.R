#setup
library(dplyr)
library(ggplot2)
library(ggthemes)


penguins <- read.csv('penguins.csv')
  #Variable de dataset

ggplot(penguins, aes(x=factor(Island))) +
  geom_bar()
