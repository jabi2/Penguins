#setup
library(dplyr)
library(ggplot2)
library(ggthemes)

penguins <- read.csv('penguins.csv')
ggplot(penguins, aes(x=factor(Island))) +
  geom_bar()
