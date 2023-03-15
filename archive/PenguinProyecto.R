#setup
library(dplyr)
library(ggplot2)
library(ggthemes)


penguins <- read.csv('penguins.csv')
  # Variable de dataset

penguins0 <- penguins
penguins0$Especies <- penguins0$Species
  # Se creo la variable penguins0 para cambiarle el nombre a la variable de "Species" y que dijera "Especies" 
  # -en las graficas en lugar del nombre en ingles
penguins0$masaKg <- penguins0$Body.Mass..g. *1/1000
  #Se creo la variable "masaKg" para poner peso en Kg en lugar de gramos


# Bar Chart Distribucion pinguinos por isla
theme_set(theme_bw())
ggplot(penguins, aes(x=factor(Island))) +
  geom_bar(col='black',fill='lightblue') +
  labs(title="Pingüinos por isla", x = 'Isla',y="Pingüinos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")

# Grafica Especies por isla
theme_set(theme_bw())
porEspecie<-penguins0 %>% group_by(Island,Especies) %>% summarise(cuenta=n())
grafica1<- ggplot(porEspecie,aes(y=cuenta, x=factor(Especies), fill=Especies)) +
  geom_bar(stat = "identity",col='black') +
  facet_wrap(facets = ~Island,drop = FALSE, nrow = 1) +
  labs(title="Distribución de especies por isla", x = 'Isla',y="Cant. Pingüinos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science") +
  theme(axis.text.x = element_blank())
plot(grafica1)

# Grafica Especies por isla
theme_set(theme_bw())
porEspecie<-penguins0 %>% group_by(Island,Especies) %>% summarise(cuenta=n())
grafica2<- ggplot(porEspecie,aes(y=cuenta, x=factor(Island), fill=Especies)) +
  geom_bar(stat = "identity",col='black', position="fill") +
  labs(title="Distribución de especies por isla", x = 'Isla',y="Prop. Pingüinos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
plot(grafica2)



# Grafica pesos por especie
theme_set(theme_bw())
pesos<-penguins0 %>% group_by(Especies)
grafica3<- ggplot(pesos, aes(y=masaKg , x=factor(Especies), fill=Especies)) +
  geom_boxplot(col='black') +
  labs(title="Peso de pingüinos por especie", x = 'Especie',y="Peso en Kg", caption="Primer Proyecto: Pingüinos, Introducción a Data Science") +
  theme(axis.text.x = element_blank())
plot(grafica3)

# Graficas pesos por pico
  # Grafica Peso y Largo
  theme_set(theme_bw())
  grafica4.1<- ggplot(penguins0, aes(y=masaKg , x=Culmen.Length..mm. , col=Especies)) +
    geom_point() +
    labs(title="Peso de pingüino vs Largo de pico", x = 'Largo de pico en mm',y="Peso en Kg", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
  plot(grafica4.1)
  
  # Grafica Peso y Altura
  theme_set(theme_bw())
  grafica4.2<- ggplot(penguins0, aes(y=masaKg , x=Culmen.Depth..mm. , col=Especies)) +
    geom_point() +
    labs(title="Peso de pingüino vs Altura de pico", x = 'Altura de pico en mm',y="Peso en Kg", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
  plot(grafica4.2)
  
  # Grafica Altura y Largo de pico
  theme_set(theme_bw())
  grafica4.3<- ggplot(penguins0, aes(y=Culmen.Depth..mm. , x=Culmen.Length..mm. , col=Especies)) +
    geom_point() +
    labs(title="Altura de pico vs Largo de pico", x = 'Largo de pico en mm',y='Altura de pico en mm', caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
  plot(grafica4.3)

