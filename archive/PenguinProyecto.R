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

#Se le cambio el nombre a la variable del tamaño de la aleta y se expresó en centímetros
penguins0$TamAleta <- penguins0$Flipper.Length..mm. *1/10

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
  labs(title="Peso de pingüinos por especie", x = 'Especie',y="Peso (Kg)", caption="Primer Proyecto: Pingüinos, Introducción a Data Science") +
  theme(axis.text.x = element_blank())
plot(grafica3)

# Graficas pesos por pico
  # Grafica Peso y Largo
  theme_set(theme_bw())
  grafica4.1<- ggplot(penguins0, aes(y=masaKg , x=Culmen.Length..mm. , col=Especies)) +
    geom_point() +
    labs(title="Peso de pingüino vs Largo de pico", x = 'Largo de pico (mm)',y="Peso (Kg)", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
  plot(grafica4.1)
  
  # Grafica Peso y Altura
  theme_set(theme_bw())
  grafica4.2<- ggplot(penguins0, aes(y=masaKg , x=Culmen.Depth..mm. , col=Especies)) +
    geom_point() +
    labs(title="Peso de pingüino vs Altura de pico", x = 'Altura de pico (mm)',y="Peso (Kg)", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
  plot(grafica4.2)
  
  # Grafica Altura y Largo de pico
  theme_set(theme_bw())
  grafica4.3<- ggplot(penguins0, aes(y=Culmen.Depth..mm. , x=Culmen.Length..mm. , col=Especies)) +
    geom_point() +
    labs(title="Altura de pico vs Largo de pico", x = 'Largo de pico (mm)',y='Altura de pico (mm)', caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
  plot(grafica4.3)

#Gráfica Tamaño de aleta por especie 
theme_set(theme_bw())
graficaEspecieAleta<- ggplot(penguins0, aes(y=TamAleta , x=factor(Especies), fill=Especies)) +
  geom_boxplot(col='black') +
  labs(title="Tamaño de las aletas por especie", x = 'Especie',y="Tamaño de la aleta (cm)", caption="Primer Proyecto: Pingüinos, Introducción a Data Science") +
  theme(axis.text.x = element_blank())
plot(graficaEspecieAleta)

#Análisis Especie Adelie
Adelie = penguins0 %>% select(Especies, TamAleta, masaKg, Sex) %>% filter(Especies== "Adelie Penguin (Pygoscelis adeliae)")
Adelie <- Adelie[!is.na(Adelie$TamAleta),]
Adelie <- Adelie[!is.null(Adelie$Sex),]
sumAdelie<-summarise(Adelie, PromedioAleta = mean(TamAleta), MedianaAleta=median(TamAleta), MaximoAleta=max(TamAleta), MinimoAleta=min(TamAleta),PromedioPeso = mean(masaKg), MedianaPeso=median(masaKg), MaximoPeso=max(masaKg), MinimoPeso=min(masaKg), Hembras=sum(Adelie$Sex=="FEMALE"), Machos=sum(Adelie$Sex=="MALE"))

#Análisis Especie Chinstrap
Chinstrap = penguins0 %>% select(Especies, TamAleta, masaKg, Sex) %>% filter(Especies== "Chinstrap penguin (Pygoscelis antarctica)")
Chinstrap <- Chinstrap[!is.na(Chinstrap$TamAleta),]
sumChinstrap<-summarise(Chinstrap, PromedioAleta = mean(TamAleta), MedianaAleta=median(TamAleta), MaximoAleta=max(TamAleta), MinimoAleta=min(TamAleta),PromedioPeso = mean(masaKg), MedianaPeso=median(masaKg), MaximoPeso=max(masaKg), MinimoPeso=min(masaKg), Hembras=sum(Chinstrap$Sex=="FEMALE"), Machos=sum(Chinstrap$Sex=="MALE"))

#Análisis Especie Gentoo
Gentoo = penguins0 %>% select(Especies, TamAleta, masaKg, Sex) %>% filter(Especies== "Gentoo penguin (Pygoscelis papua)")
Gentoo <- Gentoo[!is.na(Gentoo$TamAleta),]
sumGentoo<-summarise(Gentoo, PromedioAleta = mean(TamAleta), MedianaAleta=median(TamAleta), MaximoAleta=max(TamAleta), MinimoAleta=min(TamAleta),PromedioPeso = mean(masaKg), MedianaPeso=median(masaKg), MaximoPeso=max(masaKg), MinimoPeso=min(masaKg), Hembras=sum(Gentoo$Sex=="FEMALE"), Machos=sum(Gentoo$Sex=="MALE"))

# Nidada por especie: Describe que especie de pinguinos es mas fertil
BaseDeNidos = penguins0 %>% group_by(Especies, Clutch.Completion) %>% summarise(cuenta=n())
GraficoFertilidad <- ggplot(BaseDeNidos, aes(y=cuenta, x=factor(Clutch.Completion), fill= Especies))+ 
  geom_bar(stat = "identity",col='black') +
  facet_wrap(facets = ~Especies,drop = FALSE, nrow = 1) +
  labs(title="Fertilidad de cada especie", x ="Nidos de la población total de pinüinos" , y = "Cant. de nidos")+
  theme(axis.text.x = element_blank())
plot(GraficoFertilidad)

#Pinguinos fertiles: tienen un nido
FertilidadPorEspecie<-BaseDeNidos%>%group_by(Especies,Clutch.Completion=="Yes")


