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

#Se modifico la variable Clutch COmpletion para expresarla en español
penguins0$Clutch.Completion[penguins0$Clutch.Completion == 'Yes'] <- 'Si'

#Se modifico la variable de especies para resumirla
penguins0$Especies[penguins0$Especies == 'Adelie Penguin (Pygoscelis adeliae)'] <- 'Adelie'
penguins0$Especies[penguins0$Especies == 'Chinstrap penguin (Pygoscelis antarctica)'] <- 'Chinstrap'
penguins0$Especies[penguins0$Especies == 'Gentoo penguin (Pygoscelis papua)'] <- 'Gentoo'

# Bar Chart Distribucion pinguinos por isla
theme_set(theme_bw())
ggplot(penguins0, aes(x=factor(Island), fill=Island)) +
  geom_bar(col='black', show.legend = FALSE) +
  labs(title="Pingüinos por isla", x = 'Isla',y="No. de pingüinos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")


# Bar Chart Distribucion pinguinos por isla
theme_set(theme_bw())
ggplot(penguins0, aes(x=factor(Island), fill=Especies)) +
  geom_bar(col='black') +
  labs(title="Distribución de especies por isla", x = 'Isla',y="No. de pingüinos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")

# Bar Chart Distribucion pinguinos por especie
theme_set(theme_bw())
ggplot(penguins0, aes(x=factor(Especies), fill=Especies)) +
  geom_bar(col='black') +
  labs(title="Distribución de especies", x = 'Especie',y="No. de pingüinos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")


# Grafica Especies por isla cantidad
theme_set(theme_bw())
porEspecie<-penguins0 %>% group_by(Island,Especies) %>% summarise(cuenta=n())
grafica1<- ggplot(porEspecie,aes(y=cuenta, x=factor(Especies), fill=Especies)) +
  geom_bar(stat = "identity",col='black') +
  facet_wrap(facets = ~Island,drop = FALSE, nrow = 1) +
  labs(title="Distribución de especies por isla, cantidad", x = 'Isla',y="Cant. Pingüinos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science") +
  theme(axis.text.x = element_blank())
plot(grafica1)

# Grafica Especies por isla proporcion
theme_set(theme_bw())
porEspecie<-penguins0 %>% group_by(Island,Especies) %>% summarise(cuenta=n())
grafica2<- ggplot(porEspecie,aes(y=cuenta, x=factor(Island), fill=Especies)) +
  geom_bar(stat = "identity",col='black', position="fill") +
  labs(title="Distribución de especies por isla, proporción", x = 'Isla',y="Prop. Pingüinos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
plot(grafica2)



# Grafica pesos por especie
theme_set(theme_bw())
pesos<-penguins0 %>% group_by(Especies)
grafica3<- ggplot(pesos, aes(y=masaKg , x=factor(Especies), fill=Especies)) +
  geom_boxplot(col='black', show.legend = FALSE) +
  labs(title="Peso de pingüinos por especie", x = 'Especie',y="Peso (Kg)", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
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
  geom_boxplot(col='black', show.legend = FALSE) +
  labs(title="Tamaño de las aletas por especie", x = 'Especie',y="Tamaño de la aleta (cm)", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
plot(graficaEspecieAleta)

#Análisis Especie Adelie
Adelie = penguins0 %>% select(Especies, TamAleta, masaKg, Sex) %>% filter(Especies== "Adelie")
Adelie <- Adelie[!is.na(Adelie$TamAleta),]
Adelie <- Adelie[!Adelie$Sex=="",]
sumAdelie<-summarise(Adelie, PromedioAleta = mean(TamAleta), MedianaAleta=median(TamAleta), MaximoAleta=max(TamAleta), MinimoAleta=min(TamAleta),PromedioPeso = mean(masaKg), MedianaPeso=median(masaKg), MaximoPeso=max(masaKg), MinimoPeso=min(masaKg), Hembras=sum(Adelie$Sex=="FEMALE"), Machos=sum(Adelie$Sex=="MALE"))

#Análisis Especie Chinstrap
Chinstrap = penguins0 %>% select(Especies, TamAleta, masaKg, Sex) %>% filter(Especies== "Chinstrap")
Chinstrap <- Chinstrap[!is.na(Chinstrap$TamAleta),]
Chinstrap <- Chinstrap[!Chinstrap$Sex=="",]
sumChinstrap<-summarise(Chinstrap, PromedioAleta = mean(TamAleta), MedianaAleta=median(TamAleta), MaximoAleta=max(TamAleta), MinimoAleta=min(TamAleta),PromedioPeso = mean(masaKg), MedianaPeso=median(masaKg), MaximoPeso=max(masaKg), MinimoPeso=min(masaKg), Hembras=sum(Chinstrap$Sex=="FEMALE"), Machos=sum(Chinstrap$Sex=="MALE"))

#Análisis Especie Gentoo
Gentoo = penguins0 %>% select(Especies, TamAleta, masaKg, Sex) %>% filter(Especies== "Gentoo")
Gentoo <- Gentoo[!is.na(Gentoo$TamAleta),]
Gentoo <- Gentoo[!Gentoo$Sex=="",]
Gentoo <- Gentoo[!Gentoo$Sex==".",]
sumGentoo<-summarise(Gentoo, PromedioAleta = mean(TamAleta), MedianaAleta=median(TamAleta), MaximoAleta=max(TamAleta), MinimoAleta=min(TamAleta),PromedioPeso = mean(masaKg), MedianaPeso=median(masaKg), MaximoPeso=max(masaKg), MinimoPeso=min(masaKg), Hembras=sum(Gentoo$Sex=="FEMALE"), Machos=sum(Gentoo$Sex=="MALE"))

#Comparar peso vs tamaño de aleta solo para las hembras segun especie
Hembras = penguins0 %>% select(Especies, TamAleta, masaKg, Sex) %>% filter(Sex=="FEMALE")
theme_set(theme_bw())
graficaHembrasPT<- ggplot(Hembras, aes(y=masaKg , x=TamAleta , col=Especies)) +
  geom_point() +
  labs(title="Peso de pingüino vs Tamaño de aleta de las hembras según Especie", x = 'Tamaño de aleta',y="Peso (Kg)", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
plot(graficaHembrasPT)

#Comparar peso vs tamaño de aleta solo para los machos segun especie
Machos = penguins0 %>% select(Especies, TamAleta, masaKg, Sex) %>% filter(Sex=="MALE")
theme_set(theme_bw())
graficaMachosPT<- ggplot(Machos, aes(y=masaKg , x=TamAleta , col=Especies)) +
  geom_point() +
  labs(title="Peso de pingüino vs Tamaño de aleta de los machos según Especie", x = 'Tamaño de aleta',y="Peso (Kg)", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
plot(graficaMachosPT)


 

# Nidada por especie: Describe que especie de pinguinos es mas fertil
BaseDeNidos = penguins0 %>% group_by(Especies, Clutch.Completion) %>% summarise(cuenta=n())
GraficoFertilidad <- ggplot(BaseDeNidos, aes(y=cuenta, x=factor(Clutch.Completion), fill= Especies))+ 
  geom_bar(stat = "identity",col='black') +
  facet_wrap(facets = ~Especies,drop = FALSE, nrow = 1) +
  labs(title="Fecundidad total de cada especie", x ="Pingüinos con y sin nidos de cada especie" , y = "No. de pingüinos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
plot(GraficoFertilidad)

#Gráfico de barras de pinguinos fertiles: Cantidad de nidos que contiene por lo menos un huevo.
FertilidadPorEspecie<-filter(BaseDeNidos, Clutch.Completion =="Si")  #Filtrado de nidos con más de un huevo
Fertilidad<-ggplot(FertilidadPorEspecie, aes(x=factor(Especies), y=cuenta, fill=Especies))+
  geom_bar(stat = "identity", col='black')+
  labs(title="Fertilidad por especie", x ="Nidos con más de un huevo" , y = "Cant. de nidos", caption="Primer Proyecto: Pingüinos, Introducción a Data Science") +
  theme(axis.text.x = element_blank())
plot(Fertilidad)

#Gráfico de barras de pinguinos infertiles: Nido que no contiene ningún huevo.
InfertilidadPorEspecie<-filter(BaseDeNidos, Clutch.Completion =="No")  #Filtrado de nidos vacios
Infertilidad<-ggplot(InfertilidadPorEspecie, aes(x=factor(Especies), y=cuenta, fill=Especies))+
  geom_bar(stat = "identity", col='black')+
  labs(title="Infertilidad por especie de pinguinos", x ="Nidos vacios" , y = "Cant. de nidos vacios", caption="Primer Proyecto: Pingüinos, Introducción a Data Science") +
  theme(axis.text.x = element_blank())
plot(Infertilidad)

#Gráfico Peso por Isla: Permite deducir que Isla tiene más recursos naturales. 
theme_set(theme_bw())
GraficaPesoporIsla<- ggplot(penguins0, aes(x=Island , y=masaKg, fill=Island)) +
  geom_boxplot(col='black', show.legend = FALSE) +
  labs(title="Promedio de peso por Isla", x = 'Isla',y="Peso promedio (Kg)", caption="Primer Proyecto: Pingüinos, Introducción a Data Science")
plot(GraficaPesoporIsla)
 

