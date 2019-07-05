# install_packages("readr")
library(readr)
descargas_R <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-03/descargas_R.csv")

library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(grid)
#basic ggplot

#Pasar a día de la semana
dia.numerico <- as.POSIXlt(descargas_R$fecha)$wday + 1

semana <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

dia <- factor(semana[dia.numerico],  levels=semana, labels = semana)

#Sumar descargas por día
descargas.dia <- cbind(descargas_R, dia, dia.numerico)  %>%  
  group_by(dia, dia.numerico, sistema_operativo) %>% 
  summarise(descargas = sum(descargas, na.rm = TRUE)) %>% 
  filter (is.na(sistema_operativo)==FALSE)

#Valores para la lellenda
my_breaks = c(400, 1000, 2500, 5000, 15000)

#ggplot
descargas.dia %>%  ggplot(aes(x=dia,y=sistema_operativo,fill=descargas)) + #agregar borde
  geom_tile(colour="gray",size=0.25) +
  coord_fixed() + #Incrementar todos los tamaños de letra
 
  theme( plot.caption = element_text(size=6))  +
  theme_wsj() +
  scale_fill_viridis(tran = "log",  option= "magma",
     breaks = my_breaks, labels = my_breaks,
     labs(fill = "Número"),
     guide = guide_legend(direction = "horizontal",
        title.position = "top",
        label.position = "bottom",
        label.hjust = 0.5,
        label.vjust = 1,
        label.theme = element_text(angle = 90)))  + 
  ggtitle("Descargas de R por S.O.") +
  labs( caption = "Descargas de junio 2019", size=5)



  #theme(plot.background = element_rect(fill = 'black'))
