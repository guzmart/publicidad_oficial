# Paquetes ----
require(pacman)
p_load(
  tm, tidytext, # text mining
  readxl, # leer excel
  lubridate, zoo, # fechas
  haven, foreign, # abrir bd
  gganimate, ggcorrplot, gridExtra, ggthemes,hrbrthemes, magick, scales, RColorBrewer, # Animación y elementos extras para ggplot
  beepr, # sonido de victoria
  tidyverse, devtools # infaltables
)

# Este comando deshabilita la notación científica.
options(scipen=999)

# Directorios ----
inp <- "R/publicidad_oficial/01_datos/"
out <- "R/publicidad_oficial/03_gráficas/"

# Datos ----
# * Deflactor ----
inpc2019 <- 4.5446
# Crear una base de datos que contenga años y deflactores (precios constantes (2019))
defs <- 
  data.frame(
    año = as.character(2013:2018),
    def = c(
      3.96/inpc2019,
      4.08/inpc2019,
      2.13/inpc2019,
      3.36/inpc2019,
      6.77/inpc2019,
      1.0386
    )
  )

# * COMSOC ----
comsoc <- read_excel(paste0(inp, "comsoc_2013_2018.xlsx"))
colnames(comsoc) <- tolower(colnames(comsoc))
# Limpieza y deflactación
comsoc_def <- data.frame()
for(i in 2013:2018){
  tempo <- subset(comsoc, año==i) %>% # repetir proceso para cada año
    mutate(
      apf = case_when(
        str_detect(nombre, ( 'SECRETARÍA' )) ~ 1,
        str_detect(nombre, ( 'CONSEJERÍA JURÍDICA' )) ~ 1,
        str_detect(nombre, ( 'PRESIDENCIA' )) ~ 1,
        str_detect(nombre, ( 'COMISIÓN NACIONAL DE HIDROCARBUROS' )) ~ 1,
        str_detect(nombre, ( 'COMISIÓN REGULADORA DE ENERGÍA' )) ~ 1,
        TRUE ~ 0),
      importe = importe*subset(defs[,2], defs$año == i)) %>% 
    subset(apf==1)
  # Limpiar nombres de columnas
  colnames(tempo) <- gsub("\\.", "", colnames(tempo))
  colnames(tempo) <- gsub(" ", "_", colnames(tempo))
  colnames(tempo) <- gsub("/", "_", colnames(tempo))
  comsoc_def <- bind_rows(comsoc_def, tempo)
  rm(tempo)
}

# * Encuestas (Parametría) ----
listaaa <- 
  c(
    "N201212.sav", "N201301.sav","N201304.sav","N201305.sav","N201306.sav","N201308.sav","N201309.sav",
    "N201311.sav","N201312.sav","N201401.sav","N201402.sav","N201403.sav","N201404.sav","N201405.sav",
    "N201406.sav","N201407.sav","N201408.sav","N201409.sav","N201410.sav","N201411.sav","N201412.sav",
    "N201501.sav","N201502.sav","N201503.sav","N201504.sav","N201505.sav","N201506.sav","N201507.sav",
    "N201508.sav","N201509.sav","N201510.sav","N201511.sav","N201601.sav","N201602.sav","N201603.sav",
    "N201604.sav","N201605.sav","N201606.sav","N201607.sav","N201608.sav","N201609.sav","N201610.sav",
    "N201612.sav","N201701.sav","N201703.sav","N201704.sav","N201705.sav","N201706.sav","N201707.sav",
    "N201708.sav","N201709.sav","N201710.sav","N201712.sav","N201801.sav","N201802.sav","N201803.sav",
    "N201804.sav","N201805.sav","N201806.sav","N201807.sav","N201808.sav","N201809.sav"
  )

parametria <- data.frame()
for(i in 1:length(listaaa)){
  tempo <- read_sav(paste0(inp,"encuestas/",listaaa[i])) %>% 
    mutate(año_mes = str_sub(listaaa[i],2,7),
           id = str_sub(listaaa[i],end = -5),
           sexo = ifelse(sexo==1,"Hombre","Mujer"), # Hombre = 0, Mujer = 1
           apr_pres_gr = ifelse(apr_pres==1, "Aprueba", 
                             ifelse(apr_pres==2, "Aprueba",
                                    ifelse(apr_pres==3,"Desaprueba",
                                           ifelse(apr_pres==4,"Desaprueba",NA)))),
           edad_gr = ifelse(edad<30,"Jóvenes\n(18 a 29 años)",
                            ifelse(edad>29&edad<45,"Adultos I\n(30 a 44 años)",
                                   ifelse(edad>44&edad<60,"Adultos II\n(45 a 59 años)",
                                          ifelse(edad>59&edad<98,"Adultos mayores (60 años o más)",NA))))) %>% 
    select(id, año_mes, sexo, edad, edad_gr, apr_pres, apr_pres_gr, pond)
    
  parametria <- bind_rows(parametria,tempo)
  rm(tempo)
}
rm(i,defs,inpc2019)

# Análisis ----
# * Gasto público en Publicidad Oficial ----
apo_gr <- comsoc_def %>% 
  group_by(mes=floor_date(fecha_de_gasto, "month")) %>% 
  summarise(importe = sum(importe),
            iva = sum(iva)) %>% 
  mutate(total = importe + iva,
         año = floor_date(mes, "year")) %>% 
  ungroup() %>% 
  select(año, mes, total)

fiuf <- "Gasto en Publicidad Oficial"
fiuff <- "Administración Pública Centralizada"
fiuffi <- "Fuente: COMSOC | @guzmart_ | @sandra_juand"
ggplot(data = subset(apo_gr,mes<as.Date.character("2018-12-01",format = "%Y-%m-%d") &
                       mes>as.Date.character("2012-12-31",format = "%Y-%m-%d")),
       aes(x = as.Date(mes),
           y = total/1000000,
           fill = as.numeric(total/1000000))) +
  geom_col(show.legend = FALSE) +
  labs(title = fiuf,
       subtitle = fiuff,
       caption = fiuffi) +
  xlab("Fecha") + ylab("millones de pesos") +
  theme_ipsum(grid="Y") +
  scale_fill_distiller(palette="Spectral") +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%Y-%m"),
               limits = c(as.Date.character("2013-01-01",format = "%Y-%m-%d"),
                          as.Date.character("2018-12-01",format = "%Y-%m-%d"))) +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 30),
        plot.caption = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(angle = 90,vjust = 0.5),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18))
ggsave(filename = paste0(out,"01_gasto_po_apo.png"),
       width = 15, height = 10, dpi = 100)

presidencia <- filter(comsoc_def, str_detect(nombre, "PRESIDENCIA")) %>% 
  group_by(mes=floor_date(fecha_de_gasto, "month")) %>% 
  summarise(importe = sum(importe),
            iva = sum(iva)) %>% 
  mutate(total = importe + iva,
         año = floor_date(mes, "year")) %>% 
  ungroup()

fiuf <- "Gasto en Publicidad Oficial"
fiuff <- "Presidencia de la República"
fiuffi <- "Fuente: COMSOC | @guzmart_ | @sandra_juand"
ggplot(data = presidencia,
       aes(x = as.Date(mes),
           y = (total/1000000),
           fill = total/1000000)) +
  geom_col(show.legend = FALSE) +
  labs(title = fiuf,
       subtitle = fiuff,
       caption = fiuffi) +
  xlab("Fecha") + ylab("millones de pesos")  +
  theme_ipsum(grid="Y") +
  scale_fill_distiller(palette="Spectral") +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%Y-%m"),
               limits = c(as.Date.character("2013-01-01",format = "%Y-%m-%d"),
                          as.Date.character("2018-12-01",format = "%Y-%m-%d"))) +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 30),
        plot.caption = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(angle = 90,vjust = 0.5),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18))
ggsave(filename = paste0(out,"02_gasto_po_presi.png"),
       width = 15, height = 10, dpi = 100)

# * Aprobación presidencial ----
ids <- levels(as.factor(parametria$id))
# por mes
apr_pres <- data.frame()
for(i in 1:length(ids)){
  tempo <- subset(parametria,id==ids[i]) %>% 
    drop_na() %>% 
    group_by(apr_pres_gr) %>% 
    summarise(tot = sum(pond, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(fac = sum(tot),
           prop = tot/fac*100,
           año_mes = 
               as.Date.character(
                 paste0(
                   str_sub(listaaa[i],2,5),"-",
                   str_sub(listaaa[i],6,7),"-01"),
                 format = "%Y-%m-%d"
               )) %>% 
    select(año_mes, apr_pres_gr, prop)
  apr_pres <- bind_rows(apr_pres, tempo)
  rm(tempo)
}

fiuf <- "Aprobación presidencial"
fiuff <- "En general, ¿usted aprueba o desaprueba la forma en que Enrique Peña Nieto realiza\nsu trabajo? "
fiuffi <- "Fuente: Parametría | @guzmart_ | @sandra_juand"
ggplot(apr_pres,
       aes(x=año_mes,
           y = prop,
           col = apr_pres_gr,
           group = apr_pres_gr)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin=prop-3.3,ymax=prop+3.3,fill=apr_pres_gr),
              alpha=0.2, col = NA, show.legend = F) +
  scale_color_manual("",
                     values = c("#52854C", "#A42820")) +
  scale_fill_manual(values = c("#52854C", "#A42820")) +
  scale_x_date(date_breaks = "3 month", 
               labels = date_format("%Y-%m"),
               limits = c(as.Date.character("2013-01-01",format = "%Y-%m-%d"),
                          as.Date.character("2018-09-01",format = "%Y-%m-%d"))) +
  labs(title = fiuf,
       subtitle = fiuff,
       caption = fiuffi) +
  xlab("Fecha") + ylab("%")  +
  ylim(0,100) +
  theme_ipsum(grid="Y") +
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 30),
        plot.caption = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(angle = 90,vjust = 0.5),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        legend.position = "bottom",
        legend.text = element_text(size = 20))
ggsave(filename = paste0(out,"03_apr_pres.png"),
       width = 15, height = 10, dpi = 100)


