
library(sf)
library(raster)
# Zona de Interes
Zona = st_read("shp/Manutata.geojson")
Zona_py <- st_transform(Zona ,
                        crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Viver = st_read("shp/Vivero.geojson")
Viver_py <- st_transform(Viver ,
                         crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Per           <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
MDD           <- subset(Per, NAME_1  == "Madre de Dios")
Tambopata <- subset(Per, NAME_3 == "Las Piedras")

require(pacman)
pacman::p_load(tidyverse,sf,ggplot2, ggspatial,sp,osmdata,leaflet, ggmap )
available_tags("highway")

mad_map <- get_map(getbb("Madre de Dios"), maptype = "toner-background") # Localizamos madre de Dios
Via_Mal <- st_read ("SHP/Red_vial.geojson") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
Rio_Pol <- st_read ("SHP/Rio_Poli.geojson") # Caragmos un shp de puerto maldonado
Rio_Poli  <- st_transform(Rio_Pol ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
Puer_Mal <- st_read ("SHP/Area_puerto.shp") # Caragmos un shp de puerto maldonado
Puer_Mal <- st_transform(Puer_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

CP <- st_read ("SHP/CP.geojson") # Caragmos un shp de puerto maldonado
CetroPo  <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CetroPo_xy <- cbind(CetroPo, st_coordinates(st_centroid(CetroPo$geometry)))
# Extrayendo información de OSM
Puer_Maldonado <- opq(Puer_Mal) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf() # Informacion de osm del area

Puer_Maldonado_rios <- opq(Puer_Mal)%>% add_osm_feature(key = "waterway", value = "river") %>%osmdata_sf()
Puer_Maldonado_secu <- opq(Puer_Mal)%>% add_osm_feature(key = "highway",
                                                        value = c("residential", "living_street",
                                                                  "unclassified",
                                                                  "service", "footway")) %>% osmdata_sf()
calles <- Puer_Maldonado$osm_lines
calles <- st_intersection(calles, Puer_Mal )
calles <- calles %>%
  mutate(maxspeed = as.numeric(maxspeed),
         lanes = ifelse(is.na(lanes), 1, as.numeric(lanes)))
#  Cargamos las Librerias ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse,rnaturalearth,rnaturalearthdata,
               sf, reticulate,maptools,maps, ggplot2 ,ggspatial, rgeos, ggmap , leaflet)

# Extraemos los datos raster de Precipitacion -----------------------------------------------

Prec        <- getData("worldclim", var = "prec", 
                       res=0.5, lon=-74.8773, lat=-11.54012)

Prec_MDD    <- crop(Prec, MDD)
Prec_MDD    <- Prec_MDD <- mask(Prec_MDD,MDD)
summary(Prec_MDD)
PPAnual_MDD = Prec_MDD*10
PPAnual_MD <- do.call("sum", unstack(Prec_MDD))
plot(PPAnual_MD)


# Elaboramos los meses Para precipitacion-----------------------------------------
vls         <- rasterToPoints(PPAnual_MD) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) 

summary(vls$value)
# Elaboramos los mapas  ----------------------------------------------------------
colores2<- c('#9331dc', '#165dff', '#10aebe', '#00ffff', '#ffee21', '#f19e21', '#ff4223')
colores1<- c('#ff4223','#f19e21','#ffee21','#00ffff', '#10aebe', '#165dff','#9331dc')

MDD_GG=ggplot(vls)  +
  geom_tile(aes(x = x, y =  y, fill = value)) +
  scale_fill_gradientn(colours = colores1, 
                       breaks = c(2000, 2100, 2200, 2300,2400, 2500),
                       na.value = 'white',
                       name='Precipitación \n(mm)') +
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data = Peru , fill=NA, color="black", size=0.01)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = MDD, fill=NA, color="black", size=0.5)+
  geom_sf(data = Tambopata, fill=NA, color="black", size=0.5)+
  geom_sf(data = Zona_py, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -71, y = -13.4, hjust = 0, vjust = 1, 
           label = "Madre de Dios",size = 4, family="serif", color = 
             "black",  fontface="italic")


MDD_CLIM= ggplot()+
  geom_raster(data= vls, aes(x = x, y =  y, fill = value)) +
  scale_fill_gradientn(colours = colores1, 
                       breaks = c(2000, 2100, 2200, 2300,2400, 2500),
                       na.value = 'white',
                       name='Precipitación \n(mm)')+
  geom_sf_label(data = MDD, aes(label=MDD$NAME_3), size=1.5)+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data = Peru , fill=NA, color="black", size=0.01)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = MDD, fill=NA, color="black", size=0.5)+
  geom_sf(data = Tambopata, fill=NA, color="black", size=0.5)+
  geom_sf(data = Zona_py, fill="black", color="black")+
  coord_sf(xlim = c(-69.77802, -68.86679), ylim = c(-13.00461 ,-12.20361)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.7, y = -12.9, hjust = 0, vjust = 1, 
           label = "Inambari",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.2, y = -12.9, hjust = 0, vjust = 1, 
           label = "Tambopata",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.2, y = -12.4, hjust = 0, vjust = 1, 
           label = "Las Piedras",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.8, y = -12.6, hjust = 0, vjust = 1, 
           label = "Laberinto",size = 3, family="serif", color = 
             "black",  fontface="italic")

library(elevatr)
elev = get_elev_raster(Puer_Mal, z=12)

plot(elev)
Poligo_alt    <- crop(elev, Puer_Mal)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Puer_Mal)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


library(ggnewscale) 
library(ggspatial)


SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="gray", color="black")+
  geom_sf(data = MDD, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")

Prec_MDD    <- crop(Prec, Puer_Mal)
Prec_MDD    <- Prec_MDD <- mask(Prec_MDD,Puer_Mal)
summary(Prec_MDD)
PPAnual_MDD = Prec_MDD*10
PPAnual_MD <- do.call("sum", unstack(Prec_MDD))
plot(PPAnual_MD)


# Elaboramos los meses Para precipitacion-----------------------------------------
vls         <- rasterToPoints(PPAnual_MD) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) 
summary(vls$value)
DD= ggplot() +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data= vls, aes(x = x, y =  y, fill = value), alpha=0.6) +
  scale_fill_gradientn(colours = colores1, 
                       breaks = c(2000, 2050, 2100, 2150,2200, 2264),
                       na.value = 'white',
                       name='Precipitación \n(mm)')+
  geom_sf(data = calles,inherit.aes = FALSE,aes(color = maxspeed), size = 1, alpha = .4,
          show.legend = F) +
  scale_color_viridis_c() +
  geom_sf(data = filter(calles, str_detect(name, "Avenida")), color = "salmon") +
  geom_sf(data = Puer_Maldonado_secu$osm_lines,inherit.aes = FALSE,
          color = "gray", size = .5,alpha = .6) +
  geom_sf(data = Puer_Maldonado_rios$osm_lines,
          inherit.aes = FALSE, color = "blue",size = .5,alpha = .5) +
  #geom_point(data =Foco , aes(x = longitude, y = latitude),size=1.5, color="red", pch=21, fill="red")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=1.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_label(data = CetroPo_xy , aes(label = NOMBCP ), 
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
                size = 2, face = "bold",color = 'black',
                point.padding = unit(0.9, "lines"))+
  geom_sf(data = Zona_py, fill=NA, color="black")+
  geom_sf(data = Viver_py, fill=NA, color="black")+
  
  coord_sf(xlim = c(-69.20, -69.085), ylim = c(-12.605 ,-12.43)) +
  labs(color = '',  x = 'Longitud', y = 'Latitud')+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=11, family="serif"),
        legend.title = element_text(size=11, family="serif", face='bold',hjust=0.5),
        legend.key.width = unit(2.5,"line"), #ancho de cuadrados de referencia 
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  annotate(geom = "text", x = -69.21, y = -12.41, hjust = 0, vjust = 1, 
           label = "C) Zona de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  ggspatial::annotation_north_arrow(
    location = "rl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="black"))

Alt_Vive= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data= vls, aes(x = x, y =  y, fill = value), alpha=0.6) +
  scale_fill_gradientn(colours = colores1, 
                       breaks = c(2000, 2050, 2100, 2150,2200, 2264),
                       na.value = 'white',
                       name='Precipitación \n(mm)')+
  geom_sf(data = Viver_py, fill=NA, color="black")+
  
  coord_sf(xlim = c(-69.14034,-69.11236), ylim = c(-12.47958 ,-12.46336)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

Alt_Manutata= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data= vls, aes(x = x, y =  y, fill = value), alpha=0.6) +
  scale_fill_gradientn(colours = colores1, 
                       breaks = c(2000, 2050, 2100, 2150,2200, 2264),
                       na.value = 'white',
                       name='Precipitación \n(mm)')+
  geom_sf(data = Zona_py, fill=NA, color="black")+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  coord_sf(xlim = c(-69.16802,-69.14937), ylim = c(-12.51762 ,-12.50666)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))


col<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")

Eleva_puer = ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = col, 
                       breaks = c(0,80,100,120,180, 200, 250),
                       na.value = 'white',
                       labels = c("[0 - 79] ","[80 - 99]", "[100 - 119]",
                                  "[120 - 179]", "[180 - 199]", "[200 - 249]",
                                  "[250 - 300]"),
                       name='Elevacion \n(msnm)')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=1.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf(data = Zona_py, fill=NA, color="black")+
  geom_sf(data = Viver_py, fill=NA, color="black")+
  
  coord_sf(xlim = c(-69.20, -69.085), ylim = c(-12.605 ,-12.43)) +
  theme_classic()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin = unit(c(0,0,0,0), "cm"),
    plot.margin = unit(c(0,0,0,0), "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none", 
    
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect( color = "grey20", fill = NA, size = 1))


library(cowplot)
W =ggdraw() +
  coord_equal(xlim = c(0, 17), ylim = c(0, 25), expand = FALSE) +
  draw_plot(DD  , width = 19, height = 19,x = -3.8, y = 0)+
  
  draw_plot(MDD_CLIM, width = 7,       height = 7, x = 9.7, y = 18.7)+
  draw_plot(SurA, width = 6,       height = 6, x = -1.2, y = 19)+
  draw_plot(MDD_GG, width = 6,   height = 6, x = 3.7, y = 19)+
  
  draw_plot(Eleva_puer , width =8,       height = 8, x = 10.2, y = 2)+
  #draw_plot(Manuta_Reli_gg , width = 4,       height = 5, x = 11.8, y = 3.4)+
  
  #draw_plot(legend, width = 5,       height = 5, x = 13, y = 7.5)+
  
  draw_plot(Alt_Manutata, width = 5,       height = 5, x = 11.8, y = 9.2)+
  draw_plot(Alt_Vive, width = 5,       height = 5, x = 11.8, y = 13.2)+
  
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = 12, y = 18, hjust = 0, vjust = 1,
           label = "Fundo el Bosque",face="bold",
           size = 5,family="serif", color = "black", face = "italic")+
  annotate(geom = "text", x = 12, y = 14.1, hjust = 0, vjust = 1,
           label = "Cachuela margen Izq.",face="bold",
           size = 4,family="serif", color = "black", face = "italic")

ggsave(plot = W ,"Mapas/01_Mapa de precipitacion.png", units = "cm", width = 17,height = 25, dpi = 1200) 


