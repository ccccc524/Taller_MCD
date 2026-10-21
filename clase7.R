#######################################################################################
#######################################################################################
################      CLASE 7                     #####################################
################  MCD Carlos Castro Correa        #####################################
#######################################################################################
#######################################################################################

#Contenido: Algoritmo Google Maps, Djikstra y Datos Espaciales
library(igraph)
library(sf)
library(tidyverse)
library(leaflet)

#######################################################################################
#Datos sobre esquinas y comunicacion

#Cambia esta ruta en tu computadora
setwd("~/Desktop/Carlos/cursos/ITAM/Taller_MCD_agosto2026/Clase7")

esquinas <- read_csv("esquinas_bis.csv") %>% 
  select(-CVE_MUN)
#Revisamos los datos
View(esquinas)



#Mapa de las esquinas
token <- "XXXXXXX" #Obten tu token https://docs-carto-com.translate.goog/carto-user-manual/developers/managing-credentials/api-access-tokens?_x_tr_sl=en&_x_tr_tl=es&_x_tr_hl=es&_x_tr_pto=tc

url_carto <- paste0(
  "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png?key=",
  token
)

leaflet(esquinas, options = leafletOptions(preferCanvas = TRUE)) |>
  addTiles(
    urlTemplate = url_carto,
    attribution = "&copy; OpenStreetMap contributors &copy; CARTO",
    options = tileOptions(subdomains = "abcd", maxZoom = 20)
  ) |>
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 2, stroke = FALSE,
    fillColor = "#00C853", fillOpacity = 0.8,
    popup = ~as.character(id_esquina)
  )


#######################################################################################
#Indices de comunicacion entre esquinas de la misma calle
indices <- read_csv("esquinas_comunicacion.csv" )

#Revisamos los datos
View(indices)




#######################################################################################
#Matriz de comunicacion
#Contiene toda la información para armar Djisktra
com <- read_csv("matriz_comunicacion.csv")

View(com)

#Observa que contiene una variable llamada Distancia
  # ¿Qué significa?
round(com$distancia, 2)


#Data.frame auxilizar de coordenadas e id_esquina
unicos <- com %>% 
  select(id_esquina,lat,lon) %>% 
  unique() %>% 
  arrange((id_esquina))



#######################################################################################
################   Algoritmo Dijkstra             #####################################
#######################################################################################

#1.Aristas: solo entre esquinas que se comunican, con la distancia euclidiana como peso
aristas <- data.frame(
  from   = com$id_esquina,
  to     = com$id_esquina_comunica,
  weight = com$distancia
)
aristas <- aristas[!is.na(aristas$weight), ]

View(aristas)#Weight es distancia pero podrías ser otra medida...


#2.Creamos el grafo (no dirigido, por practicidad: estamos asumiendo que todas las calles son de doble sentido)
g <- graph_from_data_frame(aristas, directed = FALSE)

#Por el supuesto anrterior, como A-B y B-A vienen repetidas, nos quedamos con una sola arista
g <- simplify(g, edge.attr.comb = list(weight = "min"))

#Imprimmos el grafo
g


#Escogemos una esquina de origen y una de destino...
esquina_origen  <- 6363
esquina_destino <- 1258

#4.Ruta más corta (igraph usa Dijkstra automáticamente con la columna "weight")
camino <- shortest_paths(g, from = esquina_origen, to = esquina_destino)$vpath[[1]]
ruta   <- as.numeric(names(camino))

#id_esquinas para llegar desde la esquina origen a destino
camino

#Distancia total del recorrido
distancia_total <- distances(g, v = esquina_origen, to = esquina_destino)[1, 1]

distancia_total#En metros

  #Intenta otras rutas utilizando otros pares de esquinas...




#######################################################################################
#######################################################################################
#Mapa de la ruta más corta

# Token de CARTO
token <- "XXXXXXX"

url_carto <- paste0(
  "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png?key=",
  token
)

# Ruta como sf con la distancia en metros
ruta_coords <- unicos[match(ruta, unicos$id_esquina), ]

ruta_sf <- st_sf(
  distancia_m = round(distancia_total, 2),
  geometry = st_sfc(
    st_linestring(as.matrix(ruta_coords[, c("lon", "lat")])),
    crs = 4326
  )
)

bb <- st_bbox(ruta_sf)

# Mapa
leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addTiles(
    urlTemplate = url_carto,
    attribution = "&copy; OpenStreetMap contributors &copy; CARTO",
    options = tileOptions(subdomains = "abcd", maxZoom = 20)
  ) |>
  addPolylines(
    data = ruta_sf,
    color = "#0B3D91", weight = 5, opacity = 1,
    label = ~paste0(format(distancia_m, nsmall = 2), " m"),
    popup = ~paste0(format(distancia_m, nsmall = 2), " m")
  ) |>
  addCircleMarkers(
    data = ruta_coords, lng = ~lon, lat = ~lat,
    radius = 2, stroke = FALSE,
    fillColor = "#00C853", fillOpacity = 0.8
  ) |>
  fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])


#Coordenadas origen destino:
coords <- unicos %>% 
  filter(id_esquina %in% (c(esquina_origen,esquina_destino))) %>% 
  select(lat,lon)

url_gmaps <- sprintf(
  "https://www.google.com/maps/dir/?api=1&origin=%.6f,%.6f&destination=%.6f,%.6f&travelmode=walking",
  coords$lat[1], coords$lon[1], coords$lat[2], coords$lon[2]
)

#Abre enlace de Google Maps
url_gmaps
browseURL(url_gmaps)

#Compara contra nuestra ruta más corta
distancia_total#En metros




