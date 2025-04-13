# Librerias:
library(tidyverse)
library(skimr)
library(janitor)
library(sf)
library(corrplot)
library(caret)

load("C:/Users/Manel/Desktop/TFM/Idealista Madrid 2018/Madrid_Sale.RData")
load("C:/Users/Manel/Desktop/TFM/Idealista Madrid 2018/Madrid_Polygons.RData")
load("C:/Users/Manel/Desktop/TFM/Idealista Madrid 2018/Madrid_POIS.RData")

glimpse(Madrid_Sale)
skim(Madrid_Sale)

#Mas librerias
library(ggplot2)
library(GGally)
library(DataExplorer)

#Visualización general de distribución de variables numéricas
#Por qué: nos da una visión rápida de escalas, colas, picos, etc.
plot_intro(Madrid_Sale)
plot_histogram(Madrid_Sale)

#Distribución del precio
ggplot(Madrid_Sale, aes(x = PRICE)) +
  geom_histogram(bins = 60, fill = "steelblue") +
  scale_x_log10() +
  labs(title = "Distribución del precio de venta (log10)", x = "Precio (€)", y = "Frecuencia")

#Distribución del tamaño en m2
ggplot(Madrid_Sale, aes(x = CONSTRUCTEDAREA)) +
  geom_histogram(bins = 50, fill = "darkgreen") +
  labs(title = "Distribución de superficie construida", x = "Superficie (m2)", y = "Frecuencia")

#Habitaciones vs. Baños
ggplot(Madrid_Sale, aes(x = ROOMNUMBER, y = BATHNUMBER)) +
  geom_jitter(alpha = 0.3, color = "purple") +
  labs(title = "Habitaciones vs. Baños", x = "Nº Habitaciones", y = "Nº Baños")

#Precio vs Superficie
ggplot(Madrid_Sale, aes(x = CONSTRUCTEDAREA, y = PRICE)) +
  geom_point(alpha = 0.2) +
  scale_y_log10() +
  labs(title = "Relación entre superficie y precio (log)", x = "Superficie (m2)", y = "Precio (€)")

#Precio vs distancia al centro
ggplot(Madrid_Sale, aes(x = DISTANCE_TO_CITY_CENTER, y = PRICE)) +
  geom_point(alpha = 0.2) +
  scale_y_log10() +
  labs(title = "Precio vs. Distancia al centro", x = "Distancia (km)", y = "Precio (€)")

#Correlaciones entre variables numéricas (rápido)
Madrid_num <- Madrid_Sale %>% select(where(is.numeric)) %>% drop_na()
cor_matrix <- cor(Madrid_num)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

#Nos aseguramos de que la geometría esté bien cargada
Madrid_Polygons <- st_make_valid(Madrid_Polygons)

#Creamos un subset con anuncios con coordenadas válidas
Madrid_Sale_sf <- Madrid_Sale %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

#Mapa base con distritos + anuncios
ggplot() +
  #Capa de polígonos: distritos o barrios
  geom_sf(data = Madrid_Polygons, fill = "grey95", color = "white") +

  #Capa de puntos: anuncios, coloreados por precio (log)
  geom_sf(data = Madrid_Sale_sf, aes(color = log10(PRICE)), alpha = 0.6, size = 1) +

  #Estética
  scale_color_viridis_c(name = "Precio (log10)", option = "plasma") +
  theme_minimal() +
  labs(
    title = "Distribución espacial del precio de la vivienda en Madrid (2018)",
    subtitle = "Datos de Idealista (idealista18)"
  )


#Creamos versión filtrada de los anuncios que están dentro de los límites de Madrid
Madrid_Sale_dentro <- Madrid_Sale_sf %>%
  st_join(Madrid_Polygons, join = st_within, left = FALSE)

ggplot() +
  geom_sf(data = Madrid_Polygons, fill = "grey95", color = "white") +
  geom_sf(data = Madrid_Sale_dentro, aes(color = log10(PRICE)), alpha = 0.6, size = 1) +
  scale_color_viridis_c(name = "Precio (log10)", option = "plasma") +
  theme_minimal() +
  labs(
    title = "Distribución espacial del precio (solo dentro de Madrid)",
    subtitle = "Visualización previa al filtrado definitivo"
  )

#Seleccionamos variables relevantes para predecir el precio
#Incluimos tamaño, habitaciones, baños, amenities, antigüedad, altura, ubicación y tipo de edificio

Madrid_limpio <- Madrid_Sale %>%
  select(
    PRICE,                                   # Variable objetivo
    UNITPRICE,                               # Precio por m2 (puede servir como referencia)
    CONSTRUCTEDAREA,                         # Superficie en m2
    ROOMNUMBER, BATHNUMBER,                  # Habitaciones y baños
    FLOORCLEAN,                              # Planta
    HASLIFT, HASTERRACE, HASAIRCONDITIONING, # Amenities básicos
    HASPARKINGSPACE, ISPARKINGSPACEINCLUDEDINPRICE,
    PARKINGSPACEPRICE,
    HASWARDROBE, HASDOORMAN, HASGARDEN, HASSWIMMINGPOOL, # Más amenities
    DISTANCE_TO_CITY_CENTER,
    DISTANCE_TO_METRO,
    DISTANCE_TO_CASTELLANA,
    CADCONSTRUCTIONYEAR,                     # Año de construcción (fuente catastral, más fiable)
    BUILTTYPEID_1, BUILTTYPEID_2, BUILTTYPEID_3,
    LATITUDE, LONGITUDE                      # Coordenadas (por si las usamos en modelos espaciales)
  )

#Filtramos observaciones anómalas que distorsionarían el modelo

Madrid_limpio <- Madrid_limpio %>%
  filter(
    PRICE >= 10000 & PRICE <= 2000000,                 # Precios razonables
    CONSTRUCTEDAREA >= 20 & CONSTRUCTEDAREA <= 400,    # Tamaño de vivienda habitual
    ROOMNUMBER <= 10, BATHNUMBER <= 5,                 # Viviendas familiares típicas
    DISTANCE_TO_CITY_CENTER < 50,                      # Eliminamos puntos absurdos (>400 km)
    DISTANCE_TO_METRO < 20,
    DISTANCE_TO_CASTELLANA < 20
  )

#Añadimos columnas que resumen o transforman variables ya existentes

Madrid_limpio <- Madrid_limpio %>%
  mutate(
    #Cálculo manual del precio por m2 (como control)
    PRICE_PER_M2 = PRICE / CONSTRUCTEDAREA,

    #Antigüedad del inmueble (en años)
    ANTIGUEDAD = 2018 - CADCONSTRUCTIONYEAR,

    #Categoría de altura para simplificar la variable FLOORCLEAN
    FLOOR_CAT = case_when(
      FLOORCLEAN < 0 ~ "Sótano/Bajo",
      FLOORCLEAN == 0 ~ "Entresuelo",
      FLOORCLEAN <= 2 ~ "Bajo",
      FLOORCLEAN <= 5 ~ "Medio",
      FLOORCLEAN > 5 ~ "Alto",
      TRUE ~ "Desconocido"
    ) %>% as.factor()
  )

#Eliminamos cualquier fila con NA que haya quedado después del filtrado
Madrid_limpio <- Madrid_limpio %>%
  drop_na()

#Número de filas tras la limpieza
nrow(Madrid_limpio)

#Estructura general del nuevo dataset limpio
glimpse(Madrid_limpio)


#Boxplot del precio según la categoría de altura
ggplot(Madrid_limpio, aes(x = FLOOR_CAT, y = PRICE)) +
  geom_boxplot(fill = "skyblue") +
  scale_y_log10() +
  labs(
    title = "Precio de la vivienda por tipo de planta",
    x = "Categoría de planta",
    y = "Precio (escala log10)"
  ) +
  theme_minimal()

#Gráfico de dispersión del precio por m² según antigüedad
ggplot(Madrid_limpio, aes(x = ANTIGUEDAD, y = PRICE_PER_M2)) +
  geom_point(alpha = 0.2, color = "darkgreen") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "Relación entre antigüedad y precio por m²",
    x = "Antigüedad (años)",
    y = "Precio por m² (€)"
  ) +
  theme_minimal()

#Creamos una variable factor a partir de las columnas dummy
Madrid_limpio <- Madrid_limpio %>%
  mutate(
    TIPO_CONSTRUCCION = case_when(
      BUILTTYPEID_1 == 1 ~ "Unifamiliar",
      BUILTTYPEID_2 == 1 ~ "Bloque",
      BUILTTYPEID_3 == 1 ~ "Multivivienda",
      TRUE ~ "Desconocido"
    ) %>% as.factor()
  )
Madrid_limpio <- Madrid_limpio %>%
  mutate(
    TIPO_CONSTRUCCION = case_when(
      BUILTTYPEID_1 == 1 ~ "Unifamiliar",
      BUILTTYPEID_2 == 1 ~ "Bloque",
      BUILTTYPEID_3 == 1 ~ "Multivivienda",
      TRUE ~ "Desconocido"
    ) %>% as.factor()
  )

#Boxplot del precio por tipo de construcción
ggplot(Madrid_limpio, aes(x = TIPO_CONSTRUCCION, y = PRICE)) +
  geom_boxplot(fill = "tomato") +
  scale_y_log10() +
  labs(
    title = "Precio según tipo de construcción",
    x = "Tipo de construcción",
    y = "Precio (log10)"
  ) +
  theme_minimal()


# 🧭 Paso 1: Asociación de cada anuncio a su zona geográfica
# Realizamos una unión espacial entre puntos (anuncios) y polígonos (zonas)
Madrid_precio_por_zona <- Madrid_Sale_sf %>%
  st_join(Madrid_Polygons, join = st_within, left = FALSE)

# 📊 Paso 2: Cálculo del precio medio por zona
# Agrupamos por LOCATIONNAME (nombre de zona) y calculamos la media del precio
precio_medio_zona <- Madrid_precio_por_zona %>%
  group_by(LOCATIONNAME) %>%
  summarise(media_precio = mean(PRICE, na.rm = TRUE)) %>%
  ungroup()

# 🧼 Paso 3: Eliminamos geometría para poder hacer el left_join
precio_medio_zona_df <- precio_medio_zona %>%
  st_drop_geometry()

# 🔗 Paso 4: Unimos los precios medios al shapefile original
Madrid_Polygons_precio <- Madrid_Polygons %>%
  left_join(precio_medio_zona_df, by = "LOCATIONNAME")

# 🗺️ Paso 5: Mapa coroplético
# Visualizamos el precio medio por zona utilizando escala logarítmica para apreciar mejor las diferencias

ggplot(Madrid_Polygons_precio) +
  geom_sf(aes(fill = media_precio), color = "white") +
  scale_fill_viridis_c(option = "magma", trans = "log", name = "Precio medio (€)") +
  theme_minimal() +
  labs(
    title = "Precio medio de la vivienda por zona (Madrid, 2018)",
    subtitle = "Fuente: Idealista18 + Polígonos (LOCATIONNAME)",
    fill = "€ medio"
  )


# Guardamos el dataset limpio como CSV (sin la columna geometry)
Madrid_limpio_export <- Madrid_limpio %>%
  st_drop_geometry()

write.csv(Madrid_limpio_export, "C:/Users/Manel/Desktop/TFM/Idealista Madrid 2018/Madrid_limpio.csv", row.names = FALSE)

# Carga tu archivo .RData
load("C:/Users/Manel/Desktop/TFM/Idealista Madrid 2018/Madrid_Polygons.RData")

# Revisa que el objeto se llama como esperamos
print(ls())  # Esto mostrará los objetos cargados

# Asegúrate de que sea un objeto sf válido
Madrid_Polygons <- st_make_valid(Madrid_Polygons)

# Exporta como shapefile (creará varios archivos: .shp, .dbf, .shx, etc.)
st_write(Madrid_Polygons, "C:/Users/Manel/Desktop/TFM/Madrid_Polygons", driver = "ESRI Shapefile")
