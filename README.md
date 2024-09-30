---
title: "Dataton"
author: "Frida Márquez, Alexa Enríquez, Lupita Moreno, Carmen Sofía Delgado. (AVS, PLA)"
date: "2024-09-22"
---

A lo largo de este documento se trabaja con datos del Censo de Población y Vivienda de la INEGI, del Directorio Estadístico Nacional de Unidades Económicas (DENUE) y de la Secretaría de Salud. Los datos son muy pesados, los datos del INEGI se encuentran en el siguiente link: https://drive.google.com/drive/folders/1szEv161s6WpSd6SYmf5iT2rQVHHxypm8?usp=sharing y algunas de las carpetas que se crearon en el proceso que fuimos haciendo el código se encuentran en los archivos .zip. 

## Importación de paquetes

Necesitamos paquetes que nos permitan importar objetos, visualizar datos, entre otras funcionalidades.

```{r}
# Cargamos o descargamos los paquetes, si no fueron descargados previamente
pacman::p_load(readr, dplyr, ggplot2, stringr, sf, RColorBrewer, tidyverse, icd.data, archive, janitor, readxl)
```

## Importación de objetos

Ahora importaremos los datos de "shapefile", son polígonos que nos van a permitir visualizar las secciones electorales del país.

```{r}
# Indicamos que ignore los poligonos que no están cerrados
Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "NO")

# Importamos "shapefile" a nivel sección electoral
secciones <- st_read("./recursos/SHAPEFILE/SECCION.shp")
```

Veamos el mapa de México delimitado por secciones electorales.

```{r}
# Visualizamos el mapa
ggplot()+
  geom_sf(data=secciones,
         color='grey',
          fill='white',
          size=0.1)+
  theme_minimal()
```

## Importación de datos

Importamos los datos que necesitamos para el análisis.

```{r}
# Leemos los datos del censo 2020 a nivel sección electoral
censo <- read_csv("recursos/Censo_2020/conjunto_de_datos/INE_SECCION_2020.CSV")

# Leemos los datos proporcionados por la DENUE y los filtramos para quedarnos con las columnas que nos interesan
farmacias <- read_csv("recursos/denue_00_46321-46531_csv/conjunto_de_datos/denue_inegi_46321-46531_.csv") |>
  mutate(nombre_act = str_to_lower(nombre_act)) |>
  filter(str_detect(nombre_act, "farm"))

edificaciones <- read_csv("recursos/denue_00_23_csv/conjunto_de_datos/denue_inegi_23_.csv") 

servicios_de_salud <- read_csv("recursos/denue_00_62_csv/conjunto_de_datos/denue_inegi_62_.csv")

# Leemos los datos proporcionados por la secretaría de salud
pacientes_cronicos <- read_csv("recursos/SALUD/DA_EC_SIS_2023/DA_EC_SIS_2023.csv", na = "NULL")
variables_SIS <- read_csv("recursos/SALUD/LISTADO_DE_VARIABLES_SIS_2022.csv")
```

## Análisis de datos

### Inspección

Primero inspeccionaremos cada dataframe para tener una idea de los datos que tenemos y saber si hay que aplicar algunos filtros.

#### Secciones

```{r}
class(secciones)
```
```{r}
dim(secciones)
```
```{r}
names(secciones)
```
```{r}
str(secciones)
```
```{r}
head(secciones)
```
```{r}
colSums(is.na(secciones))
```
A esta tabla no es necesario hacerle una limpieza de datos.

#### Censo

Inspeccionar los datos del censo.

Verifiquemos que los datos se hayan cargado en el formato correcto.

```{r}
class(censo)
```
```{r}
dim(censo)
```
```{r}
names(censo)
```
Nos interesan las variables que pertenecen a las siguientes categorias: población, características económicas, características educativas, servicios de salud y viviendas.

```{r}
str(censo)
```
```{r}
head(censo)
```
```{r}
colSums(is.na(censo))
```
No hay valores nulos en ninguna columna.

Para cerciorarnos de que se estan considerando todas las entidades, distritos, municipios y secciones, la longitud de las listas que contiene los valores unicos de cada columna deben coincidir con las cantidades reales de cada variable.

```{r}
sapply(censo[c("ENTIDAD", "DISTRITO", "MUNICIPIO", "SECCION")], function(x) length(unique(x)))
```
Todas las cantidades coinciden con sus valores reales.

```{r}
unique(censo$TIPO)
```
Visualicemos si hay valores atipicos que deban de eliminarse en la etapa de limpieza de datos debido a su imposibilidad. Graficaremos la distribución de la variable "POBTOT".

```{r}
ggplot(censo, aes(POBTOT)) +
  geom_boxplot(fill="#d58cfc") +
  labs(title="Distribución de la población total por sección electoral",
       x = "Población total",
       y = "Frecuencia") +
  theme_minimal() 
```
A simple vista podemos ver secciones electorales con una población mucho más alta que las demas.

Analicemos la seccion con una población aproximada de 50000 habitantes.

```{r}
censo[censo$POBTOT >= 45000, c("ENTIDAD", "DISTRITO", "MUNICIPIO", "SECCION", "POBTOT")]
```
Podemos asumir que la población total de una sección electoral no puede superar los 50000 en población total.

Por lo anterior, aseguremonos que ninguna de las demás variables deben tener valores mayores a esa cantidad.

```{r}
censo[, 7:226] |>
  select(where(~ max(.x, na.rm = TRUE) > 50000))
```
Ningún valor supera los 50000. Podemos continuar con la inspección de los demás dataframes.

#### Farmacias

```{r}
class(farmacias)
```
```{r}
dim(farmacias)
```
```{r}
names(farmacias)
```
Las columnas que nos interesan son: id, nom_estab, raz_social, codigo_act, nombre_act, per_ocu, entidad, municipio, latitud, longitud y fecha_alta. Una farmacia necesita más trabajadores si tiene muchos clientes, por eso consideramos la columna per_ocu (número de trabajadores en esa unidad económica).

```{r}
str(farmacias)
```
```{r}
head(farmacias)
```

Veamos si hay valores nulos.

```{r}
colSums(is.na(farmacias))
```
Posiblemente sería una buena practica eliminar las columnas que contengan una gran cantidad de valores nulos y que no sean necesarias para el análisis.

```{r}
unique(farmacias$nombre_act)
```

#### Edificaciones

```{r}
class(edificaciones)
```
```{r}
dim(edificaciones)
```
```{r}
names(edificaciones)
```
Como podemos ver, el dataframe "edificaciones" contiene las mismas columnas que "farmacias"

```{r}
str(edificaciones)
```
```{r}
head(edificaciones)
```
```{r}
colSums(is.na(edificaciones))
```
Podemos ver que contiene valores nulos en las mismas columnas que farmacias. En los dos casos podemos eliminar las columnas que contienen una gran cantidad de valores nulos y no aportan información importante para nuestro análisis.

```{r}
unique(edificaciones$nombre_act)
```
Nos interesan las categorias 1, 5, 11 y 14, porque nos permiten localizar zonas concurridas en cada sección electoral.

#### Servicios de salud

```{r}
class(servicios_de_salud)
```
```{r}
dim(servicios_de_salud)
```
```{r}
names(servicios_de_salud)
```
```{r}
str(servicios_de_salud)
```
```{r}
head(servicios_de_salud)
```
```{r}
colSums(is.na(servicios_de_salud))
```
Imprimamos las categorias de los servicios de salud para poder identificar los que nos interesan.

```{r}
unique(servicios_de_salud$nombre_act)
```
Nos interesan las categorias que contengan las siguientes palabras clave: asilos, consultorios, hospitales generales, hospitales del sector, centros, residencia y ancianos. Es posible que se necesite hacer otro filtro.

#### Pacientes crónicos

```{r}
class(pacientes_cronicos)
```
```{r}
dim(pacientes_cronicos)
```
```{r}
names(pacientes_cronicos)
```
```{r}
str(pacientes_cronicos)
```
```{r}
head(pacientes_cronicos)
```
```{r}
colSums(is.na(pacientes_cronicos))
```
```{r}
length(unique(pacientes_cronicos$NOMBRE_CLUES))
```
Veamos la descripción de las claves que conforman las columnas.

```{r}
subset(variables_SIS, CLAVE %in% names(pacientes_cronicos))
```
Para considerar todos los pacientes mayores a 20 con enfermedades crónicas, se podría crear una nueva columna que sea la suma de las columnas: FRS01, FRS02, FRS03 Y FRS04.

### Limpieza de datos

#### Secciones

```{r}
secciones <- clean_names(secciones)
names(secciones)
```

#### Censo

En primer lugar, seleccionaremos las columnas que nos interesan de las categorías mencionadas durante la inspección de los datos.

```{r}
censo <- clean_names(censo) |>
  select(c(id:tipo, pobtot, p_60ymas, p18ym_pb, graproes, pea, tothog, pobhog, tvivpar, vivpar_hab, tvivparhab, ocupvivpar, prom_ocup, pro_ocup_c, vph_3ymasc, vph_c_serv, vph_inter))
# Verificamos que se hayan eliminado
names(censo)
```

#### Farmacias

Seleccionemos las columnas que nos interesan.

```{r}
farmacias <- farmacias[, c("id", "nom_estab", "raz_social", "codigo_act", "nombre_act", "per_ocu", "cve_ent", "cve_mun", "latitud", "longitud", "fecha_alta")] |>
  rename(entidad = cve_ent, municipio = cve_mun) |>
  mutate(
    entidad = as.numeric(entidad),
    municipio = as.numeric(municipio)
  )
names(farmacias)
```
#### Edificaciones

Seleccionemos las filas y columnas que nos interesan.

```{r}
edificaciones <- edificaciones[, c("id", "nom_estab", "raz_social", "codigo_act", "nombre_act", "per_ocu", "cve_ent", "cve_mun", "latitud", "longitud", "fecha_alta")] |>
  rename(entidad = cve_ent, municipio = cve_mun) |>
  mutate(
    entidad = as.numeric(entidad),
    municipio = as.numeric(municipio)
  ) |>
  mutate(nombre_act = str_to_lower(nombre_act)) |>
  filter(nombre_act %in% unique(nombre_act)[c(1, 5, 11, 14)])
unique(edificaciones$nombre_act)
```

#### Servicios de salud

```{r}
servicios_de_salud <- servicios_de_salud[, c("id", "nom_estab", "raz_social", "codigo_act", "nombre_act", "per_ocu", "cve_ent", "cve_mun", "latitud", "longitud", "fecha_alta")] |>
  rename(entidad = cve_ent, municipio = cve_mun) |>
  mutate(
    entidad = as.numeric(entidad),
    municipio = as.numeric(municipio)
  ) |>
  mutate(nombre_act = str_to_lower(nombre_act)) |>
  filter(nombre_act %in% unique(nombre_act)[c(3, 4, 5, 6, 7, 8, 12, 18, 21, 25, 26, 29, 31, 36, 38, 40, 43, 44, 46, 47, 49, 50, 52, 53, 54, 61, 63)])
unique(servicios_de_salud$nombre_act)
```

#### Pacientes cronicos

No necesitamos la columna ANIO porque sabemos que los datos fueron registrados en 2023.

```{r}
pacientes_cronicos <- clean_names(pacientes_cronicos) |>
  mutate(
    entidad = as.numeric(clave_entidad),
    municipio = as.numeric(clave_municipio)
  ) |>
  select(c(clave_entidad:mes, frs01, frs02, frs03, frs04)) |>
  select(-c(clave_entidad, clave_municipio))
names(pacientes_cronicos)
```

### Organización de datos

#### Secciones

```{r}
secciones <- mutate(secciones, area_seccion_km = st_area(geometry) / 1000)
names(secciones)
```

#### Censo

Queremos saber los porcentaje de la población que cumplen ciertas características en cada sección electoral,

```{r}
censo <- censo |>
  mutate(
    porc_p60ymas = p_60ymas / pobtot,
    porc_pea = pea / pobtot,
    porc_vivpar_hab = vivpar_hab / tvivpar,
    porc_vph_3ymasc = vph_3ymasc / vivpar_hab,
    porc_vph_c_serv = vph_c_serv / vivpar_hab,
    porc_vph_inter = vph_inter / vivpar_hab
  )
names(censo)
```

#### Farmacias

```{r}
farmacias <- st_as_sf(farmacias, coords = c("longitud", "latitud"), crs = 4326) |>
  st_transform(st_crs(secciones))
```

#### Edificaciones

```{r}
edificaciones <- st_as_sf(edificaciones, coords = c("longitud", "latitud"), crs = 4326) |>
  st_transform(st_crs(secciones))
```

#### Servicios de salud

```{r}
servicios_de_salud <- st_as_sf(servicios_de_salud, coords = c("longitud", "latitud"), crs = 4326) |>
  st_transform(st_crs(secciones))
```

#### Pacientes cronicos

```{r}
pacientes_cronicos <- mutate(pacientes_cronicos, totfrs = frs01 + frs02 + frs03 + frs04)
names(pacientes_cronicos)
```

#### Censo por sección

```{r}
censo_por_seccion <- left_join(secciones, censo, 
                             by = c("seccion" = "seccion",
                                    "entidad" = "entidad",
                                    "municipio" = "municipio")) |>
  mutate(
    dens_p60ymas = p_60ymas / area_seccion_km,
    dens_pea = pea / area_seccion_km,
    dens_vivpar_hab = vivpar_hab / area_seccion_km,
    dens_vph_3ymasc = vph_3ymasc / area_seccion_km,
    dens_vph_c_serv = vph_c_serv / area_seccion_km,
    dens_vph_inter = vph_inter / area_seccion_km)
names(censo_por_seccion)
```

### Análisis descriptivo

Definiremos una función que nos permita hacer un análisis descriptivo de una entidad en específico dada como parámetro. Eso nos permitirá poder automatizar generar las graficas para cada entidad.

Primero guardaremos un vector con las entidades federativas en el orden que las maneja el INEGI.

```{r}
nombres_entidades <- c(
  "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", 
  "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", 
  "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Edo. de México", 
  "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", 
  "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", 
  "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")
```


```{r}
analisis_descriptivo <- function(entidad_param, directorio="graficas"){
  
    # Crear el directorio para la entidad si no existe
  subcarpeta <- file.path(directorio, paste(entidad_param, nombres_entidades[entidad_param]))
  if (!dir.exists(subcarpeta)) {
    dir.create(subcarpeta, recursive = TRUE)
  }
  
  #Crear una carpeta para guardar más adelante un csv por cada entidad
  carpeta_csv <- "csv_entidades"
  if (!dir.exists(carpeta_csv)) {
    dir.create(carpeta_csv, recursive = TRUE)
  }
  
  #Filtramos la informacion por cada entidad
  entidad_s <- secciones |>
    filter(entidad == entidad_param)
  
  entidad_farm <- farmacias |>
    filter(entidad == entidad_param)
  
  entidad_ss <- servicios_de_salud |>
  filter(entidad == entidad_param)
  
  entidad_cps <- censo_por_seccion |>
    filter(entidad == entidad_param)
 
  entidad_zu <- edificaciones |>
  filter(entidad == entidad_param)
  
  entidad_fps <- st_join(secciones[secciones$entidad == entidad_param,], farmacias[farmacias$entidad == entidad_param,]) |>
    group_by(entidad.x, municipio.x, seccion) |>
    summarise(num_farm = n())
  
  entidad_zups <- st_join(secciones[secciones$entidad == entidad_param,], edificaciones[edificaciones$entidad == entidad_param,]) |>
    group_by(entidad.x, municipio.x, seccion) |>
    summarise(num_zu = n())
  
  entidad_ssps <- st_join(secciones[secciones$entidad == entidad_param,], servicios_de_salud[servicios_de_salud$entidad == entidad_param,]) |>
    group_by(entidad.x, municipio.x, seccion) |>
    summarise(num_ss = n())
  
  entidad_cps <- censo_por_seccion |>
    filter(entidad == entidad_param)
  
  # Descripcion numerica
  
  # A) Poblacion total
  ord_pobtot <- entidad_cps |>
    select(c(entidad, municipio, seccion, pobtot)) |>
    arrange(desc(pobtot))
  ord_pobtot[1:10,]
  
  # B) Poblacion de 60 años y mas
  ord_p_60ymas <- entidad_cps |>
    select(c(entidad, municipio, seccion, p_60ymas)) |>
    arrange(desc(p_60ymas))
  ord_p_60ymas[1:10,]
  
  # C) Poblacion economicamente activa
  ord_pea <- entidad_cps |>
    select(c(entidad, municipio, seccion, pea)) |>
    arrange(desc(pea))
  ord_pea[1:10,]
  
  # Visualizaciones
  
  # A) Poblacion total
  graf_pobtot <- ggplot()+
    # This layer maps the electoral sections and fills each one according to the 
    # population density
    geom_sf(data = ord_pobtot, aes(fill = as.numeric(pobtot)), color = "grey90")+
    scale_fill_gradient(low = "white", high = "#750014", na.value = "skyblue")+
    labs(title = paste("Población total por entidad electoral en", nombres_entidades[entidad_param]), fill="Cantidad de personas")+
    theme_minimal()+
    theme(legend.position = "bottom")
  
  # Guardar la gráfica de población total
  ggsave(file.path(subcarpeta, paste0("poblacion_total_", entidad_param, ".jpg")), graf_pobtot, width = 8, height = 6)
  
  # B) Poblacion de 60 años y mas
  graf_60ymas <- ggplot()+
    geom_sf(data = ord_p_60ymas, aes(fill = as.numeric(p_60ymas)), color = "grey90")+
    scale_fill_gradient(low = "white", high = "#750014", na.value = "skyblue")+
    labs(title = paste("Densidad de adultos mayores por km² en", nombres_entidades[entidad_param]), fill="Cantidad de personas")+
    theme_minimal()+
    theme(legend.position = "bottom")
  
  # Guardar la gráfica de población de 60 años y más
  ggsave(file.path(subcarpeta, paste0("poblacion_60ymas_", entidad_param, ".jpg")), graf_60ymas, width = 8, height = 6)
  
  # C) Poblacion economicamente activa
  graf_pea <- ggplot()+
    geom_sf(data = ord_pea, aes(fill = as.numeric(pea)), color = "grey90")+
    scale_fill_gradient(low = "white", high = "#750014", na.value = "skyblue")+
    labs(title = paste("Densidad de poblacion economicamente activa por km² en", nombres_entidades[entidad_param]), fill="Cantidad de personas")+
    theme_minimal()+
    theme(legend.position = "bottom")
  
    # Guardar la gráfica de población económicamente activa
  ggsave(file.path(subcarpeta, paste0("poblacion_pea_", entidad_param, ".jpg")), graf_pea, width = 8, height = 6)
  

    
    # D) Creación de un mapa que muestre las farmacias, los servicios de salud y las edificaciones (para conocer las zonas urbanas)
  
  graf_farm <- ggplot() +
    # Capa de las secciones del estado
    geom_sf(data = entidad_s, fill = "lightgrey", color = "white") +
    
    # Capa de las farmacias como puntos
    geom_sf(data = entidad_farm, aes(color = "Farmacias"), size = 0.5, alpha = 0.2) +
    
    # Capa de los servicios de salud como puntos
    geom_sf(data = entidad_ss, aes(color = "Servicios de Salud"), size = 0.5, alpha = 0.2) +  
    
    # Capa de las edificaciones como puntos
    geom_sf(data = entidad_zu, aes(color = "Edificaciones"), size = 0.5, alpha = 0.5) +  
    
    
    scale_color_manual(values = c("red", "blue","green")) +
    
    labs(
      title = paste("Ubicación de Farmacias, Servicios de Salud y Edificaciones en", nombres_entidades[entidad_param]),
      color = "Localizaciones") +
    theme_minimal() +
    theme(legend.position = "bottom")
    
    # Guardar la gráfica de farmacias
  ggsave(file.path(subcarpeta, paste0("farmacias_", entidad_param, ".jpg")), graf_farm, width = 8, height = 6)
  
  
  #E) Creación de un heatmap de la correlación de las variables
  entidad_fps <- rename(entidad_fps, municipio = municipio.x)
  entidad_zups <- rename(entidad_zups, municipio = municipio.x)
  entidad_ssps <- rename(entidad_ssps, municipio = municipio.x)
  
  df <- st_drop_geometry(entidad_cps) |> left_join(st_drop_geometry(entidad_fps), by = c("municipio", "seccion")) |>
    left_join(st_drop_geometry(entidad_zups), by = c("municipio", "seccion")) |>
    left_join(st_drop_geometry(entidad_ssps), by = c("municipio", "seccion"))
  
  
  corr <- df[, 8:ncol(df)] |>
    select(where(is.numeric)) |>                 
    select(-contains(c("entidad", "id", "tipo", "distrito", "porc"))) |>              
    cor(use = "pairwise.complete.obs")
  
  heatmap_plot <- ggplot(data = melt(corr), aes(x = Var1, y = Var2)) +
    geom_tile(aes(fill = value), color = "white") +  
    geom_text(aes(label = round(value, 1)), color = "black", size = 1) +  
    scale_fill_gradient(low = "white", high = "red", na.value = "grey50") +  
    theme_minimal() +  
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    labs(title = "Heatmap de Correlación", x = "Variables", y = "Variables") +
    coord_fixed()
  
  ggsave(file.path(subcarpeta, paste0("heatmap_", entidad_param, ".jpg")), heatmap_plot, width = 8, height = 6)

  #F) Creación de un csv por entidad tomando en cuenta las variables que tienen más correlación con el número de farmacias. Este csv se usará después para aplicar un modelo. 
  
  df_modelo <- df[, c("entidad", "municipio", "seccion", "pobtot", "p_60ymas", "p18ym_pb", "pea", "vph_inter", "num_zu", "num_ss", "num_farm")]
  
  write.csv(df_modelo, file = file.path(carpeta_csv, paste0(entidad_param, nombres_entidades[entidad_param], ".csv")), row.names = FALSE)
  
}

```

Ahora, probamos con Aguascalientes

```{r}
analisis_descriptivo(1, "graficas")
```


Las graficas de aguascalientes se estan guardando en un vector de visualizaciones. Eso podemos utilizarlo para vectorizar la funcion y obtener de todas. Para vectorizarlo:

```{r}
entidades <- c(1:32)
graficas_por_entidad <- lapply(entidades, analisis_descriptivo)
```

Utilizamos los csv de cada entidad para crear un nuevo csv llamado "nacion", en donde se encuentra la información de todas las entidades. Por fines practicos, el nacion.csv ya esta incluido en el repositorio.

```{r}
#Cargar los datos de nacion.cvs
df_modelo <-read_csv("nacion.csv")
df_modelo <- drop_na(df_modelo)

#Normalizamos los datos para estandarizar la escala de las variables
normalizar <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#Funcion del modelo 
modelo <- function(df) {
  df_norm <- df[, 4:ncol(df)]
  df_norm[] <- lapply(df_norm, normalizar)
  
  total <- numeric(nrow(df_norm))
  
#Asignamos estos pesos a cada columna. El número de farmacias tiene un mayor peso porque nos interesan los lugares en donde no hay muchas farmacias.
  pesos <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.3)
  
  for (i in 1:nrow(df_norm)) {
    for (j in 1:ncol(df_norm)) {
      if (j == ncol(df_norm)) {
        total[i] <- total[i] + pesos[j] * (1 -df_norm[i, j])
      } else {
        total[i] <- total[i] + pesos[j] * df_norm[i, j]
      }
    }
  }
  df_norm$total <- unlist(total)
  df_result <- cbind(df[, 1:3], df_norm)
  return(df_result)
}

#Aplicamos el modelo al dataframe con la información de la nación.
df_result <- modelo(df_modelo)

#Filtramos con las 200 farmacias que tengan un "total" mayor. La columna total nos da la calificación que calcula el modelo para decir que tan conveniente es poner una farmacia en esa ubicación.

ubicaciones_recomendadas <- head(arrange(df_result, desc(total)), 201)

#Convertimos el resultado a un csv.
write.csv(ubicaciones_recomendadas, file = "ubicaciones_recomendadas.csv", row.names = FALSE)
```

Como el csv que contiene la clave numerica con la que se identifica cada entidad y cada municipio, decidimos hacer un diccionario.

```{r}

farmacias <- read_csv("./recursos/denue_00_46321-46531_csv/conjunto_de_datos/denue_inegi_46321-46531_.csv")
nombres <- unique(select(farmacias,cve_ent,entidad,cve_mun,municipio))

nombres$cve_ent <- as.numeric(nombres$cve_ent)
nombres$cve_mun <- as.numeric(nombres$cve_mun)

diccionario<- left_join(nombres,ubicaciones_recomendadas,by=c("cve_ent"="entidad","cve_mun"="municipio")) |>
  select(c(cve_ent,entidad,cve_mun, municipio))|>
  unique()

write.csv(diccionario, file = "diccionario.csv", row.names = FALSE)
```

Usando Tableau, hicimos un leftjoin() entre los csv "diccionario" y "ubicaciones_recomendadas" para conocer el nombre de los municipios y entidades. El archivo se llama "farmacias_recomendadas_ubicacion.csv". Dada esta información, fue hecha la presentación.

