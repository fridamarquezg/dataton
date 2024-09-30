---
title: "Datatón"
author: "Frida Márquez (PLAS), Alexa Enríquez (AVS), Lupita Moreno, Carmen Sofía Delgado"
date: "2024-09-22"
output: html_document
---

A lo largo de este documento se trabaja con datos del Censo de Población y Vivienda de la INEGI, del Directorio Estadístico Nacional de Unidades Economicas (DENUE) y de la Secretaría de Salud.

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

Veamos que el mapa de mexico delimitado por sección se pueda visualizar.

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
censo <- read_csv("./recursos/Censo_2020/conjunto_de_datos/INE_SECCION_2020.CSV")

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

Primero inspeccionaremos cada dataframe para tener una idea de con que vamos a trabajar y saber si tenemos que hacerles algún cambio.

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
Ningún valor de cada variable supera los 50000. Podemos continuar con la inspección de los demás dataframes.

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

En primer lugar, selecionaremos las columnas que nos interesan de las categorías mencionadas durante la inspección de los datos.

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

#### Aguascalientes

Primero definamos subconjuntos con los datos de Aguascalientes.

```{r}
acs <- secciones |>
  filter(entidad == 1)

ags_cen <- censo |>
  filter(entidad == 1)

ags_farm <- farmacias |>
  filter(entidad == 1)

ags_zu <- edificaciones |>
  filter(entidad == 1)

ags_ss <- servicios_de_salud |>
  filter(entidad == 1)

ags_cps <- censo_por_seccion |>
  filter(entidad == 1)

ags_fps <- st_join(secciones[secciones$entidad == 1,], farmacias[farmacias$entidad == 1,]) |>
  group_by(entidad.x, municipio.x, seccion) |>
  summarise(num_farm = n())

ags_zups <- st_join(secciones[secciones$entidad == 1,], edificaciones[edificaciones$entidad == 1,]) |>
  group_by(entidad.x, municipio.x, seccion) |>
  summarise(num_zu = n())

ags_ssps <- st_join(secciones[secciones$entidad == 1,], servicios_de_salud[servicios_de_salud$entidad == 1,]) |>
  group_by(entidad.x, municipio.x, seccion) |>
  summarise(num_ss = n())

ags_pec <- left_join(secciones[secciones$entidad == 1,], pacientes_cronicos[pacientes_cronicos$entidad == 1,],
                             by = c("entidad" = "entidad",
                                    "municipio" = "municipio")) |>
  group_by(entidad, municipio) |>
  summarise(num_pec = sum(totfrs))

```

Secciones electorales más pobladas.

*Descripción numérica*

```{r}
ord_pobtot <- ags_cps |>
  select(c(entidad, municipio, seccion, pobtot)) |>
  arrange(desc(pobtot))
ord_pobtot[1:10,]
```

*Descripción gráfica*

```{r}
ggplot()+
  # This layer maps the electoral sections and fills each one according to the 
  # population density
  geom_sf(data = ord_pobtot, aes(fill = as.numeric(pobtot)), color = "grey90")+
  scale_fill_gradient(low = "white", high = "#750014", na.value = "skyblue")+
  labs(fill = "Población total por entidad electoral")+
  theme_minimal()+
  theme(legend.position = "bottom")
```

Selecciones electorales con mayor número de adultos mayores por km².

*Descripción numérica*

```{r}
ord_p_60ymas <- ags_cps |>
  select(c(entidad, municipio, seccion, p_60ymas)) |>
  arrange(desc(p_60ymas))
ord_p_60ymas[1:10,]
```

*Descripción gráfica*

```{r}
ggplot()+
  geom_sf(data = ord_p_60ymas, aes(fill = as.numeric(p_60ymas)), color = "grey90")+
  scale_fill_gradient(low = "white", high = "#750014", na.value = "skyblue")+
  labs(fill = "Densidad de adultos mayores por km²")+
  theme_minimal()+
  theme(legend.position = "bottom")
```

Secciones electorales con mayor número de población económicamente activa por km².

*Descripción numérica*

```{r}
ord_pea <- ags_cps |>
  select(c(entidad, municipio, seccion, pea)) |>
  arrange(desc(pea))
ord_pea[1:10,]
```

*Descripción Gráfica*

```{r}
ggplot()+
  geom_sf(data = ord_pea, aes(fill = as.numeric(pea)), color = "grey90")+
  scale_fill_gradient(low = "white", high = "#750014", na.value = "skyblue")+
  labs(fill = "Densidad de población económicamente activa por km²")+
  theme_minimal()+
  theme(legend.position = "bottom")
```

Número de farmacias por sección electoral.

*Descripción Numérica*

```{r}
max_10_farm <- ags_fps[, 1:4] |>
  arrange(desc(num_farm))
max_10_farm[1:10,]
```

*Descripción gráfica*

```{r}
ggplot() +
  geom_sf(data = acs, color = "grey80", fill= "white", size = 0.1) +
  geom_sf(data = ags_farm, color = "#750014", alpha = 0.05, size = 0.5) +
  theme_minimal()
```
