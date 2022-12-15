#------------------------------------------------------------------------------#
# Proyecto:                   Mapas Indicador PARA Viridiana Ríos
# Objetivo:                   Analizar las AGEBs con mayor distancia entre 
#                             riqueza y pobreza
#
# Encargadas:                 Lorenzo León Robles
# Correos:                    lorenzoln@gmail.com
# 
# Fecha de creación:          1 de junio de 2022
# Última actualización:       21 de septiembre de 2022
#------------------------------------------------------------------------------#
# Fuentes:
# - CENSO 2020:            https://www.inegi.org.mx/programas/ccpv/2020/
# - Marco GEO 2020:        https://www.inegi.org.mx/temas/mg/#Descargas

# Funciones extra ----
`%+%` <- paste0
`%notin%` <- Negate(`%in%`)
# Setup ----
pacman::p_load(tmap,
               tidyverse,
               sf, 
               rgdal,
               revgeo,
               stringi,
               ggrepel,
               lobstr, 
               mccistyleR, ggsn,sf,htmlwidgets
)
if(!dir.exists("04_mapas/")) dir.create("04_mapas/")

# Creador mapas estados ----
geo_censo_final <- readRDS("03_generados/geo_censo_casos.rds")


edos <- geo_censo_final %>% as_tibble%>%  distinct(NOM_ENT, CVE_ENT) %>% 
  drop_na(NOM_ENT)%>% arrange(CVE_ENT)
for (i in 1:nrow(edos)) {
    edo_cve = edos[[i,"NOM_ENT"]]
  print("Doing for: " %+% edo_cve )
  mm <- tm_shape(geo_censo_final %>%
                   filter(CVE_ENT == edos[[i,"CVE_ENT"]]),name = "Capa AGEBS")+
    tm_polygons(col = "caso_riqueza",
                title = "AGEBS por caso de riqueza",
                border.alpha =.2,
                border.col="black",
                alpha = .5, 
                colorNA = NULL,
                palette = c("pobres"="red","resto"= "grey","ricos"= "green"),
                popup.vars = c("Clave Geográfica"="CVEGEO",
                               "Municipio"="NOM_MUN",
                               "Localidad"="NOM_LOC",
                               "20til de Personas de 18 a 24 que van a la escuela: "="UPUP_personas_18_24_que_van_a_la_escuela_ntile",
                               "20til de Grado promedio de escolaridad ntile: "="UP_Grado_promedio_de_escolaridad_ntile",
                               "20til de Personas con servicios de salud privados: "="UPUP_personas_afiliadas_a_servicios_de_salud_privados_ntile",
                               "20til de Viviendas con autos: "="UP_viviendas_con_auto_camioneta_ntile",
                               "20til de Viviendas con PCs: "="UP_viviendas_con_pc_laptop_tablet_ntile",
                               "20til de Viviendas con Internet: "="UP_viviendas_con_internet_ntile",
                               "20til de Viviendas con TV paga: "="UP_viviendas_con_television_paga_ntile",
                               "20til de Viviendas con Videojuegos: "="UP_viviendas_con_videojuegos_ntile",
                               #"20til de Población niños que 3-5 que no van a escuela: "="DOWNDOWN_ninos_3_5_que_no_van_a_la_escuela_ntile",
                               "20til de Ocupantes por cuarto: "="down_promedio_ocupantes_por_cuarto_ntile",
                               "20til de Analfabetismo 15+: "="P15YM_AN_pob_pobtile",
                               "20til de Pob primaria inacabada: "="P15PRI_IN_pob_pobtile",
                               "20til de Viviendas sin Agua entubada: "="VPH_AGUAFV_pob_pobtile",
                               "20til de Viviendas sin drenaje: "="VPH_NODREN_pob_pobtile",
                               "20til de Viviendas piso de tierra: "="VPH_PISOTI_pob_pobtile",
                               "20til de Viviendas sin electricidad: "="VPH_S_ELEC_pob_pobtile",
                               "Densidad: "="densidad",
                               "Medida Riqueza" = "sum_ntile",
                               "Medida Pobreza" ="sum_pobtile",
                               "Población Total" = "POBTOT"))+
    tmap_options(check.and.fix = TRUE,basemaps = c(Mapa= "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",Satelite = "Esri.WorldImagery" #,
                                                   #Topografico = "Esri.WorldTopoMap"
    ))
  m <- tmap_leaflet(mm)
  if(!dir.exists("04_mapas/edos/")) dir.create("04_mapas/edos/")
  saveWidget(m, file="04_mapas/edos/map_"%+%edo_cve%+%".html",selfcontained = T)
}


# Creador mapas vecinos perfectos ----

check <- readRDS("03_generados/instersects_sparse_table.rds")
all_rich <- which(ifelse(is.na(geo_censo_final$caso_riqueza),F,geo_censo_final$caso_riqueza == "ricos"))
all_poor <- which(ifelse(is.na(geo_censo_final$caso_riqueza),F,geo_censo_final$caso_riqueza == "pobres"))

temp <-  cbind(CVEGEO=geo_censo_final$CVEGEO[all_rich],
               CVE_ENT=geo_censo_final$CVE_ENT[all_rich],
               NOM_ENT=geo_censo_final$NOM_ENT[all_rich],
               sum_ntile=geo_censo_final$sum_ntile[all_rich],
               sum_pobtile=geo_censo_final$sum_pobtile[all_rich],
               check[all_rich] %>% enframe) %>%
  select(-name) %>% 
  unnest(value) %>% 
  left_join(geo_censo_final %>%
              as_tibble() %>% 
              rowid_to_column(var ="value") %>%
              select(value,
                     caso_riqueza,
                     CVEGEO_vecino=CVEGEO,
                     NOM_ENT_vecino=NOM_ENT,
                     sum_ntile_vecino = sum_ntile,
                     sum_pobtile_vecino = sum_pobtile,
                     CVE_ENT_vecino = CVE_ENT)) %>% 
  select(-value) %>% 
  filter(CVEGEO_vecino!= CVEGEO) %>% 
  filter(caso_riqueza=="pobres") %>% 
  mutate(distance_rp_vecino =sqrt((sum_ntile_vecino -sum_ntile)^2+(sum_pobtile_vecino -sum_pobtile)^2)) %>% 
  arrange(desc(distance_rp_vecino)) %>% 
  mutate(rank_vecino = sort.list(distance_rp_vecino, decreasing = T)) %>% 
  arrange(rank_vecino) 

unique(geo_censo_final$NOM_ENT)[unique(geo_censo_final$NOM_ENT)%notin%unique(temp$NOM_ENT)]
all_data_for_end <- tibble()
for (i in 1:nrow(temp)) {
  
  print("Doing for: " %+% i)
  geos_temp <- geo_censo_final %>% 
    filter(CVEGEO==temp$CVEGEO[i]) %>% 
    mutate(original = .7) %>% bind_rows(
      geo_censo_final %>% 
        filter(CVEGEO%in%temp$CVEGEO_vecino[i]) %>% 
        mutate(original = .3)) %>% 
    left_join(temp %>%     
                filter(CVEGEO==temp$CVEGEO[i]) %>% 
                filter(CVEGEO_vecino%in%temp$CVEGEO_vecino[i]) %>% 
                
                distinct(CVEGEO=CVEGEO_vecino,rank_vecino) 
    ) 
  rankkk <- temp %>%     
    filter(CVEGEO==temp$CVEGEO[i]) %>% 
    filter(CVEGEO_vecino%in%temp$CVEGEO_vecino[i]) %>% 
    
    distinct(rank_vecino) %>% pull(rank_vecino)
  
  all_data_for_end <- bind_rows(all_data_for_end, geos_temp %>% as_tibble() %>% select(-geometry, -rank_vecino) %>% 
                                  mutate(rank = rankkk))
  mm <-  geos_temp %>% 
    
    tm_shape(name = "Capa AGEBS")+
    tm_polygons(col = "caso_riqueza",
                title = "AGEBS por caso de riqueza",
                border.alpha =.2,
                border.col="black",
                alpha = .5, 
                colorNA = NULL,
                palette = c("pobres"="red","resto"= "grey","ricos"= "green"),
                popup.vars = c("Clave Geográfica"="CVEGEO",
                               "Municipio"="NOM_MUN",
                               "Localidad"="NOM_LOC",
                               "Rank Desigualdad" = "rank_vecino",
                               "20til de Personas de 18 a 24 que van a la escuela: "="UPUP_personas_18_24_que_van_a_la_escuela_ntile",
                               "20til de Grado promedio de escolaridad ntile: "="UP_Grado_promedio_de_escolaridad_ntile",
                               "20til de Personas con servicios de salud privados: "="UPUP_personas_afiliadas_a_servicios_de_salud_privados_ntile",
                               "20til de Viviendas con autos: "="UP_viviendas_con_auto_camioneta_ntile",
                               "20til de Viviendas con PCs: "="UP_viviendas_con_pc_laptop_tablet_ntile",
                               "20til de Viviendas con Internet: "="UP_viviendas_con_internet_ntile",
                               "20til de Viviendas con TV paga: "="UP_viviendas_con_television_paga_ntile",
                               "20til de Viviendas con Videojuegos: "="UP_viviendas_con_videojuegos_ntile",
                               #"20til de Población niños que 3-5 que no van a escuela: "="DOWNDOWN_ninos_3_5_que_no_van_a_la_escuela_ntile",
                               "20til de Ocupantes por cuarto: "="down_promedio_ocupantes_por_cuarto_ntile",
                               "20til de Analfabetismo 15+: "="P15YM_AN_pob_pobtile",
                               "20til de Pob primaria inacabada: "="P15PRI_IN_pob_pobtile",
                               "20til de Viviendas sin Agua entubada: "="VPH_AGUAFV_pob_pobtile",
                               "20til de Viviendas sin drenaje: "="VPH_NODREN_pob_pobtile",
                               "20til de Viviendas piso de tierra: "="VPH_PISOTI_pob_pobtile",
                               "20til de Viviendas sin electricidad: "="VPH_S_ELEC_pob_pobtile",
                               "Densidad: "="densidad",
                               "Medida Riqueza" = "sum_ntile",
                               "Medida Pobreza" ="sum_pobtile",
                               "Población Total" = "POBTOT"))+
    tmap_options(check.and.fix = TRUE,basemaps = c(Mapa= "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",Satelite = "Esri.WorldImagery" #,
                                                   #Topografico = "Esri.WorldTopoMap"
    ))
  m <- tmap_leaflet(mm)
  if(!dir.exists("04_mapas/Ricos y sus vecinos pobres colindantes perfectos/")) dir.create("04_mapas/Ricos y sus vecinos pobres colindantes perfectos/")
  saveWidget(m, file="04_mapas/Ricos y sus vecinos pobres colindantes perfectos/vec_perf_"%+%temp[[i, "NOM_ENT"]] %+%"_rank_"%+%i%+%".html")
}

geo_censo_final %>% 
  filter(CVEGEO %in% 
           (readxl::read_excel("03_generados/viejas_ignorar_pero_no_borrar.xlsx") %>% pull(CVEGEO))
         ) %>% as_tibble() %>% 
  select(-geometry) %>% 
  openxlsx::write.xlsx("03_generados/seleccionadas_agebs_2.xlsx", overwrite = T)

all_data_for_end%>% 
  openxlsx::write.xlsx("03_generados/vecinos_perfectos.xlsx", overwrite = T)

all_data_for_end %>% 
  filter(CVEGEO %in% (readxl::read_excel("03_generados/viejas_ignorar_pero_no_borrar.xlsx") %>% pull(CVEGEO)) ) %>% 
  select(CVEGEO, NOM_ENT, NOM_MUN, NOM_LOC, rank) %>% 
  view
# Creador mapas clusters ----

all_rich <- which(ifelse(is.na(geo_censo_final$caso_riqueza),F,geo_censo_final$caso_riqueza == "ricos"))
all_poor <- which(ifelse(is.na(geo_censo_final$caso_riqueza),F,geo_censo_final$caso_riqueza == "pobres"))
all_check_rich <- cbind(CVEGEO=geo_censo_final$CVEGEO[all_rich],
                        CVE_ENT=geo_censo_final$CVE_ENT[all_rich],
                        NOM_ENT=geo_censo_final$NOM_ENT[all_rich],
                        caso_riqueza=geo_censo_final$caso_riqueza[all_rich],
                        sum_ntile=geo_censo_final$sum_ntile[all_rich],
                        sum_pobtile=geo_censo_final$sum_pobtile[all_rich],
                        check[all_rich] %>% enframe) %>%
  select(-name) %>% 
  unnest(value) %>% 
  left_join(geo_censo_final %>%
              as_tibble() %>% 
              rowid_to_column(var ="value") %>%
              select(value,
                     caso_riqueza,
                     CVEGEO,
                     NOM_ENT,
                     sum_ntile,
                     sum_pobtile,
                     CVE_ENT),suffix = c("","_vecino"),by = "value"
  ) %>% 
  select(-value) %>% 
  filter(CVEGEO_vecino  != CVEGEO) %>% 
  left_join(cbind(CVEGEO_vecino=geo_censo_final$CVEGEO,
                  CVE_ENT_vecino=geo_censo_final$CVE_ENT,
                  NOM_ENT_vecino=geo_censo_final$NOM_ENT,
                  sum_ntile_vecino=geo_censo_final$sum_ntile,
                  sum_pobtile_vecino=geo_censo_final$sum_pobtile,
                  check %>% enframe)) %>%
  select(-name) %>% 
  unnest(value) %>% 
  left_join(geo_censo_final %>%
              as_tibble() %>% 
              rowid_to_column(var ="value") %>%
              select(value,
                     caso_riqueza,
                     CVEGEO,
                     NOM_ENT,
                     sum_ntile,
                     sum_pobtile,CVE_ENT
              ),suffix = c("","_vecino2"),by = "value"
  )  %>% 
  select(-value) %>% 
  filter(CVEGEO_vecino  != CVEGEO &
           CVEGEO_vecino2 != CVEGEO_vecino &
           CVEGEO_vecino2 != CVEGEO 
  ) %>% 
  filter(caso_riqueza_vecino=="pobres"|caso_riqueza_vecino2=="pobres") %>% 
  mutate(
    checking = case_when(
      caso_riqueza_vecino=="pobres" ~  CVE_ENT_vecino == CVE_ENT,
      caso_riqueza_vecino2=="pobres" ~  CVE_ENT_vecino2 == CVE_ENT,
    ),
    dif_riquez = case_when(
      caso_riqueza_vecino == "pobres" ~ abs(sum_ntile - sum_ntile_vecino),
      caso_riqueza_vecino2 == "pobres" ~ abs(sum_ntile - sum_ntile_vecino2)
    ),
    dif_pobreza= case_when(
      caso_riqueza_vecino == "pobres" ~ abs(sum_pobtile_vecino - sum_pobtile),
      caso_riqueza_vecino2 == "pobres" ~ abs(sum_pobtile_vecino2 - sum_pobtile)
    ),
    
  )
ddd <- unit(200000, "cm")
ttt <- geo_censo_final %>% 
  filter(CVEGEO%in%all_check_rich$CVEGEO) %>% 
  st_buffer(ddd) %>% 
  select(CVEGEO) %>% 
  st_join(geo_censo_final %>% 
            filter(CVEGEO%in%all_check_rich$CVEGEO)%>% 
            select(CVEGEO))

i <-  1
grouping_var  <-  ttt %>% as_tibble() %>% distinct(CVEGEO.x) %>% pull(CVEGEO.x)
all_groups <- tibble()
while(length(grouping_var)>0) {
  save <- ttt %>% as_tibble() %>% 
    filter(CVEGEO.y == grouping_var[1]  ) %>% 
    transmute(CVEGEO.x, group = i)
  all_groups <- bind_rows(save, all_groups)
  grouping_var = grouping_var[grouping_var%notin%save$CVEGEO.x]
  i = i + 1
}

for (i in 1:max(all_groups$group)) {
  print("Doing for: " %+% i)
  vecinos <- all_check_rich %>% 
    filter(CVEGEO%in%all_groups$CVEGEO.x[all_groups$group == i]) %>% 
    distinct(CVEGEO = CVEGEO_vecino)  %>% 
    rbind(all_check_rich %>% 
            filter(CVEGEO%in%all_groups$CVEGEO.x[all_groups$group == i]) %>% 
            distinct(CVEGEO = CVEGEO_vecino2) ) %>% 
    distinct(CVEGEO) %>% 
    pull(CVEGEO)
  mm <-  geo_censo_final %>% 
    filter(CVEGEO%in%(all_groups$CVEGEO.x[all_groups$group == i])) %>% 
    mutate(original = .7) %>% 
    bind_rows(geo_censo_final %>% 
                filter(CVEGEO%in%(vecinos)) %>% 
                mutate(original = .3)) %>% 
    #distinct(CVEGEO, .keep_all = T) %>% 
    tm_shape(name = "Capa AGEBS")+
    tm_polygons(col = "caso_riqueza",
                title = "AGEBS por caso de riqueza",
                border.alpha =.2,
                border.col="black",
                alpha = .5, 
                colorNA = "darkgrey",
                palette = c("pobres"="red","resto"= "cyan","ricos"= "green"),
                popup.vars = c("Clave Geográfica"="CVEGEO",
                               "Municipio"="NOM_MUN",
                               "Localidad"="NOM_LOC",
                               "20til de Personas de 18 a 24 que van a la escuela: "="UPUP_personas_18_24_que_van_a_la_escuela_ntile",
                               "20til de Grado promedio de escolaridad ntile: "="UP_Grado_promedio_de_escolaridad_ntile",
                               "20til de Personas con servicios de salud privados: "="UPUP_personas_afiliadas_a_servicios_de_salud_privados_ntile",
                               "20til de Viviendas con autos: "="UP_viviendas_con_auto_camioneta_ntile",
                               "20til de Viviendas con PCs: "="UP_viviendas_con_pc_laptop_tablet_ntile",
                               "20til de Viviendas con Internet: "="UP_viviendas_con_internet_ntile",
                               "20til de Viviendas con TV paga: "="UP_viviendas_con_television_paga_ntile",
                               "20til de Viviendas con Videojuegos: "="UP_viviendas_con_videojuegos_ntile",
                               #"20til de Población niños que 3-5 que no van a escuela: "="DOWNDOWN_ninos_3_5_que_no_van_a_la_escuela_ntile",
                               "20til de Ocupantes por cuarto: "="down_promedio_ocupantes_por_cuarto_ntile",
                               "20til de Analfabetismo 15+: "="P15YM_AN_pob_pobtile",
                               "20til de Pob primaria inacabada: "="P15PRI_IN_pob_pobtile",
                               "20til de Viviendas sin Agua entubada: "="VPH_AGUAFV_pob_pobtile",
                               "20til de Viviendas sin drenaje: "="VPH_NODREN_pob_pobtile",
                               "20til de Viviendas piso de tierra: "="VPH_PISOTI_pob_pobtile",
                               "20til de Viviendas sin electricidad: "="VPH_S_ELEC_pob_pobtile",
                               "Densidad: "="densidad",
                               "Medida Riqueza" = "sum_ntile",
                               "Medida Pobreza" ="sum_pobtile",
                               "Población Total" = "POBTOT"))+
    tmap_options(check.and.fix = TRUE,basemaps = c(Mapa= "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",Satelite = "Esri.WorldImagery" #,
                                                   #Topografico = "Esri.WorldTopoMap"
    ))
  m <- tmap_leaflet(mm)
  if(!dir.exists("04_mapas/Ricos y sus vecinos pobres (o vecinos de vecinos pobres)/")) dir.create("04_mapas/Ricos y sus vecinos pobres (o vecinos de vecinos pobres)/")
  saveWidget(m, file="04_mapas/Ricos y sus vecinos pobres (o vecinos de vecinos pobres)/num_"%+%i%+%".html")
}

