#------------------------------------------------------------------------------#
# Proyecto:                   ANÁLISIS DE CENSO PARA Viridiana Ríos
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

options (digits=4, scipen=8, show.signif.stars=FALSE,
         dplyr.width=Inf, tibble.print_max=20, tibble.print_min=1)

ntile_special <- function (x = row_number(), n) 
{
  if (!missing(x)) {
    x <- rank(x, ties.method = "min",na.last = "keep")
  }
  len <- length(x) - sum(is.na(x))
  n <- as.integer(floor(n))
  if (len == 0L) {
    rep(NA_integer_, length(x))
  }  else {
    n_larger <- as.integer(len%%n)
    n_smaller <- as.integer(n - n_larger)
    size <- len/n
    larger_size <- as.integer(ceiling(size))
    smaller_size <- as.integer(floor(size))
    larger_threshold <- larger_size * n_larger
    bins <- if_else(x <= larger_threshold, (x + (larger_size - 
                                                   1L))/larger_size, (x + (-larger_threshold + smaller_size - 
                                                                             1L))/smaller_size + n_larger)
    as.integer(floor(bins))
  }
}
entidad_to_abr <- function(entidad) {
  y <- stri_trans_general(entidad,"latin-ascii" )
  y <- stri_replace_all(y, regex = "[:punct:]", "")
  y <- str_to_title(y)
  
  case_when(
    y == "Tabasco" ~ "TAB",
    y == "Nayarit" ~ "NAY",
    y == "Durango" ~ "DUR",
    stri_detect(y, fixed ="oaxaca", case_insensitive = T) ~ "OAX",
    y == "Oaxaca" ~ "OAX",
    y == "Mexico" ~ "MEX",
    y == "Edomex" ~ "MEX",
    y == "Estado De Mexico" ~ "MEX",
    y == "Campeche" ~ "CAM",
    y == "Zacatecas" ~ "ZAC",
    y == "Quintana Roo" ~ "QUI",
    y == "Sonora" ~ "SON",
    y == "Cdmx" ~ "CDM",
    y == "Distrito Federal" ~ "CDM",
    y == "Ciudad De Mexico" ~ "CDM",
    y == "Veracruz De Ignacio De La Llave" ~ "VER",
    y == "Veracruz" ~ "VER",
    y == "Baja California Sur" ~ "BCS",
    y == "Morelos" ~ "MOR",
    y == "Guanajuato" ~ "GUA",
    y == "Jalisco" ~ "JAL",
    y == "Tamaulipas" ~ "TAM",
    y == "Guerrero" ~ "GUE",
    y == "Baja California" ~ "BJC",
    y == "Nuevo Leon" ~ "NOL",
    y == "Chihuahua" ~ "CHA",
    y == "San Luis Potosi" ~ "SLP",
    y == "Tlaxcala" ~ "TLA",
    y == "Yucatan" ~ "YUC",
    y == "Puebla" ~ "PUE",
    y == "Coahuila De Zaragoza" ~ "COA",
    y == "Coahuila" ~ "COA",
    y == "Colima" ~ "COL",
    y == "Hidalgo" ~ "HID",
    y == "Queretaro" ~ "QUE",
    y == "Sinaloa" ~ "SIN",
    y == "Chiapas" ~ "CHS",
    y == "Michoacan De Ocampo" ~ "MIC",
    y == "Michoacan" ~ "MIC",
    y == "Aguascalientes" ~ "AGS",
    y == "Nacional" ~ "NAC",
    y == "Nacion" ~ "NAC",
    y == "Republica" ~ "NAC",
    y == "Republica Federal" ~ "NAC",
    T ~ y
  )
}
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
folder_de_censo <- "01_datos/censo"
folder_de_censo <- "~/SynologyDrive/WORK/Viri_geo/01_datos/censo"
# Unión bases y geo ----
hacer_con_cuantil_por_tamano = T
if (F) {
  # No es necesario correr
  all_censos <- list.files(folder_de_censo, pattern = "cpv2020.csv", full.names = T, recursive = T)
  all_censos <- all_censos[stri_detect(all_censos, fixed ="conjunto_de_datos")]
  if(length(all_censos)==0){
    all_censos <- list.files(folder_de_censo, pattern = "csv", full.names = T, recursive = T)
    all_censos <- all_censos[stri_detect(all_censos, fixed ="zip")]
    for(fil in all_censos){
      unzip(fil, exdir = folder_de_censo)
    }
    all_censos <- list.files(folder_de_censo, pattern = "cpv2020.csv", full.names = T, recursive = T)
    all_censos <- all_censos[stri_detect(all_censos, fixed ="conjunto_de_datos")]
  }
  all_censos_data <- tibble()
  count_filter <- tibble()
  for (edo_file in all_censos) {
    print("doing for: "%+% edo_file)
    censo_ricos_intentar <- read.csv(edo_file) %>% 
      filter(!stri_detect(NOM_MUN, fixed ="total", case_insensitive=T))%>% 
      filter(!stri_detect(NOM_LOC, fixed ="total", case_insensitive=T))%>% 
      #filter(stri_detect(NOM_LOC, fixed ="Total AGEB", case_insensitive=T))%>% 
      mutate(across(c(PROM_HNV,P3YM_HLI,P_3YMAS,PCON_DISC,POBTOT,
                      P3A5_NOA,P_3A5,P15A17A,P_15A17,P18A24A,P_18A24,
                      P15PRI_CO,P_15YMAS,P18YM_PB,GRAPROES,PAFIL_IPRIV,
                      PDER_SS,P12YM_SOLT,P_12YMAS,PSIN_RELIG,POBHOG,PRO_OCUP_C,TOTHOG,
                      VPH_3YMASC,TVIVPARHAB,VPH_AUTOM,VPH_PC,VPH_INTER,VPH_SPMVPI,
                      P15YM_AN,P15PRI_IN,POCUPADA,VPH_AGUAFV,VPH_NODREN,
                      VPH_PISOTI,VPH_S_ELEC,VIVPAR_HAB,VIVPARH_CV,
                      VPH_CVJ),as.numeric)) %>% 
      select(PROM_HNV,P3YM_HLI,P_3YMAS,PCON_DISC,POBTOT,
             P3A5_NOA,P_3A5,P15A17A,P_15A17,P18A24A,P_18A24,
             P15PRI_CO,P_15YMAS,P18YM_PB,GRAPROES,PAFIL_IPRIV,
             PDER_SS,P12YM_SOLT,P_12YMAS,PSIN_RELIG,POBHOG,PRO_OCUP_C,TOTHOG,
             VPH_3YMASC,TVIVPARHAB,VPH_AUTOM,VPH_PC,VPH_INTER,VPH_SPMVPI,
             P15YM_AN,P15PRI_IN,POCUPADA,VPH_AGUAFV,VPH_NODREN,
             VPH_PISOTI,VPH_S_ELEC,VIVPAR_HAB,VIVPARH_CV,
             VPH_CVJ, 
             CVE_ENT = ENTIDAD, 
             CVE_MUN = MUN,
             CVE_LOC = LOC,
             CVE_AGEB =AGEB,
             CVE_MZA = MZA,
             NOM_ENT,
             NOM_MUN,
             NOM_LOC
      ) %>% 
      mutate(
        CVE_ENT = stri_pad_left(CVE_ENT, width=2,pad="0"),
        CVE_MUN = stri_pad_left(CVE_MUN, width=3,pad="0"),
        CVE_LOC = stri_pad_left(CVE_LOC, width=4,pad="0"),
        CVE_MZA = stri_pad_left(CVE_MZA, width=3,pad="0"),
        #DOWNDOWN_ninos_3_5_que_no_van_a_la_escuela = P3A5_NOA / P_3A5,
        UPUP_personas_18_24_que_van_a_la_escuela = P18A24A / P_18A24,
        UP_Grado_promedio_de_escolaridad = GRAPROES,
        UPUP_personas_afiliadas_a_servicios_de_salud_privados = PAFIL_IPRIV / POBTOT,
        down_promedio_ocupantes_por_cuarto = PRO_OCUP_C,
        UP_viviendas_con_auto_camioneta = VPH_AUTOM / VIVPARH_CV ,
        UP_viviendas_con_pc_laptop_tablet = VPH_PC / VIVPARH_CV ,
        UP_viviendas_con_internet = VPH_INTER / VIVPARH_CV ,
        UP_viviendas_con_television_paga = VPH_SPMVPI / VIVPARH_CV ,
        UP_viviendas_con_videojuegos = VPH_CVJ / VIVPARH_CV,
        
        across(c(P15YM_AN,P15PRI_IN),
               list(pob= ~./P_15YMAS)),
        across(c(VPH_AGUAFV,VPH_NODREN,
                 VPH_PISOTI,VPH_S_ELEC),
               list(pob=  ~./VIVPARH_CV)),
        PRO_OCUP_C_pob = PRO_OCUP_C
      ) %>% 
      ungroup() %>% 
      mutate(across(c(starts_with(c("DOWN", "UP")),ends_with(c("pob"))),~ifelse(is.nan(.),NA,.))) %>% 
      group_by(CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN, CVE_LOC, NOM_LOC, CVE_AGEB
      ) %>% 
      summarise(across(starts_with(c("DOWN", "UP")),~mean(., na.rm=T)),
                across(ends_with(c("pob")),~mean(., na.rm=T)),
                POBTOT = sum(POBTOT, na.rm = T),
                VIVPARH_CV = sum(VIVPARH_CV,na.rm = T),
      ) 
    nom_ent <- unique(censo_ricos_intentar$CVE_ENT)
    
    censo_ricos_intentar$count_filter <- rowSums(censo_ricos_intentar %>% ungroup %>% select(starts_with(c("DOWN", "UP")),ends_with(c("pob"))) %>% mutate(across(everything(),is.na)))<=4
    count_filter <- bind_rows(count_filter, censo_ricos_intentar %>% count(NOM_ENT, count_filter))
    temp_intento <- censo_ricos_intentar %>% 
      ungroup() %>% 
      filter(count_filter) %>% 
      filter(POBTOT>=150) %>% 
      filter(!is.na(POBTOT)) %>% 
      mutate(across(c(starts_with(c("DOWN", "UP")),ends_with(c("pob"))),~ifelse(is.nan(.),NA,.)))
    
    max_ntiles <- min(20, nrow(temp_intento)-5)
    if(hacer_con_cuantil_por_tamano){
      temp_intento <- temp_intento %>%
        #group_by(CVE_ENT) %>% 
        mutate(across(starts_with(c("up")),list(ntile = ~ntile_special(., max_ntiles))),
               across(starts_with(c("down")),list(ntile = ~max_ntiles+1-ntile_special(., max_ntiles))),
               across(ends_with(c("pob")),list(pobtile = ~ntile_special(., max_ntiles))),
               
        ) 
    } else {
      for (er in temp_intento %>% select(starts_with(c("up", "down")), ends_with("pob") )%>% names) {
        y = 20
        while (T) {
          lenn <- length(unique(quantile(temp_intento[[er]],
                                         probs = seq(0, 1, length =  y),
                                         na.rm = TRUE,type = 2)))
          if(lenn == y){
            break
          } else {
            y = y - 1
          }
          if(y<=0){
            break
          }
        }
        print("Doing for : " %+% er %+% "; with length: " %+%  lenn )
        #max_ntiles <- min(max_ntiles, lenn )
        if(stri_startswith(er, fixed = "up", case_insensitive = T)){
          nom = er%+%"_ntile"
          temp_intento[[nom]] = floor(as.numeric(cut(temp_intento[[er]],
                                                     breaks = quantile(temp_intento[[er]],
                                                                       probs = seq(0, 1, length =  y),
                                                                       na.rm = TRUE,type = 2),
                                                     include.lowest = TRUE,right = T))*(20/(y-1)))
        }
        if(stri_startswith(er, fixed = "down", case_insensitive = T)){
          nom = er%+%"_ntile"
          temp_intento[[nom]] = 21-floor(as.numeric(cut(temp_intento[[er]],
                                                        breaks = quantile(temp_intento[[er]],
                                                                          probs = seq(0, 1, length =  y),
                                                                          na.rm = TRUE,type = 2),
                                                        include.lowest = TRUE,right = T))*(20/(y-1)))
        }
        if(stri_endswith(er, fixed = "pob", case_insensitive = T)){
          nom = er%+%"_pobtile"
          temp_intento[[nom]] = floor(as.numeric(cut(temp_intento[[er]],
                                                     breaks = quantile(temp_intento[[er]],
                                                                       probs = seq(0, 1, length =  y),
                                                                       na.rm = TRUE,type = 2),
                                                     include.lowest = TRUE,right = T))*(20/(y-1)))
        }
      }
    }

    
    temp_intento <- temp_intento %>%
      rowwise() %>% 
      mutate(sum_ntile = mean(c_across(ends_with("ntile")), na.rm = T),
             sum_pobtile = mean(c_across(ends_with("pobtile")), na.rm = T)
      ) %>% 
      ungroup() %>% 
      mutate(
        NOM_ENT = entidad_to_abr(NOM_ENT)
      )
    
    ricos <- quantile(temp_intento$sum_ntile, probs = (1-(1/max_ntiles)*2), na.rm=T)
    pobres <- quantile(temp_intento$sum_pobtile,probs = (1-(1/max_ntiles)*2), na.rm=T)
    medio_ricos <- quantile(temp_intento$sum_ntile,probs = (1/max_ntiles)*10, na.rm=T)
    medio_pobres <- quantile(temp_intento$sum_pobtile,probs = (1/max_ntiles)*10, na.rm=T)
    cuantiles <- data.frame(ricos=ricos,
                            pobres=pobres,
                            max_ntiles = max_ntiles,
                            medio_ricos=medio_ricos,
                            medio_pobres=medio_pobres,NOM_ENT=unique(temp_intento$NOM_ENT),CVE_ENT =nom_ent )
    
    temp_intento <-temp_intento %>% 
      left_join(cuantiles)
    all_censos_data <- bind_rows(temp_intento, all_censos_data)
    }
  all_censos_data %>% saveRDS("03_generados/all_censos_data_sum.rds")
}
geo_shp <- read_sf("01_datos/marco_geoestadistico_2021/mg2021_integrado/conjunto_de_datos/00a.shp")  %>% 
  mutate(area = st_area(geometry))
units(geo_shp$area) <- units::make_units(km*km)
all_censos_data  <- readRDS("03_generados/all_censos_data_sum.rds")


geo_censo_final <- geo_shp %>% 
  left_join(all_censos_data)%>% 
  st_transform(crs = 4326)%>% 
  mutate(densidad = POBTOT/area,
         densidad = ifelse(is.na(densidad),0,densidad)) %>% 
  mutate(
    caso_riqueza = case_when(
     # NOM_ENT %notin%c("ZAC", "SON" ,"SIN", "SLP", "NAY" ,"GUE" ,"GUA", "DUR", "CHA" ,"BCS") &
        sum_ntile >= ricos & 
        POBTOT >= 150 & 
        down_promedio_ocupantes_por_cuarto_ntile >=                    max_ntiles-ceiling(max_ntiles/4)+floor(max_ntiles/6) &
        UPUP_personas_afiliadas_a_servicios_de_salud_privados_ntile >= max_ntiles-ceiling(max_ntiles/4)+floor(max_ntiles/6) &
        UP_viviendas_con_internet_ntile >=                             max_ntiles-ceiling(max_ntiles/4)+floor(max_ntiles/6) & 
        densidad > 30 ~ "ricos",
     # NOM_ENT %in%c("ZAC", "SON" ,"SIN", "SLP", "NAY" ,"GUE" ,"GUA", "DUR", "CHA" ,"BCS") &
     #   sum_ntile >= ricos & 
     #   POBTOT >= 150 & 
     #   down_promedio_ocupantes_por_cuarto_ntile >=                    max_ntiles-ceiling(max_ntiles/2)+floor(max_ntiles/6) &
     #   UPUP_personas_afiliadas_a_servicios_de_salud_privados_ntile >= max_ntiles-ceiling(max_ntiles/2)+floor(max_ntiles/6) &
     #   UP_viviendas_con_internet_ntile >=                             max_ntiles-ceiling(max_ntiles/2)+floor(max_ntiles/6) & 
     #   densidad > 30 ~ "ricos",
      sum_pobtile >= pobres & 
        POBTOT >= 150&
        sum_ntile < medio_ricos & 
       densidad > 30~"pobres",
      is.na(NOM_MUN) ~NA_character_,
      T ~ "resto"
    )
  )

geo_censo_final %>% as_tibble()%>% count(NOM_ENT, caso_riqueza) %>% 
  filter(!is.na(caso_riqueza)) %>% 
  pivot_wider(names_from = caso_riqueza, values_from = n) %>% view

geo_censo_final %>% saveRDS("03_generados/geo_censo_casos.rds")
geo_censo_final <- readRDS("03_generados/geo_censo_casos.rds")
tuti <- geo_censo_final %>% 
  as_tibble() %>%
  select(-geometry) %>%
  filter(!is.na(caso_riqueza)) %>% 
  select(CVEGEO,
         Area_Km2  = area,
         Entidad = NOM_ENT,
         Municipio = NOM_MUN,
         Localidad	= NOM_LOC,
         Población_Total	= POBTOT,
         Viviendas_habitadas = VIVPARH_CV,
         Indicador_Riqueza	= sum_ntile                                                  ,
         Indicador_Pobreza	= sum_pobtile ,
         Caso_riqueza = caso_riqueza,
         Densidad_poblacional  = densidad,
         #Porcentaje_ninos_3_5_que_no_van_a_la_escuela = DOWNDOWN_ninos_3_5_que_no_van_a_la_escuela	,
         #Cuantil_Porcentaje_ninos_3_5_que_no_van_a_la_escuela = DOWNDOWN_ninos_3_5_que_no_van_a_la_escuela_ntile	,
         Promedio_ocupantes_por_cuarto = down_promedio_ocupantes_por_cuarto	,
         Cuantil_Promedio_ocupantes_por_cuarto = down_promedio_ocupantes_por_cuarto_ntile	,
         Porcentaje_personas_18_24_que_van_a_la_escuela = UPUP_personas_18_24_que_van_a_la_escuela	,
         Cuantil_Porcentaje_personas_18_24_que_van_a_la_escuela = UPUP_personas_18_24_que_van_a_la_escuela_ntile,
         Grado_promedio_de_escolaridad = UP_Grado_promedio_de_escolaridad	,
         Cuantil_Grado_promedio_de_escolaridad = UP_Grado_promedio_de_escolaridad_ntile	,
         Porcentaje_personas_afiliadas_a_servicios_de_salud_privados = UPUP_personas_afiliadas_a_servicios_de_salud_privados,
         Cuantil_Porcentaje_personas_afiliadas_a_servicios_de_salud_privados = UPUP_personas_afiliadas_a_servicios_de_salud_privados_ntile,
         Porcentaje_viviendas_con_auto_camioneta = UP_viviendas_con_auto_camioneta,
         Cuantil_Porcentaje_viviendas_con_auto_camioneta = UP_viviendas_con_auto_camioneta_ntile,
         Porcentaje_viviendas_con_pc_laptop_tablet = UP_viviendas_con_pc_laptop_tablet,
         Cuantil_Porcentaje_viviendas_con_pc_laptop_tablet = UP_viviendas_con_pc_laptop_tablet_ntile,
         Porcentaje_viviendas_con_internet = UP_viviendas_con_internet,
         Cuantil_Porcentaje_viviendas_con_internet = UP_viviendas_con_internet_ntile,
         Porcentaje_viviendas_con_television_paga = UP_viviendas_con_television_paga,
         Cuantil_Porcentaje_viviendas_con_television_paga = UP_viviendas_con_television_paga_ntile,
         Porcentaje_viviendas_con_videojuegos = UP_viviendas_con_videojuegos,
         Cuantil_Porcentaje_viviendas_con_videojuegos = UP_viviendas_con_videojuegos_ntile,
         Porcentaje_personas_mayores_15_analfabetas = P15YM_AN_pob,
         Cuantil_Porcentaje_personas_mayores_15_analfabetas = P15YM_AN_pob_pobtile,
         Porcentaje_personas_mayores_15_primaria_incompleta = P15PRI_IN_pob,
         Cuantil_Porcentaje_personas_mayores_15_primaria_incompleta = P15PRI_IN_pob_pobtile,
         Porcentaje_viviendas_sin_agua_entubada = VPH_AGUAFV_pob,
         Cuantil_Porcentaje_viviendas_sin_agua_entubada = VPH_AGUAFV_pob_pobtile,
         Porcentaje_viviendas_sin_drenaje = VPH_NODREN_pob,
         Cuantil_Porcentaje_viviendas_sin_drenaje = VPH_NODREN_pob_pobtile,
         Porcentaje_viviendas_con_pisos_de_tierra = VPH_PISOTI_pob,
         Cuantil_Porcentaje_viviendas_con_pisos_de_tierra = VPH_PISOTI_pob_pobtile,
         Porcentaje_viviendas_sin_red_electrica = VPH_S_ELEC_pob,
         Cuantil_Porcentaje_viviendas_sin_red_electrica = VPH_S_ELEC_pob_pobtile
         ) 
tuti%>% 
  rename_all(~stri_replace_all(., fixed ="_", " ")) %>% 
  openxlsx::write.xlsx("03_generados/censo_casos.xlsx")


tuti %>%
  mutate(across(starts_with("Porcentaje"),~ifelse(is.infinite(.), NA, .)))%>% 
  #filter(if_any(starts_with("porcentaje"),~.>1)) %>%
  summary()

# Ubicación de vecinos ----

if(F){
  # No es necesario hacer
  check <- st_intersects(geo_censo_final%>% sf::st_make_valid(),
                         geo_censo_final%>% sf::st_make_valid(), 
                         remove_self =T)
  check %>% saveRDS("03_generados/instersects_sparse_table.rds")
  tabla_intersecciones <- tibble()
  for(i in 1:length(check)){
    print("Doing for: " %+% i)
    tabla_intersecciones <- tabla_intersecciones %>% 
      bind_rows(data.frame(original = geo_censo_final$CVEGEO[i],
                           vecinos = geo_censo_final$CVEGEO[check[[i]]]) %>% 
                  filter(original != vecinos)
                 # group_by(original) %>% 
                  #summarise(vecinos = paste(vecinos, collapse = ", "))
                )
    
  }
  tabla_intersecciones %>% 
    group_by(original) %>% 
    summarise(vecinos = paste(vecinos, collapse = ", ")) %>% 
    openxlsx::write.xlsx("03_generados/tabla_vecinos.xlsx")
  
}
check <- readRDS("03_generados/instersects_sparse_table.rds")

# Calcular distancia ----
# solo con vecinos de todos las AGEBS ricas

all_rich <- which(ifelse(is.na(geo_censo_final$caso_riqueza),F,geo_censo_final$caso_riqueza == "ricos"))
all_poor <- which(ifelse(is.na(geo_censo_final$caso_riqueza),F,geo_censo_final$caso_riqueza == "pobres"))

all_check_rich <-  cbind(CVEGEO=geo_censo_final$CVEGEO[all_rich],
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

all_check_rich %>% 
  openxlsx::write.xlsx("03_generados/distancia_vecinos_perfectos.xlsx")

# Calculadora de distancia ----

calculadora_distancia_entre_agebs <- function(cvegeo1, cvegeo2) {
  temp <- geo_censo_final %>% 
    filter(CVEGEO==cvegeo1)
  temp2 <- geo_censo_final %>% 
    filter(CVEGEO==cvegeo2)
  if(nrow(temp)==0 | nrow(temp2) == 0){
    stop("No existe alguna de las AGEBs seleccionadas")
  }
  tt <-  temp %>%
    as_tibble() %>% 
    select(CVEGEO,
           CVE_ENT,
           NOM_ENT,
           sum_ntile,
           sum_pobtile,
           caso_riqueza) %>% 
    mutate(value = cvegeo2) %>% 
    left_join(temp2 %>%
                as_tibble() %>% 
                mutate(value = cvegeo2) %>%
                select(value,
                       caso_riqueza,
                       CVEGEO,
                       NOM_ENT,
                       sum_ntile,
                       sum_pobtile,
                       CVE_ENT),suffix = c("","_vecino"), by = "value") %>% 
    select(-value) %>% 
    #filter(CVEGEO_vecino != CVEGEO) %>% 
    #filter(caso_riqueza=="pobres") %>% 
    mutate(distance_rp_vecino =sqrt((sum_ntile_vecino -sum_ntile)^2+(sum_pobtile_vecino -sum_pobtile)^2))
  print("Distancia riqueza/pobreza del vecino: " %+% tt$distance_rp_vecino)
  tt
}

calculadora_distancia_entre_agebs("0200200010413",
                                  "0200200014488")

# Quick graphs ----
cor(tuti$Indicador_Riqueza,tuti$Indicador_Pobreza)
ut <- lm(Indicador_Riqueza ~ Indicador_Pobreza, data = tuti)
ut$coefficients[2]
uttu <- tuti %>% 
  group_by(Caso_riqueza) %>% 
  summarise(across(starts_with("indicador"),~mean(.,na.rm=T)))

x_lab = "Indicador_Pobreza"
y_lab = "Indicador_Riqueza"
tuti %>% 
  filter(Caso_riqueza != "resto") %>% 
  ggplot(aes(x=!!sym(x_lab), y = !!sym(y_lab), color = Caso_riqueza))+
  geom_jitter(data = tuti,
              aes(x=!!sym(x_lab), y = !!sym(y_lab), color = Caso_riqueza),
              height = 0, width = .1,alpha = .5 )+
  geom_jitter(height = 0, width = .1,alpha = .8 )+
  geom_smooth(data = tuti,
              aes(x=!!sym(x_lab), y = !!sym(y_lab), color = Caso_riqueza),
              method = "lm", color ="black")+
  ggplot2::annotate("label", x = 10,
                    y = ut$coefficients[1]+10*ut$coefficients[2]+1, 
                    label = round(ut$coefficients[2],3),
                    color ="black")+
  ggplot2::annotate("label", x = uttu[[3,3]],
                    y = uttu[[3,2]],
                    label = paste0("R: ",round(uttu[[3,2]],1), "\nP: ",round(uttu[[3,3]],1)),
                    color ="green")+
  ggplot2::annotate("label", x = uttu[[1,3]],
                    y = uttu[[1,2]],
                    label = paste0("R: ",round(uttu[[1,2]],1), "\nP: ",round(uttu[[1,3]],1)),
                    color ="red")+
  #hrbrthemes::scale_x_percent()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,20))+
  scale_color_manual(values = c("red", "green", "cyan"), breaks = c("pobres", "ricos", "resto"))+
  labs(x = stri_replace_all(x_lab, fixed ="_", " "),
       y = stri_replace_all(y_lab, fixed ="_", " "),
       color = "",
       title = "Comparación de Indicadores",
       caption = "Elaboración propia con datos del CENSO 2020 INEGI"
  )+theme_minimal()



