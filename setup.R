# ==============================================================================
# ---- Relevamiento Cerro Azul ----
# ==============================================================================
# ---- Carga de datos y paquetes ----
pacman::p_load(data.table, 
              ggplot2, 
              openxlsx)
excel <- list.files(pattern = "24")
domicilios <- openxlsx::read.xlsx(excel,
                                  sheet = "Datos de domicilio") |> 
  janitor::clean_names() |> 
  data.table()


individuos <- openxlsx::read.xlsx(excel,
                                  sheet = "Datos del individuo") |> 
  janitor::clean_names() |> 
  data.table()


# ---- Limpieza ----

domicilios[, numero_de_habitantes := as.numeric(numero_de_habitantes)]
domicilios[capacidad_de_acopio_de_agua_l %like% "50000", capacidad_de_acopio_de_agua_l := 50000]
domicilios[, capacidad_de_acopio_de_agua_l := as.numeric(capacidad_de_acopio_de_agua_l)]

habitados <- domicilios[se_encuentran_personas %ilike% "^s"]

individuos[edad_anos %ilike% "meses|nac", edad_anos := 0]
individuos[, edad_anos := as.numeric(edad_anos)]
individuos[, es_adulto := edad_anos >= 18]
individuos[, es_menor := edad_anos < 18]

composicion_hogar <- individuos[, .(
  n_adultos = sum(es_adulto, na.rm = TRUE),
  n_menores = sum(es_menor, na.rm = TRUE),
  # Para el caso de mujer a cargo: necesitamos saber el sexo del único adulto
  sexo_unico_adulto = if (sum(es_adulto, na.rm = TRUE) == 1) {
    first(sexo_registrado_en_el_dni[es_adulto])
  } else {
    NA_character_
  }
), by = no_de_domicilio]

composicion_hogar[, un_solo_adulto_a_cargo := n_adultos == 1 & n_menores >= 1]

composicion_hogar[, una_mujer_a_cargo := n_adultos == 1 & 
                    n_menores >= 1 & 
                    sexo_unico_adulto %like% "Mujer|femenino"]

total_viviendas  <-  nrow(habitados)

# ==============================================================================
# ---- Demografía ----
# ==============================================================================

media_habitantes <- habitados[, numero_de_habitantes |>  mean(na.rm=T)]
unipersonales <- paste0(round(habitados[numero_de_habitantes == 1, .N] / total_viviendas * 100),
                        "%")
                        
mayores_65 <- paste0(round(individuos[, min(edad_anos, na.rm=T), no_de_domicilio][V1 >= 65, .N] / total_viviendas * 100),
                        "%")

viviendas_un_adulto <- paste0(round(composicion_hogar[(un_solo_adulto_a_cargo), .N] / total_viviendas * 100),
                     "%")

viviendas_mujer_cargo <- composicion_hogar[(una_mujer_a_cargo), .N]
# prop_un_adulto <- round(viviendas_un_adulto / total_viviendas * 100, 1)
# prop_mujer_cargo <- round(viviendas_mujer_cargo / total_viviendas * 100, 1)

lgbt <- paste0(individuos[, round(sum(se_reconoce_del_colectivo_lgtbiq=="Sí", na.rm = T) /.N, 2) * 100 , ], "%")

indigena <- paste0(individuos[, round(sum(se_reconoce_indigena=="Sí", na.rm = T) /.N, 2) * 100 , ], "%")

afro <- paste0(individuos[, round(sum(se_reconoce_afrodescendiente=="Sí", na.rm = T) /.N, 2) * 100 , ], "%")

alguno <- paste0(individuos[, round(sum(se_reconoce_del_colectivo_lgtbiq=="Sí" | 
                                          se_reconoce_indigena=="Sí"|
                                          se_reconoce_afrodescendiente=="Sí", na.rm=T) /.N, 2)*100, ], "%")
# ==============================================================================
# ---- Infraestructura ----
# ==============================================================================
# ---- Agua ----
fuente_agua <- habitados[, .(`Red pública domiciliaria` = sum(
  fuente_de_agua_1 %ilike% "domiciliaria"|
    fuente_de_agua_2 %ilike% "domiciliaria"|
    fuente_de_agua_3 %ilike% "domiciliaria"),
  `Red pública fuera del domicilio` = sum(
    fuente_de_agua_1 %ilike% "domicilio"|
      fuente_de_agua_2 %ilike% "domicilio"|
      fuente_de_agua_3 %ilike% "domicilio"),
    `Recolección de agua de lluvia` = sum(
      fuente_de_agua_1 %ilike% "lluvia"|
        fuente_de_agua_2 %ilike% "lluvia"|
        fuente_de_agua_3 %ilike% "lluvia"),
  `Pozo o perforación` = sum(
    fuente_de_agua_1 %ilike% "pozo"|
      fuente_de_agua_2 %ilike% "pozo"|
      fuente_de_agua_3 %ilike% "pozo") 
  )] |> melt(variable.name = "Fuente de agua (puede haber más de una por vivienda)", value.name = "Total")
fuente_agua[, `%` := round(Total /habitados[,.N] * 100, 2)]


# Tabla resumen de acopio de agua
acopio_agua <- data.table(
  `Manejo de agua` = c(
    "Recolecta agua de lluvia",
    "Cuenta con tanque de agua",
    "Cuenta con cisterna",
    "Cuenta con pileta",
    "Separa aguas grises de aguas negras",
    "Trata aguas negras"
  ),
  Total = c(
    habitados[, sum(recoleccion_de_agua_de_lluvia == "Sí", na.rm = TRUE)],
    habitados[, sum(tanque_de_agua == "Sí", na.rm = TRUE)],
    habitados[, sum(cisterna == "Sí", na.rm = TRUE)],
    habitados[, sum(pileta == "Sí", na.rm = TRUE)],
    habitados[, sum(separa_aguas_grises_de_aguas_negras == "Sí", na.rm = TRUE)],
    habitados[, sum(las_aguas_negras_reciben_tratamiento == "Sí", na.rm = TRUE)]
    
  )
)

# Agregar total de viviendas
total_viviendas <- habitados[, .N]

# Calcular porcentaje
acopio_agua[, `%` := round(Total / total_viviendas * 100, 1)]

# Agregar formato de presentación
acopio_agua[, `% (n/total)` := paste0(`%`, "% (", Total, "/", total_viviendas, ")")]

habitados[, acopio_agua := cut(capacidad_de_acopio_de_agua_l,
                               right = FALSE,
                               breaks= c(0, 1000, 5000, 10000, 25000, Inf),
                               labels = c("Menos de 1.000 litros", 
                                          "Entre 1.000 y 4.999 litros",
                                          "Entre 5.000 y 9.999 litros",
                                          "Entre 10.000 y 24.999 litros",
                                          "25.000 litros o más"))]
habitados[is.na(acopio_agua), acopio_agua := "Sin dato"]
habitados[, acopio_agua := factor(acopio_agua, levels = c("Sin dato",
                                                          "Menos de 1.000 litros", 
                                                          "Entre 1.000 y 4.999 litros",
                                                          "Entre 5.000 y 9.999 litros",
                                                          "Entre 10.000 y 24.999 litros",
                                                          "25.000 litros o más"
                                                          ))]
# ---- Servicios ----

# Tabla resumen de servicios
servicios <- data.table(
  `Servicios` = c(
    "Luz eléctrica",
    "Alumbrado público",
    "Gas natural",
    "Garrafa",
    "Cocina y/o calefacción con leña",
    "Energías renovables",
    "Wi-Fi",
    "Datos móviles"
  ),
  Total = c(
    habitados[, sum(luz_electrica == "Sí", na.rm = TRUE)],
    habitados[, sum(alumbrado_publico_en_la_cuadra == "Sí", na.rm = TRUE)],
    habitados[, sum(gas_natural == "Sí", na.rm = TRUE)],
    habitados[, sum(garrafa == "Sí", na.rm = TRUE)],
    habitados[, sum(calefaccion_y_o_cocina_con_lena == "Sí", na.rm = TRUE)],
    habitados[, sum(energias_renovables == "Sí", na.rm = TRUE)],
    habitados[, sum(conexion_a_wifi_domiciliario == "Sí", na.rm = TRUE)],
    habitados[, sum(conexion_a_datos_moviles == "Sí", na.rm = TRUE)]
    
  )
)

# Agregar total de viviendas
total_viviendas <- habitados[, .N]

# Calcular porcentaje
servicios[, `%` := round(Total / total_viviendas * 100, 1)]

# Agregar formato de presentación
servicios[, `% (n/total)` := paste0(`%`, "% (", Total, "/", total_viviendas, ")")]

# ---- Vivienda ----
# [29] "el_bano_se_encuentra"                                
# [30] "y_el_tiraje_del_inodoro_es"                          
# [31] "la_vivienda_es"                                      
# [32] "material_principal_del_techo_1"                      
# [33] "material_principal_del_techo_2"                      
# [34] "material_principal_de_los_muros_1"                   
# [35] "material_principal_de_los_muros_2"                   
# [36] "material_principal_de_los_pisos_1"                   
# [37] "material_principal_de_los_pisos_2"                   
# [38] "m2_cubiertos"                                        
# [39] "m2_semicubiertos"  

habitados[, vivienda := fcase(la_vivienda_es == "propia?", "Propia",
                              la_vivienda_es %ilike% "alq|pres", "Alquilada o prestada",
                              default = "Sin dato")]


techo <- data.table(
  `Material del techo` = c("Ladrillo y/o cemento", "Chapa", "Tierra/Barro/Adobe", "Madera", "Piedra, teja o cerámicos", "Pre-fabricada", "Sin datos"),
  Domicilios = c(domicilios[material_principal_del_techo_1 %ilike% "^a"| material_principal_del_techo_2 %ilike% "^a", .N],
    domicilios[material_principal_del_techo_1 %ilike% "^b"| material_principal_del_techo_2 %ilike% "^b", .N],
    domicilios[material_principal_del_techo_1 %ilike% "^c"| material_principal_del_techo_2 %ilike% "^c", .N], 
    domicilios[material_principal_del_techo_1 %ilike% "^d"| material_principal_del_techo_2 %ilike% "^d", .N],
    domicilios[material_principal_del_techo_1 %ilike% "^e"| material_principal_del_techo_2 %ilike% "^e", .N],
    domicilios[material_principal_del_techo_1 %ilike% "^f"| material_principal_del_techo_2 %ilike% "^f", .N],
    domicilios[is.na(material_principal_del_techo_1) & is.na(material_principal_del_techo_2), .N]))
techo[, `%` := round(Domicilios /domicilios[,.N] * 100, 2)]

muros <- data.table(
  `Material de los muros` = c("Ladrillo y/o cemento", "Chapa", "Tierra/Barro/Adobe", "Madera", "Piedra, teja o cerámicos", "Pre-fabricada", "Sin datos"),
  Domicilios = c(domicilios[material_principal_de_los_muros_1 %ilike% "^a"| material_principal_de_los_muros_2 %ilike% "^a", .N],
                 domicilios[material_principal_de_los_muros_1 %ilike% "^b"| material_principal_de_los_muros_2 %ilike% "^n", .N],
                 domicilios[material_principal_de_los_muros_1 %ilike% "^c"| material_principal_de_los_muros_2 %ilike% "^c", .N],
                 domicilios[material_principal_de_los_muros_1 %ilike% "^d"| material_principal_de_los_muros_2 %ilike% "^d", .N],
                 domicilios[material_principal_de_los_muros_1 %ilike% "^e"| material_principal_de_los_muros_2 %ilike% "^e", .N],
                 domicilios[material_principal_de_los_muros_1 %ilike% "^f"| material_principal_de_los_muros_2 %ilike% "^f", .N],
                 domicilios[is.na(material_principal_de_los_muros_1) & is.na(material_principal_de_los_muros_2), .N]))
muros[, `%` := round(Domicilios /domicilios[,.N] * 100, 2)]  


pisos <- data.table(
  `Material de los pisos` = c("Ladrillo y/o cemento", "Chapa", "Tierra/Barro/Adobe", "Madera", "Piedra, teja o cerámicos", "Pre-fabricada", "Sin datos"),
  Domicilios = c(domicilios[material_principal_de_los_pisos_1 %ilike% "^a"| material_principal_de_los_pisos_2 %ilike% "^a", .N],
                 domicilios[material_principal_de_los_pisos_1 %ilike% "^b"| material_principal_de_los_pisos_2 %ilike% "^b", .N],
                 domicilios[material_principal_de_los_pisos_1 %ilike% "^c"| material_principal_de_los_pisos_2 %ilike% "^c", .N],
                 domicilios[material_principal_de_los_pisos_1 %ilike% "^d"| material_principal_de_los_pisos_2 %ilike% "^d", .N],
                 domicilios[material_principal_de_los_pisos_1 %ilike% "^e"| material_principal_de_los_pisos_2 %ilike% "^e", .N],
                 domicilios[material_principal_de_los_pisos_1 %ilike% "^f"| material_principal_de_los_pisos_2 %ilike% "^f", .N],
                 domicilios[is.na(material_principal_de_los_pisos_1) & is.na(material_principal_de_los_pisos_2), .N]))
pisos[, `%` := round(Domicilios /domicilios[,.N] * 100, 2)]


# 1. Construcción explícita
tabla_materiales <- data.table(
  Material   = techo[[1]],
  n_techo    = techo$Domicilios,
  pct_techo  = techo$`%`,
  n_muros    = muros$Domicilios,
  pct_muros  = muros$`%`,
  n_pisos    = pisos$Domicilios,
  pct_pisos  = pisos$`%`
)
# ==============================================================================
# ---- Salud ----
# ==============================================================================
# ---- Patologías ----
columnas_salud <- c(
  "hipertension_arterial", "diabetes", "celiaquia", 
  "enfermedades_respiratorias", "problemas_neurologicos", 
  "problemas_autoinmunes", "problemas_hormonales", 
  "tumores_o_cancer", "otros_problemas_de_salud"
)

tabla_salud <- lapply(columnas_salud, function(col) {
  # Calculamos métricas para la patología actual
  n_casos <- individuos[get(col) == "Sí", .N]
  pct <- round(n_casos / individuos[, .N] * 100, 1)
  edad_prom <- round(individuos[get(col) == "Sí", mean(as.numeric(edad_anos), na.rm = TRUE)], 1)
  
  # Retornamos una fila formateada
  data.table(
    Patología = gsub("_", " ", col) |> tools::toTitleCase(), # Limpiamos el nombre
    Casos = n_casos,
    `%` = pct,
    `Edad Promedio` = edad_prom
  )
}) |> rbindlist()

# 3. Ordenar por prevalencia (opcional, pero recomendado)
setorder(tabla_salud, -Casos)
tabla_salud <- tabla_salud[order(Patología %ilike% "Otros", -Casos)]


# ---- Asistencia a CAPS ----
habitados[is.na(con_que_frecuencia_el_grupo_familiar_asiste_al_caps),
          con_que_frecuencia_el_grupo_familiar_asiste_al_caps := "Sin datos"]
habitados[, con_que_frecuencia_el_grupo_familiar_asiste_al_caps := factor(
  con_que_frecuencia_el_grupo_familiar_asiste_al_caps, levels = c(
    "Muy frecuentemente", "Frecuentemente", 
    "A veces", "Muy rara vez", "Nunca", "Sin datos"
  )
)]
# ---- Prioridades de salud ----

# [52] "prioridades_de_salud_1"                              
# [53] "prioridades_de_salud_2"                              
# [54] "prioridades_de_salud_3"
# prioridades <- habitados[, unique(.SD), 
#           .SDcols = names(habitados)[names(habitados) %ilike% "prioridades_de_salud"]]
# prioridades <- rbind(prioridades$prioridades_de_salud_1 |> c(),
#       prioridades$prioridades_de_salud_2|> c(),
#       prioridades$prioridades_de_salud_3|> c()) |> c()
# prioridades[!is.na(prioridades)]

# Pasar a formato largo
# 1. Identify the target columns
cols_prioridades <- names(habitados)[names(habitados) %ilike% "prioridades_de_salud"]

# 2. Subset and melt simultaneously
prioridades_long <- melt(
  habitados[, .SD, .SDcols = cols_prioridades], 
  measure.vars = cols_prioridades, 
  value.name = "Prioridad", 
  na.rm = TRUE
)
prioridades_long[, Categoria_General := fcase(
  # 1. Emergencias (Prioridad máxima de captura)
  Prioridad %ilike% "guardia|urgencia|emergencia|ambulancia|traslado|posta|permanente", "Guardias y Emergencias",
  
  # 2. Horarios y Disponibilidad de personal
  Prioridad %ilike% "horario|l a v|todos los d[ií]as|tarde|m[aá]s m[eé]dicos|profesionales", "Ampliación de Horarios",
  
  # 3. Odontología (Muy específica, fácil de aislar)
  Prioridad %ilike% "odonto|denti", "Odontología",
  
  # 4. Salud Mental y Adicciones
  Prioridad %ilike% "mental|psicol|psiquiatr|consumo|psico", "Salud Mental y Adicciones",
  
  # 5. Pediatría 
  Prioridad %ilike% "pediatr|niñ", "Pediatría",
  
  # 6. Resto de Especialidades Médicas y Estudios complementarios
  Prioridad %ilike% "especialidad|ginecolog|oftalmol|oculista|kinesiol|fonoaudiolog|traumatolog|cl[ií]nic|general|ecograf|radiograf|laboratorio", "Especialidades y Estudios",
  
  # 7. Determinantes Ambientales (Crucial en encuestas territoriales)
  Prioridad %ilike% "agua|ambient|saneamiento", "Agua y Salud Ambiental",
  
  # 8. Prevención, Promoción y Crónicas
  Prioridad %ilike% "prevenci|vacun|dengue|hipertensi|diabetes|respiratoria|educaci[oó]n|alimentaria", "Prevención y Crónicas",
  
  # 9. Insumos y Logística
  Prioridad %ilike% "equipamiento|medicaci|oxigeno|numeracion", "Insumos e Infraestructura",
  
  # 10. Calidad de atención / Vínculo
  Prioridad %ilike% "empat[ií]a|conforme|buen|respetuoso|cara", "Trato y Calidad de Atención",
  
  # 11. Otros grupos específicos
  Prioridad %ilike% "mayor|j[oó]ven", "Poblaciones Específicas",
  Prioridad %ilike% "veterinaria", "Veterinaria",
  Prioridad %ilike% "alternativa", "Medicina Alternativa",
  
  # Categoría por defecto para lo que no atrape el regex
  default = "Otros"
)]

# Verificar qué cayó en "Otros" para refinar el regex si hace falta
# prioridades_long[Categoria_General == "Otros", unique(Prioridad)]
# Contar menciones únicas por hogar (para no duplicar si alguien puso lo mismo en 1 y 2)
resumen_prioridades <- prioridades_long[, .(Menciones = .N), by = Prioridad][order(-Menciones)]

# ---- Discapacidad ----
# [25] "limitacion_o_dificultad_1"                             
# [26] "limitacion_o_dificultad_2"                             
# [27] "limitacion_o_dificultad_3"                             
# [28] "limitacion_o_dificultad_4"                             
# [29] "cuenta_con_certificado_de_discapacidad"                
# [30] "problematica_psicosocial_en_el_ultimo_ano" 
reporta_limitacion <- paste0(round(individuos[!is.na(limitacion_o_dificultad_1) &
                                          !limitacion_o_dificultad_1 %ilike% "ninguna", 
                                        .N] / nrow(individuos) * 100),
                             "%")

tiene_cud <- paste0(round(individuos[cuenta_con_certificado_de_discapacidad == "Sí",.N ] / nrow(individuos) * 100),
                    "%")

reporta_problema_psicosocial <- paste0(round(individuos[problematica_psicosocial_en_el_ultimo_ano == "Sí",.N ] / nrow(individuos) * 100),
                                       "%")

# --- Wordcloud ----
# Extract the raw text vector, dropping NAs
raw_texts <- prioridades_long[!is.na(Prioridad), Prioridad]

# Convert to lowercase and remove accents for consistency
texts_clean <- tolower(stringi::stri_trans_general(raw_texts, "Latin-ASCII"))

# Replace punctuation with spaces
texts_clean <- gsub("[[:punct:]]", " ", texts_clean)

# Split strings into individual words (tokens) and unlist into a vector
tokens <- unlist(strsplit(texts_clean, "\\s+"))

# Cast to data.table
dt_words <- data.table(word = tokens)

# Filter out empty strings
dt_words <- dt_words[word != ""]

# Define a vector of Spanish stopwords to exclude
# You can append more words to this vector if you spot noise in your final chart
stopwords_es <- c("de", "v", "l", "la", "el", "en", "y", "a", "los", "las", "un", "una", 
                  "mas", "que", "con", "por", "para", "su", "lo", "se", "del", "al", "o")

# Keep only the meaningful words
dt_words <- dt_words[!word %in% stopwords_es]

word_freq <- dt_words[, .(freq = .N), by = word][order(-freq)]



# ==============================================================================
# # ---- Empleo y educación ----
# ==============================================================================
individuos[is.na(condicion_de_actividad_laboral), 
           condicion_de_actividad_laboral := "Sin datos"]
individuos[is.na(si_cuenta_con_trabajo_marque_el_principal), 
           si_cuenta_con_trabajo_marque_el_principal := "Sin datos"]
individuos[is.na(el_rubro_principal_en_el_cual_se_desempena), 
           el_rubro_principal_en_el_cual_se_desempena := "Sin datos"]

# [31] "condicion_de_actividad_laboral"                        
# [32] "si_cuenta_con_trabajo_marque_el_principal"             
# [33] "el_rubro_principal_en_el_cual_se_desempena"            
# [34] "percibe_jubilacion"                                    
# [35] "percibe_pension_no_contributiva_1"                     
# [36] "percibe_pension_no_contributiva_2"                     
# [37] "percibe_pension_no_contributiva_3"                     
# [38] "percibe_alguno_de_los_siguientes_programas_sociales_1" 
# [39] "percibe_alguno_de_los_siguientes_programas_sociales_2" 
# [40] "percibe_alguno_de_los_siguientes_programas_sociales_3" 
individuos[is.na(percibe_pension_no_contributiva_1), percibe_pension_no_contributiva_1 := "Sin datos"]

# menores_trabajadores <- 
adultos_mayores_trabajadores <- paste0( round(individuos[edad_anos > 64 &! condicion_de_actividad_laboral %ilike% "no|sin",.N] / individuos[edad_anos > 64,.N] * 100),
                                       "%")
adultos_mayores_jubilados <- paste0( round(individuos[edad_anos > 64 & percibe_jubilacion == "Sí",.N] / individuos[edad_anos > 64,.N] * 100),
                                     "%")
adultos_mayores_pensionados <-  paste0(round(individuos[edad_anos > 64 & !percibe_pension_no_contributiva_1 %ilike% "no",.N] / individuos[edad_anos > 64,.N] * 100),
                                    "%")

# Identificar columnas
cols_prog <- names(individuos)[names(individuos) %ilike% "programas_sociales"]

# Derretir, filtrar NAs, y contar por programa
programas_tab <- melt(individuos[, ..cols_prog], 
                      measure.vars = cols_prog, 
                      na.rm = TRUE)[value != "No", .("Beneficiarios" = .N), by = .(Programa = value)]

# Ordenar por frecuencia
setorder(programas_tab, -Beneficiarios)

# ---- Educación ----
individuos[is.na(nivel_educativo_al_que_asiste_o_maximo_que_curso),nivel_educativo_al_que_asiste_o_maximo_que_curso := "Sin datos" ]
individuos[, nivel_educativo_al_que_asiste_o_maximo_que_curso := 
             factor(nivel_educativo_al_que_asiste_o_maximo_que_curso,
                    levels = c("Sin datos", "Ninguno", "Nivel Inicial",
                               "Primario", "Secundario",
                               "Terciario", "Universitario", "Posgrado"))]

# ==============================================================================
# ---- Transporte ----
# ==============================================================================
# Identificar columnas

cols_viaja <- names(habitados)[names(habitados) %ilike% "viaja"]

# Derretir, filtrar NAs, y contar 
viaja_tab <- melt(habitados[, ..cols_viaja], 
                      measure.vars = cols_viaja, 
                      na.rm = TRUE)[value != "No", .("Familias" = .N), by = .(Motivo = variable)]

# Ordenar por frecuencia
setorder(viaja_tab, -Familias)
viaja_tab[, Motivo := gsub("viaja_para_", "", Motivo)]

viaja_tab[, Motivo := gsub("_", " ", Motivo)]

viaja_tab[, Motivo := tools::toTitleCase(Motivo)]

viaja_tab[, Motivo := gsub("Culturales O Recreativas", "Cultura/Recreación", Motivo)]

viaja_tab[, `%` := round(Familias / habitados[,.N] * 100, 1)]

# Derretir, filtrar NAs, y contar
cols_traslada <- names(habitados)[names(habitados) %ilike% "traslada"]
traslada_tab <- melt(habitados[, ..cols_traslada], 
                  measure.vars = cols_traslada, 
                  na.rm = TRUE)[value != "No", .("Familias" = .N), by = .(Medio = variable)]

# Ordenar por frecuencia
setorder(traslada_tab, -Familias)
traslada_tab[, Medio := gsub("se_traslada_en_", "", Medio)]

traslada_tab[, Medio := gsub("_", " ", Medio)]

traslada_tab[, Medio := tools::toTitleCase(Medio)]

traslada_tab[Medio == "Se Traslada Caminando", Medio := "A pié"]

traslada_tab[, `%` := round(Familias / habitados[,.N] * 100, 1)]


# ==============================================================================
# ---- Cultura y comunicación ----
# ==============================================================================
# [44] "cuales_son_las_redes_sociales_de_su_preferencia_1"     
# [45] "cuales_son_las_redes_sociales_de_su_preferencia_2"     
# [46] "cuales_son_las_redes_sociales_de_su_preferencia_3"     
# [47] "que_productos_culturales_le_interesan_1"               
# [48] "que_productos_culturales_le_interesan_2"               
# [49] "que_productos_culturales_le_interesan_3"   

# ---- Medios ----
# Derretir, filtrar NAs, y contar
cols_informa <- names(individuos)[names(individuos) %ilike% "informa"]
informa_tab <- melt(individuos[, ..cols_informa], 
                     measure.vars = cols_informa, 
                     na.rm = TRUE)[value != "No", .("Personas" = .N), by = .(Medio = value)]

# Ordenar por frecuencia
setorder(informa_tab, -Personas)

# ---- Redes ----

# Derretir, filtrar NAs, y contar
cols_redes <- names(individuos)[names(individuos) %ilike% "redes"]
redes_tab <- melt(individuos[, ..cols_redes], 
                    measure.vars = cols_redes, 
                    na.rm = TRUE)[value != "No", .("Personas" = .N), by = .(Red = value)]

# Ordenar por frecuencia
setorder(informa_tab, -Personas)
informa_tab[, `%` := round(Personas / individuos[,.N] * 100, 1)]

# ---- Piechart redes ----
# 1. Crear el grupo etario usando fcase
individuos[, grupo_etario := fcase(
  edad_anos <= 15, "Hasta 15 años",
  edad_anos >= 16 & edad_anos <= 25, "16 a 25 años",
  edad_anos >= 26 & edad_anos <= 40, "26 a 40 años",
  edad_anos > 40, "Más de 40 años",
  default = "Sin datos" # Por si hay NAs en la edad_anos
)]

# 2. Convertir a factor para que mantenga el orden lógico
individuos[, grupo_etario := factor(grupo_etario, 
                                    levels = c("Hasta 15 años", "16 a 25 años", 
                                               "26 a 40 años", "Más de 40 años", "Sin datos"))]

# 3. Hacer el melt, pero AHORA guardando el grupo etario como id.var
cols_redes <- names(individuos)[names(individuos) %ilike% "redes_sociales"]

redes_long <- melt(individuos, 
                   id.vars = "grupo_etario",     # <--- CLAVE: Conservamos la edad
                   measure.vars = cols_redes, 
                   value.name = "Red", 
                   na.rm = TRUE)[Red != "No"]    # Filtramos los "No" directamente

# 4. (Opcional) Limpiar un poco los nombres de las redes si es necesario
redes_long[, Red := trimws(Red)]


# ---- Talleres y eventos ----
conocen_talleres <- paste0(round(habitados[conocen_los_talleres == "Sí",.N] / habitados[,.N] * 100),
                           "%")
asisten_talleres <-  paste0(round(habitados[alguien_del_grupo_familiar_asiste_a_los_talleres == "Sí",.N] / habitados[,.N] * 100),
                            "%")
habitados[is.na(si_los_conocen_y_no_asisten_por_que_motivo), si_los_conocen_y_no_asisten_por_que_motivo := "Sin datos"]
# ---- Cultura ----
# 1. Identificación y Limpieza de Datos (Nivel Hogares - habitados)
# -------------------------------------------------------------------------
cols_cultura <- names(habitados)[names(habitados) %ilike% "acciones_culturales"]

# Melt eficiente: Solo nos traemos las columnas de texto, sin IDs basura
cultura_long <- melt(habitados[, ..cols_cultura], 
                     measure.vars = cols_cultura, 
                     value.name = "Propuesta", 
                     na.rm = TRUE)

# Limpieza básica
cultura_long[, Propuesta := tolower(trimws(Propuesta))]

# 2. Categorización por Ejes de Gestión
# -------------------------------------------------------------------------
cultura_long[, Categoria := fcase(
  Propuesta %ilike% "peña|festival|patronales|feria|circo|recital|baile|fiesta|musica", "Eventos y Festivales",
  Propuesta %ilike% "taller|curso|capacitacion|oficio|apoyo|tics|manejo de|guitarra|instrumento|pintura|cocina", "Talleres y Formación",
  Propuesta %ilike% "deporte|ajedrez|futbol|gimnasio|yoga|pilates|caminata|tai chi|fisica", "Deporte y Bienestar",
  Propuesta %ilike% "niñx|infancia|adolescente|joven|mayor", "Segmentos Específicos (Infancias/ Adultos mayores)",
  Propuesta %ilike% "baño|wifi|biblioteca|lugar fisico|espacio|comunicacion", "Infraestructura y Servicios",
  default = "Otras Expresiones Culturales"
)]

# 3. Procesamiento de Texto para la Nube (Bigramas y Tokens)
# -------------------------------------------------------------------------
# Definir stop-words específicas
stop_cultura <- c(stopwords_es, "todos", "todo", "haciendo", "están", "estan", "lindo", "bien", "cosas")

# Definir conceptos que deben ir juntos (Pegamento)
conceptos_clave <- list(
  c("turismo internacional", "turismo_internacional"),
  c("apoyo escolar", "apoyo_escolar"),
  c("economia circular", "economia_circular"),
  c("encuentro de escultores", "encuentro_escultores"),
  c("danza folklorica", "danza_folklorica"),
  c("niños y adolescentes", "niñxs_adolescentes"),
  c("personas mayores", "personas_mayores")
)

# Aplicar el pegamento
texto_pegado_cultura <- cultura_long$Propuesta
for(par in conceptos_clave) {
  texto_pegado_cultura <- gsub(par[1], par[2], texto_pegado_cultura)
}

# Tokenizar (Usando un nombre ÚNICO para evitar contaminación de salud)
tokens_cultura_final <- unlist(strsplit(texto_pegado_cultura, "\\s+"))

# 4. Crear Tabla de Frecuencias
# -------------------------------------------------------------------------
word_freq_cultura <- data.table(word = tokens_cultura_final)[
  !word %in% stop_cultura & word != "", 
  .(freq = .N), 
  by = word
][order(-freq)]

# Preparar para visualización
word_freq_cultura[, word_display := gsub("_", " ", word)]

# Paleta de colores consistente
paleta_cultura <- c("#2d5a27", "#4a4a4a", "#8fb989", "#d4a373", "#b36239")
word_freq_cultura[, color_paleta := sample(paleta_cultura, .N, replace = TRUE)]


# ==============================================================================
# ---- Ambiente ----
# ==============================================================================
# ---- Residuos ----
cols_residuos <- names(habitados)[names(habitados) %ilike% "residuos|carton|compos|huert"]

# Derretir, filtrar NAs, y contar 
residuos_tab <- melt(habitados[, ..cols_residuos], 
                  measure.vars = cols_residuos, 
                  na.rm = TRUE)[value != "No", .("Familias" = .N), by = .(Manejo = variable)]

# Ordenar por frecuencia
setorder(residuos_tab, -Familias)

residuos_tab[, Manejo := gsub("_", " ", Manejo)]

residuos_tab[, Manejo := tools::toTitleCase(Manejo)]

residuos_tab[, `%` := round(Familias / habitados[,.N] * 100, 1)]


# ---- Animales ----

cols_animales <- names(habitados)[names(habitados) %ilike% "tenencia|castrados|antirr"]

# Derretir, filtrar NAs, y contar 
animales_tab <- melt(habitados[, ..cols_animales], 
                     measure.vars = cols_animales, 
                     na.rm = TRUE)[value != "No", .("Familias" = .N), by = .(Manejo = variable)]

# Ordenar por frecuencia
setorder(animales_tab, -Familias)

animales_tab[, Manejo := gsub("_", " ", Manejo)]

animales_tab[, Manejo := tools::toTitleCase(Manejo)]

animales_tab[, `%` := round(Familias / habitados[,.N] * 100, 1)]

animales_tab[Manejo %ilike% "antirr", Manejo := "Antirrábica en el Último año"]

viviendas_domesticos <- paste0(round(habitados[total_de_animales_domesticos > 0, .N] / total_viviendas * 100),
                               "%")

promedio_domesticos <- habitados[total_de_animales_domesticos > 0, mean(total_de_animales_domesticos)] |> round(1)

viviendas_granja <- paste0( round(habitados[total_de_animales_de_granja > 0, .N] / total_viviendas * 100),
                               "%")

promedio_granja <- habitados[total_de_animales_de_granja > 0, mean(total_de_animales_de_granja)]|> round(1)

# ---- Problemáticas ambientales más importantes ----

# 1. Identificación y Limpieza de Datos (Nivel Hogares - habitados)
# -------------------------------------------------------------------------
cols_ambiente <- names(habitados)[names(habitados) %ilike% "problematicas_ambientales"]

# Melt eficiente: Solo nos traemos las columnas de texto, sin IDs basura
ambiente_long <- melt(habitados[, ..cols_ambiente], 
                     measure.vars = cols_ambiente, 
                     value.name = "Problemática", 
                     na.rm = TRUE)

# Limpieza básica
ambiente_long[, Problemática := tolower(trimws(Problemática))]
# 2. Categorización por Ejes de Gestión
# -------------------------------------------------------------------------
ambiente_long[, Categoría := fcase(
  Problemática %ilike% "fuego|incendi", "Incendios",
  Problemática %ilike% "agua|río|rio|sequ|napa|cloac", "Cuidado del agua y el río",
  Problemática %ilike% "cantera", "Canteras",
  Problemática %ilike% "caballo|perro|animal|suelto", "Manejo de animales de compañía y granja",
  Problemática %ilike% "planta|autócto|autocto|tala|poda|monte|nativa|guardapa|exót|exot", "Cuidado del bosque nativo y flora",
  Problemática %ilike% "ruid|vehi|auto|sonor|audit|luces", "Contaminación visual y auditiva / vehículos",
  Problemática %ilike% "residu|tacho|basur|quema|rsu", "Residuos sólidos urbanos",
  Problemática %ilike% "calle|alumbrado|vía|via|eléc|elec|luz", "Vialidad e infraestructura",
  default = "Otros problemas ambientales"
)]

# # 3. Procesamiento de Texto para la Nube (Bigramas y Tokens)
# # -------------------------------------------------------------------------
# # Definir stop-words específicas
stop_amb <- c(stopwords_es)
# 
# # Definir conceptos que deben ir juntos (Pegamento)
conceptos_clave_amb <- list(
  c("animales sueltos", "animales_sueltos"),
  c("bosque nativo", "bosque_nativo")
)
# 
# # Aplicar el pegamento
texto_pegado_amb <- ambiente_long$Problemática
for(par in conceptos_clave_amb) {
  texto_pegado_amb <- gsub(par[1], par[2], texto_pegado_amb)
}
# 
# # Tokenizar (Usando un nombre ÚNICO para evitar contaminación de salud)
tokens_amb_final <- unlist(strsplit(texto_pegado_amb, "\\s+"))
# 
# # 4. Crear Tabla de Frecuencias
# # -------------------------------------------------------------------------
word_freq_amb <- data.table(word = tokens_amb_final)[
  !word %in% stop_amb & word != "",
  .(freq = .N),
  by = word
][order(-freq)]
# 
# # Preparar para visualización
word_freq_amb[, word_display := gsub("_", " ", word)]
# 
# # Paleta de colores consistente
paleta_amb <- c("#2d5a27", "#4a4a4a", "#8fb989", "#d4a373", "#b36239")
word_freq_amb[, color_paleta := sample(paleta_amb, .N, replace = TRUE)]
