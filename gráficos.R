# source("setup.R")
library(echarts4r)
library(ggplot2)
library(kableExtra)

# ---- Pirámide poblacional ----
# 1. Filtro y recategorización de etiquetas
individuos_validos <- individuos[!is.na(as.numeric(edad_anos))]
individuos_validos[sexo_registrado_en_el_dni %like% "Mujer|femenino", sexo := "Femenino"]
individuos_validos[sexo_registrado_en_el_dni %like% "Varón|masculino", sexo := "Masculino"]

# 2. Creación de grupos decenales (ideal para poblaciones pequeñas)
individuos_validos[, grupo_etario := cut(
  as.numeric(edad_anos),
  breaks = seq(0, 100, by = 10),
  right = FALSE,
  include.lowest = TRUE,
  labels = paste0(seq(0, 90, by = 10), "-", seq(9, 99, by = 10))
)]

# 3. Agregación
piramide_plot <- individuos_validos[, .N, by = .(grupo_etario, sexo)]
piramide_plot[sexo == "Masculino", N := -N]
piramide_wide <- dcast(piramide_plot, grupo_etario ~ sexo, value.var = "N", fill = 0)

piramide <- piramide_wide |>
  e_charts(grupo_etario) |>
  e_bar(Masculino, stack = "total", name = "Masculino", itemStyle = list(color = "#b36239")) |>
  e_bar(Femenino, stack = "total", name = "Femenino", itemStyle = list(color = "#2d5a27")) |>
  e_flip_coords() |>
  # Configuración del eje X para mostrar valores positivos
  e_x_axis(
    axisLabel = list(
      formatter = htmlwidgets::JS("
        function (value) {
          return Math.abs(value);
        }
      ")
    ),
    name = "Cantidad de personas",
    nameLocation = "middle",
    nameGap = 35
  ) |>
  e_tooltip(
    trigger = "axis",
    formatter = htmlwidgets::JS("
      function(params) {
        let res = '<b>' + params[0].name + '</b><br/>';
        params.forEach(function(item) {
          let val = Array.isArray(item.value) ? item.value[0] : item.value;
          res += item.marker + ' ' + item.seriesName + ': ' + Math.abs(val) + '<br/>';
        });
        return res;
      }
    ")
  ) |>
  e_toolbox_feature(feature = "saveAsImage") |>
  e_legend(bottom = 0) |>
  e_title(text = "Pirámide poblacional", left = "center", 
          subtext = paste0("Cantidad de personas por grupo etario según sexo legal, n = ",
                           individuos_validos[,.N],
                           ", sexo femenino: ",
                           individuos_validos[, round(
                             sum(sexo_registrado_en_el_dni %ilike% "fem")/.N * 100, 2)],
                           "%")
          )

# ---- Barplots ----

mi_barplot <- function(x,
                       titulo = NULL,
                       subtitulo = NULL,
                       color_barra = "#2d5a27",        # forest-green por defecto
                       horizontal = FALSE,             # nuevo: TRUE = barras horizontales
                       rotate_labels = FALSE,          # rotar etiquetas del eje X (si vertical)
                       show_toolbox = TRUE,            # mostrar botones de descarga/datos
                       na_omit = TRUE,
                       ordenar_por_frecuencia = TRUE) {
  
  # Preparar datos
  if (is.factor(x)) {
    dt <- data.table(categoria = x)
    if (!na_omit) dt <- dt[!is.na(categoria)]
    grafico_data <- dt[, .N, by = categoria]
    if (ordenar_por_frecuencia) {
      grafico_data <- grafico_data[order(N)]
      grafico_data[, categoria := factor(categoria, levels = categoria)]
    } else {
      setorder(grafico_data, categoria)
    }
  } else if (is.character(x)) {
    dt <- data.table(categoria = as.character(x))
    if (na_omit) dt <- dt[!is.na(categoria)]
    grafico_data <- dt[, .N, by = categoria]
    
    if (ordenar_por_frecuencia) {
      # CAMBIO AQUÍ: Usamos order(N) para que el mayor sea el último 
      # y aparezca arriba en el gráfico horizontal
      grafico_data <- grafico_data[order(N)] 
      grafico_data[, categoria := factor(categoria, levels = categoria)]
    } else {
      # Para mantener coherencia, si no es por frecuencia, 
      # invertimos el orden alfabético para que la 'A' quede arriba
      grafico_data <- grafico_data[order(categoria, decreasing = TRUE)]
    }
  } else {
    stop("x debe ser un factor o character")
  }
  
  # Crear gráfico base
  p <- grafico_data |>
    e_charts(categoria, backgroundColor = "#fdfaf6")   # fondo sand-bg
  
  # Agregar barras (verticales u horizontales)
  if (!horizontal) {
    p <- p |> e_bar(
      serie = N,
      name = "Frecuencia",
      color = color_barra,
      barWidth = "60%",
      itemStyle = list(borderRadius = c(4, 4, 0, 0))
    )
  } else {
    p <- p |> e_bar(
      serie = N,
      name = "Frecuencia",
      color = color_barra,
      barWidth = "60%",
      itemStyle = list(borderRadius = c(0, 4, 4, 0))
    ) |> e_flip_coords()
  }
  
  # Eje X (según orientación)
  if (!horizontal) {
    p <- p |> e_x_axis(
      name = "",
      axisLabel = list(
        rotate = ifelse(rotate_labels, 45, 0),
        fontSize = 10,
        color = "#4a4a4a"
      ),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE)
    ) |> e_y_axis(
      name = "Frecuencia",
      nameLocation = "middle",
      nameGap = 45,
      nameTextStyle = list(fontWeight = "bold", color = "#4a4a4a"),
      axisLabel = list(fontSize = 10, color = "#4a4a4a")
    )
  } else {
    # horizontal: el eje X es el de frecuencia (abajo)
    p <- p |> e_x_axis(
      name = "Frecuencia",
      nameLocation = "middle",
      nameGap = 35,
      nameTextStyle = list(fontWeight = "bold", color = "#4a4a4a"),
      axisLabel = list(fontSize = 10, color = "#4a4a4a")
    ) |> e_y_axis(
      name = "",
      axisLabel = list(fontSize = 10, color = "#4a4a4a"),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE)
    )
  }
  
  # Tooltip unificado
  p <- p |> e_tooltip(
    trigger = "axis",
    axisPointer = list(type = "shadow"),
    formatter = htmlwidgets::JS("
      function(params) {
        return params[0].axisValue + '<br/>' +
               'Frecuencia: ' + params[0].value;
      }
    ")
  )
  
  # Toolbox (descarga y vista de datos)
  if (show_toolbox) {
    p <- p |> e_toolbox_feature(feature = "saveAsImage", title = "Guardar como imagen")
    p <- p |> e_toolbox_feature(feature = "dataView", title = "Ver datos")
  }
  
  # Leyenda (no necesaria para una sola serie, pero la ocultamos)
  p <- p |> e_legend(show = FALSE)
  
  # Grid con márgenes adecuados
  p <- p |> e_grid(containLabel = TRUE, left = "8%", right = "5%", top = "12%", bottom = "8%")
  
  # Títulos
  if (!is.null(titulo)) {
    p <- p |> e_title(
      text = titulo,
      subtext = subtitulo,
      left = "center",
      top = 5,
      textStyle = list(color = "#2d5a27", fontSize = 16, fontWeight = "bold"),
      subtextStyle = list(color = "#4a4a4a", fontSize = 11)
    )
  }
  
  return(p)
}


mi_barplot_pct <- function(x,
                           titulo = NULL,
                           subtitulo = NULL,
                           color_barra = "#2d5a27",        # forest-green por defecto
                           horizontal = FALSE,             # TRUE = barras horizontales
                           rotate_labels = FALSE,          # rotar etiquetas del eje X (si vertical)
                           show_toolbox = TRUE,            # mostrar botones de descarga/datos
                           na_omit = TRUE,
                           ordenar_por_frecuencia = TRUE,
                           decimales = 1) {                # decimales a mostrar en %
  
  # Preparar datos
  if (is.factor(x)) {
    dt <- data.table(categoria = x)
    if (!na_omit) dt <- dt[!is.na(categoria)]
    grafico_data <- dt[, .N, by = categoria]
    if (ordenar_por_frecuencia) {
      grafico_data <- grafico_data[order(N)]
      grafico_data[, categoria := factor(categoria, levels = categoria)]
    } else {
      setorder(grafico_data, categoria)
    }
  } else if (is.character(x)) {
    dt <- data.table(categoria = as.character(x))
    if (na_omit) dt <- dt[!is.na(categoria)]
    grafico_data <- dt[, .N, by = categoria]
    
    if (ordenar_por_frecuencia) {
      grafico_data <- grafico_data[order(N)]
      grafico_data[, categoria := factor(categoria, levels = categoria)]
    } else {
      grafico_data <- grafico_data[order(categoria, decreasing = TRUE)]
    }
  } else {
    stop("x debe ser un factor o character")
  }
  
  # Calcular porcentaje sobre el total (post-filtro de NA)
  grafico_data[, pct := round(100 * N / sum(N), decimales)]
  
  # Crear gráfico base
  p <- grafico_data |>
    e_charts(categoria, backgroundColor = "#fdfaf6")   # fondo sand-bg
  
  # Agregar barras (verticales u horizontales)
  if (!horizontal) {
    p <- p |> e_bar(
      serie = pct,
      name = "Porcentaje",
      color = color_barra,
      barWidth = "60%",
      itemStyle = list(borderRadius = c(4, 4, 0, 0))
    )
  } else {
    p <- p |> e_bar(
      serie = pct,
      name = "Porcentaje",
      color = color_barra,
      barWidth = "60%",
      itemStyle = list(borderRadius = c(0, 4, 4, 0))
    ) |> e_flip_coords()
  }
  
  # Eje X (según orientación)
  if (!horizontal) {
    p <- p |> e_x_axis(
      name = "",
      axisLabel = list(
        rotate = ifelse(rotate_labels, 45, 0),
        fontSize = 10,
        color = "#4a4a4a"
      ),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE)
    ) |> e_y_axis(
      name = "Porcentaje (%)",
      nameLocation = "middle",
      nameGap = 45,
      nameTextStyle = list(fontWeight = "bold", color = "#4a4a4a"),
      axisLabel = list(
        fontSize = 10, color = "#4a4a4a",
        formatter = htmlwidgets::JS("function(value){ return value + '%'; }")
      )
    )
  } else {
    p <- p |> e_x_axis(
      name = "Porcentaje (%)",
      nameLocation = "middle",
      nameGap = 35,
      nameTextStyle = list(fontWeight = "bold", color = "#4a4a4a"),
      axisLabel = list(
        fontSize = 10, color = "#4a4a4a",
        formatter = htmlwidgets::JS("function(value){ return value + '%'; }")
      )
    ) |> e_y_axis(
      name = "",
      axisLabel = list(fontSize = 10, color = "#4a4a4a"),
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE)
    )
  }
  
  # Tooltip unificado: muestra % y N absoluto
  # Usamos e_data, ya guardado en grafico_data, vía closure en el JS no es directo,
  # así que pasamos N como dataItem adicional con e_bar 'data' completo.
  p <- p |> e_tooltip(
    trigger = "axis",
    axisPointer = list(type = "shadow"),
    formatter = htmlwidgets::JS("
      function(params) {
        var d = params[0];
        return d.axisValue + '<br/>' +
               'Porcentaje: ' + d.value + '%';
      }
    ")
  )
  
  # Toolbox (descarga y vista de datos)
  if (show_toolbox) {
    p <- p |> e_toolbox_feature(feature = "saveAsImage", title = "Guardar como imagen")
    p <- p |> e_toolbox_feature(feature = "dataView", title = "Ver datos")
  }
  
  # Leyenda (no necesaria para una sola serie, pero la ocultamos)
  p <- p |> e_legend(show = FALSE)
  
  # Grid con márgenes adecuados
  p <- p |> e_grid(containLabel = TRUE, left = "8%", right = "5%", top = "12%", bottom = "8%")
  
  # Títulos
  if (!is.null(titulo)) {
    p <- p |> e_title(
      text = titulo,
      subtext = subtitulo,
      left = "center",
      top = 5,
      textStyle = list(color = "#2d5a27", fontSize = 16, fontWeight = "bold"),
      subtextStyle = list(color = "#4a4a4a", fontSize = 11)
    )
  }
  
  return(p)
}

# ---- Tablas ----

# En setup.R o en un bloque de código al inicio

tabla_univariada2 <- function(data, 
                             caption = NULL, 
                             notas = NULL,
                             digits = 1,
                             col_names = NULL,
                             align = NULL,
                             striped = TRUE,
                             hover = TRUE,
                             condensed = TRUE) {
  
  # Si no se especifican nombres de columnas, usar los que tiene data
  if (is.null(col_names)) {
    col_names <- names(data)
  }
  
  # Si no se especifica alineación, detectar tipo de cada columna
  if (is.null(align)) {
    align <- sapply(data, function(x) {
      if (is.numeric(x)) "r" else "l"
    })
    align <- paste(align, collapse = "")
  }
  
  # Crear tabla base
  tabla <- data |>
    kbl(
      booktabs = TRUE,
      caption = caption,
      col.names = col_names,
      align = align,
      digits = digits,
      format = "html"
    ) |>
    kable_styling(
      bootstrap_options = c(
        if(striped) "striped",
        if(hover) "hover",
        if(condensed) "condensed"
      ),
      full_width = TRUE,
      position = "left",
      font_size = 14
    ) |>
    row_spec(0, bold = TRUE, background = "#e8f0e7", color = "#2d5a27")
  
  # Agregar nota al pie si existe
  if (!is.null(notas)) {
    tabla <- tabla |>
      footnote(
        general = notas,
        general_title = "Nota: ",
        footnote_as_chunk = TRUE
      )
  }
  
  return(tabla)
}

library(reactable)

tabla_reactiva <- function(data,
                           caption = NULL,
                           notas = NULL,
                           digits = 1,
                           striped = TRUE,
                           highlight = TRUE,
                           compact = TRUE) {
  reactable(
    data,
    striped    = striped,
    highlight  = highlight,
    compact    = compact,
    bordered   = TRUE,
    fullWidth  = TRUE,
    defaultColDef = colDef(
      format = colFormat(digits = digits),
      headerStyle = list(
        background = "#e8f0e7",
        color      = "#2d5a27",
        fontWeight = "bold"
      )
    )
  )
}

# ---- piechart ----

mi_piechart <- function(x,
                        titulo = NULL,
                        subtitulo = NULL,
                        paleta = c("#2d5a27",  "#c47a4a","#e0d7c6", "#4a7c43",  "#c47a4a", "#b36239", "#8b6b4a"),
                        show_toolbox = TRUE,
                        na_omit = TRUE,
                        tipo = "pie", 
                        legend_position = "bottom") { # <--- Si pones FALSE, se oculta
  
  # Preparar datos (Igual que antes)
  if (is.factor(x)) {
    dt <- data.table(categoria = x)
    if (na_omit) dt <- dt[!is.na(categoria)]
    grafico_data <- dt[, .N, by = categoria]
    setorder(grafico_data, -N)
  } else if (is.character(x)) {
    dt <- data.table(categoria = as.character(x))
    if (na_omit) dt <- dt[!is.na(categoria)]
    grafico_data <- dt[, .N, by = categoria]
    setorder(grafico_data, -N)
  } else {
    stop("x debe ser un factor o character")
  }
  
  total <- grafico_data[, sum(N)]
  radius <- if (tipo == "donut") c("40%", "70%") else "70%"
  
  # Crear gráfico
  p <- grafico_data |>
    e_charts(categoria) |>
    e_pie(
      serie = N,
      name = "Frecuencia",
      radius = radius,
      center = c("50%", "55%"),
      label = list(
        show = TRUE,
        position = "outside",
        # Formateador para mostrar Nombre + Porcentaje en la etiqueta externa
        formatter = htmlwidgets::JS("
          function(params) {
            return params.name + '\\n(' + params.percent + '%)';
          }
        "),
        fontSize = 10,
        color = "#4a4a4a"
      ),
      itemStyle = list(
        borderRadius = 4,
        borderColor = "#fdfaf6",
        borderWidth = 2
      ),
      color = paleta
    ) |>
    e_tooltip(trigger = "item") |>
    # --- EL FIX DE LA LEYENDA ---
    e_legend(
      show = !isFALSE(legend_position), # Si legend_position es FALSE, show es FALSE
      orient = "horizontal",
      left = "center",
      bottom = if(is.character(legend_position)) legend_position else "bottom"
    )
  
  if (show_toolbox) {
    p <- p |> 
      e_toolbox_feature(feature = "saveAsImage", title = "Guardar") |>
      e_toolbox_feature(feature = "dataView", title = "Ver datos")
  }
  
  if (!is.null(titulo)) {
    p <- p |> e_title(text = titulo, subtext = subtitulo, left = "center", top = 5,
                      textStyle = list(color = "#2d5a27", fontSize = 15, fontWeight = "bold"))
  }
  
  return(p)
}

# ---- tabla univariada final ----
library(DT)
library(htmltools)

tabla_univariada <- function(data, 
                             caption = NULL, 
                             notas = NULL,
                             digits = 1,
                             col_names = NULL,
                             align = NULL,
                             header_above = NULL, # <--- NUEVO ARGUMENTO
                             striped = TRUE,
                             hover = TRUE,
                             condensed = TRUE) {
  
  # 1. Configurar nombres de columnas base
  if (is.null(col_names)) {
    col_names <- names(data)
  }
  
  # 2. Configurar clases de Bootstrap
  clases_css <- "table"
  if (striped) clases_css <- paste(clases_css, "table-striped")
  if (hover) clases_css <- paste(clases_css, "table-hover")
  if (condensed) clases_css <- paste(clases_css, "table-condensed")
  clases_css <- paste(clases_css, "display") 
  
  # 3. Título y Nota al pie (¡FORZANDO ARRIBA CON CSS!)
  tabla_caption <- NULL
  if (!is.null(caption) || !is.null(notas)) {
    caption_text <- if (!is.null(caption)) tags$b(caption, style = "color: #2d5a27; font-size: 16px;") else ""
    notas_text <- if (!is.null(notas)) tags$p(tags$i(paste("Nota:", notas)), style = "font-size: 12px; color: #666; margin-top: 5px; margin-bottom: 0;") else ""
    
    # El truco: style = "caption-side: top;"
    tabla_caption <- tags$caption(style = "caption-side: top; text-align: left;", caption_text, tags$br(), notas_text)
  }
  
  # 4. Construir contenedor HTML si hay 'header_above'
  contenedor_personalizado <- NULL
  if (!is.null(header_above)) {
    # Fila superior (Los agrupadores como "Techo", "Muros")
    celdas_superiores <- lapply(seq_along(header_above), function(i) {
      texto <- names(header_above)[i]
      ancho <- header_above[[i]]
      if (trimws(texto) == "") texto <- "" # Si es un espacio, dejar vacío
      
      tags$th(colspan = ancho, texto, 
              style = "text-align: center; border-bottom: 2px solid #2d5a27; background-color: #e8f0e7; color: #2d5a27;")
    })
    
    # Fila inferior (Las columnas normales como "Material", "Viviendas", "%")
    celdas_inferiores <- lapply(col_names, function(col) {
      tags$th(col, style = "background-color: #e8f0e7; color: #2d5a27;")
    })
    
    contenedor_personalizado <- tags$table(
      class = clases_css,
      tags$thead(tags$tr(celdas_superiores), tags$tr(celdas_inferiores))
    )
  }
  
  # 5. Crear la tabla (Condicional: con o sin contenedor)
  dt_args <- list(
    data = data,
    rownames = FALSE,
    caption = tabla_caption,
    class = clases_css,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = list(
        list(extend = 'copy', text = 'Copiar'),
        list(extend = 'csv', text = 'CSV'),
        list(extend = 'excel', text = 'Excel'),
        list(extend = 'pdf', text = 'PDF')
      ),
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
      pageLength = 10,
      scrollX = TRUE,
      # Si NO hay contenedor doble, pintamos el encabezado simple
      initComplete = if(is.null(header_above)) {
        JS("function(settings, json) { $(this.api().table().header()).css({'background-color': '#e8f0e7', 'color': '#2d5a27'}); }")
      } else { JS("null") }
    )
  )
  
  # Inyectar el contenedor doble o usar col_names simples
  if (!is.null(contenedor_personalizado)) {
    dt_args$container <- contenedor_personalizado
  } else {
    dt_args$colnames <- col_names
  }
  
  dt_obj <- do.call(datatable, dt_args)
  
  # 6. Redondear y alinear
  cols_numericas <- which(sapply(data, is.numeric))
  if (length(cols_numericas) > 0) {
    dt_obj <- dt_obj |> formatRound(columns = cols_numericas, digits = digits)
  }
  
  if (!is.null(align)) {
    align_chars <- strsplit(align, "")[[1]]
    for (i in seq_along(align_chars)) {
      align_word <- switch(align_chars[i], "l" = "left", "r" = "right", "c" = "center", "left")
      dt_obj <- dt_obj |> formatStyle(i, textAlign = align_word)
    }
  }
  
  dt_obj <- dt_obj |> formatStyle(1, fontWeight = 'bold', color = '#2d5a27')
  
  return(dt_obj)
}