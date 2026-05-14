# Colores -----------------------------------------------------------------
col_rojo_chillon <- "#FF0000"
col_amarillo_chillon <- "#FFC107"
col_verde_chillon <- "#00B050"
#Color de letras y bordes
col_muted   <- "#6B7280"
col_borde   <- "#D1D5DB"
col_texto   <- "#111827"
#Color de graficas
col_verde   <- "#1E5B4F"   # IMSS verde
col_guinda  <- "#611232"   # guinda
col_dorado  <- "#A57F2C"
col_verde_pastel   <- "#FFFFFF"
# Funciones de valiubox ---------------------------------------------------
fmt_delta <- function(x) {
  if (is.null(x)) return(list(label = "", col = col_muted, icon = ""))
  if (length(x) == 0) return(list(label = "", col = col_muted, icon = ""))
  if (all(is.na(x))) return(list(label = "s/d", col = col_muted, icon = ""))
  
  x <- x[1]
  
  if (x > 0) return(list(label = paste0("+", x, "%"), col = col_verde,  icon = "▲ "))
  if (x < 0) return(list(label = paste0(x, "%"),      col = col_guinda, icon = "▼ "))
  
  list(label = "0%", col = col_muted, icon = "• ")
}
fmt_num <- function(x) scales::comma(as.integer(x))

crear_card_institucional <- function(
    numero,
    titulo,
    var_vs_2025,
    var_vs_2024,
    acento = col_verde,
    size_num = 30,
    size_titulo = 13,
    size_delta = 10.5
) {
  d25 <- fmt_delta(var_vs_2025)
  d24 <- fmt_delta(var_vs_2024)
  
  mostrar_comparativos <- !is.null(var_vs_2025) && !is.null(var_vs_2024)
  
  rvg::dml(code = {
    grid.newpage()
    
    # Card base
    grid.roundrect(
      x = 0.5, y = 0.5, width = 0.98, height = 0.98,
      r = unit(10, "pt"),
      gp = gpar(fill = "white", col = col_borde, lwd = 1)
    )
    
    # Línea lateral
    grid.rect(
      x = 0.035, y = 0.5,
      width = 0.020, height = 0.90,
      gp = gpar(fill = acento, col = NA)
    )
    
    # Número
    grid.text(
      fmt_num(numero),
      x = 0.07, y = 0.73,
      just = c("left","center"),
      gp = gpar(
        col = col_dorado,
        fontsize = size_num,
        fontface = "bold",
        fontfamily = "Calibri"
      )
    )
    
    # Título
    grid.text(
      titulo,
      x = 0.07, y = 0.44,
      just = c("left","center"),
      gp = gpar(
        col = col_texto,
        fontsize = size_titulo,
        fontface = "bold",
        fontfamily = "Calibri"
      )
    )
    
    if (mostrar_comparativos) {
      # ---------- Línea vs 2025
      label_25 <- paste0(d25$icon, "vs 2025 ")
      x0 <- unit(0.07, "npc")
      y1 <- unit(0.22, "npc")
      
      grid.text(
        label_25,
        x = x0, y = y1,
        just = c("left","center"),
        gp = gpar(
          col = col_muted,
          fontsize = size_delta,
          fontfamily = "Calibri"
        )
      )
      
      x_pct_25 <- x0 + stringWidth(label_25)
      
      grid.text(
        d25$label,
        x = x_pct_25, y = y1,
        just = c("left","center"),
        gp = gpar(
          col = d25$col,
          fontsize = size_delta,
          fontface = "bold",
          fontfamily = "Calibri"
        )
      )
      
      # ---------- Línea vs 2024
      label_24 <- paste0(d24$icon, "vs 2024 ")
      y2 <- unit(0.11, "npc")
      
      grid.text(
        label_24,
        x = x0, y = y2,
        just = c("left","center"),
        gp = gpar(
          col = col_muted,
          fontsize = size_delta,
          fontfamily = "Calibri"
        )
      )
      
      x_pct_24 <- x0 + stringWidth(label_24)
      
      grid.text(
        d24$label,
        x = x_pct_24, y = y2,
        just = c("left","center"),
        gp = gpar(
          col = d24$col,
          fontsize = size_delta,
          fontface = "bold",
          fontfamily = "Calibri"
        )
      )
    }
  })
}

elige_acento <- function(var_2025, var_2024,
                         verde   = col_verde_chillon,
                         amarillo= col_amarillo_chillon,
                         rojo    = col_rojo_chillon) {
  
  v25_neg <- !is.na(var_2025) && var_2025 < 0
  v24_neg <- !is.na(var_2024) && var_2024 < 0
  
  if (v25_neg && v24_neg) return(rojo)
  if (xor(v25_neg, v24_neg)) return(amarillo)
  return(verde)
}

crear_valueboxes_2026 <- function(df_3anios,
                                  mapa_titulos,
                                  sufijo = "",
                                  incluir_comparativos = TRUE,
                                  acento_sin_comparativo = "#B0B0B0",
                                  size_num = 30,
                                  size_titulo = 13,
                                  size_delta = 10.5) {
  
  d26 <- df_3anios %>%
    dplyr::filter(anio == 2026)
  
  purrr::imap(mapa_titulos, function(titulo, var) {
    
    numero <- d26 %>% dplyr::pull(!!rlang::sym(var))
    
    if (length(numero) == 0 || all(is.na(numero))) {
      numero <- 0
    } else {
      numero <- numero[1]
    }
    
    if (incluir_comparativos) {
      var_vs_2025 <- d26 %>%
        dplyr::pull(!!rlang::sym(paste0("var_2026_vs_2025_", var)))
      
      var_vs_2024 <- d26 %>%
        dplyr::pull(!!rlang::sym(paste0("var_2026_vs_2024_", var)))
      
      if (length(var_vs_2025) == 0) var_vs_2025 <- NA_real_
      if (length(var_vs_2024) == 0) var_vs_2024 <- NA_real_
      
      var_vs_2025 <- var_vs_2025[1]
      var_vs_2024 <- var_vs_2024[1]
      
      acento <- elige_acento(var_vs_2025, var_vs_2024)
      
    } else {
      var_vs_2025 <- NULL
      var_vs_2024 <- NULL
      acento <- acento_sin_comparativo
    }
    
    crear_card_institucional(
      numero = numero,
      titulo = titulo,
      var_vs_2025 = var_vs_2025,
      var_vs_2024 = var_vs_2024,
      acento = acento,
      size_num = size_num,
      size_titulo = size_titulo,
      size_delta = size_delta
    )
  }) %>%
    rlang::set_names(paste0(names(mapa_titulos), sufijo))
}

definir_layout_valueboxes <- function(datos_consulta_funcion) {
  hay_general <- tiene_dato_2026(datos_consulta_funcion, "consulta_gral")
  hay_esp     <- tiene_dato_2026(datos_consulta_funcion, "consulta_esp")
  hay_qx      <- tiene_dato_2026(datos_consulta_funcion, "qx")
  hay_egresos <- tiene_dato_2026(datos_consulta_funcion, "egresos")
  
  # caso 1: solo consulta general
  if (hay_general && !hay_esp && !hay_qx && !hay_egresos) {
    return(list(
      layout = "2_valueboxes",
      metricas = c("consulta_gral")
    ))
  }
  
  # caso 2: general + especialidad
  if (hay_general && hay_esp && !hay_qx && !hay_egresos) {
    return(list(
      layout = "6_valueboxes",
      metricas = c("total_consultas", "consulta_gral", "consulta_esp")
    ))
  }
  
  # caso 3: general + especialidad + qx
  if (hay_general && hay_esp && hay_qx && !hay_egresos) {
    return(list(
      layout = "8_valueboxes",
      metricas = c("total_consultas", "consulta_gral", "consulta_esp", "qx")
    ))
  }
  
  # caso 4: todo
  if (hay_general && hay_esp && hay_qx && hay_egresos) {
    return(list(
      layout = "10_valueboxes",
      metricas = c("total_consultas", "consulta_gral", "consulta_esp", "qx", "egresos")
    ))
  }
  
  # fallback por si aparece una combinación rara
  metricas_presentes <- c()
  
  if (hay_general) metricas_presentes <- c(metricas_presentes, "consulta_gral")
  if (hay_esp)     metricas_presentes <- c(metricas_presentes, "consulta_esp")
  if (hay_qx)      metricas_presentes <- c(metricas_presentes, "qx")
  if (hay_egresos) metricas_presentes <- c(metricas_presentes, "egresos")
  
  # si hay más de una métrica, agregamos total al inicio
  if (length(metricas_presentes) >= 2) {
    metricas_presentes <- c("total_consultas", metricas_presentes)
  }
  
  n <- length(metricas_presentes)
  
  layout_fallback <- dplyr::case_when(
    n <= 1 ~ "2_valueboxes",
    n == 3 ~ "6_valueboxes",
    n == 4 ~ "8_valueboxes",
    n >= 5 ~ "10_valueboxes",
    TRUE ~ "10_valueboxes"
  )
  
  list(
    layout = layout_fallback,
    metricas = metricas_presentes
  )
}

tiene_dato_2026 <- function(df, columna) {
  if (!columna %in% names(df)) return(FALSE)
  
  valor <- df %>%
    dplyr::filter(anio == 2026) %>%
    dplyr::pull(.data[[columna]])
  
  if (length(valor) == 0) return(FALSE)
  
  any(!is.na(valor) & valor != 0)
}


imprimir_valueboxes_dinamicos <- function(pptx,
                                          layout_name,
                                          boxes_superior,
                                          boxes_inferior,
                                          titulo = "Productividad IMSS Bienestar",
                                          fecha = NULL,
                                          master = "Tema de Office") {
  
  pptx <- pptx %>%
    officer::add_slide(layout = layout_name, master = master) %>%
    officer::ph_with(titulo, location = officer::ph_location_label("Título 1"))
  
  if (!is.null(fecha)) {
    pptx <- pptx %>%
      officer::ph_with(value = fecha,
                       location = officer::ph_location_label("fecha"))
  }
  
  # fila superior
  for (i in seq_along(boxes_superior)) {
    pptx <- pptx %>%
      officer::ph_with(
        value = boxes_superior[[i]],
        location = officer::ph_location_label(paste0("arriba ", i))
      )
  }
  
  # fila inferior
  for (i in seq_along(boxes_inferior)) {
    pptx <- pptx %>%
      officer::ph_with(
        value = boxes_inferior[[i]],
        location = officer::ph_location_label(paste0("abajo ", i))
      )
  }
  
  pptx
}


# Barras ------------------------------------------------------------------
hay_indicador_2026 <- function(df, columna) {
  val <- valor_anio_col(df, columna, 2026)
  !is.na(val) && val != 0
}

valor_anio_col <- function(df, columna, anio_objetivo) {
  if (!columna %in% names(df)) return(NA_real_)
  
  val <- df %>%
    dplyr::filter(anio == anio_objetivo) %>%
    dplyr::pull(.data[[columna]])
  
  if (length(val) == 0) return(NA_real_)
  val[1]
}

definir_layout_historico <- function(datos_consulta_funcion) {
  hay_consultas <- any(c(
    hay_indicador_2026(datos_consulta_funcion, "total_consultas"),
    hay_indicador_2026(datos_consulta_funcion, "consulta_gral"),
    hay_indicador_2026(datos_consulta_funcion, "consulta_esp")
  ))
  
  hay_proc <- any(c(
    hay_indicador_2026(datos_consulta_funcion, "qx"),
    hay_indicador_2026(datos_consulta_funcion, "egresos")
  ))
  
  if (hay_consultas && hay_proc) {
    return("Historico consultas y procedimientos")
  }
  
  if (hay_consultas) {
    return("Historico consultas")
  }
  
  return(NA_character_)
}

grafica_planeacion <- function(df, col_total, col_avance, titulo,
                               beige = "#D9D2BE",
                               verde = "#2F6F63",
                               beige_2026 = "#A99F86",
                               verde_2026 = "#1E5B4F") {
  
  df %>%
    dplyr::mutate(
      anio_num = as.integer(anio),
      anio = factor(anio, levels = 2020:2026),
      pct_avance = dplyr::if_else(
        .data[[col_total]] > 0,
        .data[[col_avance]] / .data[[col_total]],
        NA_real_
      ),
      etiqueta_avance = dplyr::if_else(
        anio_num == 2026,
        paste0("Avance\n",
               scales::comma(.data[[col_avance]]),
               "\n(",
               scales::percent(pct_avance, accuracy = 1),
               ")"
        ),
        scales::comma(.data[[col_avance]])
      ),
      etiqueta_total = dplyr::if_else(
        anio_num == 2026,
        paste0("Meta 2026\n", scales::comma(.data[[col_total]])),
        scales::comma(.data[[col_total]])
      ),
      color_total = dplyr::if_else(anio_num == 2026, beige_2026, beige),
      color_avance = dplyr::if_else(anio_num == 2026, verde_2026, verde)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = anio)) +
    
    ggplot2::geom_col(
      ggplot2::aes(y = .data[[col_total]], fill = color_total),
      width = 0.82
    ) +
    
    ggplot2::geom_col(
      ggplot2::aes(y = .data[[col_avance]], fill = color_avance),
      width = 0.82
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(
        y = .data[[col_total]],
        label = etiqueta_total
      ),
      vjust = -0.35,
      fontface = "bold",
      size = 3.8,
      lineheight = 0.9
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(
        y = .data[[col_avance]],
        label = etiqueta_avance
      ),
      vjust = 1.25,
      color = "white",
      fontface = "bold",
      size = 3.1,
      lineheight = 0.9
    ) +
    
    ggplot2::scale_fill_identity() +
    
    ggplot2::scale_x_discrete(drop = FALSE) +
    
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, .16))
    ) +
    
    
    
    ggplot2::labs(title = titulo, x = NULL, y = NULL) +
    
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(face = "bold", color = "#6B7280"),
      axis.text.y = ggplot2::element_text(color = "#6B7280"),
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        color = "#6B7280",
        size = 18
      ),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

armar_tabla_dinamica <- function(df, indicadores, etiquetas, mes_nombre) {
  tibble::tibble(
    indicador = etiquetas,
    valor_2025 = purrr::map_dbl(indicadores, ~ valor_anio_col(df, .x, 2025)),
    valor_2026 = purrr::map_dbl(indicadores, ~ valor_anio_col(df, .x, 2026))
  ) %>%
    dplyr::mutate(
      crecimiento = dplyr::case_when(
        is.na(valor_2025) | valor_2025 == 0 ~ NA_real_,
        TRUE ~ round((valor_2026 - valor_2025) / valor_2025 * 100, 0)
      ),
      `Crecimiento anual` = dplyr::case_when(
        is.na(crecimiento) ~ "s/d",
        crecimiento > 0 ~ paste0("+", crecimiento, " %"),
        TRUE ~ paste0(crecimiento, " %")
      )
    ) %>%
    dplyr::mutate(
      valor_2025 = format(valor_2025, big.mark = ",", scientific = FALSE, trim = TRUE),
      valor_2026 = format(valor_2026, big.mark = ",", scientific = FALSE, trim = TRUE),
      valor_2025 = dplyr::if_else(valor_2025 == "NA", "s/d", valor_2025),
      valor_2026 = dplyr::if_else(valor_2026 == "NA", "s/d", valor_2026)
    ) %>%
    dplyr::select(-crecimiento) %>%
    dplyr::rename(
      !!paste0(mes_nombre, " 2025") := valor_2025,
      !!paste0(mes_nombre, " 2026") := valor_2026
    )
}

ft_planeacion <- function(df,
                          w1 = 4.05, w2 = 1.20, w3 = 1.20, w4 = 1.35,
                          header_negro = "#3B3B3B",
                          verde = "#2F6F63",
                          menta = "#D9F2EE",
                          size_header = NULL,
                          size_body = NULL,
                          h_fila = NULL) {
  
  n_filas <- nrow(df)
  
  if (is.null(size_header) || is.null(size_body) || is.null(h_fila)) {
    if (n_filas == 1) {
      size_header <- 13
      size_body   <- 12
      h_fila      <- 0.45
    } else if (n_filas == 2) {
      size_header <- 11
      size_body   <- 10
      h_fila      <- 0.35
    } else {
      size_header <- 10
      size_body   <- 9
      h_fila      <- 0.28
    }
  }
  
  ft <- flextable::flextable(df) %>%
    flextable::set_header_labels(indicador = "Indicador") %>%
    flextable::bg(part = "header", bg = header_negro) %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::bold(part = "header") %>%
    flextable::fontsize(part = "header", size = size_header) %>%
    flextable::fontsize(part = "body", size = size_body) %>%
    flextable::bg(j = 3, part = "body", bg = menta) %>%
    flextable::align(align = "center", j = 2:4, part = "all") %>%
    flextable::align(align = "left", j = 1, part = "all") %>%
    flextable::border_outer(officer::fp_border(color = "#6B7280", width = 1)) %>%
    flextable::border_inner_h(officer::fp_border(color = "#6B7280", width = 1)) %>%
    flextable::border_inner_v(officer::fp_border(color = "#6B7280", width = 1)) %>%
    flextable::width(j = 1, width = w1) %>%
    flextable::width(j = 2, width = w2) %>%
    flextable::width(j = 3, width = w3) %>%
    flextable::width(j = 4, width = w4) %>%
    flextable::height_all(height = h_fila)
  
  if (n_filas == 1) {
    ft <- flextable::align(ft, align = "center", part = "all") %>%
      flextable::align(j = 1, align = "left", part = "all")
  }
  
  ft
}
# Grafica temporal --------------------------------------------------------
fecha_fin_graf <- lubridate::floor_date(fecha_corte, "month")
grafica_consultas_periodos <- function(df,
                                       fecha_inicio = "2022-08-01",
                                       fecha_fin    = fecha_fin_graf,
                                       titulo = "Consultas totales del IMSS Bienestar",
                                       color_linea = "#6B6B6B",
                                       verde_punto = "#1F5B50",
                                       fill_2223 = "#EFEFEF",
                                       fill_2024 = "#E9DDCC",
                                       fill_2025 = "#F4F0EA",
                                       fill_2026 = "#E9DDCC",
                                       fill_valuebox = "#B99C6D") {
  
  df <- df %>%
    dplyr::mutate(fecha = as.Date(fecha)) %>%
    dplyr::filter(
      fecha >= as.Date(fecha_inicio),
      fecha <= as.Date(fecha_fin)
    ) %>%
    dplyr::arrange(fecha)
  
  ymax <- max(df$consultas_totales, na.rm = TRUE)
  ymin <- min(df$consultas_totales, na.rm = TRUE)
  
  # Bandas por periodo
  bandas <- tibble::tibble(
    xmin = c(
      as.Date("2022-08-01"),
      as.Date("2024-01-01"),
      as.Date("2025-01-01"),
      as.Date("2026-01-01")
    ),
    xmax = c(
      as.Date("2024-01-01"),
      as.Date("2025-01-01"),
      as.Date("2026-01-01"),
      as.Date(fecha_fin)
    ),
    fill = c(fill_2223, fill_2024, fill_2025, fill_2026),
    label = c(
      "2022–2023\nAños de transición",
      "2024\nPrimer año de operación",
      "2025\nSegundo año de operación",
      "2026\nTercer año de operación"
    )
  )
  
  mes_destacado <- lubridate::month(fecha_fin)
  
  puntos_destacados <- df %>%
    dplyr::filter(
      lubridate::month(fecha) == mes_destacado,
      lubridate::year(fecha) < 2026
    ) %>%
    dplyr::distinct(fecha, .keep_all = TRUE)
  
  punto_2026 <- df %>%
    dplyr::filter(
      lubridate::year(fecha) == 2026,
      lubridate::month(fecha) == mes_destacado
    ) %>%
    dplyr::slice(1)
  
  fecha_ultimo_valor <- max(df$fecha, na.rm = TRUE)
  
  valor_ultimo <- df %>%
    dplyr::filter(fecha == fecha_ultimo_valor) %>%
    dplyr::reframe(v = dplyr::first(consultas_totales)) %>%
    dplyr::pull(v)
  
  if (length(valor_ultimo) == 0 || is.na(valor_ultimo)) {
    valor_ultimo <- 0
  }
  
  ggplot(df, aes(x = fecha, y = consultas_totales)) +
    
    # Bandas
    geom_rect(
      data = bandas,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = bandas$fill
    ) +
    
    # Serie
    geom_line(color = color_linea, linewidth = 1.1) +
    geom_point(color = color_linea, size = 1.8) +
    
    # Puntos destacados
    geom_point(
      data = puntos_destacados,
      aes(x = fecha, y = consultas_totales),
      inherit.aes = FALSE,
      color = verde_punto,
      size = 4
    ) +
    
    # Etiquetas de puntos destacados
    geom_text(
      data = puntos_destacados,
      aes(
        label = paste0(
          scales::comma(consultas_totales), "\n",
          stringr::str_to_title(format(fecha, "%b-%Y"))
        )
      ),
      fontface = "bold",
      size = 3.2,
      vjust = -0.9,
      lineheight = 0.95
    ) +
    
    # Texto superior de bandas
    geom_text(
      data = bandas,
      aes(
        x = xmin + (xmax - xmin) / 2,
        y = ymax * 1.3,
        label = label
      ),
      inherit.aes = FALSE,
      fontface = "bold",
      size = 3.1,
      lineheight = 0.95
    ) +
    
    # Flecha decreto
    annotate(
      "segment",
      x = as.Date("2022-08-15"), xend = as.Date("2022-08-15"),
      y = ymin * 0.95, yend = ymax * 1.02,
      linewidth = 1.0,
      colour = verde_punto,
      arrow = arrow(length = unit(0.22, "cm"))
    ) +
    
    annotate(
      "text",
      x = as.Date("2022-09-20"),
      y = ymax * 1.05,
      label = "Decreto de creación\ndel IMSS Bienestar",
      hjust = 0,
      fontface = "bold",
      size = 3.0,
      lineheight = 0.95
    ) +
    
    # Valuebox último dato
    geom_label(
      data = punto_2026,
      aes(
        x = fecha,
        y = consultas_totales,
        label = paste0(
          scales::comma(consultas_totales),
          "\nAbril 2026"
        )
      ),
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = -0.7,
      fill = fill_valuebox,
      color = "white",
      fontface = "bold",
      label.size = 0,
      size = 3.8,
      label.padding = unit(0.35, "lines")
    ) +
    
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0.23))
    ) +
    
    # Menos etiquetas en eje X
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b-%y"
    ) +
    
    labs(title = titulo, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}




