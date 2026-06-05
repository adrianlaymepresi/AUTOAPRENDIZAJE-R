datos_ejemplo_diagrama_lineal = data.frame(
  categoria = c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011"),
  valor = c("12", "13", "8", "7", "8", "9", "5", "5", "6"),
  stringsAsFactors = FALSE
)

leer_datos_lineal = function(input, datos_base) {
  filas = nrow(datos_base)
  categorias = character(filas)
  valores = character(filas)

  for (i in seq_len(filas)) {
    categoria_input = input[[paste0("lineal_categoria_", i)]]
    valor_input = input[[paste0("lineal_valor_", i)]]

    if (is.null(categoria_input)) {
      categorias[i] = datos_base$categoria[i]
    } else {
      categorias[i] = categoria_input
    }

    if (is.null(valor_input)) {
      valores[i] = datos_base$valor[i]
    } else {
      valores[i] = valor_input
    }
  }

  data.frame(
    categoria = categorias,
    valor = valores,
    stringsAsFactors = FALSE
  )
}

obtener_valor_seguro = function(fila, nombre, posicion) {
  if (is.null(fila)) {
    return("")
  }

  if (is.list(fila)) {
    if (!is.null(fila[[nombre]])) {
      return(as.character(fila[[nombre]]))
    }

    if (length(fila) >= posicion) {
      return(as.character(fila[[posicion]]))
    }

    return("")
  }

  if (is.atomic(fila)) {
    nombres = names(fila)

    if (!is.null(nombres) && nombre %in% nombres) {
      return(as.character(fila[[nombre]]))
    }

    if (length(fila) >= posicion) {
      return(as.character(fila[[posicion]]))
    }

    return("")
  }

  ""
}

parsear_lineas_lineal = function(texto) {
  texto = gsub("\r\n", "\n", texto)
  texto = gsub("\r", "\n", texto)

  lineas = unlist(strsplit(texto, "\n", fixed = TRUE))

  categorias = character(0)
  valores = character(0)

  for (linea in lineas) {
    linea = trimws(linea)

    if (linea == "") {
      next
    }

    if (grepl("\t", linea, fixed = TRUE)) {
      partes = trimws(unlist(strsplit(linea, "\t", fixed = TRUE)))
    } else if (grepl(";", linea, fixed = TRUE)) {
      partes = trimws(unlist(strsplit(linea, ";", fixed = TRUE)))
    } else {
      partes = trimws(unlist(strsplit(linea, "[ ]+")))
    }

    partes = partes[partes != ""]

    if (length(partes) >= 2) {
      categorias = c(categorias, paste(partes[1:(length(partes) - 1)], collapse = " "))
      valores = c(valores, partes[length(partes)])
    }
  }

  data.frame(
    categoria = categorias,
    valor = valores,
    stringsAsFactors = FALSE
  )
}

convertir_filas_pegadas_lineal = function(filas) {
  if (is.null(filas) || length(filas) == 0) {
    return(data.frame(categoria = character(0), valor = character(0), stringsAsFactors = FALSE))
  }

  if (is.data.frame(filas)) {
    if (all(c("categoria", "valor") %in% names(filas))) {
      return(data.frame(
        categoria = as.character(filas$categoria),
        valor = as.character(filas$valor),
        stringsAsFactors = FALSE
      ))
    }

    if (ncol(filas) >= 2) {
      return(data.frame(
        categoria = as.character(filas[[1]]),
        valor = as.character(filas[[2]]),
        stringsAsFactors = FALSE
      ))
    }
  }

  if (is.character(filas) && length(filas) == 1 && grepl("\n|\t|;", filas)) {
    return(parsear_lineas_lineal(filas))
  }

  if (is.atomic(filas) && !is.list(filas)) {
    if (length(filas) >= 2) {
      matriz = matrix(filas, ncol = 2, byrow = TRUE)

      return(data.frame(
        categoria = as.character(matriz[, 1]),
        valor = as.character(matriz[, 2]),
        stringsAsFactors = FALSE
      ))
    }

    return(data.frame(categoria = character(0), valor = character(0), stringsAsFactors = FALSE))
  }

  categorias = character(0)
  valores = character(0)

  for (fila in filas) {
    categoria = obtener_valor_seguro(fila, "categoria", 1)
    valor = obtener_valor_seguro(fila, "valor", 2)

    categoria = trimws(as.character(categoria))
    valor = trimws(as.character(valor))

    if (categoria != "" && valor != "") {
      categorias = c(categorias, categoria)
      valores = c(valores, valor)
    }
  }

  data.frame(
    categoria = categorias,
    valor = valores,
    stringsAsFactors = FALSE
  )
}

validar_datos_lineal = function(datos) {
  if (nrow(datos) == 0) {
    return("Debe existir al menos una fila de datos.")
  }

  datos$categoria = trimws(datos$categoria)
  datos$valor = trimws(as.character(datos$valor))

  if (any(datos$categoria == "")) {
    return("Todos los periodos o categorías deben tener nombre.")
  }

  if (any(datos$valor == "")) {
    return("Todos los valores deben estar llenos.")
  }

  valores = normalizar_numero(datos$valor)

  if (any(is.na(valores))) {
    return("La columna de valores debe contener solo números.")
  }

  if (nrow(datos) < 2) {
    return("El diagrama lineal necesita al menos dos puntos.")
  }

  TRUE
}

preparar_datos_lineal = function(datos) {
  datos$categoria = trimws(datos$categoria)
  datos$valor = normalizar_numero(datos$valor)
  datos
}

modulo_diagrama_lineal_ui = function(id) {
  ns = NS(id)

  div(
    class = "tarjeta",
    h1(class = "titulo", "Diagrama lineal"),
    div(
      class = "nota",
      "Ejemplo precargado del docente: producción de conocimientos por cada año. Puede pegar dos columnas desde Excel: periodo y valor."
    ),
    textInput(
      ns("numero_grafico"),
      "Número de gráfico",
      value = "Gráfico N° 8"
    ),
    textInput(
      ns("titulo_grafico"),
      "Título",
      value = "Producción de conocimientos por cada año",
      width = "100%"
    ),
    textInput(
      ns("nombre_columna_x"),
      "Nombre columna X",
      value = "Periodo",
      width = "100%"
    ),
    textInput(
      ns("nombre_columna_y"),
      "Nombre columna Y",
      value = "Número de conocimientos",
      width = "100%"
    ),
    selectInput(
      ns("color"),
      "Color",
      choices = c(
        "Rojo" = "#c94f4f",
        "Verde" = "#174b33",
        "Azul" = "#3c6fa6",
        "Café" = "#c49a6c"
      ),
      selected = "#c94f4f"
    ),
    checkboxInput(
      ns("mostrar_valores"),
      "Mostrar valores sobre los puntos",
      value = TRUE
    ),
    numericInput(
      ns("salto_eje_y"),
      "Salto del eje Y",
      value = 2,
      min = 0.1,
      step = 0.5
    ),
    h4("Datos"),
    div(
      class = "tabla-edicion",
      div(
        class = "encabezado-edicion",
        div(textOutput(ns("encabezado_categoria"))),
        div(textOutput(ns("encabezado_valor")))
      ),
      uiOutput(ns("filas_datos"))
    ),
    actionButton(
      ns("pegar_excel"),
      "Pegar desde Excel",
      onclick = paste0("pegarDatosGrafico('", ns("datos_pegados"), "')"),
      class = "btn-warning"
    ),
    actionButton(
      ns("agregar_fila"),
      "Agregar fila",
      class = "btn-info"
    ),
    actionButton(
      ns("quitar_fila"),
      "Quitar fila",
      class = "btn-info"
    ),
    actionButton(
      ns("cargar_ejemplo"),
      "Cargar ejemplo del PDF",
      class = "btn-info"
    ),
    actionButton(
      ns("generar"),
      "Generar diagrama",
      class = "btn-primary"
    ),
    tags$hr(),
    uiOutput(ns("salida"))
  )
}

modulo_diagrama_lineal_server = function(id) {
  moduleServer(id, function(input, output, session) {
    datos_reactivos = reactiveVal(datos_ejemplo_diagrama_lineal)

    output$encabezado_categoria = renderText({
      input$nombre_columna_x
    })

    output$encabezado_valor = renderText({
      input$nombre_columna_y
    })

    output$filas_datos = renderUI({
      datos = datos_reactivos()
      ns = session$ns

      tagList(
        lapply(seq_len(nrow(datos)), function(i) {
          div(
            class = "fila-edicion",
            textInput(
              ns(paste0("lineal_categoria_", i)),
              label = NULL,
              value = datos$categoria[i],
              width = "100%"
            ),
            textInput(
              ns(paste0("lineal_valor_", i)),
              label = NULL,
              value = datos$valor[i],
              width = "100%"
            )
          )
        })
      )
    })

    observeEvent(input$datos_pegados, {
      datos_pegados = convertir_filas_pegadas_lineal(input$datos_pegados)

      if (nrow(datos_pegados) == 0) {
        return()
      }

      datos_reactivos(datos_pegados)
    })

    observeEvent(input$agregar_fila, {
      datos_actuales = leer_datos_lineal(input, datos_reactivos())

      datos_nuevos = rbind(
        datos_actuales,
        data.frame(categoria = "", valor = "", stringsAsFactors = FALSE)
      )

      datos_reactivos(datos_nuevos)
    })

    observeEvent(input$quitar_fila, {
      datos_actuales = leer_datos_lineal(input, datos_reactivos())

      if (nrow(datos_actuales) > 2) {
        datos_reactivos(datos_actuales[-nrow(datos_actuales), ])
      }
    })

    observeEvent(input$cargar_ejemplo, {
      updateTextInput(session, "numero_grafico", value = "Gráfico N° 8")
      updateTextInput(
        session,
        "titulo_grafico",
        value = "Producción de conocimientos por cada año"
      )
      updateTextInput(session, "nombre_columna_x", value = "Periodo")
      updateTextInput(session, "nombre_columna_y", value = "Número de conocimientos")
      updateSelectInput(session, "color", selected = "#c94f4f")
      updateCheckboxInput(session, "mostrar_valores", value = TRUE)
      updateNumericInput(session, "salto_eje_y", value = 2)
      datos_reactivos(datos_ejemplo_diagrama_lineal)
    })

    resultado_lineal = eventReactive(input$generar, {
      datos_actuales = leer_datos_lineal(input, datos_reactivos())
      validacion = validar_datos_lineal(datos_actuales)

      if (validacion != TRUE) {
        return(list(error = validacion))
      }

      datos = preparar_datos_lineal(datos_actuales)

      list(
        datos = datos,
        cantidad = nrow(datos),
        minimo = min(datos$valor),
        maximo = max(datos$valor),
        promedio = mean(datos$valor),
        primero = datos$valor[1],
        ultimo = datos$valor[nrow(datos)]
      )
    }, ignoreNULL = FALSE)

    output$salida = renderUI({
      resultado = resultado_lineal()

      if (!is.null(resultado$error)) {
        return(div(class = "error", resultado$error))
      }

      tagList(
        h2(class = "subtitulo", input$numero_grafico),
        h4(input$titulo_grafico),
        h4("Tabla de datos"),
        tableOutput(session$ns("tabla_lineal")),
        div(
          class = "grafico-contenedor",
          plotOutput(session$ns("grafico_lineal"), height = "560px")
        ),
        div(
          class = "resultado-final",
          h4("Resumen"),
          tableOutput(session$ns("resumen_lineal"))
        )
      )
    })

    output$tabla_lineal = renderTable({
      resultado = resultado_lineal()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      datos = resultado$datos

      salida = data.frame(
        Categoria = datos$categoria,
        Valor = formatear_numero(datos$valor, 3),
        stringsAsFactors = FALSE
      )

      names(salida) = c(input$nombre_columna_x, input$nombre_columna_y)

      salida
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$resumen_lineal = renderTable({
      resultado = resultado_lineal()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      tendencia = ifelse(
        resultado$ultimo > resultado$primero,
        "Tendencia creciente",
        ifelse(
          resultado$ultimo < resultado$primero,
          "Tendencia decreciente",
          "Tendencia estable"
        )
      )

      data.frame(
        medida = c(
          "Cantidad de puntos",
          "Valor menor",
          "Valor mayor",
          "Promedio",
          "Primer valor",
          "Último valor",
          "Interpretación"
        ),
        valor = c(
          resultado$cantidad,
          formatear_numero(resultado$minimo, 3),
          formatear_numero(resultado$maximo, 3),
          formatear_numero(resultado$promedio, 3),
          formatear_numero(resultado$primero, 3),
          formatear_numero(resultado$ultimo, 3),
          tendencia
        )
      )
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$grafico_lineal = renderPlot({
      resultado = resultado_lineal()

      if (!is.null(resultado$error)) {
        plot.new()
        text(0.5, 0.5, resultado$error)
        return()
      }

      datos = resultado$datos
      valores = datos$valor
      categorias = datos$categoria
      posiciones = seq_along(valores)
      color = input$color

      salto_y = input$salto_eje_y

      if (is.null(salto_y) || is.na(salto_y) || salto_y <= 0) {
        salto_y = 1
      }

      valor_minimo = min(valores)
      valor_maximo = max(valores)

      inicio_y = floor(valor_minimo / salto_y) * salto_y
      fin_y = ceiling(valor_maximo / salto_y) * salto_y

      if (inicio_y > 0) {
        inicio_y = 0
      }

      if (fin_y == valor_maximo) {
        fin_y = fin_y + salto_y
      }

      eje_y = seq(inicio_y, fin_y, by = salto_y)

      par(mar = c(6, 6, 5, 3), mgp = c(3.5, 1, 0))

      plot(
        posiciones,
        valores,
        type = "n",
        main = input$numero_grafico,
        xlab = "",
        ylab = "",
        xlim = c(1, length(posiciones)),
        ylim = c(inicio_y, fin_y),
        axes = FALSE
      )

      axis(1, at = posiciones, labels = categorias, las = 1)
      axis(2, at = eje_y, labels = formatear_numero(eje_y, 3), las = 1)

      abline(h = eje_y, col = "#d9d9d9", lty = "solid")
      box()

      lines(
        posiciones,
        valores,
        type = "o",
        col = color,
        lwd = 3,
        pch = 19,
        cex = 1.4
      )

      if (isTRUE(input$mostrar_valores)) {
        text(
          posiciones,
          valores,
          labels = formatear_numero(valores, 3),
          pos = 3,
          cex = 0.85
        )
      }

      mtext(input$nombre_columna_x, side = 1, line = 4.2, font = 2)
      mtext(input$nombre_columna_y, side = 2, line = 4.2, font = 2)
    })
  })
}