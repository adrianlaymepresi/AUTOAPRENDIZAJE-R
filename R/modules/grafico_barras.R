datos_ejemplo_grafico_barras = data.frame(
  categoria = c("Católica", "Evangélica", "Otra religión", "Ninguna"),
  valor = c("16956722", "2606055", "679291", "608434"),
  stringsAsFactors = FALSE
)

leer_datos_barras = function(input, datos_base) {
  filas = nrow(datos_base)
  categorias = character(filas)
  valores = character(filas)

  for (i in seq_len(filas)) {
    categoria_input = input[[paste0("categoria_", i)]]
    valor_input = input[[paste0("valor_", i)]]

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

obtener_valor_seguro_barras = function(fila, nombre, posicion) {
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

parsear_lineas_barras = function(texto) {
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

convertir_filas_pegadas_barras = function(filas) {
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
    return(parsear_lineas_barras(filas))
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
    categoria = obtener_valor_seguro_barras(fila, "categoria", 1)
    valor = obtener_valor_seguro_barras(fila, "valor", 2)

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

validar_datos_barras = function(datos) {
  if (nrow(datos) == 0) {
    return("Debe existir al menos una fila de datos.")
  }

  datos$categoria = trimws(datos$categoria)
  datos$valor = trimws(as.character(datos$valor))

  if (any(datos$categoria == "")) {
    return("Todas las categorías deben tener nombre.")
  }

  if (any(datos$valor == "")) {
    return("Todos los valores deben estar llenos.")
  }

  valores = normalizar_numero(datos$valor)

  if (any(is.na(valores))) {
    return("Todos los valores de la segunda columna deben ser numéricos.")
  }

  if (any(valores < 0)) {
    return("Los valores no pueden ser negativos.")
  }

  if (all(valores == 0)) {
    return("Al menos un valor debe ser mayor que cero.")
  }

  TRUE
}

preparar_datos_barras = function(datos) {
  datos$categoria = trimws(datos$categoria)
  datos$valor = normalizar_numero(datos$valor)
  datos
}

modulo_grafico_barras_ui = function(id) {
  ns = NS(id)

  div(
    class = "tarjeta",
    h1(class = "titulo", "Gráfico de barras"),
    div(
      class = "nota",
      "Ejemplo precargado del docente: población censada de 12 y más años de edad según tipo de religión que profesan. Puede pegar dos columnas desde Excel: categoría y valor."
    ),
    textInput(
      ns("numero_grafico"),
      "Número de gráfico",
      value = "Gráfico N° 3"
    ),
    textInput(
      ns("titulo_grafico"),
      "Título",
      value = "Perú: Población censada de 12 y más años de edad según tipo de religión que profesan",
      width = "100%"
    ),
    textInput(
      ns("nombre_columna_x"),
      "Nombre de la columna de categorías",
      value = "Tipo religión",
      width = "100%"
    ),
    textInput(
      ns("nombre_columna_y"),
      "Nombre de la columna de valores",
      value = "Número personas",
      width = "100%"
    ),
    radioButtons(
      ns("orientacion"),
      "Orientación del gráfico",
      choices = c("Horizontal", "Vertical"),
      selected = "Horizontal",
      inline = TRUE
    ),
    selectInput(
      ns("color"),
      "Color",
      choices = c(
        "Verde" = "#9bbb59",
        "Azul" = "#3c6fa6",
        "Café" = "#c49a6c",
        "Verde oscuro" = "#174b33"
      ),
      selected = "#9bbb59"
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
      "Generar gráfico",
      class = "btn-primary"
    ),
    tags$hr(),
    uiOutput(ns("salida"))
  )
}

modulo_grafico_barras_server = function(id) {
  moduleServer(id, function(input, output, session) {
    datos_reactivos = reactiveVal(datos_ejemplo_grafico_barras)

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
              ns(paste0("categoria_", i)),
              label = NULL,
              value = datos$categoria[i],
              width = "100%"
            ),
            textInput(
              ns(paste0("valor_", i)),
              label = NULL,
              value = datos$valor[i],
              width = "100%"
            )
          )
        })
      )
    })

    observeEvent(input$datos_pegados, {
      datos_pegados = convertir_filas_pegadas_barras(input$datos_pegados)

      if (nrow(datos_pegados) == 0) {
        return()
      }

      datos_reactivos(datos_pegados)
    })

    observeEvent(input$agregar_fila, {
      datos_actuales = leer_datos_barras(input, datos_reactivos())

      datos_nuevos = rbind(
        datos_actuales,
        data.frame(categoria = "", valor = "", stringsAsFactors = FALSE)
      )

      datos_reactivos(datos_nuevos)
    })

    observeEvent(input$quitar_fila, {
      datos_actuales = leer_datos_barras(input, datos_reactivos())

      if (nrow(datos_actuales) > 1) {
        datos_reactivos(datos_actuales[-nrow(datos_actuales), ])
      }
    })

    observeEvent(input$cargar_ejemplo, {
      updateTextInput(session, "numero_grafico", value = "Gráfico N° 3")
      updateTextInput(
        session,
        "titulo_grafico",
        value = "Perú: Población censada de 12 y más años de edad según tipo de religión que profesan"
      )
      updateTextInput(session, "nombre_columna_x", value = "Tipo religión")
      updateTextInput(session, "nombre_columna_y", value = "Número personas")
      updateRadioButtons(session, "orientacion", selected = "Horizontal")
      updateSelectInput(session, "color", selected = "#9bbb59")
      datos_reactivos(datos_ejemplo_grafico_barras)
    })

    resultado_barras = eventReactive(input$generar, {
      datos_actuales = leer_datos_barras(input, datos_reactivos())
      validacion = validar_datos_barras(datos_actuales)

      if (validacion != TRUE) {
        return(list(error = validacion))
      }

      datos = preparar_datos_barras(datos_actuales)

      list(
        datos = datos,
        total = sum(datos$valor),
        cantidad = nrow(datos)
      )
    }, ignoreNULL = FALSE)

    output$salida = renderUI({
      resultado = resultado_barras()

      if (!is.null(resultado$error)) {
        return(div(class = "error", resultado$error))
      }

      tagList(
        h2(class = "subtitulo", input$numero_grafico),
        h4(input$titulo_grafico),
        h4("Tabla de datos"),
        tableOutput(session$ns("tabla_barras")),
        div(
          class = "grafico-contenedor",
          plotOutput(session$ns("grafico_barras"), height = "560px")
        ),
        div(
          class = "resultado-final",
          h4("Resumen"),
          tableOutput(session$ns("resumen_barras"))
        )
      )
    })

    output$tabla_barras = renderTable({
      resultado = resultado_barras()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      datos = resultado$datos

      salida = data.frame(
        Categoria = datos$categoria,
        Valor = formatear_miles(datos$valor),
        stringsAsFactors = FALSE
      )

      names(salida) = c(input$nombre_columna_x, input$nombre_columna_y)

      total = data.frame(
        "TOTAL",
        formatear_miles(sum(datos$valor)),
        stringsAsFactors = FALSE
      )

      names(total) = names(salida)

      rbind(salida, total)
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$resumen_barras = renderTable({
      resultado = resultado_barras()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      data.frame(
        medida = c("Cantidad de barras", "Total"),
        valor = c(resultado$cantidad, formatear_miles(resultado$total))
      )
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$grafico_barras = renderPlot({
      resultado = resultado_barras()

      if (!is.null(resultado$error)) {
        plot.new()
        text(0.5, 0.5, resultado$error)
        return()
      }

      datos = resultado$datos
      valores = datos$valor
      categorias = datos$categoria
      color = input$color

      if (input$orientacion == "Horizontal") {
        par(mar = c(7, 13, 5, 7), mgp = c(3, 1, 0))

        posiciones = barplot(
          valores,
          names.arg = categorias,
          horiz = TRUE,
          las = 1,
          col = color,
          border = "#6b7f3f",
          main = input$numero_grafico,
          xlab = "",
          ylab = "",
          cex.names = 0.95,
          cex.axis = 0.9,
          xlim = c(0, max(valores) * 1.30),
          axes = FALSE
        )

        eje = pretty(c(0, max(valores)))
        axis(1, at = eje, labels = formatear_miles(eje), las = 1)

        abline(v = eje, col = "#d9d9d9", lty = "dotted")

        text(
          x = valores,
          y = posiciones,
          labels = formatear_miles(valores),
          pos = 4,
          cex = 0.85
        )

        mtext(input$nombre_columna_y, side = 1, line = 4.5, font = 2)
        mtext(input$nombre_columna_x, side = 2, line = 10.5, font = 2)

        legend(
          "right",
          legend = input$nombre_columna_y,
          fill = color,
          border = "#6b7f3f",
          bty = "n"
        )
      } else {
        par(mar = c(8, 7, 5, 3), mgp = c(3, 1, 0))

        posiciones = barplot(
          valores,
          names.arg = categorias,
          col = color,
          border = "#6b7f3f",
          main = input$numero_grafico,
          xlab = "",
          ylab = "",
          cex.names = 0.85,
          cex.axis = 0.9,
          ylim = c(0, max(valores) * 1.20),
          las = 1,
          axes = FALSE
        )

        eje = pretty(c(0, max(valores)))
        axis(2, at = eje, labels = formatear_miles(eje), las = 1)

        abline(h = eje, col = "#d9d9d9", lty = "dotted")

        mtext(input$nombre_columna_x, side = 1, line = 5, font = 2)
        mtext(input$nombre_columna_y, side = 2, line = 5, font = 2)

        text(
          x = posiciones,
          y = valores,
          labels = formatear_miles(valores),
          pos = 3,
          cex = 0.8
        )
      }
    })
  })
}