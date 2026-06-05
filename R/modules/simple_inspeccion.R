datos_ejemplo_simple_inspeccion = paste(
  "1;  0;  0;  2;  3;  1;  1;  2;  1;  4;",
  "1;  0;  2;  3;  3;  4;  1;  1;  4;  1;",
  "2;  1;  2;  0;  0;  1;  2;  1;  0;  1;",
  "2;  1;  3;  3;  2;  4;  0;  0;  0;  1;",
  sep = "\n"
)

crear_tabla_simple_inspeccion = function(texto) {
  detalle = parsear_numeros_detalle(texto)
  datos = detalle$valores
  decimales = detalle$decimales
  invalidos = detalle$invalidos

  if (length(invalidos) > 0) {
    return(list(
      error = paste(
        "Hay datos no numéricos o mal escritos:",
        paste(invalidos, collapse = ", ")
      )
    ))
  }

  if (length(datos) == 0) {
    return(list(error = "Debe ingresar al menos un dato numérico."))
  }

  valores = sort(unique(datos))
  cantidad_valores = length(valores)

  if (cantidad_valores > 10) {
    return(list(
      error = paste0(
        "La técnica de simple inspección solo debe usarse cuando existen pocos valores diferentes. ",
        "En este caso hay ",
        cantidad_valores,
        " valores distintos. Use Sturges u otra técnica de agrupación."
      )
    ))
  }

  fi = as.numeric(table(factor(datos, levels = valores)))
  n = sum(fi)

  hi = fi / n
  pi = hi * 100
  Fi = cumsum(fi)
  Hi = cumsum(hi)
  Pi = cumsum(pi)

  tabla = data.frame(
    Ii = valores,
    fi = fi,
    hi = hi,
    pi = pi,
    Fi = Fi,
    Hi = Hi,
    Pi = Pi
  )

  moda = calcular_moda_vector(datos)

  resumen = data.frame(
    medida = c(
      "n",
      "Valores diferentes",
      "Valor menor",
      "Valor mayor",
      "Media aritmética",
      "Mediana",
      "Moda",
      "Tipo de moda"
    ),
    valor = c(
      n,
      cantidad_valores,
      min(datos),
      max(datos),
      mean(datos),
      median(datos),
      moda$valor,
      moda$tipo
    )
  )

  list(
    tabla = tabla,
    resumen = resumen,
    decimales = decimales,
    datos = datos
  )
}

formatear_tabla_simple = function(resultado) {
  tabla = resultado$tabla
  decimales = resultado$decimales

  tabla$Ii = formatear_numero(tabla$Ii, decimales, forzar_decimales = decimales > 0)
  tabla$fi = formatear_numero(tabla$fi, 0)
  tabla$hi = formatear_numero(tabla$hi, 3)
  tabla$pi = formatear_porcentaje(tabla$pi, 2)
  tabla$Fi = formatear_numero(tabla$Fi, 0)
  tabla$Hi = formatear_numero(tabla$Hi, 3)
  tabla$Pi = formatear_porcentaje(tabla$Pi, 2)

  total = data.frame(
    Ii = "TOTAL",
    fi = formatear_numero(sum(resultado$tabla$fi), 0),
    hi = formatear_numero(sum(resultado$tabla$hi), 3),
    pi = formatear_porcentaje(sum(resultado$tabla$pi), 2),
    Fi = "",
    Hi = "",
    Pi = ""
  )

  rbind(tabla, total)
}

formatear_resumen_simple = function(resultado) {
  resumen = resultado$resumen
  decimales = resultado$decimales

  for (i in seq_len(nrow(resumen))) {
    valor = resumen$valor[i]
    numero = suppressWarnings(as.numeric(valor))

    if (!is.na(numero)) {
      if (resumen$medida[i] %in% c("Valor menor", "Valor mayor", "Media aritmética", "Mediana")) {
        resumen$valor[i] = formatear_numero(numero, decimales, forzar_decimales = decimales > 0)
      } else {
        resumen$valor[i] = formatear_numero(numero, 3)
      }
    }
  }

  resumen
}

modulo_simple_inspeccion_ui = function(id) {
  ns = NS(id)

  div(
    class = "tarjeta",
    h1(class = "titulo", "Tabla simple inspección"),
    div(
      class = "nota",
      "Ejemplo precargado del docente: número de relojes de pared que tienen 40 viviendas familiares en un barrio de la ciudad de Sucre. Los datos se ingresan separados por punto y coma (;)."
    ),
    textInput(
      ns("numero_tabla"),
      "Número de tabla",
      value = "Tabla N° 6"
    ),
    textInput(
      ns("titulo_tabla"),
      "Título",
      value = "Viviendas familiares agrupadas por el número de relojes de pared que disponen",
      width = "100%"
    ),
    textAreaInput(
      ns("datos"),
      "Datos",
      value = datos_ejemplo_simple_inspeccion,
      rows = 7,
      width = "100%"
    ),
    actionButton(
      ns("pegar_excel"),
      "Pegar desde Excel",
      onclick = paste0("pegarEnTextArea('", ns("datos"), "')"),
      class = "btn-warning"
    ),
    actionButton(
      ns("generar"),
      "Generar tabla",
      class = "btn-primary"
    ),
    actionButton(
      ns("cargar_ejemplo"),
      "Cargar ejemplo del PDF",
      class = "btn-info"
    ),
    tags$hr(),
    uiOutput(ns("salida"))
  )
}

modulo_simple_inspeccion_server = function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$cargar_ejemplo, {
      updateTextInput(session, "numero_tabla", value = "Tabla N° 6")
      updateTextInput(
        session,
        "titulo_tabla",
        value = "Viviendas familiares agrupadas por el número de relojes de pared que disponen"
      )
      updateTextAreaInput(session, "datos", value = datos_ejemplo_simple_inspeccion)
    })

    resultado_simple = eventReactive(input$generar, {
      crear_tabla_simple_inspeccion(input$datos)
    }, ignoreNULL = FALSE)

    output$salida = renderUI({
      resultado = resultado_simple()

      if (!is.null(resultado$error)) {
        return(div(class = "error", resultado$error))
      }

      tagList(
        h2(class = "subtitulo", input$numero_tabla),
        h4(input$titulo_tabla),
        h4("Tabla de frecuencias"),
        tableOutput(session$ns("tabla_simple")),
        div(
          class = "resultado-final",
          h4("Resumen"),
          tableOutput(session$ns("resumen_simple"))
        )
      )
    })

    output$tabla_simple = renderTable({
      resultado = resultado_simple()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      formatear_tabla_simple(resultado)
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$resumen_simple = renderTable({
      resultado = resultado_simple()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      formatear_resumen_simple(resultado)
    }, striped = FALSE, bordered = FALSE, spacing = "m")
  })
}