datos_ejemplo_sturges = paste(
  "1.64;  1.58;  1.70;  1.59;  1.55;  1.65;  1.71;  1.65;  1.71;",
  "1.60;  1.74;  1.68;  1.57;  1.66;  1.70;  1.63;  1.59;  1.67;",
  "1.63;  1.72;  1.75;  1.72;  1.70;  1.58;  1.79;  1.56;  1.65;",
  sep = "\n"
)

redondear_arriba_precision = function(valor, c) {
  ceiling((valor / c) - 1e-10) * c
}

redondear_k = function(valor) {
  floor(valor + 0.5)
}

crear_tabla_sturges = function(texto) {
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

  n = length(datos)

  if (n <= 14) {
    return(list(
      error = "La técnica de Sturges se aplica cuando el número de datos es superior a 14."
    ))
  }

  if (n >= 17000) {
    return(list(
      error = "La técnica de Sturges se aplica cuando el número de datos es inferior a 17.000."
    ))
  }

  d = min(datos)
  D = max(datos)

  c = ifelse(decimales <= 0, 1, 10^(-decimales))

  la = D - d + c

  k_teorico = 1 + 3.3 * log10(n)
  k = redondear_k(k_teorico)

  if (k < 1) {
    k = 1
  }

  t_inicial = la / k
  t = redondear_arriba_precision(t_inicial, c)

  while ((t * k) < la) {
    t = t + c
  }

  longitud_corregida = t * k
  sobrante = longitud_corregida - la
  unidades_sobrantes = round(sobrante / c)

  restar_d = floor(unidades_sobrantes / 2) * c
  sumar_D = ceiling(unidades_sobrantes / 2) * c

  d_corregido = d - restar_d
  D_corregido = D + sumar_D

  Li = d_corregido + (0:(k - 1)) * t
  Ls = Li + t

  conteos = character(k)
  fi = numeric(k)

  for (i in seq_len(k)) {
    if (i == k) {
      datos_intervalo = datos[datos >= Li[i] & datos <= Ls[i]]
    } else {
      datos_intervalo = datos[datos >= Li[i] & datos < Ls[i]]
    }

    fi[i] = length(datos_intervalo)

    if (length(datos_intervalo) == 0) {
      conteos[i] = "-"
    } else {
      conteos[i] = paste(
        formatear_numero(sort(datos_intervalo), decimales, forzar_decimales = decimales > 0),
        collapse = ", "
      )
    }
  }

  hi = fi / n
  pi = hi * 100
  Fi = cumsum(fi)
  Hi = cumsum(hi)
  Pi = cumsum(pi)
  xi = (Li + Ls) / 2

  intervalos = paste0(
    "[",
    formatear_numero(Li, decimales, forzar_decimales = decimales > 0),
    "; ",
    formatear_numero(Ls, decimales, forzar_decimales = decimales > 0),
    ifelse(seq_len(k) == k, "]", ")")
  )

  tabla = data.frame(
    Ii = intervalos,
    Li = Li,
    Ls = Ls,
    xi = xi,
    Conteo = conteos,
    fi = fi,
    hi = hi,
    pi = pi,
    Fi = Fi,
    Hi = Hi,
    Pi = Pi
  )

  pasos = data.frame(
    paso = c(
      "1° paso: Número de datos",
      "2° paso: Dato menor",
      "2° paso: Dato mayor",
      "2° paso: Alcance",
      "3° paso: Precisión de los datos",
      "3° paso: Longitud de alcance",
      "4° paso: k teórico por Sturges",
      "4° paso: k redondeado",
      "5° paso: t inicial",
      "5° paso: t ajustado",
      "Corrección: t * k",
      "Corrección: sobrante",
      "Corrección: se resta al límite inferior",
      "Corrección: se suma al límite superior",
      "Alcance corregido: d corregido",
      "Alcance corregido: D corregido"
    ),
    operacion = c(
      "n",
      "d = mínimo",
      "D = máximo",
      "a = [d; D]",
      "c",
      "la = D - d + c",
      "k = 1 + 3.3 * log10(n)",
      "k redondeado",
      "t = la / k",
      "t ajustado según c",
      "t * k",
      "t * k - la",
      "floor(sobrante / 2)",
      "ceiling(sobrante / 2)",
      "d - corrección inferior",
      "D + corrección superior"
    ),
    valor = c(
      n,
      d,
      D,
      paste0(
        "[",
        formatear_numero(d, decimales, forzar_decimales = decimales > 0),
        "; ",
        formatear_numero(D, decimales, forzar_decimales = decimales > 0),
        "]"
      ),
      c,
      la,
      k_teorico,
      k,
      t_inicial,
      t,
      longitud_corregida,
      sobrante,
      restar_d,
      sumar_D,
      d_corregido,
      D_corregido
    )
  )

  resumen = data.frame(
    medida = c(
      "n",
      "d",
      "D",
      "c",
      "la",
      "k teórico",
      "k redondeado",
      "t inicial",
      "t ajustado",
      "t * k",
      "sobrante",
      "d corregido",
      "D corregido",
      "Total fi"
    ),
    valor = c(
      n,
      d,
      D,
      c,
      la,
      k_teorico,
      k,
      t_inicial,
      t,
      longitud_corregida,
      sobrante,
      d_corregido,
      D_corregido,
      sum(fi)
    )
  )

  list(
    tabla = tabla,
    pasos = pasos,
    resumen = resumen,
    decimales = decimales,
    datos = datos
  )
}

formatear_tabla_sturges = function(resultado) {
  tabla = resultado$tabla
  decimales = resultado$decimales

  tabla$Li = formatear_numero(tabla$Li, decimales, forzar_decimales = decimales > 0)
  tabla$Ls = formatear_numero(tabla$Ls, decimales, forzar_decimales = decimales > 0)
  tabla$xi = formatear_numero(tabla$xi, decimales + 1, forzar_decimales = decimales > 0)
  tabla$fi = formatear_numero(tabla$fi, 0)
  tabla$hi = formatear_numero(tabla$hi, 3)
  tabla$pi = formatear_porcentaje(tabla$pi, 2)
  tabla$Fi = formatear_numero(tabla$Fi, 0)
  tabla$Hi = formatear_numero(tabla$Hi, 3)
  tabla$Pi = formatear_porcentaje(tabla$Pi, 2)

  total = data.frame(
    Ii = "TOTAL",
    Li = "",
    Ls = "",
    xi = "",
    Conteo = "",
    fi = formatear_numero(sum(resultado$tabla$fi), 0),
    hi = formatear_numero(sum(resultado$tabla$hi), 3),
    pi = formatear_porcentaje(sum(resultado$tabla$pi), 2),
    Fi = "",
    Hi = "",
    Pi = ""
  )

  rbind(tabla, total)
}

formatear_pasos_sturges = function(resultado) {
  pasos = resultado$pasos
  decimales = resultado$decimales

  for (i in seq_len(nrow(pasos))) {
    numero = suppressWarnings(as.numeric(pasos$valor[i]))

    if (!is.na(numero)) {
      if (grepl("k teórico", pasos$paso[i]) || grepl("t inicial", pasos$paso[i])) {
        pasos$valor[i] = formatear_numero(numero, 4)
      } else if (grepl("k redondeado|Número de datos", pasos$paso[i])) {
        pasos$valor[i] = formatear_numero(numero, 0)
      } else {
        pasos$valor[i] = formatear_numero(numero, decimales, forzar_decimales = decimales > 0)
      }
    }
  }

  pasos
}

formatear_resumen_sturges = function(resultado) {
  resumen = resultado$resumen
  decimales = resultado$decimales

  for (i in seq_len(nrow(resumen))) {
    numero = suppressWarnings(as.numeric(resumen$valor[i]))

    if (!is.na(numero)) {
      if (resumen$medida[i] %in% c("n", "k redondeado", "Total fi")) {
        resumen$valor[i] = formatear_numero(numero, 0)
      } else if (resumen$medida[i] %in% c("k teórico", "t inicial")) {
        resumen$valor[i] = formatear_numero(numero, 4)
      } else {
        resumen$valor[i] = formatear_numero(
          numero,
          decimales,
          forzar_decimales = decimales > 0
        )
      }
    }
  }

  resumen
}

modulo_sturges_ui = function(id) {
  ns = NS(id)

  div(
    class = "tarjeta",
    h1(class = "titulo", "Tabla por Sturges"),
    div(
      class = "nota",
      "Ejemplo precargado: estaturas en metros observadas en 27 soldados de un regimiento de la ciudad de Sucre. Los datos se ingresan separados por punto y coma (;)."
    ),
    textInput(
      ns("numero_tabla"),
      "Número de tabla",
      value = "Tabla N° 7"
    ),
    textInput(
      ns("titulo_tabla"),
      "Título",
      value = "Soldados agrupados por estatura en metros",
      width = "100%"
    ),
    textAreaInput(
      ns("datos"),
      "Datos",
      value = datos_ejemplo_sturges,
      rows = 6,
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
      "Generar tabla Sturges",
      class = "btn-primary"
    ),
    actionButton(
      ns("cargar_ejemplo"),
      "Cargar ejemplo",
      class = "btn-info"
    ),
    tags$hr(),
    uiOutput(ns("salida"))
  )
}

modulo_sturges_server = function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$cargar_ejemplo, {
      updateTextInput(session, "numero_tabla", value = "Tabla N° 7")
      updateTextInput(
        session,
        "titulo_tabla",
        value = "Soldados agrupados por estatura en metros"
      )
      updateTextAreaInput(session, "datos", value = datos_ejemplo_sturges)
    })

    resultado_sturges = eventReactive(input$generar, {
      crear_tabla_sturges(input$datos)
    }, ignoreNULL = FALSE)

    output$salida = renderUI({
      resultado = resultado_sturges()

      if (!is.null(resultado$error)) {
        return(div(class = "error", resultado$error))
      }

      tagList(
        h2(class = "subtitulo", "Cálculos"),
        tableOutput(session$ns("pasos_sturges")),

        tags$hr(),

        h2(class = "subtitulo", input$numero_tabla),
        h4(input$titulo_tabla),
        tableOutput(session$ns("tabla_sturges")),

        div(
          class = "resultado-final",
          h4("Resumen"),
          tableOutput(session$ns("resumen_sturges"))
        )
      )
    })

    output$pasos_sturges = renderTable({
      resultado = resultado_sturges()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      formatear_pasos_sturges(resultado)
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$tabla_sturges = renderTable({
      resultado = resultado_sturges()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      formatear_tabla_sturges(resultado)
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$resumen_sturges = renderTable({
      resultado = resultado_sturges()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      formatear_resumen_sturges(resultado)
    }, striped = FALSE, bordered = FALSE, spacing = "m")
  })
}