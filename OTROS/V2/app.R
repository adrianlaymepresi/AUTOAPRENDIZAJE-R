# CTRL + ENTER
# ejecutar: shiny::runApp(".")

library(shiny)

normalizar_numero = function(valor) {
  valor = trimws(as.character(valor))
  valor = gsub(",", ".", valor, fixed = TRUE)
  suppressWarnings(as.numeric(valor))
}

quitar_ceros = function(texto) {
  texto = sub("(\\.\\d*?[1-9])0+$", "\\1", texto)
  texto = sub("\\.0+$", "", texto)
  texto
}

contar_decimales_token = function(token) {
  token = trimws(as.character(token))
  token = gsub(",", ".", token, fixed = TRUE)

  if (!grepl("\\.", token)) {
    return(0)
  }

  decimales = sub("^.*\\.", "", token)
  decimales = sub("0+$", "", decimales)

  nchar(decimales)
}

formatear_numero = function(x, decimales = 2) {
  if (is.character(x)) {
    return(x)
  }

  ifelse(
    is.na(x),
    "",
    quitar_ceros(format(round(x, decimales), nsmall = decimales, scientific = FALSE, trim = TRUE))
  )
}

formatear_tabla = function(tabla, columnas_numericas = names(tabla), decimales = 2) {
  salida = tabla

  for (columna in columnas_numericas) {
    if (columna %in% names(salida)) {
      salida[[columna]] = formatear_numero(salida[[columna]], decimales)
    }
  }

  salida
}

separar_linea = function(linea) {
  linea = trimws(linea)

  if (linea == "") {
    return(character(0))
  }

  if (grepl("\t", linea)) {
    return(trimws(unlist(strsplit(linea, "\t+"))))
  }

  trimws(unlist(strsplit(linea, "\\s+")))
}

parsear_numeros_detalle = function(texto) {
  if (is.null(texto) || trimws(texto) == "") {
    return(list(valores = numeric(0), decimales = 0))
  }

  texto = gsub("\r", "\n", texto)
  texto = gsub(";", " ", texto)

  tokens_iniciales = unlist(strsplit(texto, "\\s+"))
  tokens_finales = character(0)

  for (token in tokens_iniciales) {
    token = trimws(token)

    if (token == "") {
      next
    }

    cantidad_comas = gregexpr(",", token, fixed = TRUE)[[1]]
    cantidad_comas = ifelse(cantidad_comas[1] == -1, 0, length(cantidad_comas))

    if (cantidad_comas >= 2) {
      tokens_finales = c(tokens_finales, unlist(strsplit(token, ",", fixed = TRUE)))
    } else if (cantidad_comas == 1 && !grepl("\\.", token)) {
      parte_decimal = sub("^.*,", "", token)

      if (nchar(parte_decimal) >= 2) {
        tokens_finales = c(tokens_finales, token)
      } else {
        tokens_finales = c(tokens_finales, unlist(strsplit(token, ",", fixed = TRUE)))
      }
    } else if (grepl(",", token) && grepl("\\.", token)) {
      tokens_finales = c(tokens_finales, unlist(strsplit(token, ",", fixed = TRUE)))
    } else {
      tokens_finales = c(tokens_finales, token)
    }
  }

  tokens_finales = trimws(tokens_finales)
  tokens_finales = tokens_finales[tokens_finales != ""]

  valores = normalizar_numero(tokens_finales)
  validos = !is.na(valores)

  valores = valores[validos]
  tokens_finales = tokens_finales[validos]

  decimales = ifelse(
    length(tokens_finales) == 0,
    0,
    max(sapply(tokens_finales, contar_decimales_token))
  )

  list(valores = valores, decimales = decimales)
}

parsear_numeros = function(texto) {
  parsear_numeros_detalle(texto)$valores
}

parsear_textos = function(texto) {
  if (is.null(texto) || trimws(texto) == "") {
    return(character(0))
  }

  texto = gsub("\r", "\n", texto)
  texto = gsub("\t", ",", texto)
  texto = gsub(";", ",", texto)
  texto = gsub("\n", ",", texto)

  partes = trimws(unlist(strsplit(texto, ",")))
  partes = partes[partes != ""]

  partes
}

parsear_datos_grafico = function(texto) {
  if (is.null(texto) || trimws(texto) == "") {
    return(data.frame(x = character(0), y = numeric(0)))
  }

  lineas = unlist(strsplit(gsub("\r", "\n", texto), "\n"))
  filas = list()

  for (linea in lineas) {
    partes = separar_linea(linea)

    if (length(partes) >= 2) {
      y = normalizar_numero(partes[length(partes)])

      if (!is.na(y)) {
        x = paste(partes[-length(partes)], collapse = " ")
        filas[[length(filas) + 1]] = data.frame(x = x, y = y)
      }
    }
  }

  if (length(filas) == 0) {
    return(data.frame(x = character(0), y = numeric(0)))
  }

  do.call(rbind, filas)
}

parsear_tabla_clasificada = function(texto) {
  if (is.null(texto) || trimws(texto) == "") {
    return(data.frame(Li = numeric(0), Ls = numeric(0), frecuencia = numeric(0)))
  }

  lineas = unlist(strsplit(gsub("\r", "\n", texto), "\n"))
  filas = list()

  for (linea in lineas) {
    partes = separar_linea(linea)
    numeros = normalizar_numero(partes)
    numeros = numeros[!is.na(numeros)]

    if (length(numeros) >= 3) {
      filas[[length(filas) + 1]] = c(numeros[1], numeros[2], numeros[3])
    }
  }

  if (length(filas) == 0) {
    return(data.frame(Li = numeric(0), Ls = numeric(0), frecuencia = numeric(0)))
  }

  matriz = do.call(rbind, filas)

  data.frame(
    Li = matriz[, 1],
    Ls = matriz[, 2],
    frecuencia = matriz[, 3]
  )
}

validar_no_clasificados = function(datos) {
  if (length(datos) == 0) {
    return("Debe ingresar al menos un dato numérico.")
  }

  TRUE
}

validar_clasificados = function(tabla) {
  if (nrow(tabla) == 0) {
    return("Debe ingresar al menos un intervalo con su frecuencia.")
  }

  if (any(tabla$Ls <= tabla$Li)) {
    return("Cada límite superior debe ser mayor que su límite inferior.")
  }

  if (any(tabla$frecuencia <= 0)) {
    return("Las frecuencias deben ser mayores que cero.")
  }

  TRUE
}

calcular_moda_vector = function(datos) {
  tabla = table(datos)
  frecuencia_mayor = max(tabla)

  if (frecuencia_mayor == 1) {
    return(list(valor = "No existe moda", tipo = "Sin moda"))
  }

  modas = names(tabla[tabla == frecuencia_mayor])

  tipo = ifelse(
    length(modas) == 1,
    "Unimodal",
    ifelse(
      length(modas) == 2,
      "Bimodal",
      ifelse(length(modas) == 3, "Trimodal", "Multimodal")
    )
  )

  list(valor = paste(modas, collapse = ", "), tipo = tipo)
}

calcular_cuantil_manual = function(datos, k, divisor) {
  datos_ordenados = sort(datos)
  n = length(datos_ordenados)
  posicion = k * (n + 1) / divisor

  if (posicion < 1 || posicion > n) {
    return(list(error = "La posición calculada queda fuera de los datos disponibles."))
  }

  if (posicion == floor(posicion)) {
    resultado = datos_ordenados[posicion]
  } else {
    inferior = floor(posicion)
    superior = ceiling(posicion)
    parte_decimal = posicion - inferior
    resultado = datos_ordenados[inferior] + parte_decimal * (datos_ordenados[superior] - datos_ordenados[inferior])
  }

  list(
    resultado = resultado,
    detalle = data.frame(
      posicion = seq_along(datos_ordenados),
      xi = datos_ordenados
    ),
    pasos = data.frame(
      paso = c("k", "n", "Posición", "Resultado"),
      valor = c(k, n, posicion, resultado)
    )
  )
}

crear_tabla_clasificada = function(tabla) {
  xi = (tabla$Li + tabla$Ls) / 2
  fi = tabla$frecuencia
  n = sum(fi)

  data.frame(
    Intervalo = paste0("[", tabla$Li, "; ", tabla$Ls, ")"),
    Li = tabla$Li,
    Ls = tabla$Ls,
    xi = xi,
    fi = fi,
    hi = fi / n,
    pi = (fi / n) * 100,
    Fi = cumsum(fi),
    Hi = cumsum(fi) / n,
    Pi = (cumsum(fi) / n) * 100
  )
}

calcular_medida_clasificada = function(tabla, medida, k = 1) {
  validacion = validar_clasificados(tabla)

  if (validacion != TRUE) {
    return(list(error = validacion))
  }

  tabla_calculada = crear_tabla_clasificada(tabla)

  xi = tabla_calculada$xi
  fi = tabla_calculada$fi
  n = sum(fi)

  if (medida == "Media aritmética") {
    return(list(
      resultado = sum(xi * fi) / n,
      detalle = tabla_calculada
    ))
  }

  if (medida == "Media geométrica") {
    if (any(xi <= 0)) {
      return(list(error = "La media geométrica requiere marcas de clase mayores a cero."))
    }

    return(list(
      resultado = exp(sum(fi * log(xi)) / n),
      detalle = tabla_calculada
    ))
  }

  if (medida == "Media armónica") {
    if (any(xi == 0)) {
      return(list(error = "La media armónica no acepta marcas de clase iguales a cero."))
    }

    return(list(
      resultado = n / sum(fi / xi),
      detalle = tabla_calculada
    ))
  }

  if (medida == "Mediana") {
    posicion = n / 2
    indice = which(tabla_calculada$Fi >= posicion)[1]

    Li = tabla$Li[indice]
    t = tabla$Ls[indice] - tabla$Li[indice]
    Fi_anterior = ifelse(indice == 1, 0, tabla_calculada$Fi[indice - 1])
    resultado = Li + ((posicion - Fi_anterior) * t / fi[indice])

    pasos = data.frame(
      paso = c("n", "n/2", "Li", "Fi anterior", "t", "fi", "Mediana"),
      valor = c(n, posicion, Li, Fi_anterior, t, fi[indice], resultado)
    )

    return(list(
      resultado = resultado,
      detalle = tabla_calculada,
      pasos = pasos
    ))
  }

  if (medida == "Moda") {
    indice = which.max(fi)

    Li = tabla$Li[indice]
    t = tabla$Ls[indice] - tabla$Li[indice]
    frecuencia_anterior = ifelse(indice == 1, 0, fi[indice - 1])
    frecuencia_posterior = ifelse(indice == length(fi), 0, fi[indice + 1])

    d1 = fi[indice] - frecuencia_anterior
    d2 = fi[indice] - frecuencia_posterior

    if ((d1 + d2) == 0) {
      return(list(error = "No se puede calcular la moda porque d1 + d2 es cero."))
    }

    resultado = Li + ((t * d1) / (d1 + d2))

    pasos = data.frame(
      paso = c("Li", "t", "fi modal", "fi anterior", "fi posterior", "d1", "d2", "Moda"),
      valor = c(Li, t, fi[indice], frecuencia_anterior, frecuencia_posterior, d1, d2, resultado)
    )

    return(list(
      resultado = resultado,
      detalle = tabla_calculada,
      pasos = pasos
    ))
  }

  if (medida %in% c("Cuartiles", "Deciles", "Percentiles")) {
    divisor = ifelse(medida == "Cuartiles", 4, ifelse(medida == "Deciles", 10, 100))
    posicion = k * n / divisor
    indice = which(tabla_calculada$Fi >= posicion)[1]

    Li = tabla$Li[indice]
    t = tabla$Ls[indice] - tabla$Li[indice]
    Fi_anterior = ifelse(indice == 1, 0, tabla_calculada$Fi[indice - 1])
    resultado = Li + ((posicion - Fi_anterior) * t / fi[indice])

    pasos = data.frame(
      paso = c("k", "n", "Posición", "Li", "Fi anterior", "t", "fi", "Resultado"),
      valor = c(k, n, posicion, Li, Fi_anterior, t, fi[indice], resultado)
    )

    return(list(
      resultado = resultado,
      detalle = tabla_calculada,
      pasos = pasos
    ))
  }
}

calcular_medida_no_clasificada = function(datos, medida, k = 1) {
  validacion = validar_no_clasificados(datos)

  if (validacion != TRUE) {
    return(list(error = validacion))
  }

  if (medida == "Media aritmética") {
    return(list(
      resultado = mean(datos),
      detalle = data.frame(
        concepto = c("n", "Suma", "Media aritmética"),
        valor = c(length(datos), sum(datos), mean(datos))
      )
    ))
  }

  if (medida == "Media geométrica") {
    if (any(datos <= 0)) {
      return(list(error = "La media geométrica no acepta datos negativos ni cero."))
    }

    resultado = exp(mean(log(datos)))

    return(list(
      resultado = resultado,
      detalle = data.frame(
        concepto = c("n", "Media geométrica"),
        valor = c(length(datos), resultado)
      )
    ))
  }

  if (medida == "Media armónica") {
    if (any(datos == 0)) {
      return(list(error = "La media armónica no acepta datos iguales a cero."))
    }

    resultado = 1 / mean(1 / datos)

    return(list(
      resultado = resultado,
      detalle = data.frame(
        concepto = c("n", "Media armónica"),
        valor = c(length(datos), resultado)
      )
    ))
  }

  if (medida == "Mediana") {
    datos_ordenados = sort(datos)

    return(list(
      resultado = median(datos),
      detalle = data.frame(
        posicion = seq_along(datos_ordenados),
        xi = datos_ordenados
      )
    ))
  }

  if (medida == "Moda") {
    moda = calcular_moda_vector(datos)

    return(list(
      resultado = moda$valor,
      detalle = data.frame(
        paso = "Tipo",
        valor = moda$tipo
      )
    ))
  }

  if (medida == "Cuartiles") {
    return(calcular_cuantil_manual(datos, k, 4))
  }

  if (medida == "Deciles") {
    return(calcular_cuantil_manual(datos, k, 10))
  }

  if (medida == "Percentiles") {
    return(calcular_cuantil_manual(datos, k, 100))
  }
}

validar_k = function(medida, k) {
  if (medida == "Cuartiles" && (k < 1 || k > 3)) {
    return("En cuartiles, k debe estar entre 1 y 3.")
  }

  if (medida == "Deciles" && (k < 1 || k > 9)) {
    return("En deciles, k debe estar entre 1 y 9.")
  }

  if (medida == "Percentiles" && (k < 1 || k > 99)) {
    return("En percentiles, k debe estar entre 1 y 99.")
  }

  TRUE
}

crear_tabla_simple_inspeccion = function(texto, tipo_variable) {
  if (tipo_variable == "Cuantitativa") {
    detalle = parsear_numeros_detalle(texto)
    datos = detalle$valores
    decimales = detalle$decimales

    if (length(datos) == 0) {
      return(list(error = "Debe ingresar datos numéricos."))
    }

    valores = sort(unique(datos))
    fi = as.numeric(table(factor(datos, levels = valores)))
    n = sum(fi)

    tabla = data.frame(
      Ii = valores,
      fi = fi,
      hi = fi / n,
      pi = (fi / n) * 100,
      Fi = cumsum(fi),
      Hi = cumsum(fi) / n,
      Pi = (cumsum(fi) / n) * 100
    )

    moda = calcular_moda_vector(datos)

    resumen = data.frame(
      medida = c("n", "Media aritmética", "Mediana", "Moda"),
      valor = c(n, mean(datos), median(datos), moda$valor)
    )

    mensajes = character(0)

    if (length(valores) >= 10) {
      mensajes = c(mensajes, "Advertencia: la técnica de simple inspección se recomienda cuando hay menos de 10 valores diferentes.")
    }

    return(list(
      tabla = tabla,
      resumen = resumen,
      mensajes = mensajes,
      decimales = decimales
    ))
  }

  datos = parsear_textos(texto)

  if (length(datos) == 0) {
    return(list(error = "Debe ingresar categorías."))
  }

  valores = names(sort(table(datos), decreasing = TRUE))
  fi = as.numeric(table(factor(datos, levels = valores)))
  n = sum(fi)

  tabla = data.frame(
    Categoria = valores,
    fi = fi,
    hi = fi / n,
    pi = (fi / n) * 100,
    Fi = cumsum(fi),
    Hi = cumsum(fi) / n,
    Pi = (cumsum(fi) / n) * 100
  )

  resumen = data.frame(
    medida = c("n", "Categoría con mayor frecuencia"),
    valor = c(n, valores[1])
  )

  list(
    tabla = tabla,
    resumen = resumen,
    mensajes = character(0),
    decimales = 0
  )
}

obtener_c = function(datos, decimales) {
  if (decimales <= 0) {
    return(1)
  }

  10^(-decimales)
}

redondear_arriba_precision = function(valor, c) {
  ceiling((valor / c) - 1e-10) * c
}

crear_tabla_sturges = function(texto) {
  detalle = parsear_numeros_detalle(texto)
  datos = detalle$valores
  decimales = detalle$decimales

  if (length(datos) == 0) {
    return(list(error = "Debe ingresar datos numéricos."))
  }

  if (length(datos) < 2) {
    return(list(error = "Debe ingresar por lo menos dos datos."))
  }

  n = length(datos)
  d = min(datos)
  D = max(datos)
  c = obtener_c(datos, decimales)

  la = D - d + c
  k_teorico = 1 + 3.322 * log10(n)
  k = round(k_teorico)

  if (k < 1) {
    k = 1
  }

  t_inicial = la / k
  t = redondear_arriba_precision(t_inicial, c)

  while ((t * k) < la) {
    t = t + c
  }

  sobrante = (t * k) - la
  unidades_sobrantes = round(sobrante / c)

  sumar_D = ceiling(unidades_sobrantes / 2) * c
  restar_d = floor(unidades_sobrantes / 2) * c

  d_corregido = d - restar_d
  D_corregido = D + sumar_D

  Li = d_corregido + (0:(k - 1)) * t
  Ls = Li + t

  fi = numeric(k)

  for (i in seq_len(k)) {
    if (i == k) {
      fi[i] = sum(datos >= Li[i] & datos <= Ls[i])
    } else {
      fi[i] = sum(datos >= Li[i] & datos < Ls[i])
    }
  }

  tabla_base = data.frame(
    Li = Li,
    Ls = Ls,
    frecuencia = fi
  )

  tabla = crear_tabla_clasificada(tabla_base)

  tabla$Intervalo = paste0(
    "[",
    formatear_numero(Li, decimales),
    "; ",
    formatear_numero(Ls, decimales),
    ifelse(seq_len(k) == k, "]", ")")
  )

  medidas = data.frame(
    medida = c("Media aritmética", "Mediana", "Moda"),
    valor = c(
      calcular_medida_clasificada(tabla_base, "Media aritmética")$resultado,
      calcular_medida_clasificada(tabla_base, "Mediana")$resultado,
      calcular_medida_clasificada(tabla_base, "Moda")$resultado
    )
  )

  pasos = data.frame(
    paso = c(
      "n",
      "d",
      "D",
      "c",
      "la = D - d + c",
      "k = 1 + 3.322 log10(n)",
      "k redondeado",
      "t inicial",
      "t ajustado",
      "t * k",
      "sobrante",
      "d corregido",
      "D corregido"
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
      t * k,
      sobrante,
      d_corregido,
      D_corregido
    )
  )

  list(
    tabla = tabla,
    pasos = pasos,
    medidas = medidas,
    decimales = decimales
  )
}

ui = fluidPage(
  tags$head(
    tags$script(HTML("
      async function pegarEnTextArea(id) {
        const texto = await navigator.clipboard.readText();
        const caja = document.getElementById(id);
        caja.value = texto;
        caja.dispatchEvent(new Event('input', { bubbles: true }));
      }
    ")),
    tags$style(HTML("
      :root {
        --verde: #174b33;
        --verde-oscuro: #0f3b27;
        --crema: #f7f1e5;
        --cafe: #c49a6c;
        --azul: #3c6fa6;
        --blanco: #ffffff;
        --rojo: #b91c1c;
      }

      body {
        background: var(--crema);
        font-family: Arial, sans-serif;
        color: #00192b;
      }

      .contenedor {
        display: flex;
        min-height: 100vh;
      }

      .sidebar {
        width: 280px;
        background: var(--verde);
        color: white;
        padding: 34px;
      }

      .sidebar h1 {
        color: #fff7d6;
        font-weight: 800;
        margin-bottom: 36px;
      }

      .sidebar label {
        color: white;
        font-size: 17px;
        font-weight: 800;
      }

      .sidebar .radio {
        margin: 18px 0;
      }

      .contenido {
        flex: 1;
        padding: 44px;
      }

      .tarjeta {
        background: white;
        border-radius: 26px;
        padding: 36px;
        max-width: 980px;
        box-shadow: 0 8px 30px rgba(0,0,0,0.08);
      }

      .titulo {
        color: var(--verde-oscuro);
        font-size: 46px;
        font-weight: 900;
        margin-top: 0;
      }

      .subtitulo {
        color: var(--verde);
        font-size: 32px;
        font-weight: 900;
      }

      .form-control {
        max-width: 560px;
        font-size: 18px;
      }

      textarea.form-control {
        min-height: 130px;
      }

      .btn {
        border-radius: 12px;
        font-size: 17px;
        font-weight: 800;
        border: none;
        padding: 10px 18px;
        margin-right: 6px;
      }

      .btn-primary {
        background: var(--verde);
        color: white;
      }

      .btn-warning {
        background: var(--cafe);
        color: white;
      }

      .btn-info {
        background: var(--azul);
        color: white;
      }

      table {
        width: auto !important;
        margin-top: 12px;
        font-size: 16px;
      }

      th {
        border-bottom: 3px solid #d8d8d8 !important;
        padding: 8px 18px !important;
      }

      td {
        border-bottom: 1px solid #dddddd !important;
        padding: 8px 18px !important;
      }

      .error {
        color: var(--rojo);
        font-weight: 800;
        padding: 12px 0;
      }

      .nota {
        background: #fff8e6;
        border-left: 6px solid var(--cafe);
        padding: 12px 14px;
        margin-bottom: 18px;
        max-width: 760px;
      }
    "))
  ),
  div(
    class = "contenedor",
    div(
      class = "sidebar",
      h1("Estadística I"),
      radioButtons(
        "apartado",
        "Apartado",
        choices = c(
          "Medidas",
          "Tabla simple inspección",
          "Tabla Sturges",
          "Gráfico de barras",
          "Diagrama lineal"
        ),
        selected = "Medidas"
      )
    ),
    div(
      class = "contenido",
      conditionalPanel(
        condition = "input.apartado == 'Medidas'",
        div(
          class = "tarjeta",
          h1(class = "titulo", "Medidas"),
          selectInput(
            "tipo_datos_medida",
            "Tipo de datos",
            choices = c("No clasificados", "Clasificados"),
            selected = "No clasificados"
          ),
          selectInput(
            "medida",
            "Medida",
            choices = c(
              "Media aritmética",
              "Media geométrica",
              "Media armónica",
              "Mediana",
              "Moda",
              "Cuartiles",
              "Deciles",
              "Percentiles"
            )
          ),
          uiOutput("entrada_k"),
          conditionalPanel(
            condition = "input.tipo_datos_medida == 'No clasificados'",
            tags$div(
              class = "nota",
              "Puede pegar datos en filas, columnas, separados por espacios, comas, punto y coma o desde Excel."
            ),
            textAreaInput(
              "entrada_no_clasificados",
              "Datos",
              value = "5 7 2 9 3 7 4",
              rows = 6
            ),
            actionButton(
              "pegar_no_clasificados",
              "Pegar desde Excel",
              onclick = "pegarEnTextArea('entrada_no_clasificados')",
              class = "btn-warning"
            )
          ),
          conditionalPanel(
            condition = "input.tipo_datos_medida == 'Clasificados'",
            tags$div(
              class = "nota",
              "Ingrese tres columnas: Li, Ls y fi. Puede pegar directamente desde Excel."
            ),
            textAreaInput(
              "entrada_clasificados",
              "Datos en tres columnas: Li  Ls  fi",
              value = "20\t30\t6\n30\t40\t12\n40\t50\t18\n50\t60\t10\n60\t70\t4",
              rows = 6
            ),
            actionButton(
              "pegar_clasificados",
              "Pegar desde Excel",
              onclick = "pegarEnTextArea('entrada_clasificados')",
              class = "btn-warning"
            )
          ),
          actionButton("calcular_medida", "Calcular", class = "btn-primary"),
          tags$hr(),
          uiOutput("salida_medida")
        )
      ),
      conditionalPanel(
        condition = "input.apartado == 'Tabla simple inspección'",
        div(
          class = "tarjeta",
          h1(class = "titulo", "Tabla simple inspección"),
          textInput("numero_tabla_simple", "Número de tabla", value = "Tabla N° 1"),
          textInput("titulo_tabla_simple", "Título", value = "Distribución por simple inspección"),
          radioButtons(
            "tipo_variable_simple",
            "Tipo de variable",
            choices = c("Cuantitativa", "Cualitativa"),
            selected = "Cuantitativa",
            inline = TRUE
          ),
          tags$div(
            class = "nota",
            "No se pide nombre de variable. Pegue los datos en filas o columnas; también puede copiar celdas desde Excel."
          ),
          textAreaInput(
            "entrada_simple",
            "Datos",
            value = "1\t0\t0\t2\t3\n1\t1\t2\t1\t4",
            rows = 6
          ),
          actionButton(
            "pegar_simple",
            "Pegar desde Excel",
            onclick = "pegarEnTextArea('entrada_simple')",
            class = "btn-warning"
          ),
          actionButton("generar_simple", "Generar tabla", class = "btn-primary"),
          tags$hr(),
          uiOutput("salida_simple")
        )
      ),
      conditionalPanel(
        condition = "input.apartado == 'Tabla Sturges'",
        div(
          class = "tarjeta",
          h1(class = "titulo", "Tabla por Sturges"),
          textInput("numero_tabla_sturges", "Número de tabla", value = "Tabla N° 2"),
          textInput("titulo_tabla_sturges", "Título", value = "Distribución por Sturges"),
          tags$div(
            class = "nota",
            "No se pide nombre de variable. Se respeta la cantidad de decimales de los datos y se muestra la corrección del alcance."
          ),
          textAreaInput(
            "entrada_sturges",
            "Datos numéricos",
            value = "12\t15\t20\t22\t25\n30\t18\t19\t21\t23\n24\t26\t28\t29\t31\n32\t17\t16\t27\t33",
            rows = 6
          ),
          actionButton(
            "pegar_sturges",
            "Pegar desde Excel",
            onclick = "pegarEnTextArea('entrada_sturges')",
            class = "btn-warning"
          ),
          actionButton("generar_sturges", "Generar tabla Sturges", class = "btn-primary"),
          tags$hr(),
          uiOutput("salida_sturges")
        )
      ),
      conditionalPanel(
        condition = "input.apartado == 'Gráfico de barras'",
        div(
          class = "tarjeta",
          h1(class = "titulo", "Gráfico de barras"),
          textInput("numero_grafico_barras", "Número de gráfico", value = "Gráfico N° 1"),
          textInput("titulo_grafico_barras", "Título", value = "Preferencia de bebidas gaseosas"),
          textInput("nombre_x_barras", "Nombre columna X", value = "Bebida gaseosa"),
          textInput("nombre_y_barras", "Nombre columna Y", value = "Frecuencia"),
          radioButtons(
            "direccion_barras",
            "Dirección",
            choices = c("Vertical", "Horizontal"),
            selected = "Vertical",
            inline = TRUE
          ),
          selectInput(
            "color_barras",
            "Color",
            choices = c(
              "Verde" = "#174b33",
              "Café" = "#6f4e37",
              "Azul" = "#3c6fa6",
              "Dorado" = "#c49a6c"
            )
          ),
          textAreaInput(
            "entrada_grafico_barras",
            "Datos en dos columnas",
            value = "Coca Cola\t10\nPepsi\t8\nSalvietti\t5\nCascada\t2",
            rows = 6
          ),
          actionButton(
            "pegar_grafico_barras",
            "Pegar desde Excel",
            onclick = "pegarEnTextArea('entrada_grafico_barras')",
            class = "btn-warning"
          ),
          actionButton("generar_barras", "Generar gráfico", class = "btn-primary"),
          tags$hr(),
          tableOutput("tabla_grafico_barras"),
          plotOutput("grafico_barras", height = "420px")
        )
      ),
      conditionalPanel(
        condition = "input.apartado == 'Diagrama lineal'",
        div(
          class = "tarjeta",
          h1(class = "titulo", "Diagrama lineal"),
          textInput("numero_grafico_lineal", "Número de gráfico", value = "Gráfico N° 2"),
          textInput("titulo_grafico_lineal", "Título", value = "Evolución de datos"),
          textInput("nombre_x_lineal", "Nombre columna X", value = "Día"),
          textInput("nombre_y_lineal", "Nombre columna Y", value = "Valor"),
          selectInput(
            "color_lineal",
            "Color",
            choices = c(
              "Verde" = "#174b33",
              "Café" = "#6f4e37",
              "Azul" = "#3c6fa6",
              "Dorado" = "#c49a6c"
            )
          ),
          textAreaInput(
            "entrada_grafico_lineal",
            "Datos en dos columnas",
            value = "Lunes\t4\nMartes\t7\nMiércoles\t5\nJueves\t9\nViernes\t6",
            rows = 6
          ),
          actionButton(
            "pegar_grafico_lineal",
            "Pegar desde Excel",
            onclick = "pegarEnTextArea('entrada_grafico_lineal')",
            class = "btn-warning"
          ),
          actionButton("generar_lineal", "Generar diagrama", class = "btn-primary"),
          tags$hr(),
          tableOutput("tabla_grafico_lineal"),
          plotOutput("grafico_lineal", height = "420px")
        )
      )
    )
  )
)

server = function(input, output, session) {
  output$entrada_k = renderUI({
    if (input$medida == "Cuartiles") {
      numericInput("k", "k", value = 1, min = 1, max = 3, step = 1)
    } else if (input$medida == "Deciles") {
      numericInput("k", "k", value = 1, min = 1, max = 9, step = 1)
    } else if (input$medida == "Percentiles") {
      numericInput("k", "k", value = 50, min = 1, max = 99, step = 1)
    }
  })

  resultado_medida = eventReactive(input$calcular_medida, {
    k = ifelse(is.null(input$k), 1, input$k)

    validacion_k = validar_k(input$medida, k)

    if (validacion_k != TRUE) {
      return(list(error = validacion_k))
    }

    if (input$tipo_datos_medida == "No clasificados") {
      detalle = parsear_numeros_detalle(input$entrada_no_clasificados)
      resultado = calcular_medida_no_clasificada(detalle$valores, input$medida, k)
      resultado$decimales = detalle$decimales
      return(resultado)
    }

    tabla = parsear_tabla_clasificada(input$entrada_clasificados)
    resultado = calcular_medida_clasificada(tabla, input$medida, k)
    resultado$decimales = 2

    resultado
  })

  output$salida_medida = renderUI({
    resultado = resultado_medida()

    if (!is.null(resultado$error)) {
      return(div(class = "error", resultado$error))
    }

    tagList(
      h3(class = "subtitulo", paste("Resultado:", formatear_numero(resultado$resultado, 4))),
      tableOutput("tabla_pasos_medida"),
      tableOutput("tabla_detalle_medida")
    )
  })

  output$tabla_pasos_medida = renderTable({
    resultado = resultado_medida()

    if (!is.null(resultado$error) || is.null(resultado$pasos)) {
      return(NULL)
    }

    formatear_tabla(resultado$pasos, "valor", 4)
  })

  output$tabla_detalle_medida = renderTable({
    resultado = resultado_medida()

    if (!is.null(resultado$error) || is.null(resultado$detalle)) {
      return(NULL)
    }

    formatear_tabla(resultado$detalle, names(resultado$detalle), 4)
  })

  resultado_simple = eventReactive(input$generar_simple, {
    crear_tabla_simple_inspeccion(input$entrada_simple, input$tipo_variable_simple)
  })

  output$salida_simple = renderUI({
    resultado = resultado_simple()

    if (!is.null(resultado$error)) {
      return(div(class = "error", resultado$error))
    }

    tagList(
      h3(class = "subtitulo", input$numero_tabla_simple),
      h4(input$titulo_tabla_simple),
      if (length(resultado$mensajes) > 0) div(class = "error", resultado$mensajes),
      h4("Tabla de frecuencias"),
      tableOutput("tabla_simple"),
      h4("Resumen"),
      tableOutput("resumen_simple")
    )
  })

  output$tabla_simple = renderTable({
    resultado = resultado_simple()
    dec = resultado$decimales

    if (input$tipo_variable_simple == "Cuantitativa") {
      tabla = resultado$tabla

      tabla$Ii = formatear_numero(tabla$Ii, dec)
      tabla$fi = formatear_numero(tabla$fi, 0)
      tabla$hi = formatear_numero(tabla$hi, 4)
      tabla$pi = formatear_numero(tabla$pi, 2)
      tabla$Fi = formatear_numero(tabla$Fi, 0)
      tabla$Hi = formatear_numero(tabla$Hi, 4)
      tabla$Pi = formatear_numero(tabla$Pi, 2)

      return(tabla)
    }

    formatear_tabla(resultado$tabla, c("fi", "hi", "pi", "Fi", "Hi", "Pi"), 2)
  })

  output$resumen_simple = renderTable({
    resultado = resultado_simple()
    resultado$resumen
  })

  resultado_sturges = eventReactive(input$generar_sturges, {
    crear_tabla_sturges(input$entrada_sturges)
  })

  output$salida_sturges = renderUI({
    resultado = resultado_sturges()

    if (!is.null(resultado$error)) {
      return(div(class = "error", resultado$error))
    }

    tagList(
      h3(class = "subtitulo", input$numero_tabla_sturges),
      h4(input$titulo_tabla_sturges),
      h4("Pasos del cálculo"),
      tableOutput("pasos_sturges"),
      h4("Tabla Sturges"),
      tableOutput("tabla_sturges"),
      h4("Medidas desde tabla agrupada"),
      tableOutput("medidas_sturges")
    )
  })

  output$pasos_sturges = renderTable({
    resultado = resultado_sturges()
    formatear_tabla(resultado$pasos, "valor", max(resultado$decimales, 2))
  })

  output$tabla_sturges = renderTable({
    resultado = resultado_sturges()

    tabla = resultado$tabla
    dec = resultado$decimales

    tabla$Li = formatear_numero(tabla$Li, dec)
    tabla$Ls = formatear_numero(tabla$Ls, dec)
    tabla$xi = formatear_numero(tabla$xi, dec + 1)
    tabla$fi = formatear_numero(tabla$fi, 0)
    tabla$hi = formatear_numero(tabla$hi, 4)
    tabla$pi = formatear_numero(tabla$pi, 2)
    tabla$Fi = formatear_numero(tabla$Fi, 0)
    tabla$Hi = formatear_numero(tabla$Hi, 4)
    tabla$Pi = formatear_numero(tabla$Pi, 2)

    tabla
  })

  output$medidas_sturges = renderTable({
    resultado = resultado_sturges()
    formatear_tabla(resultado$medidas, "valor", max(resultado$decimales + 1, 2))
  })

  datos_barras = eventReactive(input$generar_barras, {
    parsear_datos_grafico(input$entrada_grafico_barras)
  })

  output$tabla_grafico_barras = renderTable({
    datos = datos_barras()

    if (nrow(datos) == 0) {
      return(NULL)
    }

    names(datos) = c(input$nombre_x_barras, input$nombre_y_barras)

    datos
  })

  output$grafico_barras = renderPlot({
    datos = datos_barras()

    if (nrow(datos) == 0) {
      plot.new()
      text(0.5, 0.5, "Debe ingresar datos válidos.")
      return()
    }

    if (input$direccion_barras == "Vertical") {
      barplot(
        datos$y,
        names.arg = datos$x,
        col = input$color_barras,
        main = paste(input$numero_grafico_barras, "-", input$titulo_grafico_barras),
        xlab = input$nombre_x_barras,
        ylab = input$nombre_y_barras,
        ylim = c(0, max(datos$y) * 1.2)
      )
    } else {
      barplot(
        datos$y,
        names.arg = datos$x,
        col = input$color_barras,
        main = paste(input$numero_grafico_barras, "-", input$titulo_grafico_barras),
        xlab = input$nombre_y_barras,
        ylab = input$nombre_x_barras,
        xlim = c(0, max(datos$y) * 1.2),
        horiz = TRUE,
        las = 1
      )
    }
  })

  datos_lineal = eventReactive(input$generar_lineal, {
    parsear_datos_grafico(input$entrada_grafico_lineal)
  })

  output$tabla_grafico_lineal = renderTable({
    datos = datos_lineal()

    if (nrow(datos) == 0) {
      return(NULL)
    }

    names(datos) = c(input$nombre_x_lineal, input$nombre_y_lineal)

    datos
  })

  output$grafico_lineal = renderPlot({
    datos = datos_lineal()

    if (nrow(datos) == 0) {
      plot.new()
      text(0.5, 0.5, "Debe ingresar datos válidos.")
      return()
    }

    plot(
      seq_along(datos$y),
      datos$y,
      type = "o",
      col = input$color_lineal,
      lwd = 3,
      pch = 19,
      xaxt = "n",
      main = paste(input$numero_grafico_lineal, "-", input$titulo_grafico_lineal),
      xlab = input$nombre_x_lineal,
      ylab = input$nombre_y_lineal,
      ylim = c(0, max(datos$y) * 1.2)
    )

    axis(1, at = seq_along(datos$x), labels = datos$x)
    grid()
  })
}

shinyApp(ui, server)