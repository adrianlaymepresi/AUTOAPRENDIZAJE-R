# CTRL + ENTER
# Ejecutar con: source("app.R")
# O también: shiny::runApp(".")

library(shiny)

parsear_numeros = function(texto) {
  if (is.null(texto) || trimws(texto) == "") {
    return(numeric(0))
  }

  texto = gsub("\r", "\n", texto)
  texto = gsub("\t", ",", texto)
  texto = gsub(";", ",", texto)
  texto = gsub("\n", ",", texto)
  texto = gsub("\\s+", ",", texto)

  partes = trimws(unlist(strsplit(texto, ",")))
  partes = partes[partes != ""]

  numeros = suppressWarnings(as.numeric(gsub(",", ".", partes, fixed = TRUE)))
  numeros = numeros[!is.na(numeros)]

  return(numeros)
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

  return(partes)
}

parsear_tabla_clasificada = function(texto) {
  if (is.null(texto) || trimws(texto) == "") {
    return(data.frame(Li = numeric(0), Ls = numeric(0), frecuencia = numeric(0)))
  }

  lineas = unlist(strsplit(texto, "\n"))
  filas = list()

  for (linea in lineas) {
    valores = regmatches(linea, gregexpr("-?\\d+(\\.\\d+)?", linea, perl = TRUE))[[1]]
    valores = suppressWarnings(as.numeric(valores))

    if (length(valores) >= 3) {
      filas[[length(filas) + 1]] = c(valores[1], valores[2], valores[3])
    }
  }

  if (length(filas) == 0) {
    return(data.frame(Li = numeric(0), Ls = numeric(0), frecuencia = numeric(0)))
  }

  matriz = do.call(rbind, filas)

  tabla = data.frame(
    Li = matriz[, 1],
    Ls = matriz[, 2],
    frecuencia = matriz[, 3]
  )

  return(tabla)
}

parsear_datos_grafico = function(texto) {
  if (is.null(texto) || trimws(texto) == "") {
    return(data.frame(x = character(0), y = numeric(0)))
  }

  lineas = unlist(strsplit(texto, "\n"))
  filas = list()

  for (linea in lineas) {
    linea = trimws(linea)

    if (linea != "") {
      partes = unlist(strsplit(linea, "\\s+"))
      numero = suppressWarnings(as.numeric(partes[length(partes)]))

      if (!is.na(numero) && length(partes) >= 2) {
        nombre = paste(partes[-length(partes)], collapse = " ")
        filas[[length(filas) + 1]] = data.frame(x = nombre, y = numero)
      }
    }
  }

  if (length(filas) == 0) {
    return(data.frame(x = character(0), y = numeric(0)))
  }

  return(do.call(rbind, filas))
}

validar_no_clasificados = function(A) {
  if (length(A) == 0) {
    return("Debe ingresar al menos un dato numérico.")
  }

  return(TRUE)
}

validar_clasificados = function(tabla, permitir_cero = FALSE) {
  if (nrow(tabla) == 0) {
    return("Debe ingresar al menos un intervalo con su frecuencia.")
  }

  if (any(tabla$Ls <= tabla$Li)) {
    return("Cada límite superior debe ser mayor que su límite inferior.")
  }

  if (permitir_cero) {
    if (any(tabla$frecuencia < 0)) {
      return("Las frecuencias no pueden ser negativas.")
    }
  } else {
    if (any(tabla$frecuencia <= 0)) {
      return("Las frecuencias deben ser mayores que cero.")
    }
  }

  return(TRUE)
}

calcular_moda_vector = function(A) {
  tabla = table(A)
  frecuencia_mayor = max(tabla)

  if (frecuencia_mayor == 1) {
    return(list(valor = "No existe moda", tipo = "Sin moda", tabla = as.data.frame(tabla)))
  }

  moda = names(tabla[tabla == frecuencia_mayor])

  if (length(moda) == 1) {
    tipo = "Unimodal"
  } else if (length(moda) == 2) {
    tipo = "Bimodal"
  } else if (length(moda) == 3) {
    tipo = "Trimodal"
  } else {
    tipo = "Multimodal"
  }

  return(list(valor = paste(moda, collapse = ", "), tipo = tipo, tabla = as.data.frame(tabla)))
}

calcular_cuantil_manual = function(A, k, divisor) {
  A_ordenado = sort(A)
  n = length(A_ordenado)
  posicion = k * (n + 1) / divisor

  if (posicion < 1) {
    return(list(error = "La posición calculada queda antes del primer dato."))
  }

  if (posicion > n) {
    return(list(error = "La posición calculada queda después del último dato."))
  }

  if (posicion == floor(posicion)) {
    resultado = A_ordenado[posicion]
  } else {
    posicion_inferior = floor(posicion)
    posicion_superior = ceiling(posicion)
    parte_decimal = posicion - posicion_inferior
    dato_inferior = A_ordenado[posicion_inferior]
    dato_superior = A_ordenado[posicion_superior]
    resultado = dato_inferior + parte_decimal * (dato_superior - dato_inferior)
  }

  detalle = data.frame(
    posicion = seq_along(A_ordenado),
    xi = A_ordenado
  )

  pasos = data.frame(
    variable = c("k", "n", "Posición", "Resultado"),
    valor = c(k, n, round(posicion, 6), round(resultado, 6))
  )

  return(list(resultado = resultado, detalle = detalle, pasos = pasos))
}

calcular_medida_no_clasificada = function(A, medida, k) {
  validacion = validar_no_clasificados(A)

  if (validacion != TRUE) {
    return(list(error = validacion))
  }

  if (medida == "Media aritmética") {
    return(list(
      resultado = mean(A),
      detalle = data.frame(concepto = c("A", "n", "Suma", "Media"), valor = c(paste(A, collapse = ", "), length(A), sum(A), mean(A)))
    ))
  }

  if (medida == "Media geométrica") {
    if (any(A <= 0)) {
      return(list(error = "La media geométrica no acepta datos negativos ni cero."))
    }

    resultado = exp(mean(log(A)))

    return(list(
      resultado = resultado,
      detalle = data.frame(concepto = c("n", "Media geométrica"), valor = c(length(A), resultado))
    ))
  }

  if (medida == "Media armónica") {
    if (any(A == 0)) {
      return(list(error = "La media armónica no acepta datos iguales a cero."))
    }

    resultado = 1 / mean(1 / A)

    return(list(
      resultado = resultado,
      detalle = data.frame(concepto = c("n", "Media armónica"), valor = c(length(A), resultado))
    ))
  }

  if (medida == "Mediana") {
    A_ordenado = sort(A)

    return(list(
      resultado = median(A),
      detalle = data.frame(posicion = seq_along(A_ordenado), xi = A_ordenado)
    ))
  }

  if (medida == "Moda") {
    moda = calcular_moda_vector(A)

    return(list(
      resultado = moda$valor,
      detalle = moda$tabla,
      pasos = data.frame(variable = c("Tipo"), valor = c(moda$tipo))
    ))
  }

  if (medida == "Cuartiles") {
    return(calcular_cuantil_manual(A, k, 4))
  }

  if (medida == "Deciles") {
    return(calcular_cuantil_manual(A, k, 10))
  }

  if (medida == "Percentiles") {
    return(calcular_cuantil_manual(A, k, 100))
  }
}

crear_tabla_clasificada = function(tabla) {
  xi = (tabla$Li + tabla$Ls) / 2
  fi = tabla$frecuencia
  n = sum(fi)

  resultado = data.frame(
    Intervalo = paste0("[", tabla$Li, "; ", tabla$Ls, ")"),
    Li = tabla$Li,
    Ls = tabla$Ls,
    xi = xi,
    fi = fi,
    hi = round(fi / n, 4),
    pi = round((fi / n) * 100, 2),
    Fi = cumsum(fi),
    Hi = round(cumsum(fi) / n, 4),
    Pi = round((cumsum(fi) / n) * 100, 2)
  )

  return(resultado)
}

calcular_medida_clasificada = function(tabla, medida, k) {
  validacion = validar_clasificados(tabla, permitir_cero = TRUE)

  if (validacion != TRUE) {
    return(list(error = validacion))
  }

  tabla_calculada = crear_tabla_clasificada(tabla)
  xi = tabla_calculada$xi
  fi = tabla_calculada$fi
  n = sum(fi)

  if (n <= 0) {
    return(list(error = "La suma de frecuencias debe ser mayor que cero."))
  }

  if (medida == "Media aritmética") {
    resultado = sum(xi * fi) / n
    return(list(resultado = resultado, detalle = tabla_calculada))
  }

  if (medida == "Media geométrica") {
    if (any(xi <= 0 & fi > 0)) {
      return(list(error = "La media geométrica clasificada requiere marcas de clase mayores a cero."))
    }

    resultado = exp(sum(fi * log(xi)) / n)
    return(list(resultado = resultado, detalle = tabla_calculada))
  }

  if (medida == "Media armónica") {
    if (any(xi == 0 & fi > 0)) {
      return(list(error = "La media armónica clasificada no acepta marcas de clase iguales a cero."))
    }

    resultado = n / sum(fi / xi)
    return(list(resultado = resultado, detalle = tabla_calculada))
  }

  if (medida == "Mediana") {
    posicion = n / 2
    indice = which(tabla_calculada$Fi >= posicion)[1]

    if (is.na(indice) || fi[indice] == 0) {
      return(list(error = "No se puede calcular la mediana con la clase encontrada."))
    }

    Li = tabla$Li[indice]
    t = tabla$Ls[indice] - tabla$Li[indice]
    Fi_anterior = ifelse(indice == 1, 0, tabla_calculada$Fi[indice - 1])
    resultado = Li + ((posicion - Fi_anterior) * t / fi[indice])

    pasos = data.frame(
      variable = c("n", "n/2", "Li", "Fi anterior", "t", "fi", "Mediana"),
      valor = c(n, posicion, Li, Fi_anterior, t, fi[indice], resultado)
    )

    return(list(resultado = resultado, detalle = tabla_calculada, pasos = pasos))
  }

  if (medida == "Moda") {
    indice = which.max(fi)

    Li = tabla$Li[indice]
    t = tabla$Ls[indice] - tabla$Li[indice]
    frecuencia_modal = fi[indice]
    frecuencia_anterior = ifelse(indice == 1, 0, fi[indice - 1])
    frecuencia_posterior = ifelse(indice == length(fi), 0, fi[indice + 1])

    d1 = frecuencia_modal - frecuencia_anterior
    d2 = frecuencia_modal - frecuencia_posterior

    if ((d1 + d2) == 0) {
      return(list(error = "No se puede calcular la moda porque d1 + d2 es cero."))
    }

    resultado = Li + ((t * d1) / (d1 + d2))

    pasos = data.frame(
      variable = c("Li", "t", "fi modal", "fi anterior", "fi posterior", "d1", "d2", "Moda"),
      valor = c(Li, t, frecuencia_modal, frecuencia_anterior, frecuencia_posterior, d1, d2, resultado)
    )

    return(list(resultado = resultado, detalle = tabla_calculada, pasos = pasos))
  }

  if (medida %in% c("Cuartiles", "Deciles", "Percentiles")) {
    divisor = ifelse(medida == "Cuartiles", 4, ifelse(medida == "Deciles", 10, 100))
    posicion = k * n / divisor
    indice = which(tabla_calculada$Fi >= posicion)[1]

    if (is.na(indice) || fi[indice] == 0) {
      return(list(error = "No se puede calcular el valor solicitado con la clase encontrada."))
    }

    Li = tabla$Li[indice]
    t = tabla$Ls[indice] - tabla$Li[indice]
    Fi_anterior = ifelse(indice == 1, 0, tabla_calculada$Fi[indice - 1])
    resultado = Li + ((posicion - Fi_anterior) * t / fi[indice])

    pasos = data.frame(
      variable = c("k", "n", "Posición", "Li", "Fi anterior", "t", "fi", "Resultado"),
      valor = c(k, n, posicion, Li, Fi_anterior, t, fi[indice], resultado)
    )

    return(list(resultado = resultado, detalle = tabla_calculada, pasos = pasos))
  }
}

validar_k = function(medida, k) {
  if (medida == "Cuartiles" && (k < 1 || k > 3)) {
    return("En cuartiles, k solo puede valer 1, 2 o 3.")
  }

  if (medida == "Deciles" && (k < 1 || k > 9)) {
    return("En deciles, k solo puede valer desde 1 hasta 9.")
  }

  if (medida == "Percentiles" && (k < 1 || k > 99)) {
    return("En percentiles, k solo puede valer desde 1 hasta 99.")
  }

  return(TRUE)
}

crear_tabla_simple_inspeccion = function(texto, tipo_variable) {
  if (tipo_variable == "Cuantitativa") {
    datos = parsear_numeros(texto)

    if (length(datos) == 0) {
      return(list(error = "Debe ingresar datos numéricos."))
    }

    valores = sort(unique(datos))
    fi = as.numeric(table(factor(datos, levels = valores)))
    n = sum(fi)

    tabla = data.frame(
      Ii = valores,
      fi = fi,
      hi = round(fi / n, 4),
      pi = round((fi / n) * 100, 2),
      Fi = cumsum(fi),
      Hi = round(cumsum(fi) / n, 4),
      Pi = round((cumsum(fi) / n) * 100, 2)
    )

    moda = calcular_moda_vector(datos)

    resumen = data.frame(
      medida = c("n", "Media aritmética", "Mediana", "Moda"),
      valor = c(n, round(mean(datos), 6), round(median(datos), 6), moda$valor)
    )

    mensajes = character(0)

    if (length(valores) >= 10) {
      mensajes = c(mensajes, "Advertencia: la simple inspección se recomienda cuando hay menos de 10 valores diferentes.")
    }

    return(list(tabla = tabla, resumen = resumen, mensajes = mensajes))
  }

  datos = parsear_textos(texto)

  if (length(datos) == 0) {
    return(list(error = "Debe ingresar categorías."))
  }

  categorias = unique(datos)
  fi = as.numeric(table(factor(datos, levels = categorias)))
  n = sum(fi)

  tabla = data.frame(
    Categoria = categorias,
    fi = fi,
    hi = round(fi / n, 4),
    pi = round((fi / n) * 100, 2),
    Fi = cumsum(fi),
    Hi = round(cumsum(fi) / n, 4),
    Pi = round((cumsum(fi) / n) * 100, 2)
  )

  frecuencia_mayor = max(fi)
  moda = paste(categorias[fi == frecuencia_mayor], collapse = ", ")

  resumen = data.frame(
    medida = c("n", "Moda"),
    valor = c(n, moda)
  )

  return(list(tabla = tabla, resumen = resumen, mensajes = character(0)))
}

detectar_precision = function(datos) {
  textos = as.character(datos)
  decimales = ifelse(grepl("\\.", textos), nchar(sub(".*\\.", "", textos)), 0)
  max_decimales = max(decimales)

  if (max_decimales == 0) return(1)

  return(10 ^ (-max_decimales))
}

redondear_arriba_precision = function(valor, precision) {
  return(ceiling(valor / precision) * precision)
}

crear_tabla_sturges = function(texto) {
  datos = parsear_numeros(texto)

  if (length(datos) == 0) {
    return(list(error = "Debe ingresar datos numéricos."))
  }

  n = length(datos)

  if (n <= 14) {
    return(list(error = "Para Sturges se recomienda usar más de 14 datos."))
  }

  d = min(datos)
  D = max(datos)
  c = detectar_precision(datos)
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

  medidas = data.frame(
    medida = c("Media aritmética", "Mediana", "Moda"),
    valor = c(
      round(calcular_medida_clasificada(tabla_base, "Media aritmética", 1)$resultado, 6),
      round(calcular_medida_clasificada(tabla_base, "Mediana", 1)$resultado, 6),
      round(calcular_medida_clasificada(tabla_base, "Moda", 1)$resultado, 6)
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
      round(k_teorico, 6),
      k,
      round(t_inicial, 6),
      t,
      t * k,
      sobrante,
      d_corregido,
      D_corregido
    )
  )

  return(list(tabla = tabla, pasos = pasos, medidas = medidas))
}

ui = fluidPage(
  tags$head(
    tags$style(HTML("
      :root {
        --verde-starbucks: #006241;
        --verde-oscuro: #003d29;
        --verde-suave: #d4e9e2;
        --crema: #f7f1e5;
        --cafe: #6f4e37;
        --cafe-claro: #c49a6c;
        --turquesa: #00a99d;
        --blanco: #ffffff;
        --rojo: #b91c1c;
      }

      body {
        background: var(--crema);
        font-family: Arial, sans-serif;
        color: #1f2937;
      }

      .contenedor-app {
        display: flex;
        min-height: 100vh;
      }

      .sidebar {
        width: 285px;
        background: linear-gradient(180deg, var(--verde-oscuro), var(--verde-starbucks));
        color: var(--blanco);
        padding: 24px;
        flex-shrink: 0;
      }

      .sidebar h2 {
        font-size: 24px;
        margin-top: 0;
        margin-bottom: 22px;
        color: var(--crema);
        font-weight: 900;
      }

      .sidebar .control-label {
        color: var(--crema);
        font-weight: 800;
      }

      .sidebar .radio label {
        color: var(--blanco);
        margin-bottom: 9px;
        font-weight: 600;
      }

      .contenido {
        flex: 1;
        padding: 24px;
        overflow-x: hidden;
      }

      .tarjeta {
        background: var(--blanco);
        border-radius: 18px;
        padding: 24px;
        box-shadow: 0 10px 28px rgba(0,0,0,.10);
        margin-bottom: 20px;
        border: 1px solid rgba(0, 98, 65, .12);
      }

      .titulo {
        color: var(--verde-oscuro);
        font-weight: 900;
        margin-top: 0;
      }

      .subtitulo {
        color: var(--verde-starbucks);
        font-weight: 850;
      }

      .btn {
        border-radius: 11px;
        font-weight: 700;
      }

      .btn-primary {
        background: var(--verde-starbucks);
        border-color: var(--verde-starbucks);
      }

      .btn-primary:hover {
        background: var(--verde-oscuro);
        border-color: var(--verde-oscuro);
      }

      .btn-warning {
        background: var(--cafe-claro);
        border-color: var(--cafe-claro);
        color: white;
      }

      .resultado {
        font-size: 25px;
        font-weight: 900;
        color: var(--verde-starbucks);
        background: var(--verde-suave);
        padding: 14px 18px;
        border-radius: 14px;
        display: inline-block;
      }

      .error {
        color: var(--rojo);
        font-weight: 850;
        font-size: 18px;
        background: #fee2e2;
        border: 1px solid #fecaca;
        padding: 14px;
        border-radius: 12px;
      }

      .tabla-input-contenedor {
        width: 100%;
        overflow-x: auto;
        border-radius: 16px;
        border: 2px solid var(--verde-starbucks);
        background: var(--blanco);
        margin-top: 14px;
        margin-bottom: 16px;
      }

      .tabla-input {
        width: 100%;
        border-collapse: collapse;
        min-width: 560px;
      }

      .tabla-input thead th {
        background: var(--verde-starbucks);
        color: var(--crema);
        text-align: center;
        padding: 14px 10px;
        font-size: 18px;
        border: 1px solid var(--verde-oscuro);
      }

      .tabla-input tbody td {
        background: var(--verde-suave);
        padding: 6px;
        border: 1px solid rgba(0, 98, 65, .35);
      }

      .tabla-input .form-group {
        margin-bottom: 0;
      }

      .tabla-input label {
        display: none;
      }

      .tabla-input input {
        text-align: center;
        border: none;
        box-shadow: none;
        background: #fffaf0;
        font-weight: 700;
        color: var(--verde-oscuro);
        height: 38px;
      }

      @media (max-width: 900px) {
        .contenedor-app {
          flex-direction: column;
        }

        .sidebar {
          width: 100%;
        }

        .contenido {
          padding: 14px;
        }
      }
    ")),
    tags$script(HTML("
      async function pegarNoClasificados() {
        const texto = await navigator.clipboard.readText();
        const limpio = texto.replace(/\\t/g, ',').replace(/\\n/g, ',').replace(/\\r/g, ',').replace(/;/g, ',').replace(/\\s+/g, ',').replace(/,+/g, ',').replace(/^,|,$/g, '');
        Shiny.setInputValue('texto_pegado_no_clasificados', limpio, {priority: 'event'});
      }

      async function pegarTablaClasificada() {
        const texto = await navigator.clipboard.readText();
        Shiny.setInputValue('texto_pegado_clasificados', texto, {priority: 'event'});
      }

      async function pegarSimple() {
        const texto = await navigator.clipboard.readText();
        const limpio = texto.replace(/\\t/g, ',').replace(/\\n/g, ',').replace(/\\r/g, ',').replace(/;/g, ',').replace(/,+/g, ',').replace(/^,|,$/g, '');
        Shiny.setInputValue('texto_pegado_simple', limpio, {priority: 'event'});
      }

      async function pegarSturges() {
        const texto = await navigator.clipboard.readText();
        const limpio = texto.replace(/\\t/g, ',').replace(/\\n/g, ',').replace(/\\r/g, ',').replace(/;/g, ',').replace(/\\s+/g, ',').replace(/,+/g, ',').replace(/^,|,$/g, '');
        Shiny.setInputValue('texto_pegado_sturges', limpio, {priority: 'event'});
      }

      async function pegarGrafico() {
        const texto = await navigator.clipboard.readText();
        Shiny.setInputValue('texto_pegado_grafico', texto, {priority: 'event'});
      }
    "))
  ),

  div(
    class = "contenedor-app",

    div(
      class = "sidebar",
      h2("Estadística I"),
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
          h1(class = "titulo", "Medidas estadísticas"),
          radioButtons(
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
            ),
            selected = "Media aritmética",
            inline = TRUE
          ),
          radioButtons(
            "tipo_datos",
            "Tipo de datos",
            choices = c("Datos no clasificados" = "no_clasificados", "Datos clasificados" = "clasificados"),
            selected = "no_clasificados",
            inline = TRUE
          )
        ),

        conditionalPanel(
          condition = "input.medida == 'Cuartiles' || input.medida == 'Deciles' || input.medida == 'Percentiles'",
          div(class = "tarjeta", uiOutput("entrada_k"))
        ),

        conditionalPanel(
          condition = "input.tipo_datos == 'no_clasificados'",
          div(
            class = "tarjeta",
            h3(class = "subtitulo", "Datos no clasificados"),
            textAreaInput("entrada_no_clasificados", "Conjunto A", value = "5, 7, 2, 9, 3, 7, 4", rows = 4),
            actionButton("pegar_no_clasificados", "Pegar desde Excel", onclick = "pegarNoClasificados()", class = "btn-warning")
          )
        ),

        conditionalPanel(
          condition = "input.tipo_datos == 'clasificados'",
          div(
            class = "tarjeta",
            h3(class = "subtitulo", "Datos clasificados"),
            uiOutput("tabla_clasificada"),
            actionButton("agregar_fila", "Agregar fila", class = "btn-primary"),
            actionButton("quitar_fila", "Quitar última fila"),
            actionButton("pegar_clasificados", "Pegar tabla desde Excel", onclick = "pegarTablaClasificada()", class = "btn-warning")
          )
        ),

        div(
          class = "tarjeta",
          actionButton("calcular_medida", "Calcular medida", class = "btn-primary"),
          tags$hr(),
          uiOutput("salida_medida")
        )
      ),

      conditionalPanel(
        condition = "input.apartado == 'Tabla simple inspección'",
        div(
          class = "tarjeta",
          h1(class = "titulo", "Tabla por simple inspección"),
          textInput("numero_tabla_simple", "Número de tabla", value = "Tabla N° 1"),
          textInput("titulo_tabla_simple", "Título", value = "Distribución por simple inspección"),
          textInput("nombre_variable_simple", "Nombre de variable", value = "Variable"),
          radioButtons("tipo_variable_simple", "Tipo de variable", choices = c("Cuantitativa", "Cualitativa"), selected = "Cuantitativa", inline = TRUE),
          textAreaInput("entrada_simple", "Datos", value = "1,0,0,2,3,1,1,2,1,4", rows = 4),
          actionButton("pegar_simple", "Pegar desde Excel", onclick = "pegarSimple()", class = "btn-warning"),
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
          textInput("nombre_variable_sturges", "Nombre de variable", value = "Variable"),
          textAreaInput("entrada_sturges", "Datos numéricos", value = "12,15,20,22,25,30,18,19,21,23,24,26,28,29,31,32,17,16,27,33", rows = 5),
          actionButton("pegar_sturges", "Pegar desde Excel", onclick = "pegarSturges()", class = "btn-warning"),
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
          textInput("titulo_grafico_barras", "Título", value = "Preferencia de bebidas"),
          textInput("nombre_x_barras", "Nombre columna X", value = "Categoría"),
          textInput("nombre_y_barras", "Nombre columna Y", value = "Frecuencia"),
          radioButtons("direccion_barras", "Dirección", choices = c("Vertical", "Horizontal"), selected = "Vertical", inline = TRUE),
          selectInput("color_barras", "Color", choices = c("Verde" = "#006241", "Café" = "#6f4e37", "Turquesa" = "#00a99d", "Café claro" = "#c49a6c")),
          textAreaInput("entrada_grafico_barras", "Datos en dos columnas", value = "Coca Cola 10\nPepsi 8\nSalvietti 5\nCascada 2", rows = 5),
          actionButton("pegar_grafico_barras", "Pegar desde Excel", onclick = "pegarGrafico()", class = "btn-warning"),
          actionButton("generar_barras", "Generar gráfico", class = "btn-primary"),
          tags$hr(),
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
          textInput("nombre_x_lineal", "Nombre columna X", value = "X"),
          textInput("nombre_y_lineal", "Nombre columna Y", value = "Y"),
          selectInput("color_lineal", "Color", choices = c("Verde" = "#006241", "Café" = "#6f4e37", "Turquesa" = "#00a99d", "Café claro" = "#c49a6c")),
          textAreaInput("entrada_grafico_lineal", "Datos en dos columnas", value = "Lunes 4\nMartes 7\nMiércoles 5\nJueves 9\nViernes 6", rows = 5),
          actionButton("pegar_grafico_lineal", "Pegar desde Excel", onclick = "pegarGrafico()", class = "btn-warning"),
          actionButton("generar_lineal", "Generar diagrama", class = "btn-primary"),
          tags$hr(),
          plotOutput("grafico_lineal", height = "420px")
        )
      )
    )
  )
)

server = function(input, output, session) {
  tabla_base = data.frame(
    Li = c(0, 4, 8, 12, 16),
    Ls = c(4, 8, 12, 16, 20),
    frecuencia = c(3, 7, 12, 10, 8)
  )

  datos_tabla = reactiveVal(tabla_base)

  output$entrada_k = renderUI({
    if (input$medida == "Cuartiles") {
      numericInput("k", "k para Qk", value = 1, min = 1, max = 3, step = 1)
    } else if (input$medida == "Deciles") {
      numericInput("k", "k para Dk", value = 3, min = 1, max = 9, step = 1)
    } else {
      numericInput("k", "k para Pk", value = 73, min = 1, max = 99, step = 1)
    }
  })

  obtener_tabla_actual = function() {
    tabla = datos_tabla()

    Li = numeric(nrow(tabla))
    Ls = numeric(nrow(tabla))
    frecuencia = numeric(nrow(tabla))

    for (i in seq_len(nrow(tabla))) {
      Li[i] = input[[paste0("Li_", i)]]
      Ls[i] = input[[paste0("Ls_", i)]]
      frecuencia[i] = input[[paste0("frecuencia_", i)]]
    }

    data.frame(Li = Li, Ls = Ls, frecuencia = frecuencia)
  }

  output$tabla_clasificada = renderUI({
    tabla = datos_tabla()

    filas = lapply(seq_len(nrow(tabla)), function(i) {
      tags$tr(
        tags$td(numericInput(paste0("Li_", i), NULL, value = tabla$Li[i])),
        tags$td(numericInput(paste0("Ls_", i), NULL, value = tabla$Ls[i])),
        tags$td(numericInput(paste0("frecuencia_", i), NULL, value = tabla$frecuencia[i], min = 0))
      )
    })

    div(
      class = "tabla-input-contenedor",
      tags$table(
        class = "tabla-input",
        tags$thead(tags$tr(tags$th("Li"), tags$th("Ls"), tags$th("fi"))),
        tags$tbody(filas)
      )
    )
  })

  observeEvent(input$agregar_fila, {
    tabla = obtener_tabla_actual()
    amplitud = tabla$Ls[nrow(tabla)] - tabla$Li[nrow(tabla)]

    nueva_fila = data.frame(
      Li = tabla$Ls[nrow(tabla)],
      Ls = tabla$Ls[nrow(tabla)] + amplitud,
      frecuencia = 1
    )

    datos_tabla(rbind(tabla, nueva_fila))
  })

  observeEvent(input$quitar_fila, {
    tabla = obtener_tabla_actual()

    if (nrow(tabla) > 1) {
      datos_tabla(tabla[-nrow(tabla), ])
    }
  })

  observeEvent(input$texto_pegado_no_clasificados, {
    updateTextAreaInput(session, "entrada_no_clasificados", value = input$texto_pegado_no_clasificados)
  })

  observeEvent(input$texto_pegado_clasificados, {
    tabla = parsear_tabla_clasificada(input$texto_pegado_clasificados)

    if (nrow(tabla) > 0) {
      datos_tabla(tabla)
    }
  })

  observeEvent(input$texto_pegado_simple, {
    updateTextAreaInput(session, "entrada_simple", value = input$texto_pegado_simple)
  })

  observeEvent(input$texto_pegado_sturges, {
    updateTextAreaInput(session, "entrada_sturges", value = input$texto_pegado_sturges)
  })

  observeEvent(input$texto_pegado_grafico, {
    if (input$apartado == "Gráfico de barras") {
      updateTextAreaInput(session, "entrada_grafico_barras", value = input$texto_pegado_grafico)
    }

    if (input$apartado == "Diagrama lineal") {
      updateTextAreaInput(session, "entrada_grafico_lineal", value = input$texto_pegado_grafico)
    }
  })

  resultado_medida = eventReactive(input$calcular_medida, {
    medida = input$medida

    k = ifelse(is.null(input$k), 1, input$k)

    if (medida %in% c("Cuartiles", "Deciles", "Percentiles")) {
      validacion_k = validar_k(medida, k)

      if (validacion_k != TRUE) {
        return(list(error = validacion_k))
      }
    }

    if (input$tipo_datos == "no_clasificados") {
      A = parsear_numeros(input$entrada_no_clasificados)
      return(calcular_medida_no_clasificada(A, medida, k))
    }

    tabla = obtener_tabla_actual()
    return(calcular_medida_clasificada(tabla, medida, k))
  })

  output$salida_medida = renderUI({
    resultado = resultado_medida()

    if (!is.null(resultado$error)) {
      return(div(class = "error", resultado$error))
    }

    etiqueta = input$medida

    if (input$medida == "Cuartiles") etiqueta = paste0("Q", input$k)
    if (input$medida == "Deciles") etiqueta = paste0("D", input$k)
    if (input$medida == "Percentiles") etiqueta = paste0("P", input$k)

    tagList(
      h3(class = "subtitulo", "Resultado"),
      div(class = "resultado", paste(etiqueta, "=", resultado$resultado)),
      tags$hr(),
      if (!is.null(resultado$pasos)) tagList(h4("Pasos"), tableOutput("tabla_pasos_medida")),
      h4("Detalle"),
      tableOutput("tabla_detalle_medida")
    )
  })

  output$tabla_detalle_medida = renderTable({
    resultado = resultado_medida()

    if (!is.null(resultado$error)) return(NULL)

    resultado$detalle
  })

  output$tabla_pasos_medida = renderTable({
    resultado = resultado_medida()

    if (!is.null(resultado$error) || is.null(resultado$pasos)) return(NULL)

    resultado$pasos
  })

  resultado_simple = eventReactive(input$generar_simple, {
    crear_tabla_simple_inspeccion(input$entrada_simple, input$tipo_variable_simple)
  })

  output$salida_simple = renderUI({
    resultado = resultado_simple()

    if (!is.null(resultado$error)) {
      return(div(class = "error", resultado$error))
    }

    mensajes = resultado$mensajes

    tagList(
      h3(class = "subtitulo", input$numero_tabla_simple),
      h4(input$titulo_tabla_simple),
      if (length(mensajes) > 0) div(class = "error", mensajes),
      h4("Tabla de frecuencias"),
      tableOutput("tabla_simple"),
      h4("Resumen"),
      tableOutput("resumen_simple")
    )
  })

  output$tabla_simple = renderTable({
    resultado_simple()$tabla
  })

  output$resumen_simple = renderTable({
    resultado_simple()$resumen
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
    resultado_sturges()$pasos
  })

  output$tabla_sturges = renderTable({
    resultado_sturges()$tabla
  })

  output$medidas_sturges = renderTable({
    resultado_sturges()$medidas
  })

  datos_barras = eventReactive(input$generar_barras, {
    parsear_datos_grafico(input$entrada_grafico_barras)
  })

  output$grafico_barras = renderPlot({
    datos = datos_barras()

    if (nrow(datos) == 0) {
      plot.new()
      text(0.5, 0.5, "Debe ingresar datos válidos.")
      return()
    }

    color = input$color_barras

    if (input$direccion_barras == "Vertical") {
      barplot(
        datos$y,
        names.arg = datos$x,
        col = color,
        main = paste(input$numero_grafico_barras, "-", input$titulo_grafico_barras),
        xlab = input$nombre_x_barras,
        ylab = input$nombre_y_barras,
        ylim = c(0, max(datos$y) * 1.2)
      )
    } else {
      barplot(
        datos$y,
        names.arg = datos$x,
        col = color,
        main = paste(input$numero_grafico_barras, "-", input$titulo_grafico_barras),
        xlab = input$nombre_y_barras,
        ylab = input$nombre_x_barras,
        horiz = TRUE,
        xlim = c(0, max(datos$y) * 1.2),
        las = 1
      )
    }
  })

  datos_lineal = eventReactive(input$generar_lineal, {
    parsear_datos_grafico(input$entrada_grafico_lineal)
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
      pch = 19,
      lwd = 3,
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

shinyApp(ui = ui, server = server)