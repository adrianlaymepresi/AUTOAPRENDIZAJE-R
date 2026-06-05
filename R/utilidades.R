normalizar_numero = function(valor) {
  valor = trimws(as.character(valor))
  valor = gsub("\\.", "", valor)
  valor = gsub(",", ".", valor, fixed = TRUE)
  suppressWarnings(as.numeric(valor))
}

normalizar_numero_simple = function(valor) {
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
  decimales_sin_ceros = sub("0+$", "", decimales)

  nchar(decimales_sin_ceros)
}

formatear_numero = function(x, decimales = 2, forzar_decimales = FALSE) {
  if (is.character(x)) {
    return(x)
  }

  ifelse(
    is.na(x),
    "",
    {
      texto = format(
        round(x, decimales),
        nsmall = decimales,
        scientific = FALSE,
        trim = TRUE
      )

      if (forzar_decimales) {
        texto
      } else {
        quitar_ceros(texto)
      }
    }
  )
}

formatear_porcentaje = function(x, decimales = 2) {
  ifelse(
    is.na(x),
    "",
    paste0(formatear_numero(x, decimales), "%")
  )
}

formatear_miles = function(x) {
  format(
    round(as.numeric(x), 0),
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

parsear_numeros_detalle = function(texto) {
  if (is.null(texto) || trimws(texto) == "") {
    return(list(valores = numeric(0), decimales = 0, invalidos = character(0)))
  }

  texto = gsub("\r", "\n", texto)
  texto = gsub("\n", ";", texto)
  texto = gsub("\t", ";", texto)
  texto = gsub(",", ".", texto, fixed = TRUE)

  tokens = trimws(unlist(strsplit(texto, ";", fixed = TRUE)))
  tokens = tokens[tokens != ""]

  valores = normalizar_numero_simple(tokens)
  validos = !is.na(valores)

  valores_validos = valores[validos]
  tokens_validos = tokens[validos]
  tokens_invalidos = tokens[!validos]

  decimales = ifelse(
    length(tokens_validos) == 0,
    0,
    max(sapply(tokens_validos, contar_decimales_token))
  )

  list(
    valores = valores_validos,
    decimales = decimales,
    invalidos = tokens_invalidos
  )
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

  list(
    valor = paste(modas, collapse = ", "),
    tipo = tipo
  )
}