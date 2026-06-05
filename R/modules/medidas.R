ejemplos_medidas_no_clasificados = list(
  "Todos" = list(
    datos = "5;  7;  2;  9;  3;  7;  4;",
    nota = "Ejemplo general para calcular todas las medidas de posición con datos no clasificados."
  ),
  "Media aritmética" = list(
    datos = "5;  7;  2;  9;  3;  7;  4;",
    nota = "Ejemplo del PDF: llamadas diarias realizadas por una persona durante siete días."
  ),
  "Media geométrica" = list(
    datos = "1.08;  1.10;  1.15;  1.20;",
    nota = "Ejemplo del PDF adaptado a factores de crecimiento: 8%, 10%, 15% y 20% se ingresan como 1.08, 1.10, 1.15 y 1.20."
  ),
  "Media armónica" = list(
    datos = "2.00;  1.60;  1.30;  1.00;",
    nota = "Ejemplo del PDF: velocidades en m/s de un atleta en los lados de una pista rectangular."
  ),
  "Mediana" = list(
    datos = "8;  10;  0;  9;  2;  12;  12;",
    nota = "Ejemplo del PDF: veces que 7 estudiantes llegaron tarde durante un semestre."
  ),
  "Moda" = list(
    datos = "1;  2;  2;  4;  1;  2;  4;  2;",
    nota = "Ejemplo del PDF: número de veces que estudiantes bachilleres postularon a una universidad."
  ),
  "Cuartiles" = list(
    datos = "8;  7;  15;  4;  10;  9;  1;",
    nota = "Ejemplo del PDF: llamadas telefónicas recepcionadas por la FELC-V en medias jornadas de trabajo.",
    k = 1
  ),
  "Deciles" = list(
    datos = "8;  7;  15;  4;  10;  9;  1;",
    nota = "Ejemplo para deciles con datos no clasificados usando interpolación si la posición no es entera.",
    k = 3
  ),
  "Percentiles" = list(
    datos = "8;  7;  15;  4;  10;  9;  1;",
    nota = "Ejemplo para percentiles con datos no clasificados usando interpolación si la posición no es entera.",
    k = 75
  )
)

ejemplos_medidas_clasificados = list(
  "Todos" = list(
    tabla = data.frame(
      Li = c(21, 29, 37, 45, 53),
      Ls = c(29, 37, 45, 53, 61),
      fi = c(4, 8, 10, 6, 2)
    ),
    nota = "Ejemplo general para calcular todas las medidas de posición con datos clasificados."
  ),
  "Media aritmética" = list(
    tabla = data.frame(
      Li = c(0, 4, 8, 12, 16),
      Ls = c(4, 8, 12, 16, 20),
      fi = c(3, 7, 12, 10, 8)
    ),
    nota = "Ejemplo del PDF: calificaciones de 40 estudiantes en Pensamiento Matemático."
  ),
  "Media geométrica" = list(
    tabla = data.frame(
      Li = c(5, 9, 13, 17, 21),
      Ls = c(9, 13, 17, 21, 25),
      fi = c(1, 3, 8, 18, 10)
    ),
    nota = "Ejemplo del PDF: distribución porcentual de descuentos por faltas laborales de 40 trabajadores."
  ),
  "Media armónica" = list(
    tabla = data.frame(
      Li = c(54, 57, 60, 63, 66, 69),
      Ls = c(57, 60, 63, 66, 69, 72),
      fi = c(2, 3, 5, 6, 10, 14)
    ),
    nota = "Ejemplo del PDF: velocidades en km/h de 40 automóviles que pasan por un puente."
  ),
  "Mediana" = list(
    tabla = data.frame(
      Li = c(18.3, 23.6, 28.9, 34.2, 39.5),
      Ls = c(23.6, 28.9, 34.2, 39.5, 44.8),
      fi = c(8, 10, 9, 6, 3)
    ),
    nota = "Ejemplo del PDF: ingresos promedios diarios de 36 canillitas."
  ),
  "Moda" = list(
    tabla = data.frame(
      Li = c(5, 12, 19, 26, 33),
      Ls = c(12, 19, 26, 33, 40),
      fi = c(2, 7, 10, 5, 1)
    ),
    nota = "Ejemplo del PDF: tiempos de vida en días de 25 juegos de luces navideñas."
  ),
  "Cuartiles" = list(
    tabla = data.frame(
      Li = c(21, 29, 37, 45, 53),
      Ls = c(29, 37, 45, 53, 61),
      fi = c(4, 8, 10, 6, 2)
    ),
    nota = "Ejemplo del PDF: coeficientes de liderazgo observados en 30 presidentes de juntas vecinales.",
    k = 1
  ),
  "Deciles" = list(
    tabla = data.frame(
      Li = c(21, 29, 37, 45, 53),
      Ls = c(29, 37, 45, 53, 61),
      fi = c(4, 8, 10, 6, 2)
    ),
    nota = "Ejemplo para deciles con datos clasificados usando la fórmula del docente.",
    k = 3
  ),
  "Percentiles" = list(
    tabla = data.frame(
      Li = c(21, 29, 37, 45, 53),
      Ls = c(29, 37, 45, 53, 61),
      fi = c(4, 8, 10, 6, 2)
    ),
    nota = "Ejemplo para percentiles con datos clasificados usando la fórmula del docente.",
    k = 75
  )
)

normalizar_numero_medida = function(valor) {
  valor = trimws(as.character(valor))

  valor = sapply(valor, function(x) {
    x = trimws(x)

    if (x == "") {
      return(NA_character_)
    }

    tiene_coma = grepl(",", x, fixed = TRUE)
    tiene_punto = grepl(".", x, fixed = TRUE)

    if (tiene_coma && tiene_punto) {
      posicion_coma = max(gregexpr(",", x, fixed = TRUE)[[1]])
      posicion_punto = max(gregexpr(".", x, fixed = TRUE)[[1]])

      if (posicion_coma > posicion_punto) {
        x = gsub(".", "", x, fixed = TRUE)
        x = gsub(",", ".", x, fixed = TRUE)
      } else {
        x = gsub(",", "", x, fixed = TRUE)
      }
    } else if (tiene_coma && !tiene_punto) {
      x = gsub(",", ".", x, fixed = TRUE)
    } else if (tiene_punto && !tiene_coma) {
      if (grepl("^[-]?[0-9]{1,3}(\\.[0-9]{3})+$", x)) {
        x = gsub(".", "", x, fixed = TRUE)
      }
    }

    x
  })

  suppressWarnings(as.numeric(valor))
}

parsear_numeros_medidas = function(texto) {
  if (is.null(texto) || trimws(texto) == "") {
    return(list(valores = numeric(0), invalidos = character(0)))
  }

  texto = gsub("\r", "\n", texto)
  texto = gsub("\t", ";", texto)
  texto = gsub("\n", ";", texto)

  tokens = trimws(unlist(strsplit(texto, ";", fixed = TRUE)))
  tokens = tokens[tokens != ""]

  if (length(tokens) == 1 && grepl(",", tokens[1], fixed = TRUE)) {
    partes = trimws(unlist(strsplit(tokens[1], ",", fixed = TRUE)))
    if (length(partes) > 1 && all(nchar(partes) <= 3)) {
      tokens = partes
    }
  }

  tokens = gsub("^,+", "", tokens)
  tokens = gsub(",+$", "", tokens)
  tokens = trimws(tokens)
  tokens = tokens[tokens != ""]

  valores = normalizar_numero_medida(tokens)
  validos = !is.na(valores)

  list(
    valores = valores[validos],
    invalidos = tokens[!validos]
  )
}

validar_k_medida = function(medida, k) {
  if (is.null(k) || is.na(k)) {
    return("El valor de k no puede estar vacío.")
  }

  if (k != floor(k)) {
    return("El valor de k debe ser un número entero.")
  }

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

nombre_resultado_medida = function(medida, k) {
  if (medida == "Cuartiles") {
    return(paste0("Q", k))
  }

  if (medida == "Deciles") {
    return(paste0("D", k))
  }

  if (medida == "Percentiles") {
    return(paste0("P", k))
  }

  medida
}

validar_no_clasificados_medidas = function(datos) {
  if (length(datos) == 0) {
    return("Debe ingresar al menos un dato numérico.")
  }

  TRUE
}

validar_clasificados_medidas = function(tabla) {
  if (nrow(tabla) == 0) {
    return("Debe existir al menos una fila en la tabla clasificada.")
  }

  if (any(is.na(tabla$Li)) || any(is.na(tabla$Ls)) || any(is.na(tabla$fi))) {
    return("La tabla clasificada contiene valores vacíos o no numéricos.")
  }

  if (any(tabla$Ls <= tabla$Li)) {
    return("Cada límite superior Ls debe ser mayor que su límite inferior Li.")
  }

  if (any(tabla$fi <= 0)) {
    return("Las frecuencias fi deben ser mayores que cero.")
  }

  if (any(abs(tabla$fi - round(tabla$fi)) > 0.000001)) {
    return("Las frecuencias absolutas fi deben ser números enteros.")
  }

  TRUE
}

crear_tabla_frecuencia_clasificada_medidas = function(tabla) {
  fi = tabla$fi
  n = sum(fi)
  xi = (tabla$Li + tabla$Ls) / 2

  data.frame(
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

calcular_medida_no_clasificada = function(datos, medida, k = 1) {
  validacion = validar_no_clasificados_medidas(datos)

  if (validacion != TRUE) {
    return(list(error = validacion))
  }

  datos_ordenados = sort(datos)
  n = length(datos)

  if (medida == "Media aritmética") {
    resultado = mean(datos)

    pasos = data.frame(
      paso = c("n", "Suma de datos", "Media aritmética"),
      valor = c(n, sum(datos), resultado)
    )

    detalle = data.frame(
      posicion = seq_along(datos_ordenados),
      xi_ordenado = datos_ordenados
    )

    return(list(resultado = resultado, pasos = pasos, detalle = detalle))
  }

  if (medida == "Media geométrica") {
    if (any(datos <= 0)) {
      return(list(error = "La media geométrica carece de sentido para datos negativos y nulos. Todos los valores deben ser mayores que cero."))
    }

    resultado = exp(mean(log(datos)))

    pasos = data.frame(
      paso = c("n", "Producto de datos", "Media geométrica"),
      valor = c(n, prod(datos), resultado)
    )

    detalle = data.frame(
      posicion = seq_along(datos_ordenados),
      xi_ordenado = datos_ordenados,
      log_xi = log(datos_ordenados)
    )

    return(list(resultado = resultado, pasos = pasos, detalle = detalle))
  }

  if (medida == "Media armónica") {
    if (any(datos == 0)) {
      return(list(error = "La media armónica no acepta datos iguales a cero porque se divide entre cada xi."))
    }

    resultado = n / sum(1 / datos)

    pasos = data.frame(
      paso = c("n", "Suma de 1/xi", "Media armónica"),
      valor = c(n, sum(1 / datos), resultado)
    )

    detalle = data.frame(
      posicion = seq_along(datos_ordenados),
      xi_ordenado = datos_ordenados,
      uno_entre_xi = 1 / datos_ordenados
    )

    return(list(resultado = resultado, pasos = pasos, detalle = detalle))
  }

  if (medida == "Mediana") {
    if (n %% 2 == 1) {
      posicion = (n + 1) / 2
      resultado = datos_ordenados[posicion]

      pasos = data.frame(
        paso = c("n", "Tipo de n", "Posición", "Mediana"),
        valor = c(n, "Impar", posicion, resultado)
      )
    } else {
      posicion_1 = n / 2
      posicion_2 = posicion_1 + 1
      resultado = (datos_ordenados[posicion_1] + datos_ordenados[posicion_2]) / 2

      pasos = data.frame(
        paso = c("n", "Tipo de n", "Posición 1", "Posición 2", "Mediana"),
        valor = c(n, "Par", posicion_1, posicion_2, resultado)
      )
    }

    detalle = data.frame(
      posicion = seq_along(datos_ordenados),
      xi_ordenado = datos_ordenados
    )

    return(list(resultado = resultado, pasos = pasos, detalle = detalle))
  }

  if (medida == "Moda") {
    tabla = table(datos)
    frecuencia_mayor = max(tabla)

    if (frecuencia_mayor == 1) {
      resultado = "No existe moda"
      tipo = "Sin moda"
    } else {
      modas = names(tabla[tabla == frecuencia_mayor])
      resultado = paste(modas, collapse = ", ")

      tipo = ifelse(
        length(modas) == 1,
        "Unimodal",
        ifelse(length(modas) == 2, "Bimodal", ifelse(length(modas) == 3, "Trimodal", "Multimodal"))
      )
    }

    detalle = data.frame(
      xi = as.numeric(names(tabla)),
      fi = as.numeric(tabla)
    )

    detalle = detalle[order(detalle$xi), ]

    pasos = data.frame(
      paso = c("Frecuencia mayor", "Tipo de moda", "Moda"),
      valor = c(frecuencia_mayor, tipo, resultado)
    )

    return(list(resultado = resultado, pasos = pasos, detalle = detalle))
  }

  if (medida %in% c("Cuartiles", "Deciles", "Percentiles")) {
    divisor = ifelse(medida == "Cuartiles", 4, ifelse(medida == "Deciles", 10, 100))
    nombre = nombre_resultado_medida(medida, k)

    posicion = k * (n + 1) / divisor

    if (posicion < 1 || posicion > n) {
      return(list(error = paste("La posición calculada queda fuera del conjunto de datos. Posición:", formatear_numero(posicion, 4))))
    }

    if (abs(posicion - round(posicion)) < 0.000001) {
      posicion_entera = round(posicion)
      resultado = datos_ordenados[posicion_entera]

      pasos = data.frame(
        paso = c("k", "n", "Divisor", "Posición = k(n+1)/divisor", "Posición entera", nombre),
        valor = c(k, n, divisor, posicion, posicion_entera, resultado)
      )
    } else {
      posicion_inferior = floor(posicion)
      posicion_superior = ceiling(posicion)
      parte_decimal = posicion - posicion_inferior
      dato_inferior = datos_ordenados[posicion_inferior]
      dato_superior = datos_ordenados[posicion_superior]
      resultado = dato_inferior + parte_decimal * (dato_superior - dato_inferior)

      pasos = data.frame(
        paso = c(
          "k",
          "n",
          "Divisor",
          "Posición = k(n+1)/divisor",
          "Posición inferior",
          "Posición superior",
          "Parte decimal",
          "Dato inferior",
          "Dato superior",
          "Interpolación",
          nombre
        ),
        valor = c(
          k,
          n,
          divisor,
          posicion,
          posicion_inferior,
          posicion_superior,
          parte_decimal,
          dato_inferior,
          dato_superior,
          paste0(dato_inferior, " + ", parte_decimal, " * (", dato_superior, " - ", dato_inferior, ")"),
          resultado
        )
      )
    }

    detalle = data.frame(
      posicion = seq_along(datos_ordenados),
      xi_ordenado = datos_ordenados
    )

    return(list(resultado = resultado, pasos = pasos, detalle = detalle))
  }
}

calcular_medida_clasificada = function(tabla, medida, k = 1) {
  validacion = validar_clasificados_medidas(tabla)

  if (validacion != TRUE) {
    return(list(error = validacion))
  }

  tabla_frecuencia = crear_tabla_frecuencia_clasificada_medidas(tabla)
  n = sum(tabla_frecuencia$fi)
  xi = tabla_frecuencia$xi
  fi = tabla_frecuencia$fi

  if (medida == "Media aritmética") {
    resultado = sum(xi * fi) / n

    pasos = data.frame(
      paso = c("n", "Suma xi * fi", "Media aritmética"),
      valor = c(n, sum(xi * fi), resultado)
    )

    detalle = tabla_frecuencia
    detalle$xi_por_fi = xi * fi

    return(list(resultado = resultado, pasos = pasos, detalle = detalle))
  }

  if (medida == "Media geométrica") {
    if (any(xi <= 0)) {
      return(list(error = "La media geométrica clasificada no acepta marcas de clase negativas o iguales a cero."))
    }

    resultado = exp(sum(fi * log(xi)) / n)

    pasos = data.frame(
      paso = c("n", "Suma fi * ln(xi)", "Media geométrica"),
      valor = c(n, sum(fi * log(xi)), resultado)
    )

    detalle = tabla_frecuencia
    detalle$log_xi = log(xi)
    detalle$fi_por_log_xi = fi * log(xi)

    return(list(resultado = resultado, pasos = pasos, detalle = detalle))
  }

  if (medida == "Media armónica") {
    if (any(xi == 0)) {
      return(list(error = "La media armónica clasificada no acepta marcas de clase iguales a cero."))
    }

    resultado = n / sum(fi / xi)

    pasos = data.frame(
      paso = c("n", "Suma fi / xi", "Media armónica"),
      valor = c(n, sum(fi / xi), resultado)
    )

    detalle = tabla_frecuencia
    detalle$fi_entre_xi = fi / xi

    return(list(resultado = resultado, pasos = pasos, detalle = detalle))
  }

  if (medida == "Mediana") {
    posicion = n / 2
    indice = which(tabla_frecuencia$Fi >= posicion)[1]

    Li = tabla_frecuencia$Li[indice]
    t = tabla_frecuencia$Ls[indice] - tabla_frecuencia$Li[indice]
    Fi_anterior = ifelse(indice == 1, 0, tabla_frecuencia$Fi[indice - 1])
    fi_clase = tabla_frecuencia$fi[indice]

    resultado = Li + ((posicion - Fi_anterior) * t / fi_clase)

    pasos = data.frame(
      paso = c("n", "n/2", "Li", "Fi anterior", "t", "fi", "Mediana"),
      valor = c(n, posicion, Li, Fi_anterior, t, fi_clase, resultado)
    )

    return(list(resultado = resultado, pasos = pasos, detalle = tabla_frecuencia))
  }

  if (medida == "Moda") {
    indice = which.max(fi)

    Li = tabla_frecuencia$Li[indice]
    t = tabla_frecuencia$Ls[indice] - tabla_frecuencia$Li[indice]
    fi_modal = tabla_frecuencia$fi[indice]
    fi_anterior = ifelse(indice == 1, 0, tabla_frecuencia$fi[indice - 1])
    fi_posterior = ifelse(indice == length(fi), 0, tabla_frecuencia$fi[indice + 1])

    d1 = fi_modal - fi_anterior
    d2 = fi_modal - fi_posterior

    if ((d1 + d2) == 0) {
      return(list(error = "No se puede calcular la moda porque d1 + d2 es igual a cero."))
    }

    resultado = Li + ((t * d1) / (d1 + d2))

    pasos = data.frame(
      paso = c("Li", "t", "fi modal", "fi anterior", "fi posterior", "d1", "d2", "Moda"),
      valor = c(Li, t, fi_modal, fi_anterior, fi_posterior, d1, d2, resultado)
    )

    return(list(resultado = resultado, pasos = pasos, detalle = tabla_frecuencia))
  }

  if (medida %in% c("Cuartiles", "Deciles", "Percentiles")) {
    divisor = ifelse(medida == "Cuartiles", 4, ifelse(medida == "Deciles", 10, 100))
    nombre = nombre_resultado_medida(medida, k)

    posicion = k * n / divisor
    indice = which(tabla_frecuencia$Fi >= posicion)[1]

    if (is.na(indice)) {
      return(list(error = "No se encontró la clase correspondiente para la posición calculada."))
    }

    Li = tabla_frecuencia$Li[indice]
    t = tabla_frecuencia$Ls[indice] - tabla_frecuencia$Li[indice]
    Fi_anterior = ifelse(indice == 1, 0, tabla_frecuencia$Fi[indice - 1])
    fi_clase = tabla_frecuencia$fi[indice]

    resultado = Li + ((posicion - Fi_anterior) * t / fi_clase)

    pasos = data.frame(
      paso = c(
        "k",
        "n",
        "Divisor",
        paste0("Posición = k*n/", divisor),
        "Li",
        "Fi anterior",
        "t",
        "fi",
        nombre
      ),
      valor = c(k, n, divisor, posicion, Li, Fi_anterior, t, fi_clase, resultado)
    )

    return(list(resultado = resultado, pasos = pasos, detalle = tabla_frecuencia))
  }
}

calcular_todas_no_clasificadas = function(datos, k_cuartil, k_decil, k_percentil) {
  validacion = validar_no_clasificados_medidas(datos)

  if (validacion != TRUE) {
    return(list(error = validacion))
  }

  validacion_q = validar_k_medida("Cuartiles", k_cuartil)
  validacion_d = validar_k_medida("Deciles", k_decil)
  validacion_p = validar_k_medida("Percentiles", k_percentil)

  media_aritmetica = calcular_medida_no_clasificada(datos, "Media aritmética")$resultado

  if (any(datos <= 0)) {
    media_geometrica = "No se puede: existen valores menores o iguales a 0"
  } else {
    media_geometrica = calcular_medida_no_clasificada(datos, "Media geométrica")$resultado
  }

  if (any(datos == 0)) {
    media_armonica = "No se puede: existe al menos un valor igual a 0"
  } else {
    media_armonica = calcular_medida_no_clasificada(datos, "Media armónica")$resultado
  }

  mediana = calcular_medida_no_clasificada(datos, "Mediana")$resultado
  moda = calcular_medida_no_clasificada(datos, "Moda")$resultado

  if (validacion_q == TRUE) {
    cuartil = calcular_medida_no_clasificada(datos, "Cuartiles", k_cuartil)$resultado
  } else {
    cuartil = validacion_q
  }

  if (validacion_d == TRUE) {
    decil = calcular_medida_no_clasificada(datos, "Deciles", k_decil)$resultado
  } else {
    decil = validacion_d
  }

  if (validacion_p == TRUE) {
    percentil = calcular_medida_no_clasificada(datos, "Percentiles", k_percentil)$resultado
  } else {
    percentil = validacion_p
  }

  datos_ordenados = sort(datos)

  tabla_resultados = data.frame(
    medida = c(
      "Media aritmética",
      "Media geométrica",
      "Media armónica",
      "Mediana",
      "Moda",
      paste0("Q", k_cuartil),
      paste0("D", k_decil),
      paste0("P", k_percentil)
    ),
    resultado = c(
      media_aritmetica,
      media_geometrica,
      media_armonica,
      mediana,
      moda,
      cuartil,
      decil,
      percentil
    ),
    stringsAsFactors = FALSE
  )

  detalle = data.frame(
    posicion = seq_along(datos_ordenados),
    xi_ordenado = datos_ordenados
  )

  pasos = data.frame(
    paso = c(
      "Cantidad de datos",
      "Dato menor",
      "Dato mayor",
      "Q seleccionado",
      "D seleccionado",
      "P seleccionado"
    ),
    valor = c(
      length(datos),
      min(datos),
      max(datos),
      paste0("Q", k_cuartil),
      paste0("D", k_decil),
      paste0("P", k_percentil)
    )
  )

  list(
    todos = TRUE,
    resultado = "Ver tabla de resultados",
    pasos = pasos,
    detalle = detalle,
    tabla_todos = tabla_resultados
  )
}

calcular_todas_clasificadas = function(tabla, k_cuartil, k_decil, k_percentil) {
  validacion = validar_clasificados_medidas(tabla)

  if (validacion != TRUE) {
    return(list(error = validacion))
  }

  validacion_q = validar_k_medida("Cuartiles", k_cuartil)
  validacion_d = validar_k_medida("Deciles", k_decil)
  validacion_p = validar_k_medida("Percentiles", k_percentil)

  tabla_frecuencia = crear_tabla_frecuencia_clasificada_medidas(tabla)
  xi = tabla_frecuencia$xi

  media_aritmetica = calcular_medida_clasificada(tabla, "Media aritmética")$resultado

  if (any(xi <= 0)) {
    media_geometrica = "No se puede: existen marcas de clase menores o iguales a 0"
  } else {
    media_geometrica = calcular_medida_clasificada(tabla, "Media geométrica")$resultado
  }

  if (any(xi == 0)) {
    media_armonica = "No se puede: existe una marca de clase igual a 0"
  } else {
    media_armonica = calcular_medida_clasificada(tabla, "Media armónica")$resultado
  }

  mediana = calcular_medida_clasificada(tabla, "Mediana")$resultado
  moda = calcular_medida_clasificada(tabla, "Moda")$resultado

  if (validacion_q == TRUE) {
    cuartil = calcular_medida_clasificada(tabla, "Cuartiles", k_cuartil)$resultado
  } else {
    cuartil = validacion_q
  }

  if (validacion_d == TRUE) {
    decil = calcular_medida_clasificada(tabla, "Deciles", k_decil)$resultado
  } else {
    decil = validacion_d
  }

  if (validacion_p == TRUE) {
    percentil = calcular_medida_clasificada(tabla, "Percentiles", k_percentil)$resultado
  } else {
    percentil = validacion_p
  }

  tabla_resultados = data.frame(
    medida = c(
      "Media aritmética",
      "Media geométrica",
      "Media armónica",
      "Mediana",
      "Moda",
      paste0("Q", k_cuartil),
      paste0("D", k_decil),
      paste0("P", k_percentil)
    ),
    resultado = c(
      media_aritmetica,
      media_geometrica,
      media_armonica,
      mediana,
      moda,
      cuartil,
      decil,
      percentil
    ),
    stringsAsFactors = FALSE
  )

  pasos = data.frame(
    paso = c(
      "Cantidad de datos",
      "Total de frecuencias",
      "Q seleccionado",
      "D seleccionado",
      "P seleccionado"
    ),
    valor = c(
      nrow(tabla),
      sum(tabla$fi),
      paste0("Q", k_cuartil),
      paste0("D", k_decil),
      paste0("P", k_percentil)
    )
  )

  list(
    todos = TRUE,
    resultado = "Ver tabla de resultados",
    pasos = pasos,
    detalle = tabla_frecuencia,
    tabla_todos = tabla_resultados
  )
}

formatear_valor_medida = function(valor) {
  numero = suppressWarnings(as.numeric(valor))

  if (!is.na(numero)) {
    return(formatear_numero(numero, 4))
  }

  as.character(valor)
}

formatear_tabla_todos_medidas = function(tabla) {
  salida = tabla

  salida$resultado = sapply(salida$resultado, formatear_valor_medida)

  salida
}

formatear_tabla_medidas = function(tabla) {
  salida = tabla

  for (columna in names(salida)) {
    if (is.numeric(salida[[columna]])) {
      if (columna %in% c("fi", "Fi")) {
        salida[[columna]] = formatear_numero(salida[[columna]], 0)
      } else if (columna %in% c("hi", "Hi")) {
        salida[[columna]] = formatear_numero(salida[[columna]], 4)
      } else if (columna %in% c("pi", "Pi")) {
        salida[[columna]] = formatear_porcentaje(salida[[columna]], 2)
      } else {
        salida[[columna]] = formatear_numero(salida[[columna]], 4)
      }
    }
  }

  salida
}

modulo_medidas_ui = function(id) {
  ns = NS(id)

  div(
    class = "tarjeta",
    h1(class = "titulo", "Medidas de posición"),
    div(
      class = "nota",
      "Seleccione el tipo de datos y la medida que desea calcular. Si selecciona Todos, el sistema calcula todas las medidas disponibles con los mismos datos."
    ),
    div(
      class = "grupo-controles",
      selectInput(
        ns("tipo_datos"),
        "Tipo de datos",
        choices = c(
          "Datos no clasificados" = "no_clasificados",
          "Datos clasificados" = "clasificados"
        ),
        selected = "no_clasificados"
      ),
      selectInput(
        ns("medida"),
        "Medida",
        choices = c(
          "Todos",
          "Media aritmética",
          "Media geométrica",
          "Media armónica",
          "Mediana",
          "Moda",
          "Cuartiles",
          "Deciles",
          "Percentiles"
        ),
        selected = "Media aritmética"
      )
    ),
    uiOutput(ns("entrada_k")),
    uiOutput(ns("nota_ejemplo")),
    conditionalPanel(
      condition = paste0("input['", ns("tipo_datos"), "'] == 'no_clasificados'"),
      h4("Datos no clasificados"),
      textAreaInput(
        ns("datos_no_clasificados"),
        "Datos",
        value = ejemplos_medidas_no_clasificados[["Media aritmética"]]$datos,
        rows = 5,
        width = "100%"
      ),
      actionButton(
        ns("pegar_no_clasificados"),
        "Pegar desde Excel",
        onclick = paste0("pegarEnTextArea('", ns("datos_no_clasificados"), "')"),
        class = "btn-warning"
      )
    ),
    conditionalPanel(
      condition = paste0("input['", ns("tipo_datos"), "'] == 'clasificados'"),
      h4("Datos clasificados"),
      div(
        class = "tabla-edicion",
        div(
          class = "encabezado-edicion-clasificada",
          div("Li"),
          div("Ls"),
          div("fi")
        ),
        uiOutput(ns("filas_clasificadas"))
      ),
      actionButton(
        ns("pegar_clasificados"),
        "Pegar desde Excel",
        onclick = paste0("pegarDatosClasificados('", ns("datos_clasificados_pegados"), "')"),
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
      )
    ),
    actionButton(
      ns("cargar_ejemplo"),
      "Cargar ejemplo recomendado",
      class = "btn-info"
    ),
    actionButton(
      ns("calcular"),
      "Calcular medida",
      class = "btn-primary"
    ),
    tags$hr(),
    uiOutput(ns("salida"))
  )
}

modulo_medidas_server = function(id) {
  moduleServer(id, function(input, output, session) {
    tabla_reactiva = reactiveVal(ejemplos_medidas_clasificados[["Media aritmética"]]$tabla)

    leer_tabla_actual = function() {
      tabla_base = tabla_reactiva()

      if (nrow(tabla_base) == 0) {
        return(tabla_base)
      }

      Li = numeric(nrow(tabla_base))
      Ls = numeric(nrow(tabla_base))
      fi = numeric(nrow(tabla_base))

      for (i in seq_len(nrow(tabla_base))) {
        valor_Li = input[[paste0("Li_", i)]]
        valor_Ls = input[[paste0("Ls_", i)]]
        valor_fi = input[[paste0("fi_", i)]]

        Li[i] = ifelse(is.null(valor_Li), tabla_base$Li[i], valor_Li)
        Ls[i] = ifelse(is.null(valor_Ls), tabla_base$Ls[i], valor_Ls)
        fi[i] = ifelse(is.null(valor_fi), tabla_base$fi[i], valor_fi)
      }

      data.frame(Li = Li, Ls = Ls, fi = fi)
    }

    cargar_ejemplo_actual = function() {
      medida = input$medida
      tipo = input$tipo_datos

      if (tipo == "no_clasificados") {
        ejemplo = ejemplos_medidas_no_clasificados[[medida]]
        updateTextAreaInput(session, "datos_no_clasificados", value = ejemplo$datos)

        if (medida == "Todos") {
          updateNumericInput(session, "k_cuartil_todos", value = 2)
          updateNumericInput(session, "k_decil_todos", value = 5)
          updateNumericInput(session, "k_percentil_todos", value = 50)
        }

        if (medida %in% c("Cuartiles", "Deciles", "Percentiles") && !is.null(ejemplo$k)) {
          updateNumericInput(session, "k", value = ejemplo$k)
        }
      } else {
        ejemplo = ejemplos_medidas_clasificados[[medida]]
        tabla_reactiva(ejemplo$tabla)

        if (medida == "Todos") {
          updateNumericInput(session, "k_cuartil_todos", value = 2)
          updateNumericInput(session, "k_decil_todos", value = 5)
          updateNumericInput(session, "k_percentil_todos", value = 50)
        }

        if (medida %in% c("Cuartiles", "Deciles", "Percentiles") && !is.null(ejemplo$k)) {
          updateNumericInput(session, "k", value = ejemplo$k)
        }
      }
    }

    observeEvent(list(input$tipo_datos, input$medida), {
      cargar_ejemplo_actual()
    }, ignoreInit = FALSE)

    observeEvent(input$cargar_ejemplo, {
      cargar_ejemplo_actual()
    })

    output$entrada_k = renderUI({
      if (input$medida == "Todos") {
        div(
          class = "resultado-final",
          h4("Medidas Q, D y P a mostrar"),
          div(
            class = "grupo-controles",
            numericInput(session$ns("k_cuartil_todos"), "Cuartil", value = 2, min = 1, max = 3, step = 1),
            numericInput(session$ns("k_decil_todos"), "Decil", value = 5, min = 1, max = 9, step = 1)
          ),
          numericInput(session$ns("k_percentil_todos"), "Percentil", value = 50, min = 1, max = 99, step = 1)
        )
      } else if (input$medida == "Cuartiles") {
        numericInput(session$ns("k"), "Valor de k para Qk", value = 1, min = 1, max = 3, step = 1)
      } else if (input$medida == "Deciles") {
        numericInput(session$ns("k"), "Valor de k para Dk", value = 3, min = 1, max = 9, step = 1)
      } else if (input$medida == "Percentiles") {
        numericInput(session$ns("k"), "Valor de k para Pk", value = 75, min = 1, max = 99, step = 1)
      }
    })

    output$nota_ejemplo = renderUI({
      if (input$tipo_datos == "no_clasificados") {
        nota = ejemplos_medidas_no_clasificados[[input$medida]]$nota
      } else {
        nota = ejemplos_medidas_clasificados[[input$medida]]$nota
      }

      div(class = "nota", nota)
    })

    output$filas_clasificadas = renderUI({
      tabla = tabla_reactiva()
      ns = session$ns

      tagList(
        lapply(seq_len(nrow(tabla)), function(i) {
          div(
            class = "fila-edicion-clasificada",
            numericInput(
              ns(paste0("Li_", i)),
              label = NULL,
              value = tabla$Li[i],
              step = 0.01,
              width = "100%"
            ),
            numericInput(
              ns(paste0("Ls_", i)),
              label = NULL,
              value = tabla$Ls[i],
              step = 0.01,
              width = "100%"
            ),
            numericInput(
              ns(paste0("fi_", i)),
              label = NULL,
              value = tabla$fi[i],
              min = 0,
              step = 1,
              width = "100%"
            )
          )
        })
      )
    })

    observeEvent(input$datos_clasificados_pegados, {
      filas = input$datos_clasificados_pegados

      if (length(filas) == 0) {
        return()
      }

      Li = sapply(filas, function(fila) normalizar_numero_medida(fila$Li))
      Ls = sapply(filas, function(fila) normalizar_numero_medida(fila$Ls))
      fi = sapply(filas, function(fila) normalizar_numero_medida(fila$fi))

      tabla = data.frame(Li = Li, Ls = Ls, fi = fi)

      if (nrow(tabla) > 0) {
        tabla_reactiva(tabla)
      }
    })

    observeEvent(input$agregar_fila, {
      tabla = leer_tabla_actual()

      if (nrow(tabla) == 0) {
        nueva = data.frame(Li = 0, Ls = 1, fi = 1)
      } else {
        amplitud = tabla$Ls[nrow(tabla)] - tabla$Li[nrow(tabla)]
        if (is.na(amplitud) || amplitud <= 0) {
          amplitud = 1
        }

        nueva = data.frame(
          Li = tabla$Ls[nrow(tabla)],
          Ls = tabla$Ls[nrow(tabla)] + amplitud,
          fi = 1
        )
      }

      tabla_reactiva(rbind(tabla, nueva))
    })

    observeEvent(input$quitar_fila, {
      tabla = leer_tabla_actual()

      if (nrow(tabla) > 1) {
        tabla_reactiva(tabla[-nrow(tabla), ])
      }
    })

    resultado_medida = eventReactive(input$calcular, {
      medida = input$medida
      tipo = input$tipo_datos
      k = ifelse(is.null(input$k), 1, input$k)

      if (tipo == "no_clasificados") {
        parseado = parsear_numeros_medidas(input$datos_no_clasificados)

        if (length(parseado$invalidos) > 0) {
          return(list(error = paste("Hay datos no numéricos o mal escritos:", paste(parseado$invalidos, collapse = ", "))))
        }

        if (medida == "Todos") {
          return(calcular_todas_no_clasificadas(
            parseado$valores,
            input$k_cuartil_todos,
            input$k_decil_todos,
            input$k_percentil_todos
          ))
        }

        if (medida %in% c("Cuartiles", "Deciles", "Percentiles")) {
          validacion_k = validar_k_medida(medida, k)

          if (validacion_k != TRUE) {
            return(list(error = validacion_k))
          }
        }

        return(calcular_medida_no_clasificada(parseado$valores, medida, k))
      }

      tabla = leer_tabla_actual()

      if (medida == "Todos") {
        return(calcular_todas_clasificadas(
          tabla,
          input$k_cuartil_todos,
          input$k_decil_todos,
          input$k_percentil_todos
        ))
      }

      if (medida %in% c("Cuartiles", "Deciles", "Percentiles")) {
        validacion_k = validar_k_medida(medida, k)

        if (validacion_k != TRUE) {
          return(list(error = validacion_k))
        }
      }

      calcular_medida_clasificada(tabla, medida, k)
    }, ignoreNULL = FALSE)

    output$salida = renderUI({
      resultado = resultado_medida()

      if (!is.null(resultado$error)) {
        return(div(class = "error", resultado$error))
      }

      if (isTRUE(resultado$todos)) {
        return(tagList(
          h2(class = "subtitulo", "Resultados de todas las medidas"),
          tableOutput(session$ns("tabla_todos")),
          h4("Detalle"),
          tableOutput(session$ns("tabla_detalle")),
          div(
            class = "resultado-final",
            h4("Resumen"),
            tableOutput(session$ns("tabla_resumen"))
          )
        ))
      }

      etiqueta = nombre_resultado_medida(input$medida, ifelse(is.null(input$k), 1, input$k))

      tagList(
        h2(class = "subtitulo", "Resultado"),
        div(
          class = "resultado-principal",
          paste(etiqueta, "=", resultado$resultado)
        ),
        h4("Cálculos"),
        tableOutput(session$ns("tabla_pasos")),
        h4("Detalle"),
        tableOutput(session$ns("tabla_detalle")),
        div(
          class = "resultado-final",
          h4("Resumen"),
          tableOutput(session$ns("tabla_resumen"))
        )
      )
    })

    output$tabla_todos = renderTable({
      resultado = resultado_medida()

      if (!isTRUE(resultado$todos)) {
        return(NULL)
      }

      formatear_tabla_todos_medidas(resultado$tabla_todos)
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$tabla_pasos = renderTable({
      resultado = resultado_medida()

      if (!is.null(resultado$error) || isTRUE(resultado$todos)) {
        return(NULL)
      }

      formatear_tabla_medidas(resultado$pasos)
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$tabla_detalle = renderTable({
      resultado = resultado_medida()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      formatear_tabla_medidas(resultado$detalle)
    }, striped = FALSE, bordered = FALSE, spacing = "m")

    output$tabla_resumen = renderTable({
      resultado = resultado_medida()

      if (!is.null(resultado$error)) {
        return(NULL)
      }

      if (isTRUE(resultado$todos)) {
        return(data.frame(
          medida = c("Tipo de datos", "Medida calculada", "Resultado"),
          valor = c(
            ifelse(input$tipo_datos == "no_clasificados", "Datos no clasificados", "Datos clasificados"),
            "Todas",
            "Ver tabla de resultados"
          )
        ))
      }

      etiqueta = nombre_resultado_medida(input$medida, ifelse(is.null(input$k), 1, input$k))

      data.frame(
        medida = c("Tipo de datos", "Medida calculada", "Resultado"),
        valor = c(
          ifelse(input$tipo_datos == "no_clasificados", "Datos no clasificados", "Datos clasificados"),
          etiqueta,
          resultado$resultado
        )
      )
    }, striped = FALSE, bordered = FALSE, spacing = "m")
  })
}