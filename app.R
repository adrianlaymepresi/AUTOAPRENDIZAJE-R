# CTRL + ENTER
# ejecutar: shiny::runApp(".")

library(shiny)

source("R/utilidades.R")
source("R/modules/simple_inspeccion.R")
source("R/modules/sturges.R")
source("R/modules/grafico_barras.R")

ui = fluidPage(
  tags$head(
    tags$script(HTML("
      function normalizarDatosPegados(texto) {
        texto = texto.replace(/\\r\\n/g, '\\n').replace(/\\r/g, '\\n');

        const lineas = texto.split('\\n');

        const lineasNormalizadas = lineas.map(function(linea) {
          linea = linea.trim();

          if (linea === '') {
            return '';
          }

          let datos = [];

          if (linea.includes('\\t')) {
            datos = linea.split('\\t');
          } else if (linea.includes(';')) {
            datos = linea.split(';');
          } else {
            datos = linea.split(/[ ]+/);
          }

          datos = datos
            .map(function(dato) {
              return dato.trim();
            })
            .filter(function(dato) {
              return dato !== '';
            });

          if (datos.length === 0) {
            return '';
          }

          return datos.join(';  ') + ';';
        });

        return lineasNormalizadas.filter(function(linea) {
          return linea !== '';
        }).join('\\n');
      }

      function convertirNumeroGrafico(valor) {
        valor = String(valor).trim();

        if (valor.includes(',') && valor.includes('.')) {
          valor = valor.replace(/\\./g, '').replace(',', '.');
        } else if (valor.includes(',') && !valor.includes('.')) {
          valor = valor.replace(',', '.');
        }

        return valor;
      }

      function normalizarFilasGrafico(texto) {
        texto = texto.replace(/\\r\\n/g, '\\n').replace(/\\r/g, '\\n');

        const lineas = texto.split('\\n');
        const filas = [];

        lineas.forEach(function(linea) {
          linea = linea.trim();

          if (linea === '') {
            return;
          }

          let partes = [];

          if (linea.includes('\\t')) {
            partes = linea.split('\\t');
          } else if (linea.includes(';')) {
            partes = linea.split(';');
          } else {
            partes = linea.split(/[ ]+/);
          }

          partes = partes
            .map(function(parte) {
              return parte.trim();
            })
            .filter(function(parte) {
              return parte !== '';
            });

          if (partes.length < 2) {
            return;
          }

          const valor = convertirNumeroGrafico(partes[partes.length - 1]);
          const categoria = partes.slice(0, partes.length - 1).join(' ');

          filas.push({
            categoria: categoria,
            valor: valor
          });
        });

        return filas;
      }

      async function pegarEnTextArea(id) {
        const texto = await navigator.clipboard.readText();
        const textoNormalizado = normalizarDatosPegados(texto);
        const caja = document.getElementById(id);

        caja.value = textoNormalizado;
        caja.dispatchEvent(new Event('input', { bubbles: true }));
      }

      async function pegarDatosGrafico(inputId) {
        const texto = await navigator.clipboard.readText();
        const filas = normalizarFilasGrafico(texto);
        Shiny.setInputValue(inputId, filas, {priority: 'event'});
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
        --gris: #e5e7eb;
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

      .contenido {
        flex: 1;
        padding: 44px;
      }

      .tarjeta {
        background: white;
        border-radius: 26px;
        padding: 36px;
        max-width: 1150px;
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
        font-size: 30px;
        font-weight: 900;
      }

      .form-control {
        max-width: 760px;
        font-size: 18px;
      }

      textarea.form-control {
        min-height: 150px;
      }

      .btn {
        border-radius: 12px;
        font-size: 17px;
        font-weight: 800;
        border: none;
        padding: 10px 18px;
        margin-right: 6px;
        margin-bottom: 8px;
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
        text-align: center !important;
      }

      td {
        border-bottom: 1px solid #dddddd !important;
        padding: 8px 18px !important;
        text-align: center !important;
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
        max-width: 860px;
      }

      .resultado-final {
        background: #f8fafc;
        border: 1px solid var(--gris);
        border-radius: 14px;
        padding: 16px;
        margin-top: 16px;
        max-width: 860px;
      }

      .tabla-edicion {
        max-width: 860px;
        margin-bottom: 16px;
      }

      .encabezado-edicion {
        display: grid;
        grid-template-columns: 1fr 220px;
        gap: 12px;
        font-weight: 800;
        margin-bottom: 8px;
      }

      .fila-edicion {
        display: grid;
        grid-template-columns: 1fr 220px;
        gap: 12px;
        margin-bottom: 8px;
      }

      .fila-edicion .form-group {
        margin-bottom: 0;
      }

      .grafico-contenedor {
        background: #ffffff;
        border: 1px solid var(--gris);
        border-radius: 14px;
        padding: 18px;
        margin-top: 16px;
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
          "Tabla simple inspección",
          "Tabla Sturges",
          "Gráfico de barras"
        ),
        selected = "Tabla simple inspección"
      )
    ),
    div(
      class = "contenido",
      conditionalPanel(
        condition = "input.apartado == 'Tabla simple inspección'",
        modulo_simple_inspeccion_ui("simple")
      ),
      conditionalPanel(
        condition = "input.apartado == 'Tabla Sturges'",
        modulo_sturges_ui("sturges")
      ),
      conditionalPanel(
        condition = "input.apartado == 'Gráfico de barras'",
        modulo_grafico_barras_ui("barras")
      )
    )
  )
)

server = function(input, output, session) {
  modulo_simple_inspeccion_server("simple")
  modulo_sturges_server("sturges")
  modulo_grafico_barras_server("barras")
}

shinyApp(ui, server)