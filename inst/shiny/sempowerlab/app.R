# =====================================================================
#  SemPowerLab  ·  Tamaño de muestra para modelos con variables latentes
#  Envoltura interactiva de semPower::semPower.powerRegression()
#
#  Dr. José Ventura-León · Seminario de Tesis III · USMP 2026-2
#  R 4.4.1 · semPower 2.1.1 · shiny · bslib · ggplot2
# =====================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(semPower)

# semPower delega en lavaan la construcción de la matriz de covarianzas
# poblacional del modelo. Es una dependencia real aunque la app no la llame
# directamente: sin esta línea no entra en el manifest del despliegue y
# Connect Cloud responde "This function depends on the lavaan package".
requireNamespace("lavaan", quietly = TRUE)

# ---------------------------------------------------------------------
# PALETA  (sobria: azul marino académico, pizarra y un granate de acento)
# ---------------------------------------------------------------------
COL <- list(
  ink    = "#14202E",
  navy   = "#1D3557",
  navy2  = "#2A4A73",
  slate  = "#55677D",
  line   = "#DCE3EB",
  paper  = "#F4F6F9",
  white  = "#FFFFFF",
  accent = "#9E2A2B",   # el efecto que se contrasta, y el número clave
  gold   = "#B07D2B",
  green  = "#2F6F4E",
  b1     = "#2A6F97",   # beta 1
  b2     = "#9E2A2B",   # beta 2
  corr   = "#4F772D",   # correlacion entre predictores
  load   = "#6A4C93"    # cargas factoriales
)

tema <- bs_theme(
  version = 5,
  bg = COL$white, fg = COL$ink,
  primary = COL$navy, secondary = COL$slate,
  base_font = font_google("Source Sans 3", local = FALSE),
  heading_font = font_google("Source Serif 4", local = FALSE),
  "font-size-base" = "0.95rem"
)

CSS <- sprintf('
:root{
  --ink:%s; --navy:%s; --navy2:%s; --slate:%s; --line:%s;
  --paper:%s; --accent:%s; --gold:%s; --green:%s;
}
body{ background:var(--paper); }

/* ---------- cabecera ---------- */
.app-head{
  background:linear-gradient(100deg, var(--ink) 0%%, var(--navy) 62%%, var(--navy2) 100%%);
  color:#fff; padding:22px 30px 20px 30px; border-bottom:4px solid var(--accent);
}
.app-head h1{
  font-family:"Source Serif 4",Georgia,serif; font-weight:600;
  font-size:1.62rem; margin:0; letter-spacing:.2px;
}
.app-head .sub{ font-size:.86rem; opacity:.82; margin-top:5px; }
.app-head .marca{
  float:right; text-align:right; font-size:.74rem; opacity:.72; line-height:1.55;
  padding-top:5px;
}

/* ---------- panel de controles ---------- */
.bloque{
  background:#fff; border:1px solid var(--line); border-radius:9px;
  padding:15px 16px 6px 16px; margin-bottom:15px;
}
.bloque > .rot{
  font-family:"Source Serif 4",Georgia,serif; font-weight:600; color:var(--navy);
  font-size:.95rem; margin:-2px 0 4px 0;
  border-bottom:1px solid var(--line); padding-bottom:7px;
}
.bloque > .rot .n{
  display:inline-block; width:20px; height:20px; line-height:20px; text-align:center;
  border-radius:50%%; background:var(--navy); color:#fff; font-size:.72rem;
  font-family:"Source Sans 3",sans-serif; margin-right:8px; vertical-align:1px;
}
.ayuda{ font-size:.76rem; color:var(--slate); line-height:1.42; margin:-8px 0 12px 0; }

/* --- una fila por constructo: nombre + dimensiones + items + carga --- */
.cab-constructo{
  display:grid; grid-template-columns:1fr 58px 66px 62px; gap:7px;
  font-size:.68rem; text-transform:uppercase; letter-spacing:.5px;
  color:var(--slate); font-weight:700; margin:2px 0 5px 0;
}
.cab-constructo span:not(:first-child){ text-align:center; }
.fila-constructo{
  display:grid; grid-template-columns:1fr 58px 66px 62px; gap:7px;
  align-items:start; margin-bottom:7px;
}
.fila-constructo .form-group{ margin-bottom:0; }
.fila-constructo input{ font-size:.82rem; padding:5px 7px; }
.fila-constructo input[type=number]{ text-align:center; }

.resumen-ind{
  background:#F0F3F7; border:1px solid var(--line); border-radius:7px;
  padding:9px 12px; font-size:.79rem; color:var(--slate); line-height:1.45;
  margin:2px 0 12px 0;
}
.resumen-ind b{ color:var(--navy); font-family:Consolas,monospace; font-size:.86rem; }
.resumen-ind .aviso-mini{ color:var(--accent); display:block; margin-top:5px; }
.form-label{ font-weight:600; font-size:.83rem; color:var(--ink); margin-bottom:3px; }
.form-control, .form-select{ font-size:.86rem; }

/* ---------- tarjetas de resultado ---------- */
.tarjetas{ display:grid; grid-template-columns:1.5fr 1fr 1fr 1fr; gap:14px; margin-bottom:16px; }
.tar{
  background:#fff; border:1px solid var(--line); border-radius:9px;
  padding:14px 18px 13px 18px; border-top:4px solid var(--slate);
}
.tar.clave{ border-top-color:var(--accent); }
.tar .rot{ font-size:.72rem; letter-spacing:.7px; text-transform:uppercase; color:var(--slate); }
.tar .val{
  font-family:"Source Serif 4",Georgia,serif; font-weight:600; color:var(--ink);
  font-size:2.05rem; line-height:1.12; margin:4px 0 2px 0;
}
.tar.clave .val{ color:var(--accent); font-size:2.6rem; }
.tar .pie{ font-size:.76rem; color:var(--slate); line-height:1.38; }

/* ---------- panel de pestañas ---------- */
.nav-tabs .nav-link{ font-size:.87rem; font-weight:600; color:var(--slate); }
.nav-tabs .nav-link.active{ color:var(--navy); border-bottom:2px solid var(--accent); }
.card{ border:1px solid var(--line); border-radius:9px; }

/* ---------- bloques de codigo y texto ---------- */
.consola{
  background:#16202B; color:#E6EDF3; border-radius:8px; padding:16px 18px;
  font-family:Consolas,"Courier New",monospace; font-size:.82rem; line-height:1.62;
  white-space:pre; overflow-x:auto;
}
.consola .cmt{ color:#7FB08A; }
.consola .arg{ color:#9CC7EA; }
.consola .num{ color:#E3B778; }
.consola .str{ color:#E3A08B; }
.consola .fun{ color:#E8D9A0; }

.parrafo{
  background:#fff; border-left:5px solid var(--navy); border-radius:0 8px 8px 0;
  padding:20px 24px; font-size:.95rem; line-height:1.86; text-align:justify;
}
.parrafo b{ color:var(--accent); }

/* --- los seis elementos obligatorios, cada uno con su color --------- */
.tramo{
  padding:1px 2px; border-radius:3px; font-weight:500;
  text-decoration:underline dotted currentColor;
  text-decoration-thickness:1.5px; text-underline-offset:4px;
  transition:opacity .15s ease, background-color .15s ease;
}
.s1{ color:#2A6F97; } .s2{ color:#14746F; } .s3{ color:#B45309; }
.s4{ color:#6A4C93; } .s5{ color:#9E2A2B; } .s6{ color:#8A6D1D; }

/* al pasar el raton por un chip (o por el propio tramo) se enciende
   ese fragmento y se apaga el resto */
.parrafo.enfoque .tramo{ opacity:.22; }
.parrafo.enfoque .tramo.on{ opacity:1; background:#F3F6FA; }

.chips-6{ display:flex; flex-wrap:wrap; gap:9px; margin-top:16px; }
.chip6{
  flex:1 1 150px; background:#fff; border:1px solid var(--line);
  border-top:4px solid var(--slate); border-radius:7px;
  padding:9px 12px 8px 12px; font-size:.79rem; line-height:1.32;
  color:var(--slate); cursor:default; transition:transform .12s ease, box-shadow .12s ease;
}
.chip6 b{ display:block; font-size:1.15rem; font-weight:700; margin-bottom:2px; }
.chip6:hover{ transform:translateY(-2px); box-shadow:0 4px 12px rgba(20,32,46,.12); }
.chip6.k1{ border-top-color:#2A6F97; } .chip6.k1 b{ color:#2A6F97; }
.chip6.k2{ border-top-color:#14746F; } .chip6.k2 b{ color:#14746F; }
.chip6.k3{ border-top-color:#B45309; } .chip6.k3 b{ color:#B45309; }
.chip6.k4{ border-top-color:#6A4C93; } .chip6.k4 b{ color:#6A4C93; }
.chip6.k5{ border-top-color:#9E2A2B; } .chip6.k5 b{ color:#9E2A2B; }
.chip6.k6{ border-top-color:#8A6D1D; } .chip6.k6 b{ color:#8A6D1D; }

.tabla-s{ border-collapse:collapse; width:100%%; font-size:.86rem; }
.tabla-s th, .tabla-s td{ border:1px solid var(--line); padding:8px 10px; text-align:center; }
.tabla-s th{ background:var(--navy); color:#fff; font-weight:600; }
.tabla-s td.fila{ background:#F0F3F7; font-weight:600; text-align:left; }
.tabla-s td.aqui{ background:#FBEDED; color:var(--accent); font-weight:700; }

.leyenda{ font-size:.79rem; color:var(--slate); line-height:1.5; margin-top:12px; }
.aviso{
  background:#FBEDED; border-left:4px solid var(--accent); color:#7A2224;
  padding:12px 16px; border-radius:0 6px 6px 0; font-size:.87rem;
}
.pill{
  display:inline-block; background:#EEF2F7; border:1px solid var(--line);
  border-radius:20px; padding:3px 12px; font-size:.78rem; color:var(--navy);
  margin:0 6px 6px 0; font-weight:600;
}
.btn-calc{ background:var(--navy); border-color:var(--navy); font-weight:600; }
.btn-calc:hover{ background:var(--ink); border-color:var(--ink); }
', COL$ink, COL$navy, COL$navy2, COL$slate, COL$line, COL$paper,
   COL$accent, COL$gold, COL$green)

# ---------------------------------------------------------------------
# NÚCLEO DE CÁLCULO
# ---------------------------------------------------------------------
correr <- function(b1, b2, corXX, nullWhich, nInd, loadM, alpha, power,
                   modo = "apriori", N = 300) {
  # En la app los constructos 1 y 2 son los predictores y el 3 el criterio;
  # semPower toma el PRIMER factor como criterio Y y los siguientes como X1, X2
  orden_sp <- c(3, 1, 2)
  args <- list(
    slopes     = c(b1, b2),
    corXX      = corXX,
    nullEffect = "slope = 0",
    nullWhich  = nullWhich,
    nIndicator = nInd[orden_sp],
    loadM      = if (length(loadM) == 3) loadM[orden_sp] else loadM,
    alpha      = alpha
  )
  if (modo == "apriori") {
    res <- do.call(semPower.powerRegression, c(list(type = "a-priori", power = power), args))
    list(ok = TRUE, modo = modo, N = res$requiredN, power = res$impliedPower,
         fmin = res$fmin, df = res$df, obj = res)
  } else {
    res <- do.call(semPower.powerRegression, c(list(type = "post-hoc", N = N), args))
    list(ok = TRUE, modo = modo, N = N, power = res$power,
         fmin = res$fmin, df = res$df, obj = res)
  }
}

# potencia analítica a partir del F0 y los gl que devuelve semPower
# (verificado contra semPower.powerRegression: coincide en la 5.a decimal)
pot_en <- function(N, fmin, df, alpha) {
  1 - pchisq(qchisq(1 - alpha, df), df, ncp = (N - 1) * fmin)
}

n_para <- function(objetivo, fmin, df, alpha, tope = 20000) {
  n <- 20
  while (n < tope && pot_en(n, fmin, df, alpha) < objetivo) n <- n + 1
  n
}

# ---------------------------------------------------------------------
# DIAGRAMA DEL MODELO
# ---------------------------------------------------------------------
dibujar_modelo <- function(nom, b1, b2, corXX, loadM, nInd, nullWhich) {
  op <- par(mar = c(0, 0, 0, 0), bg = "white"); on.exit(par(op))
  plot(NA, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "", asp = NA)

  elipse <- function(x, y, rx, ry, borde, relleno = "white", lwd = 2) {
    t <- seq(0, 2 * pi, length.out = 120)
    polygon(x + rx * cos(t), y + ry * sin(t), border = borde, col = relleno, lwd = lwd)
  }
  # se dibuja UNA caja por indicador: el numero de cajas es el nIndicator del
  # factor. Si no caben legibles se comprimen; pasado el tope se dibujan las
  # primeras y una caja de continuacion con el resto.
  cajas <- function(xc, yc, k, alto = 44) {
    vis <- min(k, 22)
    sep <- min(7.0, alto / vis)
    h   <- sep * .76
    ys  <- yc + (seq_len(vis) - (vis + 1) / 2) * sep    # ys[1] es la de abajo
    an  <- if (sep < 3.2) 5.2 else 6.2
    # la etiqueta encoge con la caja; por debajo de ~14 indicadores por factor
    # sigue cabiendo, y solo se omite cuando ya no hay altura para ella
    cex <- max(.46, min(.72, sep * .105))
    pone <- sep >= 2.9
    for (i in seq_len(vis)) {
      rect(xc - an, ys[i] - h / 2, xc + an, ys[i] + h / 2,
           border = COL$slate, col = "#F7F9FB", lwd = if (sep < 3) .9 else 1.4)
      if (pone) {
        etq <- if (k > vis && i == 1) paste0("+", k - vis + 1) else paste0("i", vis - i + 1)
        text(xc, ys[i], etq, cex = cex, col = COL$slate)
      }
    }
    if (k > vis && !pone)
      text(xc, min(ys) - sep * 1.6, paste0("+", k - vis), cex = .6, col = COL$slate)
    ys
  }
  flechas_carga <- function(xc, ys, xe, ye, rx, ry) {
    for (y in ys) {
      dx <- xe - xc; dy <- ye - y; L <- sqrt(dx^2 + dy^2)
      arrows(xe - rx * dx / L * 1.02, ye - ry * dy / L * 1.02, xc + 7.2, y,
             length = .07, col = COL$load, lwd = 1.3)
    }
  }

  # posiciones
  p1 <- c(38, 75); p2 <- c(38, 25); cr <- c(72, 50)
  rx <- 11.5; ry <- 8.5

  ys1 <- cajas(12, 75, nInd[1], 42);  flechas_carga(12, ys1, p1[1], p1[2], rx, ry)
  ys2 <- cajas(12, 25, nInd[2], 42);  flechas_carga(12, ys2, p2[1], p2[2], rx, ry)
  ys3 <- cajas(93, 50, nInd[3], 88)
  for (y in ys3) {
    dx <- 93 - cr[1]; dy <- y - cr[2]; L <- sqrt(dx^2 + dy^2)
    arrows(cr[1] + rx * dx / L * 1.02, cr[2] + ry * dy / L * 1.02, 93 - 7.2, y,
           length = .07, col = COL$load, lwd = 1.3)
  }

  elipse(p1[1], p1[2], rx, ry, COL$b1)
  elipse(p2[1], p2[2], rx, ry, COL$b2)
  elipse(cr[1], cr[2], rx, ry, COL$navy)

  envol <- function(s, n = 15) paste(strwrap(s, width = n), collapse = "\n")
  text(p1[1], p1[2], envol(nom[1]), cex = .82, col = COL$ink, font = 2)
  text(p2[1], p2[2], envol(nom[2]), cex = .82, col = COL$ink, font = 2)
  text(cr[1], cr[2], envol(nom[3]), cex = .82, col = COL$ink, font = 2)

  # la carga media de cada factor, bajo su elipse; el número de indicadores
  # no se escribe: se ve en cuántas cajas cuelgan
  pie_factor <- function(x, y, lm)
    text(x, y - ry - 3.6, sprintf("carga %.2f", lm), cex = .74, col = COL$load, font = 2)
  pie_factor(p1[1], p1[2], loadM[1])
  pie_factor(p2[1], p2[2], loadM[2])
  pie_factor(cr[1], cr[2], loadM[3])

  # estructurales
  flecha_estr <- function(de, a, col, lwd) {
    dx <- a[1] - de[1]; dy <- a[2] - de[2]; L <- sqrt(dx^2 + dy^2)
    arrows(de[1] + rx * dx / L * 1.06, de[2] + ry * dy / L * 1.06,
           a[1] - rx * dx / L * 1.12, a[2] - ry * dy / L * 1.12,
           length = .13, col = col, lwd = lwd)
  }
  lw1 <- if (nullWhich == 1) 3.4 else 2
  lw2 <- if (nullWhich == 2) 3.4 else 2
  flecha_estr(p1, cr, COL$b1, lw1)
  flecha_estr(p2, cr, COL$b2, lw2)
  text(56, 69.5, sprintf("%.2f", b1), col = COL$b1, font = 2, cex = 1.08)
  text(56, 30.5, sprintf("%.2f", b2), col = COL$b2, font = 2, cex = 1.08)

  # correlacion entre predictores
  xs <- seq(0, 1, length.out = 60)
  bx <- (1 - xs)^2 * p1[1] + 2 * (1 - xs) * xs * 16 + xs^2 * p2[1]
  by <- (1 - xs)^2 * (p1[2] - ry) + 2 * (1 - xs) * xs * 50 + xs^2 * (p2[2] + ry)
  lines(bx, by, col = COL$corr, lwd = 2)
  arrows(bx[3], by[3], bx[1], by[1], length = .11, col = COL$corr, lwd = 2)
  arrows(bx[58], by[58], bx[60], by[60], length = .11, col = COL$corr, lwd = 2)
  text(23.5, 50, sprintf("%.2f", corXX), col = COL$corr, font = 2, cex = 1.08)

  text(50, 99, sprintf("H0:  beta del %s  =  0",
                       if (nullWhich == 1) "predictor 1" else "predictor 2"),
       col = COL$accent, cex = .92, font = 2)
  text(50, 2, "el trazo grueso marca el efecto del que se contrasta la hipotesis nula",
       col = COL$slate, cex = .78)
}

# ---------------------------------------------------------------------
# UNA FILA POR CONSTRUCTO: nombre · dimensiones · items por dimension · carga
# ---------------------------------------------------------------------
fila_constructo <- function(i, nombre, dim, items, carga) {
  div(class = "fila-constructo",
      textInput(paste0("nom", i), NULL, nombre),
      numericInput(paste0("dim", i), NULL, dim, min = 1, max = 20, step = 1),
      numericInput(paste0("it", i), NULL, items, min = 1, max = 40, step = 1),
      numericInput(paste0("ld", i), NULL, carga, min = .2, max = .95, step = .05))
}

# indicadores que aporta un constructo segun como se decida medirlo
n_indicadores <- function(dim, items, modo) {
  dim <- max(1, dim); items <- max(1, items)
  if (modo == "parcelas" && dim > 1) dim else dim * items
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
ui <- page_fillable(
  theme = tema,
  tags$head(tags$style(HTML(CSS)), tags$title("SemPowerLab")),

  div(class = "app-head",
      div(class = "marca", HTML("Dr. José Ventura-León")),
      h1("SemPowerLab"),
      div(class = "sub",
          "Tamaño de muestra para un modelo predictivo con variables latentes  ·  ",
          "envoltura de ", tags$code("semPower.powerRegression()", style = "color:#DCE3EB"))),

  layout_sidebar(
    sidebar = sidebar(
      width = 395, bg = COL$paper, padding = 14,

      div(class = "bloque",
          div(class = "rot", span(class = "n", "1"), "Tus tres constructos"),
          div(class = "cab-constructo",
              span("Constructo"), span("Dim."), span("Ítems/dim"), span("Carga")),
          fila_constructo("1", "Autorregulación", 5, 4, .70),
          fila_constructo("2", "Autoeficacia", 1, 10, .70),
          fila_constructo("3", "Satisfacción académica", 1, 6, .70),
          div(class = "ayuda",
              strong("Dim."), " es el número de dimensiones del constructo. Uno unidimensional
               lleva 1 y sus ítems; uno multidimensional (habilidades sociales con seis
               factores, por ejemplo) lleva 6 y los ítems que tiene cada factor.
               La ", strong("carga"), " es la media esperada de sus indicadores.")
      ),

      div(class = "bloque",
          div(class = "rot", span(class = "n", "2"), "Los efectos esperados"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("b1", "β del predictor 1", .30, min = .01, max = .95, step = .05),
            numericInput("b2", "β del predictor 2", .20, min = .01, max = .95, step = .05)),
          numericInput("corXX", "Correlación entre los predictores", .40,
                       min = 0, max = .95, step = .05),
          radioButtons("nullWhich", "¿De cuál efecto se contrasta H₀: β = 0?",
                       choices = c("El más pequeño (recomendado)" = "auto",
                                   "Del predictor 1" = "1",
                                   "Del predictor 2" = "2"),
                       selected = "auto"),
          div(class = "ayuda",
              "El que manda es el efecto menor: si dimensionas por el mayor, el otro se queda
               sin potencia y saldrá no significativo aunque exista.")
      ),

      div(class = "bloque",
          div(class = "rot", span(class = "n", "3"), "Qué cuelga de cada factor"),
          radioButtons("medida", NULL,
                       choices = c("Una parcela por dimensión" = "parcelas",
                                   "Todos los ítems como indicadores" = "items"),
                       selected = "parcelas"),
          uiOutput("resumen_medida"),
          div(class = "ayuda",
              "Un constructo multidimensional no entra entero: entra por sus partes.
               Con parcelas, cada dimensión se promedia y aporta un indicador, y las cargas
               suelen subir (.75 a .85). Con ítems entran todos, el modelo de medida pesa
               mucho más y las cargas bajan (.50 a .70). Un constructo de una sola dimensión
               entra siempre por sus ítems.")
      ),

      div(class = "bloque",
          div(class = "rot", span(class = "n", "4"), "El criterio de decisión"),
          radioButtons("modo", NULL,
                       choices = c("¿Cuántos participantes necesito?" = "apriori",
                                   "¿Qué potencia tengo con la muestra que puedo conseguir?" = "posthoc"),
                       selected = "apriori"),
          conditionalPanel("input.modo == 'apriori'",
                           sliderInput("power", "Potencia deseada (1 − β)", .50, .99, .80, step = .05)),
          conditionalPanel("input.modo == 'posthoc'",
                           numericInput("Nfijo", "Participantes disponibles", 250,
                                        min = 30, max = 20000, step = 10)),
          selectInput("alpha", "Nivel de significancia (α)",
                      c(".05" = .05, ".01" = .01, ".10" = .10), selected = .05)
      ),

      actionButton("calc", "Calcular", class = "btn btn-primary btn-calc w-100",
                   icon = icon("play")),
      div(class = "ayuda", style = "margin-top:9px",
          "Cálculo con semPower 2.1.1 (Moshagen & Bader, 2024) sobre R 4.4.1.")
    ),

    uiOutput("tarjetas"),

    navset_card_tab(
      nav_panel("Diagrama del modelo",
                plotOutput("diagrama", height = "530px"),
                div(class = "leyenda",
                    "Cada argumento de la función es una parte del dibujo: las flechas entre
                     elipses son los efectos estructurales, la curva de doble punta es la
                     correlación entre predictores y las flechas hacia las cajas son las cargas.
                     Hay ", strong("una caja por indicador"), ", así que el tamaño de cada bloque
                     enseña cuánto pesa el modelo de medida de ese factor: seis parcelas y treinta
                     ítems no se dibujan igual. Por encima de 22 indicadores se dibujan los
                     primeros y una marca con los que faltan.")),

      nav_panel("Curva de potencia",
                plotOutput("curva", height = "440px"),
                div(class = "leyenda", uiOutput("txt_curva"))),

      nav_panel("Qué pasa si cambian los supuestos",
                div(style = "padding:6px 0 14px 0",
                    actionButton("sens", "Calcular la tabla", class = "btn btn-outline-secondary btn-sm"),
                    span(class = "leyenda", style = "margin-left:12px",
                         "24 modelos: el efecto menor cruzado con la carga factorial media.")),
                uiOutput("tabla_sens")),

      nav_panel("Código R reproducible",
                uiOutput("codigo"),
                div(style = "margin-top:14px",
                    downloadButton("dl_codigo", "Descargar el .R",
                                   class = "btn btn-outline-secondary btn-sm")),
                div(class = "leyenda",
                    "Pega este bloque en tu script y guárdalo junto a la base de datos: es el
                     respaldo del número que aparece en la tesis.")),

      nav_panel("Párrafo para la tesis",
                uiOutput("parrafo"),
                div(style = "margin-top:14px",
                    downloadButton("dl_texto", "Descargar el párrafo",
                                   class = "btn btn-outline-secondary btn-sm")),
                div(class = "leyenda",
                    "Revisa las cifras y completa lo que está entre corchetes antes de pegarlo
                     en la sección Participantes."))
    )
  )
)

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  cual <- reactive({
    if (input$nullWhich == "auto") {
      if (isTruthy(input$b1) && isTruthy(input$b2) && abs(input$b2) <= abs(input$b1)) 2L else 1L
    } else as.integer(input$nullWhich)
  })

  entrada <- reactive({
    dim   <- c(input$dim1, input$dim2, input$dim3)
    items <- c(input$it1,  input$it2,  input$it3)
    nInd  <- mapply(n_indicadores, dim, items, MoreArgs = list(modo = input$medida))
    list(b1 = input$b1, b2 = input$b2, corXX = input$corXX,
         dim = dim, items = items, medida = input$medida,
         nInd = as.integer(nInd), loadM = c(input$ld1, input$ld2, input$ld3),
         alpha = as.numeric(input$alpha), power = input$power,
         modo = input$modo, N = input$Nfijo, w = cual(),
         nom = c(input$nom1, input$nom2, input$nom3))
  })

  # lo que va a colgar de cada factor, dicho en la propia barra lateral
  output$resumen_medida <- renderUI({
    e <- entrada()
    req(all(is.finite(e$nInd)))
    unidim <- e$medida == "parcelas" & e$dim <= 1
    div(class = "resumen-ind",
        "Indicadores por factor: ", tags$b(sprintf("c(%d, %d, %d)", e$nInd[1], e$nInd[2], e$nInd[3])),
        if (any(unidim))
          span(class = "aviso-mini",
               sprintf("%s %s una sola dimensión: %s por sus ítems aunque elijas parcelas.",
                       paste(e$nom[unidim], collapse = " y "),
                       if (sum(unidim) > 1) "tienen" else "tiene",
                       if (sum(unidim) > 1) "entran" else "entra")),
        if (any(e$nInd < 2))
          span(class = "aviso-mini", "Un factor con menos de dos indicadores no se identifica."))
  })

  res <- eventReactive(input$calc, ignoreNULL = FALSE, {
    e <- entrada()
    validate(
      need(isTruthy(e$b1) && isTruthy(e$b2), "Escribe los dos efectos."),
      need(all(abs(c(e$b1, e$b2)) > 0 & abs(c(e$b1, e$b2)) < 1), "Los efectos van entre .01 y .95."),
      need(e$corXX > -1 && e$corXX < 1, "La correlación va entre −.95 y .95."),
      need(all(is.finite(e$nInd)) && all(e$nInd >= 2),
           "Cada factor necesita al menos dos indicadores: revisa las dimensiones y los ítems."),
      need(all(is.finite(e$loadM)) && all(e$loadM > .1 & e$loadM < 1),
           "Las cargas factoriales van entre .20 y .95.")
    )
    out <- tryCatch(
      correr(e$b1, e$b2, e$corXX, e$w, e$nInd, e$loadM, e$alpha, e$power, e$modo, e$N),
      error = function(err) list(ok = FALSE, msg = conditionMessage(err))
    )
    # la especificacion que produjo ESTE resultado: tarjetas, curva, script y
    # parrafo la leen de aqui para no mezclarse con entradas aun no calculadas
    out$e <- e
    out
  })

  # ---- tarjetas de resultado ----
  output$tarjetas <- renderUI({
    r <- res(); e <- r$e
    if (isFALSE(r$ok))
      return(div(class = "aviso", strong("semPower no pudo con esta especificación. "), r$msg))

    bcontr <- if (e$w == 1) e$b1 else e$b2
    if (r$modo == "apriori") {
      v1 <- format(r$N, big.mark = " "); r1 <- "Participantes necesarios"
      p1 <- sprintf("para detectar β = %.2f con potencia %.2f", bcontr, e$power)
    } else {
      v1 <- sprintf("%.3f", r$power); r1 <- "Potencia alcanzada"
      p1 <- sprintf("con %s participantes, para β = %.2f", format(r$N, big.mark = " "), bcontr)
    }
    if (r$modo == "apriori") {
      t2 <- div(class = "tar", div(class = "rot", "Potencia lograda"),
                div(class = "val", sprintf("%.3f", r$power)),
                div(class = "pie", "el redondeo hacia arriba de N la deja algo por encima de la pedida"))
    } else {
      n80 <- n_para(.80, r$fmin, r$df, e$alpha)
      falta <- n80 - r$N
      t2 <- div(class = "tar", div(class = "rot", "N para llegar a .80"),
                div(class = "val", if (n80 >= 20000) "> 20 000" else format(n80, big.mark = " ")),
                div(class = "pie", if (falta > 0)
                  sprintf("te faltan %d participantes con esta especificación", falta)
                  else "la muestra disponible ya supera ese umbral"))
    }
    div(class = "tarjetas",
        div(class = "tar clave", div(class = "rot", r1),
            div(class = "val", v1), div(class = "pie", p1)),
        t2,
        div(class = "tar", div(class = "rot", "Efecto del contraste (F₀)"),
            div(class = "val", sprintf("%.4f", r$fmin)),
            div(class = "pie", "lo que el modelo pierde si ese β fuera cero")),
        div(class = "tar", div(class = "rot", "Grados de libertad"),
            div(class = "val", r$df),
            div(class = "pie", "un solo parámetro fijado a cero")))
  })

  # ---- diagrama ----
  output$diagrama <- renderPlot({
    e <- entrada()
    validate(need(all(e$nInd >= 2), "Cada factor necesita al menos dos indicadores."))
    dibujar_modelo(e$nom, e$b1, e$b2, e$corXX, e$loadM, e$nInd, e$w)
  }, res = 96)

  # ---- curva de potencia ----
  output$curva <- renderPlot({
    r <- res(); e <- r$e
    validate(need(isTRUE(r$ok), "Sin resultado."))
    nmax <- max(600, ceiling(r$N * 1.7 / 50) * 50)
    d <- data.frame(N = seq(30, nmax, by = 5))
    d$pot <- pot_en(d$N, r$fmin, r$df, e$alpha)
    obj <- if (r$modo == "apriori") e$power else r$power

    ggplot(d, aes(N, pot)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = obj,
               fill = COL$accent, alpha = .045) +
      geom_line(linewidth = 1.25, colour = COL$navy) +
      geom_hline(yintercept = obj, linetype = "22", colour = COL$accent, linewidth = .7) +
      geom_vline(xintercept = r$N, linetype = "22", colour = COL$accent, linewidth = .7) +
      geom_point(data = data.frame(N = r$N, pot = r$power), size = 3.4, colour = COL$accent) +
      annotate("label", x = r$N, y = min(r$power, .96), hjust = -.08,
               label = sprintf(" N = %d · potencia = %.3f ", r$N, r$power),
               fill = "white", colour = COL$accent, label.size = .3, size = 3.9,
               family = "sans", fontface = 2) +
      scale_y_continuous("Potencia", limits = c(0, 1), breaks = seq(0, 1, .1),
                         expand = expansion(0)) +
      scale_x_continuous("Participantes (N)", expand = expansion(c(0, .02))) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = COL$line),
            axis.title = element_text(colour = COL$slate, face = 2),
            axis.text  = element_text(colour = COL$slate),
            plot.margin = margin(10, 18, 6, 6))
  }, res = 96)

  output$txt_curva <- renderUI({
    r <- res(); e <- r$e
    if (isFALSE(r$ok)) return(NULL)
    n70 <- n_para(.70, r$fmin, r$df, e$alpha)
    n90 <- n_para(.90, r$fmin, r$df, e$alpha)
    HTML(sprintf(
      "La curva sale del efecto F<sub>0</sub> = %.4f y de los %d grado(s) de libertad que devuelve
       semPower, no de una aproximación. Con esta misma especificación, una potencia de .70 se
       alcanza con <b>%s</b> participantes y una de .90 con <b>%s</b>. La zona sombreada es el
       territorio en el que el estudio ya no distingue ese efecto del azar.",
      r$fmin, r$df, format(n70, big.mark = " "), format(n90, big.mark = " ")))
  })

  # ---- tabla de sensibilidad ----
  tabla <- eventReactive(input$sens, {
    e <- entrada()
    betas  <- c(.10, .15, .20, .25, .30, .35)
    cargas <- c(.50, .60, .70, .80)
    m <- matrix(NA_integer_, length(betas), length(cargas),
                dimnames = list(sprintf("%.2f", betas), sprintf("%.2f", cargas)))
    withProgress(message = "Calculando 24 modelos", value = 0, {
      for (i in seq_along(betas)) for (j in seq_along(cargas)) {
        sl <- if (e$w == 1) c(betas[i], e$b2) else c(e$b1, betas[i])
        out <- tryCatch(
          correr(sl[1], sl[2], e$corXX, e$w, e$nInd, cargas[j], e$alpha,
                 if (e$modo == "apriori") e$power else .80, "apriori")$N,
          error = function(err) NA_integer_)
        m[i, j] <- out
        incProgress(1 / 24)
      }
    })
    list(m = m, betas = betas, cargas = cargas, e = e)
  })

  output$tabla_sens <- renderUI({
    t <- tabla(); e <- t$e
    bact <- if (e$w == 1) e$b1 else e$b2
    filas <- lapply(seq_along(t$betas), function(i) {
      celdas <- lapply(seq_along(t$cargas), function(j) {
        aqui <- abs(t$betas[i] - bact) < .001 &&
                length(unique(e$loadM)) == 1 && abs(t$cargas[j] - e$loadM[1]) < .001
        tags$td(class = if (aqui) "aqui" else NULL,
                if (is.na(t$m[i, j])) "—" else format(t$m[i, j], big.mark = " "))
      })
      tags$tr(tags$td(class = "fila", sprintf("β = %.2f", t$betas[i])), celdas)
    })
    tagList(
      tags$table(class = "tabla-s",
                 tags$tr(tags$th("Efecto contrastado"),
                         lapply(t$cargas, function(c) tags$th(sprintf("carga %.2f", c)))),
                 filas),
      div(class = "leyenda",
          sprintf("Participantes necesarios con potencia %.2f y α = %.2f, manteniendo
                   c(%s) indicadores y una correlación de %.2f entre predictores. La columna
                   fija la misma carga en los tres factores, de modo que la celda solo queda
                   resaltada si tu especificación también las tiene iguales. Bajar la carga
                   media de .70 a .50 encarece el estudio más que recortar el efecto esperado.",
                  if (e$modo == "apriori") e$power else .80, e$alpha,
                  paste(e$nInd, collapse = ", "), e$corXX)))
  })

  # ---- código reproducible ----
  codigo_txt <- reactive({
    e <- res()$e
    ln <- function(arg, val, cmt = "")
      sprintf("  %-10s = %-17s %s", arg, val, if (nzchar(cmt)) paste("#", cmt) else "")
    sinacento <- function(s)
      chartr("áéíóúÁÉÍÓÚñÑüÜ", "aeiouAEIOUnNuU", s)

    cuerpo <- if (e$modo == "apriori")
      c(ln("type", '"a-priori",', "quiero la N, no la potencia de una N ya fijada"),
        ln("power", sprintf("%.2f,", e$power), "potencia deseada"))
    else
      c(ln("type", '"post-hoc",', "quiero la potencia de una N ya fijada"),
        ln("N", sprintf("%d,", e$N), "participantes disponibles"))

    cuerpo <- c(
      cuerpo,
      ln("slopes", sprintf("c(%.2f, %.2f),", e$b1, e$b2),
         sinacento(sprintf("beta de %s y beta de %s", e$nom[1], e$nom[2]))),
      ln("corXX", sprintf("%.2f,", e$corXX), "correlacion esperada entre los predictores"),
      ln("nullEffect", '"slope = 0",', "la hipotesis nula que se contrasta"),
      ln("nullWhich", sprintf("%d,", e$w),
         sprintf("sobre CUAL efecto: el %s", if (e$w == 2) "segundo" else "primero")),
      ln("nIndicator", sprintf("c(%d, %d, %d),", e$nInd[3], e$nInd[1], e$nInd[2]),
         sinacento(sprintf("indicadores de %s (criterio, va primero), %s y %s",
                           e$nom[3], e$nom[1], e$nom[2]))),
      ln("loadM", if (length(unique(e$loadM)) == 1)
                    sprintf("%.2f,", e$loadM[1])
                  else sprintf("c(%.2f, %.2f, %.2f),", e$loadM[3], e$loadM[1], e$loadM[2]),
         if (length(unique(e$loadM)) == 1) "carga factorial media esperada"
         else "carga media de cada factor, en el mismo orden"),
      ln("alpha", sprintf("%.2f", e$alpha), "nivel de significancia")
    )

    paste0(
      "library(semPower)\n\nres <- semPower.powerRegression(\n",
      paste(sub("\\s+$", "", cuerpo), collapse = "\n"),
      "\n)\n\nsummary(res)\n",
      if (e$modo == "apriori") "res$requiredN" else "res$power", "\n")
  })

  output$codigo <- renderUI({
    txt <- codigo_txt()
    h <- htmltools::htmlEscape(txt)
    h <- gsub("(#[^\n]*)", '<span class="cmt">\\1</span>', h)
    h <- gsub("(&quot;[^&]*&quot;)", '<span class="str">\\1</span>', h)
    h <- gsub("(semPower.powerRegression|library|summary)", '<span class="fun">\\1</span>', h)
    div(class = "consola", HTML(h))
  })

  # ---- párrafo ----
  # decimal sin el cero inicial, como pide APA para r, beta, alfa y potencia
  d2 <- function(x) {
    s <- sprintf("%.2f", x)
    if (startsWith(s, "0.")) substring(s, 2)
    else if (startsWith(s, "-0.")) paste0("-", substring(s, 3))
    else s
  }

  # los seis elementos obligatorios del párrafo, cada uno como un tramo
  tramos <- reactive({
    r <- res(); e <- r$e
    if (isFALSE(r$ok)) return(character(0))
    bcontr <- if (e$w == 1) e$b1 else e$b2
    pw <- d2(if (e$modo == "apriori") e$power else r$power)
    c(
      sprintf(
"El tamaño muestral se estimó mediante análisis de potencia para modelos de ecuaciones \
estructurales con el paquete semPower (Moshagen & Bader, 2024) en R 4.4.1."),
      sprintf(
"Se especificó un modelo con dos predictores latentes correlacionados (r = %s), %s y %s, \
y un criterio latente, %s.",
        d2(e$corXX), e$nom[1], e$nom[2], e$nom[3]),
      sprintf(
"Los tres factores se midieron con %d, %d y %d indicadores respectivamente, obtenidos %s, \
con %s, %s de las propiedades psicométricas reportadas para los instrumentos empleados.",
        e$nInd[1], e$nInd[2], e$nInd[3],
        if (e$medida == "parcelas" && any(e$dim > 1))
          "promediando los ítems de cada dimensión en una parcela por dimensión en los constructos multidimensionales"
        else "tomando cada ítem como un indicador",
        if (length(unique(e$loadM)) == 1)
          sprintf("una carga factorial media de %s", d2(e$loadM[1]))
        else sprintf("cargas factoriales medias de %s, %s y %s respectivamente",
                     d2(e$loadM[1]), d2(e$loadM[2]), d2(e$loadM[3])),
        if (length(unique(e$loadM)) == 1) "valor tomado" else "valores tomados"),
      if (e$modo == "apriori")
        sprintf("Se fijaron α = %s y una potencia (1 − β) = %s.", d2(e$alpha), pw)
      else
        sprintf("Se fijó α = %s y se calculó la potencia (1 − β) alcanzada con la muestra disponible.",
                d2(e$alpha)),
      if (e$modo == "apriori")
        sprintf(
"Bajo esos supuestos, la detección del menor coeficiente estructural de interés (β = %s) \
requiere %d participantes; se recomienda reclutar un [__] %% adicional para compensar la \
pérdida de casos y los cuestionarios incompletos.",
          d2(bcontr), r$N)
      else {
        n80 <- n_para(.80, r$fmin, r$df, e$alpha)
        sprintf(
"Bajo esos supuestos, con %d participantes la potencia para detectar el menor coeficiente \
estructural de interés (β = %s) es de %s%s.",
          r$N, d2(bcontr), pw,
          if (r$power < .80)
            sprintf(", por debajo del .80 convencional, que requeriría %s participantes",
                    if (n80 >= 20000) "más de 20 000" else format(n80, big.mark = " "))
          else "")
      },
      sprintf(
"El cálculo es reproducible con el script adjunto y se acompaña del análisis de sensibilidad \
correspondiente: con la muestra finalmente obtenida, el estudio detecta efectos estructurales \
de β ≥ %s con una potencia de %s.",
        d2(bcontr), d2(r$power))
    )
  })

  parrafo_txt <- reactive({
    tr <- tramos()
    if (!length(tr)) "" else paste(tr, collapse = " ")
  })

  ROTULOS <- c("paquete y versión", "modelo declarado", "indicadores y cargas",
               "α y potencia", "muestra resultante", "sensibilidad")

  output$parrafo <- renderUI({
    r <- res()
    if (isFALSE(r$ok)) return(div(class = "aviso", "Corrige la especificación para generar el texto."))
    tr <- tramos()
    esc <- function(x) gsub("&", "&amp;", x, fixed = TRUE)
    cuerpo <- paste(sprintf('<span class="tramo s%d" data-k="%d">%s</span>',
                            seq_along(tr), seq_along(tr), esc(tr)), collapse = " ")
    div(
      div(class = "parrafo", id = "parrafoTesis", HTML(cuerpo)),
      div(class = "chips-6",
          lapply(seq_along(ROTULOS), function(i)
            div(class = paste0("chip6 k", i), `data-k` = i,
                tags$b(i), ROTULOS[i]))),
      tags$script(HTML("
        (function(){
          var p = document.getElementById('parrafoTesis');
          if (!p) return;
          var chips = document.querySelectorAll('.chip6');
          function enfocar(k){
            p.classList.add('enfoque');
            p.querySelectorAll('.tramo').forEach(function(t){
              t.classList.toggle('on', t.getAttribute('data-k') === String(k));
            });
          }
          function soltar(){
            p.classList.remove('enfoque');
            p.querySelectorAll('.tramo.on').forEach(function(t){ t.classList.remove('on'); });
          }
          chips.forEach(function(c){
            var k = c.getAttribute('data-k');
            c.addEventListener('mouseenter', function(){ enfocar(k); });
            c.addEventListener('mouseleave', soltar);
          });
          p.querySelectorAll('.tramo').forEach(function(t){
            t.addEventListener('mouseenter', function(){ enfocar(t.getAttribute('data-k')); });
            t.addEventListener('mouseleave', soltar);
          });
        })();
      ")),
      div(class = "leyenda",
          HTML("Referencia del paquete: Moshagen, M., &amp; Bader, M. (2024). semPower: General
                power analysis for structural equation models. <i>Behavior Research Methods,
                56</i>(4), 2901–2922. https://doi.org/10.3758/s13428-023-02254-7")))
  })

  # ---- descargas ----
  output$dl_codigo <- downloadHandler(
    filename = function() sprintf("semPower_tamano_muestra_%s.R", format(Sys.Date(), "%Y%m%d")),
    content  = function(file) writeLines(codigo_txt(), file, useBytes = TRUE)
  )
  output$dl_texto <- downloadHandler(
    filename = function() sprintf("parrafo_participantes_%s.txt", format(Sys.Date(), "%Y%m%d")),
    content  = function(file) writeLines(parrafo_txt(), file, useBytes = TRUE)
  )
}

shinyApp(ui, server)
