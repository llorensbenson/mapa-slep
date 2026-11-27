## =============================================================================
## PILOTO — Mapa nacional SLEP (Chile por región → comunas)
## Observatorio de Políticas Educativas | Rumbo Colectivo
##
## QUÉ HACE: muestra Chile por regiones; al hacer clic en una región se ven sus
## comunas. Un control de año (2015-2025) cambia el mapa en ambos niveles.
##   - Nivel región: color según % de la matrícula pública que está en SLEP.
##   - Nivel comuna: color según quién administra la educación pública
##     (SLEP, DAEM o Corporación Municipal), con hover por comuna.
##   - En el hover, la ficha y la comparación: ingresos, deuda flotante y SIMCE
##     desde la planilla única nacional (todas las comunas, pesos de 2025).
##
## ARCHIVOS (misma carpeta que el script):
##   - base_nacional_comunas_2015_2025.csv  (Mineduc, todas las comunas)
##   - planilla_unica_Chile_2015_2025.xlsx  (ingresos, deuda flotante y SIMCE, pesos de 2025)
## SALIDA: mapa.html, solo el mapa a página completa (el Rmd genera la versión con
##         la nota metodológica, mapa_slep_chile_piloto.html)
## =============================================================================

## ---- configuracion ----
archivo_nacional <- "base_nacional_comunas_2015_2025.csv"
# Planilla con ingresos, deuda y SIMCE: la nacional (o, si no está, la de la RM)
archivo_rm       <- if (file.exists("planilla_unica_Chile_2015_2025.xlsx")) "planilla_unica_Chile_2015_2025.xlsx" else "planilla_unica_RM_2015_2025.xlsx"
archivo_salida   <- "mapa.html"        # solo el mapa, a página completa (para el sitio web)
link_metodologia <- "index.html"       # página con la nota metodológica (el Rmd tejido); "" = sin link

# Comunas insulares que no se dibujan (alejan el zoom de su región); siguen en los datos
comunas_no_dibujadas <- c("05201" = "Isla de Pascua", "05104" = "Juan Fernández")
# Además se quitan las islas oceánicas que forman parte de otra comuna (las Desventuradas,
# San Félix y San Ambrosio, son parte de la comuna de Valparaíso): sin esto, el zoom de la
# región incluye 9° de océano y las comunas quedan diminutas
longitud_minima <- -76

colores_adm <- c("SLEP"        = "#2C5F6F",
                 "DAEM"        = "#D9C9A3",
                 "Corporación" = "#A89A80")

simce_sin_puntaje <- c("2019" = "Sin SIMCE comparable ese año",
                       "2020" = "No hubo SIMCE ese año (pandemia)",
                       "2021" = "No hubo SIMCE ese año (pandemia)")

## ---- paquetes ----
# Usa el pipe nativo de R (|>), disponible desde R 4.1
paquetes <- c("readxl", "dplyr", "sf", "plotly", "htmlwidgets", "htmltools",
              "jsonlite", "stringi", "chilemapas")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) {
  stop("Faltan paquetes: ", paste(faltan, collapse = ", "),
       ".\nReinicia R (Session > Restart R) y corre en la consola:\n",
       'install.packages(c("', paste(faltan, collapse = '", "'), '"))',
       "\nLuego vuelve a correr el script.")
}
suppressPackageStartupMessages({
  library(dplyr); library(sf); library(plotly); library(htmlwidgets); library(chilemapas)
})
sf_use_s2(FALSE)   # uniones de polígonos más tolerantes

## ---- datos ----
nac <- read.csv(archivo_nacional, colClasses = c(codigo_region = "character",
                                                 codigo_comuna = "character"),
                encoding = "UTF-8", check.names = FALSE)
anios <- sort(unique(nac$año))

fmt_num <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
fmt_millones <- function(x) case_when(
  is.na(x) ~ NA_character_, x == 0 ~ "$0",
  abs(x) < 1e6 ~ paste0("$", fmt_num(round(x))),
  TRUE ~ paste0("$", fmt_num(round(x / 1e6)), " millones"))

# Datos de la planilla única (nacional o RM): se usa su clasificación administrativa
# y se agregan ingresos, deuda y SIMCE al hover
rm_extra <- NULL
if (file.exists(archivo_rm)) {
  hoja <- grep("^Base", readxl::excel_sheets(archivo_rm), value = TRUE)[1]
  normalizar <- function(x) gsub("[^A-Z]", "", toupper(stringi::stri_trans_general(x, "Latin-ASCII")))
  txt_deuda <- function(v, mod, corto = FALSE) case_when(
    !is.na(v) ~ fmt_millones(v),
    grepl("^Corporaci", mod) ~ if (corto) "No registrada (Corp.)" else "No registrada (Corporación: fuera de la base de Contraloría)",
    grepl("sin deuda", mod) ~ "$0",                     # sin ítem 34.07 ese año = cero
    grepl("sin área", mod) ~ "No registrada",
    TRUE ~ if (corto) "s/d" else "Sin dato")
  simce_txt <- function(a, l, m) ifelse(as.character(a) %in% names(simce_sin_puntaje),
    unname(simce_sin_puntaje[as.character(a)]),
    paste0("L ", coalesce(as.character(l), "s/d"), " · M ", coalesce(as.character(m), "s/d")))
  # Montos en pesos reales de 2025 (columnas *_pesos_2025 de la planilla, deflactadas con
  # el IPC promedio anual); los nominales quedan en la planilla como registro de la fuente
  planilla <- readxl::read_excel(archivo_rm, sheet = hoja)
  rm_extra <- planilla |>
    mutate(k = if ("codigo_comuna" %in% names(planilla)) codigo_comuna else normalizar(comuna)) |>
    mutate(ingreso_total_educ_pesos = ingreso_total_educ_pesos_2025,
           deuda_flotante_educ_devengado = deuda_flotante_educ_devengado_pesos_2025) |>
    transmute(
      k, año, comuna_rm = comuna,
      adm_rm = case_when(!is.na(slep) ~ "SLEP",
                         grepl("^Corporaci", modalidad_administrativa) ~ "Corporación",
                         TRUE ~ "DAEM"),
      slep_rm = slep,
      extra = paste0(
        "<b>Ingresos en educación ($ de 2025):</b> ", coalesce(fmt_millones(ingreso_total_educ_pesos), "Sin dato"), "<br>",
        "<b>Deuda flotante ($ de 2025):</b> ", txt_deuda(deuda_flotante_educ_devengado, modalidad_administrativa), "<br>",
        "<b>SIMCE 4° básico:</b> ", simce_txt(año, simce_4basico_lectura, simce_4basico_matematica), "<br>",
        "<b>SIMCE 2° medio:</b> ", simce_txt(año, simce_2medio_lectura, simce_2medio_matematica),
        ifelse(grepl("parcial", fuente_financiera),
               "<br><i>Año de traspaso: ingresos de solo una parte del año</i>",
               ifelse(!is.na(slep), "<br><i>Ingresos y deuda: SLEP completo</i>", ""))),
      # versiones cortas para la ficha por comuna
      f_ingreso = coalesce(fmt_millones(ingreso_total_educ_pesos), "s/d"),
      f_deuda   = txt_deuda(deuda_flotante_educ_devengado, modalidad_administrativa, corto = TRUE),
      f_simce4  = ifelse(as.character(año) %in% names(simce_sin_puntaje), "Sin SIMCE",
                         simce_txt(año, simce_4basico_lectura, simce_4basico_matematica)),
      f_simce2  = ifelse(as.character(año) %in% names(simce_sin_puntaje), "Sin SIMCE",
                         simce_txt(año, simce_2medio_lectura, simce_2medio_matematica)),
      # valores numéricos para el gráfico de comparación
      n_ingreso = ingreso_total_educ_pesos,
      n_s4l = simce_4basico_lectura, n_s4m = simce_4basico_matematica,
      n_s2l = simce_2medio_lectura,  n_s2m = simce_2medio_matematica)
  usa_codigo <- !all(grepl("[A-Z]", rm_extra$k))
  nac <- nac |>
    mutate(k = if (usa_codigo) codigo_comuna else normalizar(nombre_comuna)) |>
    left_join(rm_extra, by = c("k", "año")) |>
    mutate(administracion = coalesce(adm_rm, administracion),
           nombre_slep    = ifelse(is.na(extra), nombre_slep, slep_rm),
           nombre_comuna  = coalesce(comuna_rm, nombre_comuna)) |>   # con tildes
    select(-k, -adm_rm, -slep_rm, -comuna_rm)
  if (usa_codigo && any(is.na(nac$extra)))
    warning("Hay comunas que no cruzaron con la planilla.")
}
for (v in c("extra", "f_ingreso", "f_deuda", "f_simce4", "f_simce2"))
  if (is.null(nac[[v]])) nac[[v]] <- NA_character_
for (v in c("n_ingreso", "n_s4l", "n_s4m", "n_s2l", "n_s2m"))
  if (is.null(nac[[v]])) nac[[v]] <- NA_real_

# Nombres de región con tildes (chilemapas los trae sin tildes)
nombres_region <- c("01" = "Tarapacá", "02" = "Antofagasta", "03" = "Atacama", "04" = "Coquimbo",
  "05" = "Valparaíso", "06" = "O'Higgins", "07" = "Maule", "08" = "Biobío",
  "09" = "La Araucanía", "10" = "Los Lagos", "11" = "Aysén", "12" = "Magallanes",
  "13" = "Metropolitana", "14" = "Los Ríos", "15" = "Arica y Parinacota", "16" = "Ñuble")
nac$nombre_region <- unname(nombres_region[nac$codigo_region])

## ---- comunas (hover y color por año) ----
comunas <- nac |>
  mutate(
    txt_adm = ifelse(administracion == "SLEP",
                     paste0("SLEP ", coalesce(nombre_slep, "(nombre s/d)")),
                     paste0("Sin SLEP · ", ifelse(administracion == "DAEM", "DAEM", "Corporación Municipal"))),
    hover = paste0(
      "<b>", nombre_comuna, "</b> · ", año, "<br>",
      "<b>Administración:</b> ", txt_adm, "<br>",
      "<b>Matrícula pública:</b> ", fmt_num(matricula_publica),
      " de ", fmt_num(matricula_total), " (",
      round(100 * matricula_publica / pmax(matricula_total, 1)), "%)",
      ifelse(is.na(extra), "", paste0("<br>", extra)),
      "<br><i>Clic para ver su ficha</i>"),
    color = unname(colores_adm[administracion])
  )

## ---- fichas (trayectoria por comuna, al hacer clic) ----
fichas <- comunas |>
  arrange(codigo_comuna, año) |>
  transmute(
    codigo_comuna, nombre_comuna, anio = año,
    adm = ifelse(administracion == "SLEP", paste0("SLEP ", coalesce(nombre_slep, "")),
                 administracion),
    mat = paste0(fmt_num(matricula_publica), " (",
                 round(100 * matricula_publica / pmax(matricula_total, 1)), "%)"),
    ingreso = f_ingreso, deuda = f_deuda, simce4 = f_simce4, simce2 = f_simce2
  )
# Series numéricas para comparar comunas. El ingreso por estudiante de un SLEP se calcula
# sobre la matrícula pública de TODO su territorio (el ingreso es del servicio completo)
series <- comunas |>
  group_by(año, nombre_slep_g = ifelse(administracion == "SLEP", nombre_slep, NA)) |>
  mutate(mat_territorio = ifelse(is.na(nombre_slep_g), matricula_publica, sum(matricula_publica))) |>
  ungroup() |>
  arrange(codigo_comuna, año) |>
  group_by(codigo_comuna) |>
  mutate(g = if (any(administracion == "SLEP")) min(año[administracion == "SLEP"]) else NA_real_) |>
  ungroup() |>
  transmute(codigo_comuna, nombre_region, g, anio = año,
            mat_pub = matricula_publica,
            pct_pub = round(100 * matricula_publica / pmax(matricula_total, 1), 1),
            ingreso = round(n_ingreso / 1e6),
            ingreso_pe = round(n_ingreso / mat_territorio / 1e6, 2),
            s4l = n_s4l, s4m = n_s4m, s2l = n_s2l, s2m = n_s2m)
series <- split(series, series$codigo_comuna)

fichas <- lapply(split(fichas, fichas$codigo_comuna), function(d) {
  s <- series[[d$codigo_comuna[1]]]
  list(
    nombre = d$nombre_comuna[1],
    region = s$nombre_region[1],
    g = if (is.na(s$g[1])) NULL else s$g[1],     # año de traspaso a SLEP
    completa = !all(is.na(d$ingreso)),           # RM: con ingresos, deuda y SIMCE
    filas = lapply(seq_len(nrow(d)), function(i)
      as.list(d[i, c("anio", "adm", "mat", "ingreso", "deuda", "simce4", "simce2")])),
    serie = as.list(s[c("anio", "mat_pub", "pct_pub", "ingreso", "ingreso_pe",
                        "s4l", "s4m", "s2l", "s2m")]))
})

## ---- regiones (resumen por año) ----
regiones <- comunas |>
  group_by(codigo_region, nombre_region, año) |>
  summarise(
    n_comunas = n(),
    n_slep    = sum(administracion == "SLEP"),
    mat_pub   = sum(matricula_publica),
    mat_slep  = sum(matricula_slep),
    sleps     = paste(sort(unique(na.omit(nombre_slep))), collapse = ", "),
    .groups = "drop"
  ) |>
  mutate(
    pct_slep = round(100 * mat_slep / pmax(mat_pub, 1)),
    hover = paste0(
      "<b>", nombre_region, "</b> · ", año, "<br>",
      "<b>Comunas en SLEP:</b> ", n_slep, " de ", n_comunas, "<br>",
      "<b>Matrícula pública en SLEP:</b> ", pct_slep, "%",
      ifelse(sleps == "", "", paste0("<br><b>SLEP:</b> ", sleps)),
      "<br><i>Clic para ver sus comunas</i>")
  )

## ---- geometria ----
# Los polígonos se pasan como coordenadas (x = longitud, y = latitud) y se dibujan
# como áreas rellenas: no depende de mapas base externos (funciona sin internet).
simplificar <- function(x, tol) x |> st_transform(32719) |>
  st_simplify(dTolerance = tol, preserveTopology = TRUE) |> st_transform(4326)
a_coords <- function(g) {
  # Une todos los anillos de un (multi)polígono, separados por NA (= null en JS)
  m <- st_coordinates(g)
  grupos <- interaction(m[, "L1"], if ("L2" %in% colnames(m)) m[, "L2"] else 1,
                        if ("L3" %in% colnames(m)) m[, "L3"] else 1, drop = TRUE)
  partes <- split(seq_len(nrow(m)), grupos)
  idx <- unlist(lapply(partes, function(i) c(i, NA)))
  list(x = round(m[idx, "X"], 3), y = round(m[idx, "Y"], 3))
}
a_lista <- function(sfobj) setNames(lapply(st_geometry(sfobj), a_coords), sfobj$id)

sin_islas <- function(g) {
  partes <- st_cast(st_sfc(g), "POLYGON")
  lejos  <- vapply(partes, function(p) st_bbox(p)[["xmin"]] < longitud_minima, logical(1))
  if (!any(lejos)) return(g)
  st_combine(partes[!lejos])[[1]]
}
geo_com <- st_as_sf(mapa_comunas) |>
  filter(!codigo_comuna %in% names(comunas_no_dibujadas)) |>
  select(id = codigo_comuna, region = codigo_region)
st_geometry(geo_com) <- st_sfc(lapply(st_geometry(geo_com), sin_islas), crs = st_crs(geo_com))
geo_com <- st_make_valid(geo_com)
# Regiones = unión de sus comunas (no depende de rmapshaper)
geo_reg <- geo_com |> group_by(id = region) |> summarise(.groups = "drop")

pol_com <- a_lista(simplificar(geo_com, 250))
pol_reg <- a_lista(simplificar(geo_reg, 2000))

sin_poligono <- setdiff(unique(comunas$codigo_comuna), c(geo_com$id, names(comunas_no_dibujadas)))
if (length(sin_poligono)) stop("Comunas sin polígono: ", paste(sin_poligono, collapse = ", "))

## ---- datos para el navegador ----
# Por año: vectores alineados (código, valor/color, hover) para cada nivel
por_anio <- function(df, cols) lapply(split(df, df$año), function(d) as.list(d[cols]))
datos_js <- list(
  anios   = anios,
  pol_reg = pol_reg,
  pol_com = pol_com,
  reg     = por_anio(regiones, c("codigo_region", "pct_slep", "hover")),
  com     = por_anio(comunas |> filter(codigo_comuna %in% geo_com$id),
                     c("codigo_comuna", "codigo_region", "color", "hover")),
  nombres_reg  = setNames(as.list(unique(regiones$nombre_region)), unique(regiones$codigo_region)),
  colores      = as.list(colores_adm),
  fichas       = fichas,
  no_dibujadas = c(unname(comunas_no_dibujadas), "las islas Desventuradas")
)

## ---- mapa ----
# El widget parte vacío; el JavaScript dibuja la vista nacional o regional
widget <- plot_ly(type = "scatter", mode = "lines", x = numeric(0), y = numeric(0),
                  height = 820) |>
  layout(font = list(family = "Titillium Web"),
         margin = list(l = 0, r = 0, t = 10, b = 0),
         hoverlabel = list(bgcolor = "#FAF7F0", align = "left",
                           font = list(family = "Titillium Web", size = 12))) |>
  # Botones de zoom (+, −, encuadre inicial) para comunas pequeñas; doble clic también
  # vuelve al encuadre de la región. En celular se puede además pellizcar la pantalla
  config(displaylogo = FALSE, displayModeBar = TRUE, scrollZoom = FALSE,
         modeBarButtons = list(list("zoomIn2d", "zoomOut2d", "resetScale2d")))

js <- "
function(el, x, D) {
  var gd = el, estado = {anio: D.anios[D.anios.length - 1], region: null, comuna: null, comparar: [], variable: 'mat_pub'};
  el.style.width = '100%'; el.style.maxWidth = '100%';   // en el Rmd el widget trae un ancho fijo
  // ---- barra de controles: volver, título, slider de año, leyenda ----
  var barra = document.createElement('div');
  barra.style.cssText = 'font-family:\"Titillium Web\",sans-serif;display:flex;flex-wrap:wrap;' +
    'align-items:center;gap:10px 18px;padding:8px 4px 6px;border-bottom:1px solid #E5DFD3;margin-bottom:4px';
  barra.innerHTML =
    '<button id=\"volver\" style=\"display:none;border:1px solid #2C5F6F;background:#fff;color:#2C5F6F;' +
    'border-radius:4px;padding:3px 10px;cursor:pointer;font-family:inherit\">← Chile</button>' +
    '<b id=\"titulo\" style=\"font-size:17px;color:#2C5F6F\"></b>' +
    '<label style=\"display:flex;align-items:center;gap:6px\">Año ' +
    '<input id=\"anio\" type=\"range\" min=\"' + D.anios[0] + '\" max=\"' + D.anios[D.anios.length-1] +
    '\" step=\"1\" value=\"' + estado.anio + '\" style=\"width:220px;accent-color:#2C5F6F\"><b id=\"anio_txt\"></b></label>' +
    '<span id=\"leyenda\" style=\"font-size:13px;color:#444\"></span>';
  el.parentNode.insertBefore(barra, el);
  var nota = document.createElement('div');
  nota.style.cssText = 'font-family:\"Titillium Web\",sans-serif;font-size:12px;color:#777;margin-top:4px';
  el.parentNode.insertBefore(nota, el.nextSibling);
  // ---- ficha de la comuna (bajo el mapa; pensada también para celular) ----
  var ficha = document.createElement('div');
  ficha.style.cssText = 'font-family:\"Titillium Web\",sans-serif;font-size:13px;margin:10px 0 4px;' +
    'padding:10px 12px;border:1px solid #D9D2C3;border-radius:6px;background:#FAF7F0;display:none';
  nota.parentNode.insertBefore(ficha, nota.nextSibling);
  function esc(t) { return String(t === null || t === undefined ? 's/d' : t); }
  function renderFicha() {
    if (!estado.comuna || !D.fichas[estado.comuna]) { ficha.style.display = 'none'; return; }
    var F = D.fichas[estado.comuna], a = String(estado.anio);
    var th = function(t, s) { return '<th style=\"padding:5px 8px;text-align:left;white-space:nowrap;' + (s || '') + '\">' + t + '</th>'; };
    var td = function(t, s) { return '<td style=\"padding:5px 8px;white-space:nowrap;' + (s || '') + '\">' + esc(t) + '</td>'; };
    var fijo = 'position:sticky;left:0;background:inherit;';
    var h = '<div style=\"display:flex;justify-content:space-between;align-items:baseline;gap:8px\">' +
      '<b style=\"font-size:16px;color:#2C5F6F\">' + F.nombre + '</b>' +
      '<span style=\"white-space:nowrap\">' + botonComparar() +
      ' <a href=\"#\" class=\"cerrar\" style=\"color:#888;text-decoration:none;margin-left:10px\">✕ cerrar</a></span></div>' +
      '<div style=\"color:#777;font-size:12px;margin:2px 0 6px\">Trayectoria ' + D.anios[0] + '-' + D.anios[D.anios.length - 1] +
      '; el año del control queda destacado.' + (F.completa ? ' Ingresos y deuda de un SLEP corresponden al servicio completo.' : '') +
      '</div><div style=\"overflow-x:auto;-webkit-overflow-scrolling:touch\">' +
      '<table style=\"border-collapse:collapse;font-size:12.5px;width:100%\"><thead><tr style=\"border-bottom:2px solid #2C5F6F;background:#FAF7F0\">' +
      th('Año', fijo) + th('Administración') + th('Matrícula pública') +
      (F.completa ? th('Ingresos educación ($ 2025)') + th('Deuda flotante ($ 2025)') + th('SIMCE 4° básico') + th('SIMCE 2° medio') : '') +
      '</tr></thead><tbody>';
    F.filas.forEach(function(r) {
      var act = String(r.anio) === a, bg = act ? '#E3ECEE' : '#FAF7F0';
      h += '<tr style=\"border-bottom:1px solid #E5DFD3;background:' + bg + (act ? ';font-weight:600' : '') + '\">' +
        td(r.anio, fijo) + td(r.adm) + td(r.mat) +
        (F.completa ? td(r.ingreso) + td(r.deuda) + td(r.simce4) + td(r.simce2) : '') + '</tr>';
    });
    ficha.innerHTML = h + '</tbody></table></div>';
    ficha.style.display = '';
    ficha.querySelector('.cerrar').onclick = function(e) { e.preventDefault(); estado.comuna = null; dibujar(); };
    var bc = ficha.querySelector('.comparar');
    if (bc) bc.onclick = function() {
      var i = estado.comparar.indexOf(estado.comuna);
      if (i >= 0) estado.comparar.splice(i, 1);
      else if (estado.comparar.length < MAX_COMP) estado.comparar.push(estado.comuna);
      renderFicha(); renderComp();
      if (estado.comparar.length) comp.scrollIntoView({behavior: 'smooth', block: 'nearest'});
    };
  }

  // ---- comparación de comunas (hasta 4, de cualquier región) ----
  var MAX_COMP = 4, COLORES = ['#2C5F6F', '#D9772B', '#7A5195', '#5E9C46'];
  var VARIABLES = {
    mat_pub:    {t: 'Matrícula pública', u: 'estudiantes'},
    pct_pub:    {t: '% de la matrícula en establecimientos públicos', u: '%'},
    ingreso:    {t: 'Ingresos en educación (millones de $ de 2025)', u: 'MM$ de 2025', rm: true},
    ingreso_pe: {t: 'Ingreso por estudiante público (millones de $ de 2025)', u: 'MM$ de 2025', rm: true},
    s4l: {t: 'SIMCE 4° básico Lectura', u: 'puntos', rm: true},
    s4m: {t: 'SIMCE 4° básico Matemática', u: 'puntos', rm: true},
    s2l: {t: 'SIMCE 2° medio Lectura', u: 'puntos', rm: true},
    s2m: {t: 'SIMCE 2° medio Matemática', u: 'puntos', rm: true}
  };
  function botonComparar() {
    var esta = estado.comparar.indexOf(estado.comuna) >= 0, lleno = estado.comparar.length >= MAX_COMP;
    var txt = esta ? '✓ En comparación' : (lleno ? 'Máximo ' + MAX_COMP + ' comunas' : '+ Comparar');
    return '<button class=\"comparar\"' + (!esta && lleno ? ' disabled' : '') +
      ' style=\"font-family:inherit;font-size:12.5px;cursor:pointer;border-radius:4px;padding:2px 9px;' +
      'border:1px solid #2C5F6F;' + (esta ? 'background:#2C5F6F;color:#fff' : 'background:#fff;color:#2C5F6F') + '\">' + txt + '</button>';
  }
  var comp = document.createElement('div');
  comp.style.cssText = ficha.style.cssText; comp.style.display = 'none';
  ficha.parentNode.insertBefore(comp, ficha.nextSibling);
  function renderComp() {
    var L = estado.comparar;
    if (!L.length) { comp.style.display = 'none'; comp.innerHTML = ''; return; }
    var a = String(estado.anio), F = L.map(function(c) { return D.fichas[c]; });
    var hayRM = F.some(function(f) { return f.completa; });
    if (VARIABLES[estado.variable].rm && !hayRM) estado.variable = 'mat_pub';
    var chips = L.map(function(c, i) {
      return '<span style=\"display:inline-flex;align-items:center;gap:4px;margin:2px 8px 2px 0;white-space:nowrap\">' +
        '<span style=\"width:10px;height:10px;border-radius:2px;background:' + COLORES[i] + '\"></span>' + D.fichas[c].nombre +
        ' <a href=\"#\" data-quitar=\"' + c + '\" style=\"color:#888;text-decoration:none\">✕</a></span>';
    }).join('');
    var fila = function(et, f) {
      return '<tr style=\"border-bottom:1px solid #E5DFD3\"><th style=\"padding:5px 8px;text-align:left;white-space:nowrap;' +
        'position:sticky;left:0;background:#FAF7F0\">' + et + '</th>' +
        F.map(function(x) { return '<td style=\"padding:5px 8px;white-space:nowrap\">' + esc(f(x)) + '</td>'; }).join('') + '</tr>';
    };
    var delAnio = function(x) { return x.filas.filter(function(r) { return String(r.anio) === a; })[0] || {}; };
    var guion = function(x, v) { return x.completa ? v : '—'; };
    var h = '<div style=\"display:flex;justify-content:space-between;align-items:baseline;gap:8px;flex-wrap:wrap\">' +
      '<b style=\"font-size:16px;color:#2C5F6F\">Comparación de comunas · ' + a + '</b>' +
      '<a href=\"#\" class=\"limpiar\" style=\"color:#888;text-decoration:none\">Limpiar</a></div>' +
      '<div style=\"margin:4px 0 8px\">' + chips + '</div>' +
      '<div style=\"overflow-x:auto;-webkit-overflow-scrolling:touch\"><table style=\"border-collapse:collapse;font-size:12.5px;width:100%\">' +
      '<thead><tr style=\"border-bottom:2px solid #2C5F6F\"><th style=\"position:sticky;left:0;background:#FAF7F0\"></th>' +
      F.map(function(x, i) { return '<th style=\"padding:5px 8px;text-align:left;white-space:nowrap;color:' + COLORES[i] + '\">' + x.nombre + '</th>'; }).join('') +
      '</tr></thead><tbody>' +
      fila('Región', function(x) { return x.region; }) +
      fila('Administración', function(x) { return delAnio(x).adm; }) +
      fila('Traspaso a SLEP', function(x) { return x.g || 'Sin SLEP a ' + D.anios[D.anios.length - 1]; }) +
      fila('Matrícula pública', function(x) { return delAnio(x).mat; }) +
      (hayRM ? fila('Ingresos educación ($ 2025)', function(x) { return guion(x, delAnio(x).ingreso); }) +
               fila('Deuda flotante ($ 2025)', function(x) { return guion(x, delAnio(x).deuda); }) +
               fila('SIMCE 4° básico', function(x) { return guion(x, delAnio(x).simce4); }) +
               fila('SIMCE 2° medio', function(x) { return guion(x, delAnio(x).simce2); }) : '') +
      '</tbody></table></div>' +
      '<div style=\"display:flex;align-items:center;gap:8px;flex-wrap:wrap;margin:12px 0 0\"><span>Trayectoria:</span>' +
      '<select class=\"variable\" style=\"font-family:inherit;font-size:13px;max-width:100%\">' +
      Object.keys(VARIABLES).filter(function(k) { return hayRM || !VARIABLES[k].rm; }).map(function(k) {
        return '<option value=\"' + k + '\"' + (k === estado.variable ? ' selected' : '') + '>' + VARIABLES[k].t + '</option>';
      }).join('') + '</select></div>' +
      '<div class=\"grafico\" style=\"width:100%;height:320px\"></div>' +
      '<div style=\"color:#777;font-size:12px\">Rombo: año de traspaso a SLEP. Línea vertical: año del control.' +

      (estado.variable.indexOf('ingreso') === 0 ? ' En un SLEP, el ingreso es del servicio completo; el ingreso por estudiante usa la matrícula pública de todo su territorio.' : '') +
      (estado.variable.charAt(0) === 's' ? ' Sin SIMCE comparable 2019-2021.' : '') + '</div>';
    comp.innerHTML = h; comp.style.display = '';
    comp.querySelector('.limpiar').onclick = function(e) { e.preventDefault(); estado.comparar = []; renderFicha(); renderComp(); };
    comp.querySelectorAll('[data-quitar]').forEach(function(x) {
      x.onclick = function(e) { e.preventDefault(); estado.comparar.splice(estado.comparar.indexOf(this.getAttribute('data-quitar')), 1); renderFicha(); renderComp(); };
    });
    comp.querySelector('.variable').onchange = function() { estado.variable = this.value; renderComp(); };
    var v = estado.variable, trazos = [];
    F.forEach(function(x, i) {
      var S = x.serie, y = S[v];
      trazos.push({type: 'scatter', mode: 'lines+markers', x: S.anio, y: y, name: x.nombre, connectgaps: false,
        line: {color: COLORES[i], width: 2.5}, marker: {color: COLORES[i], size: 6},
        hovertemplate: x.nombre + ', %{x}: %{y:,}<extra></extra>'});
      if (x.g) {
        var j = S.anio.indexOf(x.g);
        if (j >= 0 && y[j] !== null && y[j] !== undefined)
          trazos.push({type: 'scatter', mode: 'markers', x: [x.g], y: [y[j]], showlegend: false,
            marker: {symbol: 'diamond', size: 13, color: COLORES[i], line: {color: '#fff', width: 1.5}},
            hovertemplate: x.nombre + ': traspaso a SLEP en ' + x.g + '<extra></extra>'});
      }
    });
    var div = comp.querySelector('.grafico');
    Plotly.react(div, trazos, {
      margin: {l: 56, r: 12, t: 10, b: 36}, font: {family: 'Titillium Web, sans-serif', size: 12},
      paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)', showlegend: false,
      separators: ',.', hovermode: 'closest',
      xaxis: {dtick: comp.clientWidth < 520 ? 2 : 1, tickangle: 0, range: [D.anios[0] - 0.4, D.anios[D.anios.length - 1] + 0.4], fixedrange: true, gridcolor: '#EEE'},
      yaxis: {title: {text: VARIABLES[v].u}, fixedrange: true, gridcolor: '#E5E5E5', zeroline: false},
      shapes: [{type: 'line', x0: estado.anio, x1: estado.anio, yref: 'paper', y0: 0, y1: 1,
                line: {color: '#999', width: 1, dash: 'dot'}}]
    }, {displayModeBar: false, responsive: true});
  }
  var $ = function(id) { return barra.querySelector('#' + id); };

  // Escala para % de matrícula pública en SLEP (0% gris cálido; >0 de celeste a petróleo)
  function mezcla(a, b, t) {
    var h = function(c, i) { return parseInt(c.substr(1 + 2 * i, 2), 16); };
    return 'rgb(' + [0, 1, 2].map(function(i) { return Math.round(h(a, i) + (h(b, i) - h(a, i)) * t); }).join(',') + ')';
  }
  function colorPct(p) { return p <= 0 ? '#EEEAE0' : mezcla('#CFE0E4', '#1E4652', Math.min(p, 100) / 100); }
  function caja(c, t) { return '<span style=\"white-space:nowrap\"><span style=\"display:inline-block;width:11px;height:11px;background:' + c +
    ';margin:0 4px -1px 10px\"></span>' + t + '</span>'; }

  function poligono(p, color, texto, ancho) {
    return {type: 'scatter', mode: 'lines', x: p.x, y: p.y, fill: 'toself', fillcolor: color,
            line: {color: 'white', width: ancho}, hoveron: 'fills', hoverinfo: 'text',
            text: texto, showlegend: false};
  }
  function ejes(trazos) {
    // Encuadre y proporción: 1° de longitud mide cos(latitud) veces 1° de latitud
    var xs = [], ys = [];
    trazos.forEach(function(t) { t.x.forEach(function(v) { if (v !== null) xs.push(v); });
                                 t.y.forEach(function(v) { if (v !== null) ys.push(v); }); });
    var x0 = Math.min.apply(null, xs), x1 = Math.max.apply(null, xs),
        y0 = Math.min.apply(null, ys), y1 = Math.max.apply(null, ys);
    var lat = (y0 + y1) / 2 * Math.PI / 180;
    // Alto según la forma de lo que se dibuja: en celular una región ancha no deja
    // franjas vacías arriba y abajo; Chile completo sigue siendo alto
    var ancho = gd.clientWidth || 760, prop = (y1 - y0) / ((x1 - x0) * Math.cos(lat));
    // tope: alto de la ventana menos la barra de controles (entre 560 y 1000 px)
    var tope = Math.max(560, Math.min(1000, (window.innerHeight || 900) - 130));
    var alto = Math.max(320, Math.min(tope, Math.round(ancho * prop) + 20));
    return {height: alto, xaxis: {visible: false, range: [x0, x1], fixedrange: false},
            yaxis: {visible: false, range: [y0, y1], scaleanchor: 'x', scaleratio: 1 / Math.cos(lat)}};
  }

  function dibujar() {
    var a = String(estado.anio), trazos = [], ids = [];
    $('anio_txt').textContent = a;
    if (!estado.region) {
      var R = D.reg[a];
      R.codigo_region.forEach(function(r, i) {
        if (!D.pol_reg[r]) return;
        trazos.push(poligono(D.pol_reg[r], colorPct(R.pct_slep[i]), R.hover[i], 0.8)); ids.push(r);
      });
      $('titulo').textContent = 'Chile · por región';
      $('volver').style.display = 'none';
      $('leyenda').innerHTML = '% de la matrícula pública en SLEP:' +
        caja('#EEEAE0', '0%') + caja(colorPct(25), '25%') + caja(colorPct(50), '50%') + caja(colorPct(100), '100%');
      nota.innerHTML = 'Haz clic en una región para ver sus comunas.';
    } else {
      var C = D.com[a];
      C.codigo_region.forEach(function(r, i) {
        var c = C.codigo_comuna[i];
        if (r !== estado.region || !D.pol_com[c]) return;
        var t = poligono(D.pol_com[c], C.color[i], C.hover[i], 0.7);
        if (c === estado.comuna) t.line = {color: '#1F1F1F', width: 2.5};
        trazos.push(t); ids.push(c);
      });
      $('titulo').textContent = D.nombres_reg[estado.region];
      $('volver').style.display = '';
      $('leyenda').innerHTML = Object.keys(D.colores).map(function(k) { return caja(D.colores[k], k); }).join('');
      nota.innerHTML = (estado.region === '05' ? 'No se dibujan ' + D.no_dibujadas.join(' ni ') +
        ' (islas oceánicas). ' : '') +
        'Montos en pesos de 2025; en un SLEP, ingresos y deuda son del servicio completo.' +
        ' Haz clic en una comuna para ver su ficha.';
    }
    gd._ids = ids;
    var lay = Object.assign({}, gd.layout, ejes(trazos), {dragmode: 'pan', autosize: false, width: el.clientWidth || 760,
                     // el zoom se mantiene al mover el año y se reinicia al cambiar de región
                     uirevision: estado.region || 'chile'});
    el.style.height = lay.height + 'px';   // el contenedor del widget acompaña al alto
    Plotly.react(gd, trazos, lay);
    renderFicha();
    renderComp();
  }

  $('anio').addEventListener('input', function() { estado.anio = +this.value; dibujar(); });
  $('volver').addEventListener('click', function() { estado.region = null; estado.comuna = null; dibujar(); });
  gd.on('plotly_click', function(ev) {
    var id = gd._ids[ev.points[0].curveNumber];
    if (!estado.region) { estado.region = id; dibujar(); return; }
    estado.comuna = (estado.comuna === id) ? null : id;   // segundo clic en la misma comuna la cierra
    dibujar();
    if (estado.comuna) ficha.scrollIntoView({behavior: 'smooth', block: 'nearest'});
  });
  // al girar el celular o cambiar el ancho de la ventana, se vuelve a encuadrar
  var t0; window.addEventListener('resize', function() { clearTimeout(t0); t0 = setTimeout(dibujar, 200); });
  dibujar();
}"

widget <- htmlwidgets::onRender(widget, js, data = datos_js)

## ---- exportar ----
exportar_mapa <- function(w, archivo) {
  # Página solo con el mapa: sin la estructura de informe, a todo el ancho
  # (hasta 1200 px), con un encabezado mínimo y un link a la nota metodológica
  encabezado <- htmltools::tags$div(
    style = paste("font-family:'Titillium Web',sans-serif;display:flex;flex-wrap:wrap;",
                  "align-items:baseline;justify-content:space-between;gap:4px 16px;margin:0 0 6px"),
    htmltools::tags$span(style = "font-size:19px;font-weight:600;color:#2C5F6F",
                         "Mapa interactivo SLEP · Chile"),
    if (nzchar(link_metodologia))
      htmltools::tags$a(href = link_metodologia, target = "_blank",
                        style = "font-size:13px;color:#2C5F6F",
                        "Nota metodológica y fuentes →"))
  w <- htmlwidgets::prependContent(w,
    # sin esta línea los celulares muestran la página a ~980 px de ancho, achicada
    htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    htmltools::tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Titillium+Web:wght@400;600&display=swap"),
    htmltools::tags$style(htmltools::HTML(
      "body{background:#fff;}
       body>div{max-width:1200px;margin-left:auto;margin-right:auto;}")),
    encabezado)
  w$sizingPolicy$browser$fill <- FALSE      # que no ocupe la ventana en modo absoluto
  w$sizingPolicy$browser$padding <- 14       # margen de la página (htmlwidgets lo aplica al body)
  auto <- requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()
  htmlwidgets::saveWidget(w, archivo, selfcontained = auto, title = "Mapa interactivo SLEP · Chile")
  message("Mapa guardado en: ", normalizePath(archivo))
}

if (!isTRUE(getOption("knitr.in.progress"))) exportar_mapa(widget, archivo_salida)
