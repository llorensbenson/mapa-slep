## =============================================================================
## PILOTO — Mapa nacional SLEP (Chile por región → comunas)
## Observatorio de Políticas Educativas | Rumbo Colectivo
##
## QUÉ HACE: muestra Chile por regiones; al hacer clic en una región se ven sus
## comunas. Un control de año (2015-2025) cambia el mapa en ambos niveles.
##   - Nivel región: color según % de la matrícula pública que está en SLEP.
##   - Nivel comuna: color según quién administra la educación pública
##     (SLEP, DAEM o Corporación Municipal), con hover por comuna.
##   - Región Metropolitana: además trae ingresos, deuda flotante y SIMCE
##     desde la planilla única (es la región con datos completos por ahora).
##
## ARCHIVOS (misma carpeta que el script):
##   - base_nacional_comunas_2015_2025.csv  (Mineduc, todas las comunas)
##   - planilla_unica_RM_2015_2025.xlsx     (opcional: datos completos de la RM)
## SALIDA: mapa_slep_chile_solo_mapa.html (el Rmd genera mapa_slep_chile_piloto.html)
## =============================================================================

## ---- configuracion ----
archivo_nacional <- "base_nacional_comunas_2015_2025.csv"
archivo_rm       <- "planilla_unica_RM_2015_2025.xlsx"   # si no está, la RM queda solo con matrícula
archivo_salida   <- "mapa_slep_chile_solo_mapa.html"   # distinto del .html que genera el Rmd

# Comunas insulares que no se dibujan (alejan el zoom de su región); siguen en los datos
comunas_no_dibujadas <- c("05201" = "Isla de Pascua", "05104" = "Juan Fernández")

colores_adm <- c("SLEP"        = "#2C5F6F",
                 "DAEM"        = "#D9C9A3",
                 "Corporación" = "#A89A80")

simce_sin_puntaje <- c("2019" = "Sin SIMCE comparable ese año",
                       "2020" = "No hubo SIMCE ese año (pandemia)",
                       "2021" = "No hubo SIMCE ese año (pandemia)")

## ---- paquetes ----
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

# Datos completos de la RM (si está la planilla): se usa su clasificación
# administrativa y se agregan ingresos, deuda y SIMCE al hover
rm_extra <- NULL
if (file.exists(archivo_rm)) {
  hoja <- grep("^Base RM", readxl::excel_sheets(archivo_rm), value = TRUE)[1]
  normalizar <- function(x) gsub("[^A-Z]", "", toupper(stringi::stri_trans_general(x, "Latin-ASCII")))
  simce_txt <- function(a, l, m) ifelse(as.character(a) %in% names(simce_sin_puntaje),
    unname(simce_sin_puntaje[as.character(a)]),
    paste0("L ", coalesce(as.character(l), "s/d"), " · M ", coalesce(as.character(m), "s/d")))
  rm_extra <- readxl::read_excel(archivo_rm, sheet = hoja) %>%
    transmute(
      k = normalizar(comuna), año, comuna_rm = comuna,
      adm_rm = case_when(!is.na(slep) ~ "SLEP",
                         grepl("^Corporaci", modalidad_administrativa) ~ "Corporación",
                         TRUE ~ "DAEM"),
      slep_rm = slep,
      extra = paste0(
        "<b>Ingresos en educación:</b> ", coalesce(fmt_millones(ingreso_total_educ_pesos), "Sin dato"), "<br>",
        "<b>Deuda flotante:</b> ", coalesce(fmt_millones(deuda_flotante_educ_devengado), "Sin dato"), "<br>",
        "<b>SIMCE 4° básico:</b> ", simce_txt(año, simce_4basico_lectura, simce_4basico_matematica), "<br>",
        "<b>SIMCE 2° medio:</b> ", simce_txt(año, simce_2medio_lectura, simce_2medio_matematica),
        ifelse(!is.na(slep), "<br><i>Ingresos y deuda: SLEP completo</i>", "")),
      # versiones cortas para la ficha por comuna
      f_ingreso = coalesce(fmt_millones(ingreso_total_educ_pesos), "s/d"),
      f_deuda   = case_when(!is.na(deuda_flotante_educ_devengado) ~ fmt_millones(deuda_flotante_educ_devengado),
                            grepl("^Corporaci", modalidad_administrativa) ~ "No registrada (Corp.)",
                            TRUE ~ "s/d"),
      f_simce4  = ifelse(as.character(año) %in% names(simce_sin_puntaje), "Sin SIMCE",
                         simce_txt(año, simce_4basico_lectura, simce_4basico_matematica)),
      f_simce2  = ifelse(as.character(año) %in% names(simce_sin_puntaje), "Sin SIMCE",
                         simce_txt(año, simce_2medio_lectura, simce_2medio_matematica)))
  nac <- nac %>%
    mutate(k = normalizar(nombre_comuna)) %>%
    left_join(rm_extra, by = c("k", "año")) %>%
    mutate(administracion = coalesce(adm_rm, administracion),
           nombre_slep    = ifelse(codigo_region == "13", slep_rm, nombre_slep),
           nombre_comuna  = coalesce(comuna_rm, nombre_comuna)) %>%   # con tildes
    select(-k, -adm_rm, -slep_rm, -comuna_rm)
  if (any(nac$codigo_region == "13" & is.na(nac$extra)))
    warning("Hay comunas de la RM que no cruzaron con la planilla.")
}
for (v in c("extra", "f_ingreso", "f_deuda", "f_simce4", "f_simce2"))
  if (is.null(nac[[v]])) nac[[v]] <- NA_character_

# Nombres de región con tildes (chilemapas los trae sin tildes)
nombres_region <- c("01" = "Tarapacá", "02" = "Antofagasta", "03" = "Atacama", "04" = "Coquimbo",
  "05" = "Valparaíso", "06" = "O'Higgins", "07" = "Maule", "08" = "Biobío",
  "09" = "La Araucanía", "10" = "Los Lagos", "11" = "Aysén", "12" = "Magallanes",
  "13" = "Metropolitana", "14" = "Los Ríos", "15" = "Arica y Parinacota", "16" = "Ñuble")
nac$nombre_region <- unname(nombres_region[nac$codigo_region])

## ---- comunas (hover y color por año) ----
comunas <- nac %>%
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
fichas <- comunas %>%
  arrange(codigo_comuna, año) %>%
  transmute(
    codigo_comuna, nombre_comuna, anio = año,
    adm = ifelse(administracion == "SLEP", paste0("SLEP ", coalesce(nombre_slep, "")),
                 administracion),
    mat = paste0(fmt_num(matricula_publica), " (",
                 round(100 * matricula_publica / pmax(matricula_total, 1)), "%)"),
    ingreso = f_ingreso, deuda = f_deuda, simce4 = f_simce4, simce2 = f_simce2
  )
fichas <- lapply(split(fichas, fichas$codigo_comuna), function(d) list(
  nombre = d$nombre_comuna[1],
  completa = !all(is.na(d$ingreso)),       # RM: con ingresos, deuda y SIMCE
  filas = lapply(seq_len(nrow(d)), function(i)
    as.list(d[i, c("anio", "adm", "mat", "ingreso", "deuda", "simce4", "simce2")]))))

## ---- regiones (resumen por año) ----
regiones <- comunas %>%
  group_by(codigo_region, nombre_region, año) %>%
  summarise(
    n_comunas = n(),
    n_slep    = sum(administracion == "SLEP"),
    mat_pub   = sum(matricula_publica),
    mat_slep  = sum(matricula_slep),
    sleps     = paste(sort(unique(na.omit(nombre_slep))), collapse = ", "),
    .groups = "drop"
  ) %>%
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
simplificar <- function(x, tol) x %>% st_transform(32719) %>%
  st_simplify(dTolerance = tol, preserveTopology = TRUE) %>% st_transform(4326)
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

geo_com <- st_as_sf(mapa_comunas) %>%
  filter(!codigo_comuna %in% names(comunas_no_dibujadas)) %>%
  select(id = codigo_comuna, region = codigo_region) %>%
  st_make_valid()
# Regiones = unión de sus comunas (no depende de rmapshaper)
geo_reg <- geo_com %>% group_by(id = region) %>% summarise(.groups = "drop")

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
  com     = por_anio(comunas %>% filter(codigo_comuna %in% geo_com$id),
                     c("codigo_comuna", "codigo_region", "color", "hover")),
  nombres_reg  = setNames(as.list(unique(regiones$nombre_region)), unique(regiones$codigo_region)),
  colores      = as.list(colores_adm),
  fichas       = fichas,
  no_dibujadas = unname(comunas_no_dibujadas)
)

## ---- mapa ----
# El widget parte vacío; el JavaScript dibuja la vista nacional o regional
widget <- plot_ly(type = "scatter", mode = "lines", x = numeric(0), y = numeric(0),
                  height = 820) %>%
  layout(font = list(family = "Titillium Web"),
         margin = list(l = 0, r = 0, t = 10, b = 0),
         hoverlabel = list(bgcolor = "#FAF7F0", align = "left",
                           font = list(family = "Titillium Web", size = 12))) %>%
  config(displayModeBar = FALSE)

js <- "
function(el, x, D) {
  var gd = el, estado = {anio: D.anios[D.anios.length - 1], region: null, comuna: null};
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
      '<a href=\"#\" class=\"cerrar\" style=\"color:#888;text-decoration:none;white-space:nowrap\">✕ cerrar</a></div>' +
      '<div style=\"color:#777;font-size:12px;margin:2px 0 6px\">Trayectoria ' + D.anios[0] + '-' + D.anios[D.anios.length - 1] +
      '; el año del control queda destacado.' + (F.completa ? ' Ingresos y deuda de un SLEP corresponden al servicio completo.' : '') +
      '</div><div style=\"overflow-x:auto;-webkit-overflow-scrolling:touch\">' +
      '<table style=\"border-collapse:collapse;font-size:12.5px;width:100%\"><thead><tr style=\"border-bottom:2px solid #2C5F6F;background:#FAF7F0\">' +
      th('Año', fijo) + th('Administración') + th('Matrícula pública') +
      (F.completa ? th('Ingresos educación') + th('Deuda flotante') + th('SIMCE 4° básico') + th('SIMCE 2° medio') : '') +
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
    var alto = Math.max(320, Math.min(820, Math.round(ancho * prop) + 20));
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
        ' (comunas insulares). ' : '') +
        (estado.region === '13' ? 'RM: incluye ingresos, deuda flotante y SIMCE de la planilla única.'
                                : 'Fuera de la RM, por ahora solo administración y matrícula (Mineduc).') +
        ' Haz clic en una comuna para ver su ficha.';
    }
    gd._ids = ids;
    var lay = Object.assign({}, gd.layout, ejes(trazos), {dragmode: 'pan', autosize: false, width: el.clientWidth || 760});
    el.style.height = lay.height + 'px';   // el contenedor del widget acompaña al alto
    Plotly.react(gd, trazos, lay);
    renderFicha();
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
  w <- htmlwidgets::prependContent(w,
    # sin esta línea los celulares muestran la página a ~980 px de ancho, achicada
    htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    htmltools::tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Titillium+Web:wght@400;600&display=swap"))
  auto <- requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()
  htmlwidgets::saveWidget(w, archivo, selfcontained = auto, title = "Mapa SLEP Chile (piloto)")
  message("Mapa guardado en: ", normalizePath(archivo))
}
if (!isTRUE(getOption("knitr.in.progress"))) exportar_mapa(widget, archivo_salida)
