# ==============================================================================
# PAINEL EPIDEMIOLÓGICO DA DENGUE (SINAN) - BELÉM / SESMA / CIEVS
# Versão Autocontida e Reativa: Análise Temporal, Espacial, Epidemiológica e Notificadora
# Sem consultas externas em tempo de execução | 100% dados reais oficiais CISC Belém
# ==============================================================================

options(shiny.maxRequestSize = 500 * 1024^2)

# ------------------------------------------------------------------------------
# 0. PACOTES NECESSÁRIOS
# ------------------------------------------------------------------------------
pacotes <- c("shiny", "bslib", "foreign", "dplyr", "plotly", "lubridate", 
             "bsicons", "shinyjs", "base64enc", "openxlsx", "leaflet", "sf", "leaflet.extras",
             "officer", "gridExtra")
novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if (length(novos_pacotes) > 0 && !nzchar(Sys.getenv("SHINY_PORT"))) {
  try(install.packages(novos_pacotes, repos = "https://cloud.r-project.org"), silent = TRUE)
}

library(shiny)
library(bslib)
library(foreign)
library(dplyr)
library(lubridate)
library(bsicons)
library(shinyjs)
library(base64enc)
library(openxlsx)
library(leaflet)
library(leaflet.extras)
library(sf)
library(plotly) # Carregado por último para prioridade de namespace
library(officer)
library(gridExtra)
library(grid)
library(ggplot2)

# Paletas de Cores Padrão da Vigilância Epidemiológica
PALETTE_PRIMARY <- "#0000A1"
PALETTE_PIE     <- c("#0000A1", "#3B82F6", "#0EA5E9", "#F59E0B", "#EF4444", "#10B981", "#8B5CF6", "#64748B", "#EC4899", "#84CC16")
PALETTE_ANOS    <- c("#0000A1", "#EF4444", "#10B981", "#F59E0B", "#8B5CF6", "#EC4899", "#06B6D4", "#64748B", "#D97706", "#2563EB", "#059669", "#7C3AED", "#D946EF")

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DAS BASES LOCAIS PRÉ-SALVAS (SEM REQUISIÇÕES EXTERNAS)
# ------------------------------------------------------------------------------
obter_caminho_arquivo <- function(nome_arquivo) {
  caminhos_tentativa <- c(
    file.path(getwd(), nome_arquivo),
    file.path(getwd(), "data", nome_arquivo),
    file.path(getwd(), "www", nome_arquivo),
    file.path("C:/Users/CISC/Desktop/Painel dengue", nome_arquivo),
    file.path("C:/Users/CISC/Desktop/Painel dengue/data", nome_arquivo),
    file.path("C:/Users/CISC/Desktop/Painel dengue/www", nome_arquivo),
    nome_arquivo
  )
  for (caminho in caminhos_tentativa) {
    if (file.exists(caminho)) return(caminho)
  }
  return(nome_arquivo)
}

caminho_hist      <- obter_caminho_arquivo("dados_historico_dengue.rds")
# Fallback seguro para base sintética de demonstração (LGPD Compliant) caso a base restrita oficial não esteja presente
DADOS_MODO_DEMO   <- FALSE
if (!file.exists(caminho_hist)) {
  caminho_exemplo <- obter_caminho_arquivo("dados_historico_dengue_exemplo.rds")
  if (file.exists(caminho_exemplo)) {
    caminho_hist    <- caminho_exemplo
    DADOS_MODO_DEMO <- TRUE
  }
}

caminho_mapa      <- obter_caminho_arquivo("mapa_bairros_belem.rds")
caminho_distritos <- obter_caminho_arquivo("mapa_distritos_belem.rds")
caminho_ilhas     <- obter_caminho_arquivo("mapa_ilhas_belem.rds")
caminho_estab     <- obter_caminho_arquivo("estabelecimentos_pa.rds")
caminho_notif     <- obter_caminho_arquivo("notificacoes_estabelecimentos.rds")

# Carregamento seguro da base histórica (dados oficiais locais ou dados sintéticos de demonstração)
HISTORICO_BASE    <- if (file.exists(caminho_hist)) readRDS(caminho_hist) else data.frame()
MAPA_BELEM_SF                 <- if (file.exists(caminho_mapa)) readRDS(caminho_mapa) else NULL
MAPA_DISTRITOS_SF             <- if (file.exists(caminho_distritos)) readRDS(caminho_distritos) else NULL
if (!is.null(MAPA_DISTRITOS_SF) && "sigla" %in% names(MAPA_DISTRITOS_SF)) {
  # Garante enquadramento preciso dos centróides dos distritos sobre as suas massas territoriais principais
  # DAOUT posicionado no centro da Ilha de Caratateua (Outeiro)
  MAPA_DISTRITOS_SF$centroid_lon[MAPA_DISTRITOS_SF$sigla == "DAOUT"] <- -48.4500
  MAPA_DISTRITOS_SF$centroid_lat[MAPA_DISTRITOS_SF$sigla == "DAOUT"] <- -1.2600
  MAPA_DISTRITOS_SF$centroid_lon[MAPA_DISTRITOS_SF$sigla == "DAGUA"] <- -48.4572
  MAPA_DISTRITOS_SF$centroid_lat[MAPA_DISTRITOS_SF$sigla == "DAGUA"] <- -1.4517
}
MAPA_ILHAS_SF                 <- if (file.exists(caminho_ilhas)) readRDS(caminho_ilhas) else NULL
ESTABELECIMENTOS_PA           <- if (file.exists(caminho_estab)) readRDS(caminho_estab) else data.frame()
NOTIFICACOES_ESTABELECIMENTOS <- if (file.exists(caminho_notif)) readRDS(caminho_notif) else data.frame()

# Fallback em memória para as Ilhas de Belém caso o arquivo específico não exista
if (is.null(MAPA_ILHAS_SF) && !is.null(MAPA_DISTRITOS_SF) && !is.null(MAPA_BELEM_SF)) {
  tryCatch({
    u_b <- sf::st_union(MAPA_BELEM_SF)
    u_d <- sf::st_union(MAPA_DISTRITOS_SF)
    ilhas_raw <- sf::st_difference(u_d, u_b) %>% sf::st_make_valid()
    ilhas_p <- sf::st_cast(ilhas_raw, "POLYGON")
    areas_p <- as.numeric(sf::st_area(ilhas_p)) / 1e6
    MAPA_ILHAS_SF <- ilhas_p[areas_p >= 0.05]
  }, error = function(e) NULL)
}

# ------------------------------------------------------------------------------
# 2. FUNÇÕES AUXILIARES DE FORMATAÇÃO, NORMALIZAÇÃO E DECODIFICAÇÃO
# ------------------------------------------------------------------------------
format_num <- function(x, digits = 0) {
  if (is.null(x) || length(x) == 0) return("0")
  x_num <- suppressWarnings(as.numeric(x))
  x_num[is.na(x_num)] <- 0
  if (digits > 0) {
    format(round(x_num, digits), big.mark = ".", decimal.mark = ",", nsmall = digits, trim = TRUE)
  } else {
    format(round(x_num), big.mark = ".", decimal.mark = ",", trim = TRUE)
  }
}

format_pct <- function(x, digits = 2) {
  paste0(format_num(x, digits = digits), "%")
}

normalizar_texto <- function(s) {
  if (is.null(s)) return("")
  s <- toupper(as.character(s))
  s <- iconv(s, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  s <- gsub("[^A-Z0-9 ]", "", s)
  trimws(s)
}

# Tabela de normalização para os 71 bairros oficiais de Belém
alias_bairros_belem <- c(
  "SAO BRAZ" = "SAO BRAS",
  "SÃO BRAZ" = "SAO BRAS",
  "VAL DE CANS" = "VALDECAES",
  "VAL DE CAES" = "VALDECAES",
  "VALDE CANS" = "VALDECAES",
  "VALDECANS" = "VALDECAES",
  "MONTESE" = "MONTESE TERRA FIRME",
  "TERRA FIRME" = "MONTESE TERRA FIRME",
  "TERRRA FIRME" = "MONTESE TERRA FIRME",
  "TERR FIRME" = "MONTESE TERRA FIRME",
  "TERRA FRIME" = "MONTESE TERRA FIRME",
  "CURIO UTINGA" = "CURIOUTINGA",
  "CURIO" = "CURIOUTINGA",
  "SACRAMETA" = "SACRAMENTA",
  "SACRAM" = "SACRAMENTA",
  "SACRA" = "SACRAMENTA",
  "TELEG" = "TELEGRAFO",
  "BATIOSTA CAMPO" = "BATISTA CAMPOS",
  "BAIRRO DA MARAMBAIA" = "MARAMBAIA",
  "MARNBAIA" = "MARAMBAIA",
  "JURUANS" = "JURUNAS",
  "JURUNA" = "JURUNAS",
  "TENO" = "TENONE",
  "TENOME" = "TENONE",
  "PRATINHA 2" = "PRATINHA",
  "PRATINHA I" = "PRATINHA",
  "PRATINHA II" = "PRATINHA",
  "PARQUE VERDE  BAIRRO" = "PARQUE VERDE",
  "PARK VERDE" = "PARQUE VERDE",
  "SIDERAL" = "PARQUE VERDE",
  "NOVO HORIZONTE" = "PARQUE VERDE",
  "AGUSTO MONTE NEGRO" = "PARQUE VERDE",
  "AUGUSTO MONTENEGRO" = "PARQUE VERDE",
  "PARACURI II  BAIRRO" = "PARACURI",
  "PARACURI I" = "PARACURI",
  "PARACURI II" = "PARACURI",
  "CONJUNTO PARACURI" = "PARACURI",
  "CAMPINA ICOARACI" = "CAMPINA DE ICOARACI",
  "RAMAL DE ICOARACI" = "CAMPINA DE ICOARACI",
  "BAIRRO NOVO" = "CAMPINA DE ICOARACI",
  "DISTRITO INDUSTRIAL FABRICAS" = "MARACACUERA",
  "SAO JOA DO OUTEIRO" = "SAO JOAO DO OUTEIRO",
  "FIDELIS" = "SAO JOAO DO OUTEIRO",
  "FAMA" = "SAO JOAO DO OUTEIRO",
  "ILHA DE COTIJUBA" = "SAO JOAO DO OUTEIRO",
  "COTIJUBA" = "SAO JOAO DO OUTEIRO",
  "BAHIA DO SOL" = "BAIA DO SOL",
  "BAIIA DO SOL" = "BAIA DO SOL",
  "VILA MOSQUEIRO" = "VILA",
  "SUCURIJAQUARA" = "SUCURIJUQUARA",
  "MARI MARI" = "MARAHU",
  "MARIMARI" = "MARAHU",
  "CONCEICAO" = "CARANANDUBA",
  "CAMBOINHA" = "CARANANDUBA",
  "CAICARA" = "ARIRAMBA",
  "ARIRAMBA" = "ARIRAMBA",
  "BOM FIM" = "BONFIM",
  "CARUARU" = "CARUARA",
  "CENTRO" = "CAMPINA",
  "COMERCIO" = "CAMPINA",
  "CENTRO HISTORICO" = "CAMPINA",
  "SOUSA" = "SOUZA",
  "BENGUI II" = "BENGUI",
  "NOVO BENGUI" = "BENGUI",
  "NEWTON MIRANDA" = "BENGUI",
  "CONJ RES ALMIR GABRIEL" = "MARAMBAIA",
  "COQUEIRO VILA" = "COQUEIRO",
  "AGUAS NEGRAS ICOARACI" = "AGUAS NEGRAS",
  "PONTA GROSSA ICOARACI" = "PONTA GROSSA"
)

mapear_bairro_mapa <- function(vetor_bairros) {
  if (is.null(MAPA_BELEM_SF) || length(vetor_bairros) == 0) return(rep(NA_character_, length(vetor_bairros)))
  
  df_ref <- data.frame(
    oficial = MAPA_BELEM_SF$nm_bairro,
    key = normalizar_texto(MAPA_BELEM_SF$nm_bairro),
    stringsAsFactors = FALSE
  )
  
  key_in <- normalizar_texto(vetor_bairros)
  key_in_aliased <- ifelse(key_in %in% names(alias_bairros_belem), alias_bairros_belem[key_in], key_in)
  
  match_idx <- match(key_in_aliased, df_ref$key)
  ifelse(!is.na(match_idx), df_ref$oficial[match_idx], NA_character_)
}

# Dicionário dinâmico de estabelecimentos do Pará com prioridade por ano selecionado
obter_dicionario_estabelecimentos <- function(anos_sel = NULL) {
  if (is.null(ESTABELECIMENTOS_PA) || nrow(ESTABELECIMENTOS_PA) == 0) return(list())
  
  df_sub <- ESTABELECIMENTOS_PA
  if (!is.null(anos_sel) && length(anos_sel) > 0) {
    anos_num <- suppressWarnings(as.numeric(anos_sel))
    anos_num <- anos_num[!is.na(anos_num)]
    if (length(anos_num) > 0) {
      df_ano <- df_sub %>% filter(ano %in% anos_num)
      if (nrow(df_ano) > 0) df_sub <- df_ano
    }
  }
  
  df_cnes <- df_sub %>%
    arrange(desc(ano), nome) %>%
    distinct(cnes, .keep_all = TRUE)
  
  dic <- as.list(df_cnes$nome)
  names(dic) <- as.character(df_cnes$cnes)
  
  cnes_faltantes <- setdiff(ESTABELECIMENTOS_PA$cnes, names(dic))
  if (length(cnes_faltantes) > 0) {
    df_falt <- ESTABELECIMENTOS_PA %>%
      filter(cnes %in% cnes_faltantes) %>%
      arrange(desc(ano), nome) %>%
      distinct(cnes, .keep_all = TRUE)
    for (i in seq_len(nrow(df_falt))) {
      dic[[as.character(df_falt$cnes[i])]] <- df_falt$nome[i]
    }
  }
  
  return(dic)
}

# Helper para converter imagens locais para Base64 (Logos)
get_bg64_img <- function(path) {
  caminho_real <- obter_caminho_arquivo(path)
  if (file.exists(caminho_real)) {
    ext <- tolower(tools::file_ext(caminho_real))
    mime_type <- case_when(
      ext == "png" ~ "image/png",
      ext %in% c("jpg", "jpeg") ~ "image/jpeg",
      ext %in% c("svg") ~ "image/svg+xml",
      TRUE ~ "image/png"
    )
    b64 <- base64enc::base64encode(caminho_real)
    return(paste0("data:", mime_type, ";base64,", b64))
  }
  return("")
}

# Tiles com cores sólidas 100% limpas (zero requisições externas, sem marca d'água "API KEY REQUIRED")
TILE_BRANCO_PURO <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5+hHgAHggJ/PchI7wAAAABJRU5ErkJggg=="
TILE_PRETO_PURO  <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="

# Carregamento do Logo Oficial SESMA / Prefeitura de Belém
obter_logo_sesma_b64 <- function() {
  candidatos <- c(
    obter_caminho_arquivo("logo_sesma.png"),
    obter_caminho_arquivo("www/logo_sesma.png"),
    file.path(getwd(), "www", "logo_sesma.png"),
    file.path(getwd(), "logo_sesma.png"),
    obter_caminho_arquivo("logo sesma.png"),
    "C:/Users/CISC/Pictures/logo sesma.png",
    "C:/Users/CISC/Pictures/logo_sesma.png"
  )
  for (cand in candidatos) {
    if (file.exists(cand)) {
      b64 <- base64enc::base64encode(cand)
      return(paste0("data:image/png;base64,", b64))
    }
  }
  return("")
}

logo_sesma_b64 <- obter_logo_sesma_b64()
logo_cievs_b64 <- get_bg64_img("input_file_1.png")

# Header dos Cards com Exportação
card_header_download <- function(title_text, prefix_id) {
  plotly_id <- paste0("plot_", prefix_id)
  tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    tags$span(title_text, style = "font-weight: 600; font-size: 0.9rem; color: #0000A1;"),
    tags$div(
      class = "dropdown btn-group no-print",
      tags$button(
        class = "btn btn-sm btn-outline-secondary dropdown-toggle py-0 px-2",
        type = "button",
        `data-bs-toggle` = "dropdown",
        `aria-expanded` = "false",
        style = "font-size: 0.75rem;",
        bs_icon("download"), " Exportar"
      ),
      tags$ul(
        class = "dropdown-menu dropdown-menu-end shadow-sm",
        tags$li(
          downloadLink(
            paste0("dl_excel_", prefix_id),
            label = HTML("<i class='fa-regular fa-file-excel text-success me-2'></i>Tabela em Excel (.xlsx)"),
            class = "dropdown-item small"
          )
        ),
        tags$li(
          tags$a(
            class = "dropdown-item small",
            href = "#",
            onclick = sprintf("exportPlotlyGraph('%s', '%s', 'png'); return false;", plotly_id, prefix_id),
            HTML("<i class='fa-regular fa-file-image text-primary me-2'></i>Gráfico em PNG")
          )
        ),
        tags$li(
          tags$a(
            class = "dropdown-item small",
            href = "#",
            onclick = sprintf("exportPlotlyGraph('%s', '%s', 'pdf'); return false;", plotly_id, prefix_id),
            HTML("<i class='fa-regular fa-file-pdf text-danger me-2'></i>Gráfico em PDF")
          )
        )
      )
    )
  )
}

# ==============================================================================
# 3. INTERFACE DE USUÁRIO (UI)
# ==============================================================================
ui <- page_fillable(
  title = "Painel Epidemiológico da Dengue (SINAN) - Belém",
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr",
    primary = "#0000A1",
    base_font = "system-ui, -apple-system, 'Segoe UI', Roboto, Arial, sans-serif"
  ),
  class = "bslib-page-navbar has-page-sidebar",
  padding = 0,
  gap = 0,
  
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, shrink-to-fit=no"),
    
    # Favicons & Ícone Mobile (Aedes aegypti)
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
    
    # Metatags Open Graph (OG) para Compartilhamento em WhatsApp, Telegram e Redes Sociais
    tags$meta(property = "og:title", content = "Painel Epidemiológico da Dengue — Belém / SESMA / CIEVS"),
    tags$meta(property = "og:description", content = "Sistema Oficial de Vigilância Epidemiológica da Dengue em Belém. Monitoramento de 2026, canal endêmico e análise por bairros."),
    tags$meta(property = "og:image", content = "https://cievsbelem.shinyapps.io/painel-dengue-belem/card_compartilhamento.png"),
    tags$meta(property = "og:image:secure_url", content = "https://cievsbelem.shinyapps.io/painel-dengue-belem/card_compartilhamento.png"),
    tags$meta(property = "og:image:type", content = "image/png"),
    tags$meta(property = "og:image:width", content = "1200"),
    tags$meta(property = "og:image:height", content = "630"),
    tags$meta(property = "og:url", content = "https://cievsbelem.shinyapps.io/painel-dengue-belem/"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:site_name", content = "CIEVS Belém — Vigilância Epidemiológica"),
    
    # Twitter Card
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "Painel Epidemiológico da Dengue — Belém / SESMA / CIEVS"),
    tags$meta(name = "twitter:description", content = "Sistema Oficial de Vigilância Epidemiológica da Dengue em Belém."),
    tags$meta(name = "twitter:image", content = "https://cievsbelem.shinyapps.io/painel-dengue-belem/card_compartilhamento.png"),
    
    useShinyjs(),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js"),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        $('a[data-bs-toggle=\"tab\"], button[data-bs-toggle=\"tab\"]').on('shown.bs.tab', function(e) {
          window.dispatchEvent(new Event('resize'));
          var plots = document.querySelectorAll('.js-plotly-plot');
          plots.forEach(function(el) { Plotly.Plots.resize(el); });
          setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 250);
        });
      });

      function exportPlotlyGraph(plotlyId, filenamePrefix, format) {
        var el = document.getElementById(plotlyId);
        if (!el || !el._fullLayout) {
          alert('Aguarde o gráfico renderizar totalmente antes de exportar.');
          return;
        }
        var w = el.clientWidth || 800;
        var h = el.clientHeight || 450;
        if (format === 'png') {
          Plotly.downloadImage(el, {
            format: 'png',
            width: w * 2,
            height: h * 2,
            filename: filenamePrefix + '_' + new Date().toISOString().slice(0,10)
          });
        } else if (format === 'pdf') {
          Plotly.toImage(el, { format: 'png', width: w * 2, height: h * 2 })
            .then(function(dataUrl) {
              const { jsPDF } = window.jspdf;
              var isLandscape = w > h;
              var doc = new jsPDF({
                orientation: isLandscape ? 'landscape' : 'portrait',
                unit: 'px',
                format: [w, h]
              });
              doc.addImage(dataUrl, 'PNG', 0, 0, w, h);
              doc.save(filenamePrefix + '_' + new Date().toISOString().slice(0,10) + '.pdf');
            })
            .catch(function(err) {
              alert('Falha ao exportar PDF do gráfico.');
            });
        }
      }

      // Atalhos de teclado para o Modo Apresentação
      document.addEventListener('keydown', function(e) {
        var activeTab = document.querySelector('.nav-link.active');
        if (activeTab && activeTab.textContent.indexOf('Apresentação') !== -1) {
          if (e.key === 'ArrowRight' || e.key === 'PageDown') {
            var btnProx = document.getElementById('btn_slide_prox');
            if (btnProx) btnProx.click();
          } else if (e.key === 'ArrowLeft' || e.key === 'PageUp') {
            var btnAnt = document.getElementById('btn_slide_ant');
            if (btnAnt) btnAnt.click();
          }
        }
      });

      function toggleFullscreenApresentacao() {
        var elem = document.getElementById('bloco_modo_apresentacao') || document.getElementById('apresentacao_container') || document.documentElement;
        var isFull = !!(document.fullscreenElement || document.webkitFullscreenElement || document.mozFullScreenElement || document.msFullscreenElement);
        if (!isFull) {
          var req = elem.requestFullscreen || elem.webkitRequestFullscreen || elem.mozRequestFullScreen || elem.msRequestFullscreen;
          if (req) {
            req.call(elem).catch(function(err) {
              console.warn('Fallback fullscreen para documentElement:', err);
              if (document.documentElement.requestFullscreen) {
                document.documentElement.requestFullscreen();
              }
            });
          }
        } else {
          var exit = document.exitFullscreen || document.webkitExitFullscreen || document.mozCancelFullScreen || document.msExitFullscreen;
          if (exit) { exit.call(document); }
        }
      }

      document.addEventListener('fullscreenchange', function() {
        var isFull = !!(document.fullscreenElement || document.webkitFullscreenElement || document.mozFullScreenElement || document.msFullscreenElement);
        var btn = document.getElementById('btn_fullscreen_apres');
        if (btn) {
          if (isFull) {
            btn.innerHTML = '<i class=\"fa-solid fa-compress me-1\"></i> Sair Tela Cheia';
            btn.classList.remove('btn-outline-secondary');
            btn.classList.add('btn-secondary');
          } else {
            btn.innerHTML = '<i class=\"fa-solid fa-expand me-1\"></i> Tela Cheia';
            btn.classList.remove('btn-secondary');
            btn.classList.add('btn-outline-secondary');
          }
        }
      });
    ")),
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, Arial, sans-serif !important; }
      #bloco_modo_apresentacao:fullscreen {
        background-color: #F8FAFC !important;
        padding: 16px !important;
        overflow-y: auto !important;
        width: 100vw !important;
        height: 100vh !important;
      }
      #bloco_modo_apresentacao:-webkit-full-screen {
        background-color: #F8FAFC !important;
        padding: 16px !important;
        overflow-y: auto !important;
        width: 100vw !important;
        height: 100vh !important;
      }
      @media print {
        .sidebar-toggle, .bslib-sidebar, .btn, .no-print, .dropdown, .navbar { display: none !important; }
        .header-logos-top { border-bottom: 2px solid #0000A1 !important; margin-bottom: 12px !important; }
        .main { width: 100% !important; margin: 0 !important; padding: 0 !important; }
        .card { break-inside: avoid; page-break-inside: avoid; }
      }
      .bslib-sidebar-layout { margin-bottom: 0 !important; }
      .bslib-page-main { padding: 0 !important; margin: 0 !important; }
      .tab-content { padding: 0 !important; margin: 0 !important; }
      .tab-pane { padding: 8px 14px 18px 14px !important; }
      .card { margin-bottom: 0.85rem; border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,0.05); }
      .card-header { background-color: #F8FAFC; padding: 0.4rem 0.85rem; }
      .header-logos-top {
        display: flex;
        align-items: center;
        justify-content: space-between;
        background-color: #FFFFFF;
        padding: 5px 20px;
        border-bottom: 2px solid #E2E8F0;
        box-shadow: 0 1px 3px rgba(0,0,0,0.03);
        margin: 0;
        width: 100%;
        flex-shrink: 0;
        z-index: 1030;
      }
      .header-logos-top img {
        max-height: 40px;
        max-width: 220px;
        width: auto;
        object-fit: contain;
      }
      .navbar {
        border-bottom: 1px solid #CBD5E1 !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.03);
        padding-top: 0.15rem !important;
        padding-bottom: 0.15rem !important;
        background-color: #F8FAFC !important;
        min-height: 40px !important;
        flex-shrink: 0;
      }
      .navbar-brand {
        font-weight: 700 !important;
        color: #0000A1 !important;
        font-size: 0.95rem !important;
        letter-spacing: -0.2px;
        padding-top: 0.15rem !important;
        padding-bottom: 0.15rem !important;
      }
      .nav-underline .nav-link {
        font-weight: 600;
        font-size: 0.85rem;
        color: #475569;
        padding: 0.35rem 0.75rem !important;
      }
      .nav-underline .nav-link.active {
        color: #0000A1 !important;
        border-bottom: 3px solid #0000A1 !important;
      }
      .nav-underline .nav-link:hover {
        color: #0000A1 !important;
      }
      .value-box { margin-bottom: 0.45rem; padding: 0.42rem 0.75rem !important; border-radius: 8px; }
      .value-box .value-box-value { font-size: 1.25rem !important; font-weight: 700; }
      .value-box .value-box-title { font-size: 0.72rem !important; text-transform: uppercase; letter-spacing: 0.3px; }
      .dropdown-item { cursor: pointer; }
      .sidebar .form-group { margin-bottom: 0.55rem; }
      .sidebar .sidebar-content { padding-top: 10px !important; }
      .status-box { background-color: #ECFDF5; border-left: 4px solid #10B981; padding: 7px 10px; border-radius: 4px; font-size: 0.75rem; color: #065F46; }
      .leaflet-container { border-radius: 6px; background-color: #FFFFFF !important; background: #FFFFFF !important; }
      .leaflet-pane, .leaflet-tile-pane, .leaflet-map-pane, .leaflet-layer { background-color: transparent !important; background: transparent !important; }
      .leaflet-control-attribution { display: none !important; }
      .custom-scroll-table { max-height: 250px; overflow-y: auto; border: 1px solid #E2E8F0; border-radius: 6px; margin-bottom: 10px; }
      .custom-scroll-table table { margin-bottom: 0 !important; font-size: 0.8rem; }
      .custom-scroll-table th { position: sticky; top: 0; background: #F1F5F9; z-index: 10; color: #0000A1; font-weight: 600; }
      .controles-mapa-centralizados .form-group { margin-bottom: 0 !important; }
      .controles-mapa-centralizados .selectize-input,
      .controles-mapa-centralizados .selectize-input input,
      .controles-mapa-centralizados select {
        text-align: center !important;
        text-align-last: center !important;
        font-size: 0.82rem !important;
        padding: 4px 8px !important;
      }
      .controles-mapa-centralizados .selectize-dropdown,
      .controles-mapa-centralizados .selectize-dropdown-content {
        text-align: center !important;
      }
      /* Estilos Executivos do Modo Apresentação */
      .slide-deck-toolbar {
        background: #FFFFFF;
        border: 1px solid #CBD5E1;
        border-radius: 8px;
        padding: 8px 14px;
        margin-bottom: 12px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.03);
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
        gap: 8px 12px;
      }
      .slide-deck-toolbar .form-group,
      .slide-deck-toolbar .shiny-input-container {
        margin-bottom: 0 !important;
        padding-bottom: 0 !important;
        display: inline-flex !important;
        align-items: center !important;
      }
      .slide-deck-toolbar .btn,
      .slide-deck-toolbar .btn-sm,
      .slide-deck-toolbar .selectize-input {
        height: 34px !important;
        min-height: 34px !important;
        max-height: 34px !important;
        line-height: 1.2 !important;
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
        font-size: 0.82rem !important;
        box-sizing: border-box !important;
        border-radius: 6px !important;
      }
      .slide-deck-toolbar .selectize-control {
        margin-bottom: 0 !important;
        display: inline-block !important;
        width: 380px !important;
      }
      .slide-deck-toolbar .selectize-input {
        width: 380px !important;
        padding: 4px 10px !important;
        white-space: nowrap !important;
        overflow: hidden !important;
        text-overflow: ellipsis !important;
        background-color: #F8FAFC !important;
        border: 1px solid #CBD5E1 !important;
        color: #0000A1 !important;
        font-weight: 600 !important;
      }
      .slide-deck-toolbar .selectize-input input {
        display: none !important;
      }
      .slide-deck-toolbar .selectize-dropdown {
        min-width: 400px !important;
        font-size: 0.82rem !important;
        z-index: 1050 !important;
        box-shadow: 0 4px 14px rgba(0,0,0,0.12) !important;
        border: 1px solid #CBD5E1 !important;
        border-radius: 6px !important;
      }
      .slide-deck-toolbar .form-check-inline,
      .slide-deck-toolbar .radio-inline {
        display: inline-flex !important;
        align-items: center !important;
        margin-bottom: 0 !important;
        margin-right: 8px !important;
        font-size: 0.82rem !important;
        font-weight: 600 !important;
        color: #334155 !important;
      }
      .slide-deck-toolbar .form-check-input {
        margin-top: 0 !important;
        margin-right: 4px !important;
      }
      /* Caixinha Lateral de Seleção de Gráficos */
      .grupo-grafico-caixa {
        background: #F8FAFC;
        border: 1px solid #E2E8F0;
        border-radius: 6px;
        padding: 6px 8px;
      }
      .grupo-grafico-titulo {
        font-size: 0.74rem;
        font-weight: 700;
        color: #0000A1;
        text-transform: uppercase;
        letter-spacing: 0.3px;
        margin-bottom: 4px;
        border-bottom: 1px solid #E2E8F0;
        padding-bottom: 2px;
      }
      .item-grafico-check {
        font-size: 0.80rem;
        font-weight: 500;
        color: #334155;
      }
      .item-grafico-check .form-group {
        margin-bottom: 2px !important;
      }
      .item-grafico-check .form-check {
        margin-bottom: 0 !important;
        min-height: 22px !important;
        display: flex !important;
        align-items: center !important;
      }
      .item-grafico-check .form-check-label {
        font-size: 0.78rem !important;
        cursor: pointer !important;
        margin-left: 4px !important;
        color: #334155 !important;
        white-space: nowrap !important;
        overflow: hidden !important;
        text-overflow: ellipsis !important;
      }
      .item-grafico-check .form-check-input {
        cursor: pointer !important;
        margin-top: 0 !important;
      }
      .slide-frame {
        background: #FFFFFF;
        border: 1px solid #E2E8F0;
        border-radius: 10px;
        padding: 16px 20px 14px 20px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.05);
        margin-bottom: 18px;
        position: relative;
      }
      .slide-header {
        border-left: 4px solid #0000A1;
        padding-left: 12px;
        margin-bottom: 12px;
      }
      .slide-header h4 {
        margin: 0;
        font-weight: 800;
        color: #0000A1;
        font-size: 1.05rem;
        letter-spacing: -0.2px;
      }
      .slide-header p {
        margin: 2px 0 0 0;
        color: #64748B;
        font-size: 0.80rem;
        font-weight: 500;
      }
      .slide-kpi-card {
        background: #F8FAFC;
        border: 1px solid #E2E8F0;
        border-radius: 8px;
        padding: 8px 12px;
        text-align: center;
      }
      .slide-kpi-value {
        font-size: 1.30rem;
        font-weight: 800;
        color: #0000A1;
        line-height: 1.1;
      }
      .slide-kpi-label {
        font-size: 0.70rem;
        font-weight: 700;
        color: #64748B;
        text-transform: uppercase;
        letter-spacing: 0.3px;
        margin-top: 3px;
      }
      .slide-insight-box {
        background: #EFF6FF;
        border-left: 4px solid #3B82F6;
        border-radius: 6px;
        padding: 8px 14px;
        margin-top: 12px;
        font-size: 0.82rem;
        color: #0000A1;
      }
      #apresentacao_container:fullscreen {
        background: #0F172A;
        overflow-y: auto;
        padding: 20px 30px;
      }
      #apresentacao_container:fullscreen .slide-frame {
        max-width: 1400px;
        margin: 0 auto;
        min-height: 90vh;
      }

      /* =======================================================================
         OTIMIZAÇÃO RESPONSIVA DEDICADA PARA CELULAR E DISPOSITIVOS MÓVEIS
         ======================================================================= */
      @media (max-width: 768px) {
        /* 1. Cabeçalho Institucional Mobile */
        .header-logos-top {
          flex-direction: column !important;
          padding: 8px 12px !important;
          text-align: center !important;
          gap: 6px !important;
        }
        .header-logos-top img {
          max-height: 34px !important;
          margin-bottom: 2px !important;
        }
        .header-logos-top h5 {
          font-size: 0.82rem !important;
          line-height: 1.25 !important;
        }
        .header-logos-top div {
          font-size: 0.72rem !important;
        }

        /* 2. Barra de Navegação e Abas */
        .navbar {
          padding-left: 8px !important;
          padding-right: 8px !important;
        }
        .navbar-brand {
          font-size: 0.82rem !important;
          white-space: normal !important;
          line-height: 1.2 !important;
        }
        .navbar-nav {
          padding: 6px 0 !important;
        }
        .nav-underline .nav-link {
          font-size: 0.80rem !important;
          padding: 0.35rem 0.6rem !important;
        }

        /* 3. Espaçamento Geral do Conteúdo */
        .tab-pane {
          padding: 6px 6px 20px 6px !important;
        }
        .card {
          margin-bottom: 0.65rem !important;
          border-radius: 8px !important;
        }
        .card-header {
          padding: 0.35rem 0.65rem !important;
          font-size: 0.82rem !important;
        }
        .card-header h6, .card-header .h6 {
          font-size: 0.80rem !important;
        }

        /* 4. Cartões de Indicadores (ValueBoxes em grade 2 colunas no mobile) */
        .bslib-layout-column-wrap {
          grid-template-columns: repeat(2, minmax(0, 1fr)) !important;
          gap: 6px !important;
        }
        .value-box {
          margin-bottom: 0 !important;
          padding: 0.45rem 0.65rem !important;
          min-height: 75px !important;
        }
        .value-box .value-box-value {
          font-size: 1.15rem !important;
        }
        .value-box .value-box-title {
          font-size: 0.65rem !important;
          letter-spacing: 0.2px !important;
        }
        .value-box .value-box-showcase {
          max-width: 28px !important;
          font-size: 1.1rem !important;
        }

        /* 5. Altura Adaptativa dos Gráficos e Mapas */
        .js-plotly-plot {
          height: 330px !important;
          max-height: 330px !important;
        }
        .leaflet-container {
          height: 340px !important;
          min-height: 340px !important;
        }

        /* 6. Layout de Colunas (empilhamento responsivo completo) */
        .bslib-layout-columns {
          grid-template-columns: 1fr !important;
        }

        /* 7. Barra Lateral (Gaveta Móvel) */
        .bslib-sidebar-layout .sidebar {
          width: 85vw !important;
          max-width: 340px !important;
        }

        /* 8. Tabelas com Rolagem Tátil Suave */
        .custom-scroll-table {
          max-height: 220px !important;
          -webkit-overflow-scrolling: touch !important;
        }
        .custom-scroll-table table {
          font-size: 0.72rem !important;
        }

        /* 9. Barra do Modo Apresentação */
        .slide-deck-toolbar {
          padding: 6px 8px !important;
        }
        .slide-deck-toolbar .selectize-control {
          width: 100% !important;
        }
        .slide-deck-toolbar .selectize-input {
          width: 100% !important;
        }
        .slide-deck-toolbar .btn-group {
          width: 100% !important;
          justify-content: center !important;
        }
        .slide-frame {
          padding: 10px !important;
        }
      }
    "))
  ),
  
  # ============================================================================
  # 1. BANNER INSTITUCIONAL SUPERIOR (SECRETARIA MUNICIPAL DE SAÚDE - SESMA)
  # ============================================================================
  tags$header(
    class = "header-logos-top",
    if(logo_sesma_b64 != "") tags$img(src = logo_sesma_b64, alt = "Prefeitura de Belém - SESMA") else tags$div(style = "width: 140px;"),
    tags$div(
      class = "text-center flex-grow-1 px-3",
      tags$h5("SECRETARIA MUNICIPAL DE SAÚDE DE BELÉM — SESMA / CIEVS", style = "margin:0; font-weight:800; color:#0000A1; letter-spacing: 0.3px; font-size: 0.95rem;"),
      tags$div(
        tags$span("Série Histórica 2015-2025 / Ano de monitoramento 2026", style = "color:#64748B; font-weight: 600; font-size: 0.80rem; margin-top: 1px;"),
        if (DADOS_MODO_DEMO) tags$span(class = "badge bg-warning text-dark ms-2", style = "font-size: 0.68rem;", "Modo Demonstração (Dados Sintéticos - LGPD)") else tags$span(class = "badge bg-success ms-2", style = "font-size: 0.68rem;", "Base Oficial")
      )
    ),
    tags$div(
      class = "d-none d-md-flex flex-column align-items-end justify-content-center",
      style = "min-width: 140px;",
      tags$span(class = "badge bg-primary px-3 py-2 text-uppercase", style = "font-size: 0.70rem; letter-spacing: 0.5px; font-weight:700; background-color: #0000A1 !important;", "Vigilância Epidemiológica"),
      tags$small(class = "text-muted mt-1 fw-semibold", style = "font-size: 0.70rem; color: #64748B;", "SINAN / CISC Belém")
    )
  ),
  
  # ============================================================================
  # 2. BARRA DE NAVEGAÇÃO E COMANDOS DAS ABAS (NAVBAR INFERIOR AO BANNER)
  # ============================================================================
  navset_bar(
    id = "tabs_painel",
    title = "Painel Epidemiológico da Dengue (SINAN) - Belém",
    sidebar = sidebar(
      width = 310,
      title = "Controles & Filtros",
      
      tags$b("1. Dados Integrados:"),
      tags$div(
        class = "status-box mb-2",
        tags$i(class = "fa-solid fa-database me-1"),
        textOutput("status_dados")
      ),
      fileInput("file_dbf", "Upload Direto DBF/DBC:", accept = c(".dbf", ".DBF", ".dbc", ".DBC")),
      span(textOutput("status_upload"), style = "font-size:0.75rem; color:#059669; font-weight:600;"),
      actionButton(
        inputId = "btn_processar_pasta_dados",
        label = "Processar Pasta 'dados'",
        icon = icon("rotate"),
        class = "btn-outline-primary btn-sm w-100 mt-1 mb-2 fw-semibold"
      ),
      hr(),
      
      tags$b("2. Comparação Temporal:"),
      radioButtons(
        inputId = "modo_ano",
        label = NULL,
        choices = c("Ano Único (Individual)" = "individual", "Cruzamento / Série Multi-anos" = "cruzado"),
        selected = "individual"
      ),
      uiOutput("filtro_anos_ui"),
      sliderInput("sel_semana", "Semanas Epidemiológicas:", min = 1, max = 53, value = c(1, 53)),
      hr(),
      
      tags$b("3. Filtros Epidemiológicos:"),
      radioButtons(
        inputId = "filtro_abrangencia",
        label = "Abrangência Territorial:",
        choices = c("Todos os Casos Notificados" = "todos", "Apenas Residentes em Belém" = "residencia"),
        selected = "residencia"
      ),
      uiOutput("filtro_bairros_ui"),
      uiOutput("filtro_unidades_ui"),
      uiOutput("filtro_classificacao_ui"),
      uiOutput("filtro_sexo_ui"),
      uiOutput("filtro_evolucao_ui"),
      uiOutput("filtro_criterio_ui"),
      uiOutput("filtro_sorotipo_ui"),
      hr(),
      
      numericInput("populacao", "População Estimada (Belém):", value = 1303403, min = 1),
      numericInput("max_bairros", "Top Bairros no Gráfico:", value = 15, min = 5, max = 71),
      numericInput("max_unidades", "Top Estabelecimentos:", value = 15, min = 5, max = 50),
      hr(),
      actionButton("btn_pdf", "Imprimir / Salvar Relatório PDF", icon = icon("print"), class = "btn-danger w-100 fw-bold")
    ),
    
    # ABA 1: VISÃO GERAL
    nav_panel(
      title = "Visão Geral",
      fillable = FALSE,
      layout_column_wrap(
      width = 1/4, fill = FALSE,
      value_box(title = "Total Notificados", value = textOutput("kpi_notificados"), showcase = bs_icon("clipboard-data-fill"), theme = "primary"),
      value_box(title = "Casos Prováveis", value = textOutput("kpi_provaveis"), showcase = bs_icon("exclamation-triangle-fill"), theme = "warning"),
      value_box(title = "Confirmados", value = textOutput("kpi_confirmados"), showcase = bs_icon("check-circle-fill"), theme = "danger"),
      value_box(title = "Inconclusivos", value = textOutput("kpi_inconclusivos"), showcase = bs_icon("info-circle-fill"), theme = "info")
    ),
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      value_box(title = "Descartados", value = textOutput("kpi_descartados"), showcase = bs_icon("x-circle-fill"), theme = "secondary"),
      value_box(title = "Óbitos por Dengue", value = textOutput("kpi_obitos"), showcase = bs_icon("person-x-fill"), theme = "dark"),
      value_box(title = "Incidência (por 100 mil hab.)", value = textOutput("kpi_incidencia"), showcase = bs_icon("graph-up-arrow"), theme = "warning"),
      value_box(title = "Letalidade Geral", value = textOutput("kpi_letalidade"), showcase = bs_icon("heart-pulse-fill"), theme = "danger")
    ),
    
    card(
      card_header(card_header_download("Distribuição de casos por Semana Epidemiológica", "semanas_geral")),
      plotlyOutput("plot_semanas_geral", height = "450px")
    )
  ),
  
  # ABA 2: ANÁLISE ESPACIAL
  nav_panel(
    title = "Análise Espacial",
    fillable = FALSE,
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header(
          tags$div(
            class = "w-100 py-1",
            tags$div(
              class = "d-flex align-items-center gap-2 mb-2",
              tags$i(class = "fa-solid fa-map-location-dot text-primary fs-5"),
              tags$span("ANÁLISE ESPACIAL DO TERRITÓRIO DE BELÉM", style = "font-weight: 800; color: #0000A1; font-size: 0.95rem; letter-spacing: 0.3px;")
            ),
            tags$div(
              class = "d-flex justify-content-center align-items-end gap-3 flex-wrap w-100 controles-mapa-centralizados",
              # 1. Caixa de Seleção: Tipo de Mapa
              tags$div(
                style = "min-width: 175px; text-align: center;",
                tags$label("Tipo de Mapa", style = "font-size: 0.72rem; font-weight: 700; color: #0000A1; margin-bottom: 3px; display: block; text-transform: uppercase; letter-spacing: 0.4px; text-align: center;"),
                selectInput(
                  "tipo_visualizacao_mapa",
                  label = NULL,
                  choices = c("Mapa Coroplético" = "coropletico", "Mapa de Calor" = "calor"),
                  selected = "coropletico",
                  width = "100%"
                )
              ),
              # 2. Caixa de Seleção: Camada Territorial
              tags$div(
                style = "min-width: 145px; text-align: center;",
                tags$label("Camada Territorial", style = "font-size: 0.72rem; font-weight: 700; color: #0000A1; margin-bottom: 3px; display: block; text-transform: uppercase; letter-spacing: 0.4px; text-align: center;"),
                selectInput(
                  "camada_mapa_ativa",
                  label = NULL,
                  choices = c("Por Bairro" = "bairros", "Por Distrito" = "distritos"),
                  selected = "bairros",
                  width = "100%"
                )
              ),
              # 3. Caixa de Seleção: Dados
              tags$div(
                style = "min-width: 195px; text-align: center;",
                tags$label("Dados", style = "font-size: 0.72rem; font-weight: 700; color: #0000A1; margin-bottom: 3px; display: block; text-transform: uppercase; letter-spacing: 0.4px; text-align: center;"),
                selectInput(
                  "metrica_mapa",
                  label = NULL,
                  choices = c(
                    "Taxa de Incidência" = "incidencia",
                    "Casos Prováveis" = "provaveis",
                    "Casos Confirmados" = "confirmados",
                    "Total Notificados" = "total_casos"
                  ),
                  selected = "incidencia",
                  width = "100%"
                )
              )
            )
          )
        ),
        leafletOutput("mapa_dengue_belem", height = "620px"),
        card_footer(
          tags$div(
            class = "d-flex justify-content-between align-items-center small text-muted flex-wrap gap-2",
            tags$span(textOutput("mapa_tipo_legenda_rodape", inline = TRUE)),
            tags$span(textOutput("mapa_subtitulo_anos", inline = TRUE), style = "font-weight: 600; color: #0000A1;")
          )
        )
      ),
      tags$div(
        layout_column_wrap(
          width = 1, fill = FALSE,
          value_box(title = "Localidade Mais Crítica", value = textOutput("mapa_kpi_top_bairro"), showcase = bs_icon("geo-alt-fill"), theme = "danger"),
          value_box(title = "Bairros com Notificações", value = textOutput("mapa_kpi_bairros_atingidos"), showcase = bs_icon("pin-map-fill"), theme = "warning"),
          value_box(title = "Incidência Média Territorial", value = textOutput("mapa_kpi_incidencia_media"), showcase = bs_icon("activity"), theme = "primary")
        ),
        
        # Tabela 1: Detalhamento por bairro com Barra de Rolagem
        card(
          card_header(
            tags$div(
              class = "d-flex justify-content-between align-items-center w-100",
              tags$b("Detalhamento por bairro", style = "font-size: 0.85rem; color: #0000A1;"),
              downloadButton("dl_excel_mapa_bairros", "Baixar Excel", class = "btn-sm py-0 px-2 btn-outline-success")
            )
          ),
          tags$div(class = "custom-scroll-table", tableOutput("tabela_todos_bairros_mapa"))
        ),
        
        # Tabela 2: Distritos Administrativos de Belém
        card(
          card_header(
            tags$div(
              class = "d-flex justify-content-between align-items-center w-100",
              tags$b("Distritos Administrativos no Período", style = "font-size: 0.85rem; color: #0000A1;"),
              downloadButton("dl_excel_mapa_distritos", "Baixar Excel", class = "btn-sm py-0 px-2 btn-outline-success")
            )
          ),
          tags$div(class = "custom-scroll-table", tableOutput("tabela_distritos_mapa"))
        )
      )
    )
  ),
  
  # ABA 3: SOCIODEMOGRÁFICO
  nav_panel(
    title = "Sociodemográfico",
    fillable = FALSE,
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(card_header_download("Distribuição por Sexo", "sexo")), plotlyOutput("plot_sexo", height = "400px")),
      card(card_header(card_header_download("Distribuição por Raça/Cor", "raca")), plotlyOutput("plot_raca", height = "400px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(card_header_download("Faixa etária e sexo", "piramide")), plotlyOutput("plot_piramide", height = "400px")),
      card(card_header(card_header_download("Casos por bairro", "bairro")), plotlyOutput("plot_bairro", height = "400px"))
    )
  ),
  
  # ABA 4: DIAGNÓSTICO & DESFECHOS
  nav_panel(
    title = "Diagnóstico & Desfechos",
    fillable = FALSE,
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(card_header_download("Classificação Final dos Casos", "classificacao")), plotlyOutput("plot_classificacao", height = "420px")),
      card(card_header(card_header_download("Evolução Clínica / Desfecho", "evolucao")), plotlyOutput("plot_evolucao", height = "420px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(card_header_download("Critério de Confirmação / Diagnóstico", "criterio")), plotlyOutput("plot_criterio", height = "420px")),
      card(card_header(card_header_download("Casos por sorotipo", "sorotipo")), plotlyOutput("plot_sorotipo", height = "420px"))
    )
  ),
  
  # ABA 5: UNIDADES NOTIFICADORAS
  nav_panel(
    title = "Unidades Notificadoras",
    fillable = FALSE,
    
    # 1. Indicadores / KPIs da Rede Notificadora
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      value_box(
        title = "Maior Notificadora (1º Lugar)",
        value = textOutput("kpi_top_unidade"),
        showcase = bs_icon("trophy-fill"),
        theme = "primary"
      ),
      value_box(
        title = "Total de Notificações Registradas",
        value = textOutput("kpi_total_notif_unidades"),
        showcase = bs_icon("clipboard2-pulse-fill"),
        theme = "success"
      ),
      value_box(
        title = "Estabelecimentos Notificadores Ativos",
        value = textOutput("kpi_qtd_unidades_ativas"),
        showcase = bs_icon("hospital-fill"),
        theme = "info"
      ),
      value_box(
        title = "Concentração no Top 3",
        value = textOutput("kpi_concentracao_top3"),
        showcase = bs_icon("pie-chart-fill"),
        theme = "warning"
      )
    ),
    
    # 2. Gráfico Ranking e Tabela Detalhada
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header(card_header_download("Ranking de Estabelecimentos Notificadores (Maior para Menor)", "unidades")),
        plotlyOutput("plot_unidades", height = "640px"),
        card_footer(
          tags$div(
            class = "d-flex justify-content-between align-items-center small text-muted",
            tags$span("Ranking decrescente de notificações oficiais registradas pelos estabelecimentos de saúde no SINAN."),
            tags$span(textOutput("subtitulo_anos_unidades", inline = TRUE), style = "font-weight: 600; color: #0000A1;")
          )
        )
      ),
      card(
        card_header(
          tags$div(
            class = "d-flex justify-content-between align-items-center w-100",
            tags$b("Detalhamento do Ranking", style = "font-size: 0.85rem; color: #0000A1;"),
            downloadButton("dl_excel_tabela_ranking_unidades", "Baixar Excel", class = "btn-sm py-0 px-2 btn-outline-success")
          )
        ),
        tags$div(
          class = "custom-scroll-table",
          style = "max-height: 590px;",
          tableOutput("tabela_ranking_unidades")
        ),
        card_footer(
          tags$small(class = "text-muted", "Relação ordenada com posição, CNES, volume de notificações e percentual de participação.")
        )
      )
    )
  ),
  
  # ABA 6: ANÁLISE EPIDEMIOLÓGICA
  nav_panel(
    title = "Análise epidemiológica",
    fillable = FALSE,
    # Gráfico 1: Diagrama de Controle (Canal Endêmico)
    card(
      card_header(card_header_download("Diagrama de Controle (Canal Endêmico - Monitoramento 2026)", "diagrama_controle")),
      plotlyOutput("plot_diagrama_controle", height = "460px"),
      card_footer(
        tags$div(
          class = "d-flex justify-content-between align-items-center small text-muted",
          tags$span("Série de referência não epidêmica: 2015, 2016, 2017, 2018, 2019, 2022 e 2023. Limite Superior = Média + 1,96 × DP."),
          tags$span("Ano de Observação Vigente: 2026 (Colunas por Semana Epidemiológica)", style = "font-weight:600; color:#0000A1;")
        )
      )
    ),
    # Gráficos 2 e 3: Evolução de Incidência e Evolução de Letalidade
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(card_header_download("Evolução de Incidência", "evolucao_incidencia")), plotlyOutput("plot_evolucao_incidencia", height = "420px")),
      card(card_header(card_header_download("Evolução de Letalidade", "evolucao_letalidade")), plotlyOutput("plot_evolucao_letalidade", height = "420px"))
    ),
    # Gráficos 4 e 5: Casos anuais (empilhados) e Série histórica de óbitos lado a lado
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(card_header_download("Casos notificados, casos prováveis e confirmados por ano", "confirmados_ano")),
        plotlyOutput("plot_confirmados_ano", height = "420px")
      ),
      card(
        card_header(card_header_download("Série histórica de óbitos", "obitos_ano")),
        plotlyOutput("plot_obitos_ano", height = "420px")
      )
    )
  ),
  
  # ABA 7: MODO APRESENTAÇÃO
  nav_panel(
    title = "Modo Apresentação",
    fillable = FALSE,
    tags$div(
      id = "bloco_modo_apresentacao",
      style = "width: 100%;",
    
    # 1. Barra Superior da Apresentação Executiva
    tags$div(
      class = "slide-deck-toolbar",
      
      # Grupo Esquerdo: Marca, Botão de Toggle da Caixinha Lateral e Modo de Exibição
      tags$div(
        class = "d-flex align-items-center gap-2 flex-wrap",
        tags$div(
          class = "d-flex align-items-center gap-2 me-1",
          tags$i(class = "fa-solid fa-person-chalkboard text-primary fs-5"),
          tags$span("APRESENTAÇÃO EXECUTIVA", style = "font-weight: 800; color: #0000A1; font-size: 0.92rem; letter-spacing: 0.3px;")
        ),
        actionButton(
          "btn_toggle_caixinha",
          label = tags$span(
            tags$i(class = "fa-solid fa-sliders me-1"),
            "Gráficos (", textOutput("txt_badge_sel", inline = TRUE), ")"
          ),
          class = "btn-sm btn-outline-primary fw-bold"
        ),
        tags$div(
          class = "d-flex align-items-center ms-2",
          radioButtons(
            "modo_apres_layout",
            label = NULL,
            choices = c("Slide a Slide" = "slide", "Painel Contínuo" = "todos"),
            selected = "slide",
            inline = TRUE
          )
        )
      ),
      
      # Grupo Central: Navegação de Slides (visível no modo slide)
      conditionalPanel(
        condition = "input.modo_apres_layout == 'slide'",
        tags$div(
          class = "d-flex align-items-center gap-1",
          actionButton("btn_slide_ant", "◀ Anterior", class = "btn-sm btn-outline-primary fw-bold"),
          selectInput(
            "sel_slide_apres",
            label = NULL,
            choices = c("Carregando..." = "g_cartoes"),
            selected = "g_cartoes",
            width = "380px"
          ),
          actionButton("btn_slide_prox", "Próximo ▶", class = "btn-sm btn-outline-primary fw-bold")
        )
      ),
      
      # Grupo Direito: Ações (Tela Cheia, Download PPTX e PDF)
      tags$div(
        class = "d-flex align-items-center gap-2 flex-wrap",
        actionButton("btn_fullscreen_apres", "Tela Cheia", icon = icon("expand"), class = "btn-sm btn-outline-secondary fw-semibold", onclick = "toggleFullscreenApresentacao();"),
        downloadButton("dl_apresentacao_pptx", "Baixar PPTX", icon = icon("file-powerpoint"), class = "btn-sm btn-success fw-bold"),
        downloadButton("dl_apresentacao_pdf", "Baixar PDF", icon = icon("file-pdf"), class = "btn-sm btn-danger fw-bold")
      )
    ),
    
    # 2. Container com Caixinha Lateral e Área dos Slides
    tags$div(
      id = "apresentacao_container",
      tags$div(
        class = "row g-3",
        
        # 2.1 Caixinha Lateral com Lista de Gráficos e Checkbox (Escolha Prévia)
        tags$div(
          id = "caixinha_lateral_graficos",
          class = "col-12 col-lg-3 col-xl-3",
          tags$div(
            class = "card shadow-sm border",
            style = "border-radius: 8px; border-color: #CBD5E1 !important; background: #FFFFFF;",
            tags$div(
              class = "card-header bg-white d-flex justify-content-between align-items-center py-2 px-3",
              style = "border-bottom: 1px solid #E2E8F0;",
              tags$div(
                class = "d-flex align-items-center gap-2",
                tags$i(class = "fa-solid fa-list-check text-primary"),
                tags$b("Seleção dos Gráficos", style = "font-size: 0.88rem; color: #0000A1;")
              ),
              tags$span(class = "badge bg-primary text-white fw-bold", style = "font-size: 0.70rem;", textOutput("txt_badge_sel2", inline = TRUE))
            ),
            tags$div(
              class = "d-flex justify-content-between align-items-center px-2 py-1 bg-light",
              style = "border-bottom: 1px solid #E2E8F0; gap: 4px;",
              actionButton("btn_sel_todos", "Todos", class = "btn-xs btn-outline-primary flex-fill fw-bold", style = "font-size: 0.68rem; padding: 2px 2px; height: 26px;"),
              actionButton("btn_sel_padrao", "Padrão (8)", class = "btn-xs btn-outline-secondary flex-fill fw-bold", style = "font-size: 0.68rem; padding: 2px 2px; height: 26px;"),
              actionButton("btn_limpar_todos", "Limpar", class = "btn-xs btn-outline-danger flex-fill fw-bold", style = "font-size: 0.68rem; padding: 2px 2px; height: 26px;")
            ),
            tags$div(
              class = "card-body p-2",
              style = "max-height: 720px; overflow-y: auto;",
              
              # GRUPO 1: VISÃO GERAL
              tags$div(
                class = "grupo-grafico-caixa mb-2",
                tags$div(class = "grupo-grafico-titulo", tags$i(class = "fa-solid fa-chart-line me-1 text-primary"), "1. Visão Geral"),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_cartoes", "Dados de Destaque (Cartões)", value = TRUE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_semanas", "Curva Epidêmica Semanal", value = TRUE))
              ),
              
              # GRUPO 2: ANÁLISE ESPACIAL
              tags$div(
                class = "grupo-grafico-caixa mb-2",
                tags$div(class = "grupo-grafico-titulo", tags$i(class = "fa-solid fa-map-location-dot me-1 text-info"), "2. Análise Espacial"),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_mapa_bairros", "Mapa Territorial por Bairro", value = TRUE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_mapa_distritos", "Mapa Territorial por Distrito", value = FALSE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_bairros", "Top Bairros Notificados", value = TRUE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_distritos", "Tabela de Distritos", value = FALSE))
              ),
              
              # GRUPO 3: SOCIODEMOGRÁFICO
              tags$div(
                class = "grupo-grafico-caixa mb-2",
                tags$div(class = "grupo-grafico-titulo", tags$i(class = "fa-solid fa-users me-1 text-purple"), "3. Sociodemográfico"),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_piramide", "Pirâmide Etária (Idade x Sexo)", value = TRUE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_sexo_raca", "Distribuição por Raça/Cor", value = FALSE))
              ),
              
              # GRUPO 4: DIAGNÓSTICO & DESFECHOS
              tags$div(
                class = "grupo-grafico-caixa mb-2",
                tags$div(class = "grupo-grafico-titulo", tags$i(class = "fa-solid fa-stethoscope me-1 text-warning"), "4. Diagnóstico & Desfechos"),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_classificacao", "Classificação Final dos Casos", value = TRUE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_criterio", "Critério Confirmação / Diagnóstico", value = FALSE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_sorotipo", "Casos por Sorotipo Viral", value = FALSE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_evolucao", "Evolução Clínica & Desfechos", value = FALSE))
              ),
              
              # GRUPO 5: UNIDADES NOTIFICADORAS
              tags$div(
                class = "grupo-grafico-caixa mb-2",
                tags$div(class = "grupo-grafico-titulo", tags$i(class = "fa-solid fa-hospital me-1 text-success"), "5. Unidades Notificadoras"),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_unidades", "Top Estabelecimentos Notificadores", value = TRUE))
              ),
              
              # GRUPO 6: ANÁLISE EPIDEMIOLÓGICA
              tags$div(
                class = "grupo-grafico-caixa mb-1",
                tags$div(
                  class = "grupo-grafico-titulo d-flex justify-content-between align-items-center",
                  tags$span(tags$i(class = "fa-solid fa-chart-column me-1 text-danger"), "6. Análise Epidemiológica"),
                  tags$span(class = "badge bg-warning text-dark", style = "font-size: 0.58rem;", "Vigente 2026")
                ),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_canal", "Canal Endêmico (Ano Vigente 2026)", value = TRUE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_evol_incidencia", "Evolução Mensal Incidência", value = FALSE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_evol_letalidade", "Evolução Mensal Letalidade", value = FALSE)),
                tags$div(class = "item-grafico-check", checkboxInput("sel_g_casos_obitos_ano", "Série Anual Casos e Óbitos", value = FALSE))
              )
            )
          )
        ),
        
        # 2.2 Área de Exibição dos Slides Organizados na Sequência Lógica das Abas
        tags$div(
          id = "area_apresentacao_slides",
          class = "col-12 col-lg-9 col-xl-9",
          
          # Mensagem caso nenhum gráfico esteja marcado
          conditionalPanel(
            condition = "!input.sel_g_cartoes && !input.sel_g_semanas && !input.sel_g_mapa_bairros && !input.sel_g_mapa_distritos && !input.sel_g_bairros && !input.sel_g_distritos && !input.sel_g_piramide && !input.sel_g_sexo_raca && !input.sel_g_classificacao && !input.sel_g_criterio && !input.sel_g_sorotipo && !input.sel_g_evolucao && !input.sel_g_unidades && !input.sel_g_canal && !input.sel_g_evol_incidencia && !input.sel_g_evol_letalidade && !input.sel_g_casos_obitos_ano",
            tags$div(
              class = "alert alert-warning text-center p-4 border",
              style = "border-radius: 8px;",
              tags$i(class = "fa-solid fa-triangle-exclamation fs-3 text-warning mb-2"),
              tags$h5("Nenhum Gráfico Selecionado", style = "font-weight: 700; color: #854D0E;"),
              tags$p("Por favor, marque ao menos uma das opções na caixinha lateral para visualizar a sua apresentação executiva.", class = "mb-0")
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE INICIAL: CAPA EXECUTIVA DA APRESENTAÇÃO
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "input.sel_slide_apres == 'g_capa' || input.modo_apres_layout == 'todos'",
            tags$div(
              class = "slide-frame mb-3",
              style = "background: linear-gradient(135deg, #0000A1 0%, #0F172A 100%); color: white; border-radius: 8px; padding: 48px 32px; text-align: center; box-shadow: 0 4px 12px rgba(30,58,138,0.25); border: 1px solid #0000A1;",
              tags$div(
                tags$i(class = "fa-solid fa-shield-virus fs-1 mb-3", style = "color: #93C5FD;"),
                tags$h1("Atualização Epidemiológica de Dengue", style = "color: #FFFFFF; font-weight: 800; font-size: 2.2rem; letter-spacing: 0.5px; margin-bottom: 12px;"),
                tags$h3("Belém - Pará", style = "color: #93C5FD; font-weight: 700; font-size: 1.45rem; margin-bottom: 24px;"),
                tags$p("Secretaria Municipal de Saúde - Centro de Informações em Vigilância em Saúde - CIEVS/ Monitoramento de Epidemiológico da Dengue", style = "color: #E2E8F0; font-size: 1.05rem; font-weight: 500; max-width: 850px; margin: 0 auto 16px auto;"),
                tags$span(class = "badge bg-white text-primary fw-bold px-3 py-2", style = "font-size: 0.88rem;", paste0("Emissão Oficial: ", format(Sys.Date(), "%d/%m/%Y")))
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: DADOS DE DESTAQUE - CARTÕES (ABA 1: VISÃO GERAL)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_cartoes') && input.sel_g_cartoes == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("PAINEL DE INDICADORES ESTRATÉGICOS & DADOS DE DESTAQUE"),
                  tags$p("Consolidação Global dos Casos de Dengue, Incidência e Mortalidade (Aba 1: Visão Geral)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_cartoes", inline = TRUE))
              ),
              layout_column_wrap(
                width = 1/4, fill = FALSE,
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-primary", textOutput("apres_kpi_total_dest")), tags$div(class = "slide-kpi-label", "Total Notificados")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-warning", textOutput("apres_kpi_provaveis_dest")), tags$div(class = "slide-kpi-label", "Casos Prováveis")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-danger", textOutput("apres_kpi_confirmados_dest")), tags$div(class = "slide-kpi-label", "Casos Confirmados")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-info", textOutput("apres_kpi_inconclusivos_dest")), tags$div(class = "slide-kpi-label", "Inconclusivos"))
              ),
              div(style = "margin-top: 10px;"),
              layout_column_wrap(
                width = 1/4, fill = FALSE,
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-secondary", textOutput("apres_kpi_descartados_dest")), tags$div(class = "slide-kpi-label", "Descartados")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-dark", textOutput("apres_kpi_obitos_dest")), tags$div(class = "slide-kpi-label", "Óbitos por Dengue")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-danger", textOutput("apres_kpi_incidencia_dest")), tags$div(class = "slide-kpi-label", "Incidência (por 100 mil hab.)")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-danger", textOutput("apres_kpi_letalidade_dest")), tags$div(class = "slide-kpi-label", "Letalidade Geral (%)"))
              ),
              tags$div(
                class = "slide-insight-box mt-3",
                tags$i(class = "fa-solid fa-clipboard-check me-2 text-primary"),
                tags$b("Síntese Executiva dos Indicadores: "),
                textOutput("apres_insight_cartoes", inline = TRUE)
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: CURVA EPIDÊMICA SEMANAL (ABA 1: VISÃO GERAL)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_semanas') && input.sel_g_semanas == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("CURVA EPIDÊMICA DA DENGUE & EVOLUÇÃO SEMANAL"),
                  tags$p("Distribuição das Notificações por Semana Epidemiológica nos Anos Selecionados (Aba 1: Visão Geral)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_semanas", inline = TRUE))
              ),
              layout_column_wrap(
                width = 1/4, fill = FALSE,
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-primary", textOutput("apres_kpi_total")), tags$div(class = "slide-kpi-label", "Total Notificados")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-danger", textOutput("apres_kpi_confirmados")), tags$div(class = "slide-kpi-label", "Casos Confirmados")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-warning", textOutput("apres_kpi_incidencia")), tags$div(class = "slide-kpi-label", "Incidência (por 100 mil hab.)")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-dark", textOutput("apres_kpi_letalidade")), tags$div(class = "slide-kpi-label", "Letalidade Geral (%)"))
              ),
              div(style = "margin-top: 10px;"),
              plotlyOutput("plot_apres_semanas", height = "390px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-lightbulb me-2 text-warning"),
                tags$b("Destaque Executivo: "),
                textOutput("apres_insight_slide1", inline = TRUE)
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: MAPA POR BAIRRO (ABA 2: ANÁLISE ESPACIAL)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_mapa_bairros') && input.sel_g_mapa_bairros == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("MAPA TERRITORIAL DA DENGUE — DIVISÃO POR BAIRROS"),
                  tags$p(textOutput("apres_legenda_mapa_bairros", inline = TRUE))
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_mapa_bairros", inline = TRUE))
              ),
              leafletOutput("plot_apres_mapa_bairros", height = "520px"),
              tags$div(
                class = "slide-insight-box mt-2",
                tags$i(class = "fa-solid fa-map-location-dot me-2 text-primary"),
                tags$b("Vigilância Espacial por Bairro: "),
                textOutput("apres_insight_mapa_bairros", inline = TRUE)
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: MAPA POR DISTRITO (ABA 2: ANÁLISE ESPACIAL)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_mapa_distritos') && input.sel_g_mapa_distritos == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("MAPA TERRITORIAL DA DENGUE — DIVISÃO POR DISTRITO"),
                  tags$p(textOutput("apres_legenda_mapa_distritos", inline = TRUE))
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_mapa_distritos", inline = TRUE))
              ),
              leafletOutput("plot_apres_mapa_distritos", height = "520px"),
              tags$div(
                class = "slide-insight-box mt-2",
                tags$i(class = "fa-solid fa-draw-polygon me-2 text-primary"),
                tags$b("Vigilância Distrital: "),
                textOutput("apres_insight_mapa_distritos", inline = TRUE)
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: TOP BAIRROS NOTIFICADOS (ABA 2: ANÁLISE ESPACIAL)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_bairros') && input.sel_g_bairros == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("VIGILÂNCIA TERRITORIAL & BAIRROS CRÍTICOS"),
                  tags$p("Concentração Espacial e Priorização Operacional das Brigadas do CCZ (Aba 2: Análise Espacial)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_bairros", inline = TRUE))
              ),
              layout_column_wrap(
                width = 1/3, fill = FALSE,
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-danger", textOutput("apres_kpi_top_bairro")), tags$div(class = "slide-kpi-label", "Bairro Mais Notificado")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-warning", textOutput("apres_kpi_top_incidencia_bairro")), tags$div(class = "slide-kpi-label", "Maior Incidência")),
                tags$div(class = "slide-kpi-card", tags$div(class = "slide-kpi-value text-primary", textOutput("apres_kpi_bairros_atingidos")), tags$div(class = "slide-kpi-label", "Bairros com Notificações"))
              ),
              div(style = "margin-top: 10px;"),
              plotlyOutput("plot_apres_bairros", height = "390px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-map-location-dot me-2 text-primary"),
                tags$b("Diretriz Operacional: "),
                textOutput("apres_insight_slide2", inline = TRUE)
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: SÍNTESE POR DISTRITO (ABA 2: ANÁLISE ESPACIAL)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_distritos') && input.sel_g_distritos == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("SÍNTESE DISTRITAL ADMINISTRATIVA DE BELÉM"),
                  tags$p("Consolidação por Distrito (População Censo 2022, Casos, Incidência e Letalidade) (Aba 2: Análise Espacial)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_distritos", inline = TRUE))
              ),
              tags$div(class = "custom-scroll-table", style = "max-height: 440px;", tableOutput("tabela_apres_distritos")),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-circle-info me-2 text-info"),
                tags$b("Nota Distrital: "),
                "Os distritos administrativos organizam o planejamento regional da SESMA para suprimento de testes rápidos e escalonamento dos agentes de endemias."
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: PIRÂMIDE ETÁRIA (ABA 3: SOCIODEMOGRÁFICO)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_piramide') && input.sel_g_piramide == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("PERFIL DEMOGRÁFICO & PIRÂMIDE ETÁRIA"),
                  tags$p("Distribuição Cruzada das Notificações por Faixa Etária e Sexo (Aba 3: Sociodemográfico)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_piramide", inline = TRUE))
              ),
              plotlyOutput("plot_apres_piramide", height = "410px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-users me-2 text-info"),
                tags$b("Conclusão Demográfica: "),
                textOutput("apres_insight_slide3", inline = TRUE)
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: DISTRIBUIÇÃO POR RAÇA/COR (ABA 3: SOCIODEMOGRÁFICO)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_sexo_raca') && input.sel_g_sexo_raca == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("PERFIL POR RAÇA/COR DECLARADA"),
                  tags$p("Distribuição dos Casos Notificados Segundo Autodeclaração Étnico-Racial (Aba 3: Sociodemográfico)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_sexo_raca", inline = TRUE))
              ),
              plotlyOutput("plot_apres_sexo_raca", height = "410px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-fingerprint me-2 text-primary"),
                tags$b("Qualidade da Notificação: "),
                "O preenchimento do campo raça/cor no SINAN subsidia a equidade nas políticas públicas municipais de saúde."
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: CLASSIFICAÇÃO FINAL (ABA 4: DIAGNÓSTICO & DESFECHOS)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_classificacao') && input.sel_g_classificacao == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("CLASSIFICAÇÃO FINAL DOS CASOS"),
                  tags$p("Proporção de Casos Confirmados, Descartados, Suspeitos e Inconclusivos (Aba 4: Diagnóstico & Desfechos)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_classificacao", inline = TRUE))
              ),
              plotlyOutput("plot_apres_classificacao", height = "400px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-vial-circle-check me-2 text-primary"),
                tags$b("Vigilância Diagnóstica: "),
                "Proporção de confirmação laboratorial e clínica reflete a resolutividade da triagem e testagem nas portas de entrada."
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: CRITÉRIO DE CONFIRMAÇÃO / DIAGNÓSTICO (ABA 4: DIAGNÓSTICO & DESFECHOS)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_criterio') && input.sel_g_criterio == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("CRITÉRIO DE CONFIRMAÇÃO / DIAGNÓSTICO"),
                  tags$p("Distribuição dos Casos por Critério Diagnóstico Laboratorial vs. Clínico-Epidemiológico (Aba 4: Diagnóstico & Desfechos)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_criterio", inline = TRUE))
              ),
              plotlyOutput("plot_apres_criterio", height = "400px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-microscope me-2 text-info"),
                tags$b("Oportunidade Diagnóstica: "),
                "A confirmação laboratorial oportuna permite caracterizar o perfil sorológico dos casos e a validação do limiar de transmissão local."
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: CASOS POR SOROTIPO VIRAL (ABA 4: DIAGNÓSTICO & DESFECHOS)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_sorotipo') && input.sel_g_sorotipo == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("DISTRIBUIÇÃO POR SOROTIPO VIRAL"),
                  tags$p("Circulação dos Sorotipos DENV-1, DENV-2, DENV-3 e DENV-4 Identificados no Período (Aba 4: Diagnóstico & Desfechos)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_sorotipo", inline = TRUE))
              ),
              plotlyOutput("plot_apres_sorotipo", height = "400px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-virus me-2 text-danger"),
                tags$b("Vigilância Virologica: "),
                "A introdução ou reintrodução de sorotipos distintos em populações suscetíveis eleva o risco de formas graves e hospitalizações."
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: EVOLUÇÃO CLÍNICA & ÓBITOS (ABA 4: DIAGNÓSTICO & DESFECHOS)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_evolucao') && input.sel_g_evolucao == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("EVOLUÇÃO CLÍNICA & VIGILÂNCIA DE ÓBITOS"),
                  tags$p("Desfechos dos Pacientes, Cura e Monitoramento da Letalidade (Aba 4: Diagnóstico & Desfechos)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_evolucao", inline = TRUE))
              ),
              plotlyOutput("plot_apres_evolucao", height = "400px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-heart-pulse me-2 text-danger"),
                tags$b("Vigilância de Letalidade: "),
                textOutput("apres_insight_slide4", inline = TRUE)
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: TOP UNIDADES NOTIFICADORAS (ABA 5: UNIDADES NOTIFICADORAS)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_unidades') && input.sel_g_unidades == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("REDE NOTIFICADORA DE SAÚDE"),
                  tags$p("Top 10 Estabelecimentos de Saúde com Maior Volume de Notificações (Aba 5: Unidades Notificadoras)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_unidades", inline = TRUE))
              ),
              plotlyOutput("plot_apres_unidades", height = "400px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-hospital me-2 text-success"),
                tags$b("Diagnóstico Assistencial: "),
                textOutput("apres_insight_slide5", inline = TRUE)
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: CANAL ENDÊMICO (ABA 6: ANÁLISE EPIDEMIOLÓGICA - SEMPRE 2026)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_canal') && input.sel_g_canal == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("CANAL ENDÊMICO & DIAGRAMA DE CONTROLE (ANO VIGENTE: 2026)"),
                  tags$p("Histórico Municipal de Referência com Limiar Epidêmico e Casos de 2026 (Aba 6: Análise Epidemiológica)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_canal", inline = TRUE))
              ),
              plotlyOutput("plot_apres_canal", height = "410px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-chart-line me-2 text-primary"),
                tags$b("Vigilância Preditiva: "),
                textOutput("apres_insight_slide6", inline = TRUE)
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: EVOLUÇÃO MENSAL DE INCIDÊNCIA (ABA 6: ANÁLISE EPIDEMIOLÓGICA)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_evol_incidencia') && input.sel_g_evol_incidencia == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("EVOLUÇÃO HISTÓRICA DA TAXA DE INCIDÊNCIA"),
                  tags$p("Taxa Mensal de Incidência por 100 mil habitantes no Período Selecionado (Aba 6: Análise Epidemiológica)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_evol_incidencia", inline = TRUE))
              ),
              plotlyOutput("plot_apres_evol_incidencia", height = "400px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-arrow-trend-up me-2 text-primary"),
                tags$b("Tendência Histórica: "),
                "Acompanhamento da sazonalidade da dengue em Belém nos meses chuvosos (dezembro a maio) para antecipação de leitos."
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: EVOLUÇÃO MENSAL DE LETALIDADE (ABA 6: ANÁLISE EPIDEMIOLÓGICA)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_evol_letalidade') && input.sel_g_evol_letalidade == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("EVOLUÇÃO HISTÓRICA DA TAXA DE LETALIDADE"),
                  tags$p("Taxa Mensal de Letalidade (%) Frente à Meta Rigorosa do Ministério da Saúde (Aba 6: Análise Epidemiológica)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_evol_letalidade", inline = TRUE))
              ),
              plotlyOutput("plot_apres_evol_letalidade", height = "400px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-heart-pulse me-2 text-danger"),
                tags$b("Meta de Letalidade: "),
                "O Ministério da Saúde preconiza letalidade inferior a 0,05% em casos suspeitos/confirmados. Valores acima demandam capacitação de manejo clínico."
              )
            )
          ),
          
          # -------------------------------------------------------------
          # SLIDE: SÉRIE ANUAL DE CASOS E ÓBITOS (ABA 6: ANÁLISE EPIDEMIOLÓGICA)
          # -------------------------------------------------------------
          conditionalPanel(
            condition = "(input.modo_apres_layout == 'todos' || input.sel_slide_apres == 'g_casos_obitos_ano') && input.sel_g_casos_obitos_ano == true",
            tags$div(
              class = "slide-frame",
              tags$div(
                class = "d-flex justify-content-between align-items-center slide-header",
                tags$div(
                  tags$h4("SÉRIE ANUAL DE CASOS NOTIFICADOS, PROVÁVEIS E CONFIRMADOS"),
                  tags$p("Comparativo Anual Consolidado para Avaliação do Histórico Epidêmico de Belém (Aba 6: Análise Epidemiológica)")
                ),
                tags$span(class = "badge bg-primary px-3 py-2 text-uppercase fw-bold", style = "font-size: 0.72rem;", textOutput("badge_g_casos_obitos_ano", inline = TRUE))
              ),
              plotlyOutput("plot_apres_casos_ano", height = "400px"),
              tags$div(
                class = "slide-insight-box",
                tags$i(class = "fa-solid fa-chart-simple me-2 text-secondary"),
                tags$b("Análise de Carga Anual: "),
                "Demonstra os ciclos epidêmicos da dengue no município de Belém ao longo da série histórica do SINAN."
              )
            )
          )
        )
      )
    )
  )
)
)

)
# ==============================================================================
# 4. LÓGICA E PROCESSAMENTO (SERVER)
# ==============================================================================
server <- function(input, output, session) {
  
  # Bases Reativas: O histórico já inicia carregado com a série 2015-2026 completa
  dados_historico <- reactiveVal(HISTORICO_BASE)
  dados_upload    <- reactiveVal(NULL)
  
  observeEvent(input$btn_pdf, {
    runjs("window.print();")
  })
  
  # Padronização de dados SINAN (Upload DBF/DBC)
  padronizar_sinan_upload <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    
    names(df) <- toupper(trimws(names(df)))
    df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
    df <- df %>% mutate(across(where(is.character), ~ trimws(iconv(as.character(.), from = "latin1", to = "UTF-8", sub = ""))))
    
    # 1. Município de Residência
    col_mun_res <- intersect(c("ID_MN_RESI", "CO_MUN_RES", "ID_MUNI_RE", "MUN_RES", "CD_MUNICIPIO_RES"), names(df))
    df$COD_MUN_RES <- if(length(col_mun_res) > 0) trimws(as.character(df[[col_mun_res[1]]])) else "150140"
    df$COD_MUN_RES[is.na(df$COD_MUN_RES) | df$COD_MUN_RES == ""] <- "150140"
    
    # 2. Data de Notificação e Ano
    col_dt <- intersect(c("DT_NOTIFICACAO", "DT_NOTIFIC", "DT_NOTI", "DT_NOT"), names(df))
    if(length(col_dt) > 0) {
      dt_tmp <- suppressWarnings(as.Date(as.character(df[[col_dt[1]]])))
      dt_tmp[is.na(dt_tmp)] <- Sys.Date()
      df$DT_NOTIFIC <- dt_tmp
    } else {
      df$DT_NOTIFIC <- Sys.Date()
    }
    
    col_ano <- intersect(c("ANO_NOTIFICACAO", "NU_ANO", "ANO_NOTIF", "ANO"), names(df))
    if(length(col_ano) > 0) {
      ano_tmp <- suppressWarnings(as.numeric(as.character(df[[col_ano[1]]])))
      ano_tmp[is.na(ano_tmp)] <- as.numeric(year(df$DT_NOTIFIC[is.na(ano_tmp)]))
      df$ANO_NOTIF <- as.numeric(ano_tmp)
    } else {
      df$ANO_NOTIF <- as.numeric(year(df$DT_NOTIFIC))
    }
    df$ANO_NOTIF[is.na(df$ANO_NOTIF)] <- as.numeric(year(Sys.Date()))
    
    # 3. Semana Epidemiológica
    col_sem <- intersect(c("SEMANA_NOTIFICACAO", "SEM_NOT", "SEMANA_NOT", "SEM_NOTIF"), names(df))
    if(length(col_sem) > 0) {
      sem_str <- as.character(df[[col_sem[1]]])
      sem_num <- suppressWarnings(as.numeric(substr(sem_str, nchar(sem_str)-1, nchar(sem_str))))
      sem_num[is.na(sem_num)] <- epiweek(df$DT_NOTIFIC[is.na(sem_num)])
      df$SEMANA_EPID <- as.numeric(sem_num)
      # Alinhamento do ano epidemiológico oficial SINAN (primeiros 4 dígitos da SEM_NOT)
      if(any(nchar(sem_str) == 6, na.rm = TRUE)) {
        ano_epi <- suppressWarnings(as.numeric(substr(sem_str, 1, 4)))
        idx_val <- which(!is.na(ano_epi) & ano_epi >= 2010 & ano_epi <= 2035)
        df$ANO_NOTIF[idx_val] <- ano_epi[idx_val]
      }
    } else {
      df$SEMANA_EPID <- as.numeric(epiweek(df$DT_NOTIFIC))
    }
    df$SEMANA_EPID[is.na(df$SEMANA_EPID) | df$SEMANA_EPID < 1 | df$SEMANA_EPID > 53] <- 1
    
    # 4. Sexo
    col_sexo <- intersect(c("CS_SEXO", "SEXO"), names(df))
    df$CS_SEXO_VAL <- if(length(col_sexo) > 0) as.character(df[[col_sexo[1]]]) else "I"
    df$CS_SEXO <- case_when(
      df$CS_SEXO_VAL %in% c("M", "1", "MASCULINO") ~ "Masculino",
      df$CS_SEXO_VAL %in% c("F", "2", "FEMININO") ~ "Feminino",
      TRUE ~ "Ignorado"
    )
    
    # 5. Raça/Cor
    col_raca <- intersect(c("RACA_COR", "CS_RACA", "RACA"), names(df))
    df$CS_RACA_VAL <- if(length(col_raca) > 0) as.character(df[[col_raca[1]]]) else "9"
    df$CS_RACA <- case_when(
      df$CS_RACA_VAL %in% c("1", "BRANCA", "Branca") ~ "Branca",
      df$CS_RACA_VAL %in% c("2", "PRETA", "Preta") ~ "Preta",
      df$CS_RACA_VAL %in% c("3", "AMARELA", "Amarela") ~ "Amarela",
      df$CS_RACA_VAL %in% c("4", "PARDA", "Parda") ~ "Parda",
      df$CS_RACA_VAL %in% c("5", "INDIGENA", "Indígena") ~ "Indígena",
      TRUE ~ "Ignorado/Sem Info"
    )
    
    # 6. Classificação Final
    col_classi <- intersect(c("CLASSIFICACAO_FINAL", "CLASSI_FIN", "CLASSI_FIN_N", "CLASSIFIC"), names(df))
    df$CLASSI_FIN_VAL <- if(length(col_classi) > 0) trimws(as.character(df[[col_classi[1]]])) else "0"
    df$CLASSI_FIN <- case_when(
      df$CLASSI_FIN_VAL %in% c("10", "11", "12", "1", "Confirmado", "CONFIRMADO", "Dengue", "Dengue com sinais de alarme", "Dengue grave") ~ "Confirmado",
      df$CLASSI_FIN_VAL %in% c("8", "Inconclusivo", "INCONCLUSIVO") ~ "Inconclusivo",
      df$CLASSI_FIN_VAL %in% c("5", "2", "Descartado", "DESCARTADO") ~ "Descartado",
      TRUE ~ "Em Investigação/Suspeito"
    )
    
    # 7. Evolução
    col_evo <- intersect(c("EVOLUCAO_CASO", "EVOLUCAO", "EVOLUCAO_N"), names(df))
    df$EVOLUCAO_VAL <- if(length(col_evo) > 0) trimws(as.character(df[[col_evo[1]]])) else "9"
    df$EVOLUCAO <- case_when(
      df$EVOLUCAO_VAL %in% c("1", "Cura", "CURA") ~ "Cura",
      df$EVOLUCAO_VAL %in% c("2", "Óbito por dengue", "OBITO POR DENGUE") ~ "Óbito por dengue",
      df$EVOLUCAO_VAL %in% c("3", "Óbito por outras causas") ~ "Óbito por outras causas",
      TRUE ~ "Ignorado/Investigação"
    )
    
    # 8. Sorotipo (Busca dinâmica por tp_sorotipo, TP_SOROTIPO, RESUL_SORO, etc.)
    col_soro <- intersect(c("tp_sorotipo", "TP_SOROTIPO", "TP_SOROTIP", "tp_sorotip", "RESUL_SORO", "RES_SORO", "SOROTIPO", "sorotipo"), names(df))
    df$tp_sorotipo_val <- if(length(col_soro) > 0) trimws(as.character(df[[col_soro[1]]])) else "9"
    df$tp_sorotipo <- case_when(
      df$tp_sorotipo_val %in% c("1", "DENV 1", "DENV-1", "DENV1", "1 - DENV 1") ~ "DENV 1",
      df$tp_sorotipo_val %in% c("2", "DENV 2", "DENV-2", "DENV2", "2 - DENV 2") ~ "DENV 2",
      df$tp_sorotipo_val %in% c("3", "DENV 3", "DENV-3", "DENV3", "3 - DENV 3") ~ "DENV 3",
      df$tp_sorotipo_val %in% c("4", "DENV 4", "DENV-4", "DENV4", "4 - DENV 4") ~ "DENV 4",
      TRUE ~ "Não Realizado/Sem Info"
    )
    df$RESUL_SORO  <- df$tp_sorotipo
    df$TP_SOROTIPO <- df$tp_sorotipo
    
    # 9. Bairro e Mapeamento para o Território Oficial
    col_bairro <- intersect(c("BAIRRO_RES", "NM_BAIRRO", "NO_BAIRRO", "NM_BAIRR_N", "ID_BAIRRO"), names(df))
    df$NM_BAIRRO <- if(length(col_bairro) > 0) toupper(trimws(as.character(df[[col_bairro[1]]]))) else "NÃO INFORMADO"
    df$NM_BAIRRO[is.na(df$NM_BAIRRO) | df$NM_BAIRRO == ""] <- "NÃO INFORMADO"
    df$NM_BAIRRO_MAPA <- mapear_bairro_mapa(df$NM_BAIRRO)
    
    # 10. Idade
    col_idade <- intersect(c("IDADE_ANOS", "NU_IDADE_N", "IDADE_N", "NU_IDADE"), names(df))
    if(length(col_idade) > 0) {
      val_raw <- suppressWarnings(as.numeric(as.character(df[[col_idade[1]]])))
      df$IDADE_ANOS <- case_when(
        is.na(val_raw) ~ 0,
        val_raw >= 4000 & val_raw < 5000 ~ as.numeric(val_raw - 4000),
        val_raw >= 100 ~ as.numeric(val_raw),
        TRUE ~ as.numeric(val_raw)
      )
    } else {
      df$IDADE_ANOS <- 0
    }
    
    # 11. Critério de Confirmação
    crit_col <- intersect(c("CRITERIO_DIAGNOSTICO", "CRITERIO", "CRIT_CONS", "CRITERIO_N"), names(df))
    if(length(crit_col) > 0) {
      df$CRITERIO_FINAL <- case_when(
        as.character(df[[crit_col[1]]]) %in% c("1", "Laboratorial", "LABORATORIAL") ~ "Laboratorial",
        as.character(df[[crit_col[1]]]) %in% c("2", "Clínico-Epidemiológico", "CLINICO-EPIDEMIOLOGICO") ~ "Clínico-Epidemiológico",
        TRUE ~ "Em Investigação"
      )
    } else {
      df$CRITERIO_FINAL <- "Não Informado"
    }
    
    # 12. Unidade Notificadora
    col_unidade <- intersect(c("NM_NOTIFIC", "ID_UNIDADE", "CO_UNI_NOT", "ID_UNI_NOT", "ID_ESTAB", "CNES"), names(df))
    if(length(col_unidade) > 0) {
      df$ID_UNIDADE_RAW <- trimws(as.character(df[[col_unidade[1]]]))
      dic_estab <- obter_dicionario_estabelecimentos()
      dic_vec <- unlist(dic_estab)
      nome_trad <- unname(dic_vec[df$ID_UNIDADE_RAW])
      df$UNIDADE_NOTIF <- ifelse(!is.na(nome_trad) & nome_trad != "", nome_trad, df$ID_UNIDADE_RAW)
      df$UNIDADE_NOTIF[is.na(df$UNIDADE_NOTIF) | df$UNIDADE_NOTIF == "" | df$UNIDADE_NOTIF == "0"] <- "NÃO INFORMADO"
    } else {
      df$ID_UNIDADE_RAW <- NA_character_
      df$UNIDADE_NOTIF <- "NÃO INFORMADO"
    }
    
    df$ORIGEM_DADO <- "upload_vigente"
    return(df)
  }
  
  # Ação: Upload de DBF/DBC Ano Vigente
  observeEvent(input$file_dbf, {
    req(input$file_dbf)
    showNotification("Processando arquivo DBF/DBC do ano vigente...", type = "message", id = "msg_upload", duration = NULL)
    tryCatch({
      caminho <- input$file_dbf$datapath
      extensao <- tolower(tools::file_ext(input$file_dbf$name))
      
      if (extensao == "dbc") {
        if (!requireNamespace("read.dbc", quietly = TRUE)) {
          stop("Pacote 'read.dbc' necessário para ler arquivos .dbc no R.")
        }
        df_raw <- read.dbc::read.dbc(caminho)
      } else {
        df_raw <- foreign::read.dbf(caminho, as.is = TRUE)
      }
      
      df_proc <- padronizar_sinan_upload(df_raw)
      dados_upload(df_proc)
      removeNotification("msg_upload")
      showNotification(
        paste("Arquivo vigente carregado com sucesso:", format_num(nrow(df_proc)), "casos adicionados/atualizados."),
        type = "default", duration = 6
      )
    }, error = function(e) {
      removeNotification("msg_upload")
      showNotification(paste("Erro ao processar arquivo:", e$message), type = "error", duration = 12)
    })
  })
  
  # Ação: Processar arquivos da pasta 'dados/' com Mecanismo de Auditoria e Ingestão
  observeEvent(input$btn_processar_pasta_dados, {
    showNotification("Procurando e auditando arquivos na pasta 'dados/'...", type = "message", id = "msg_proc_dados", duration = NULL)
    tryCatch({
      caminho_dados <- if (dir.exists("dados")) "dados" else file.path(getwd(), "dados")
      arquivos <- if (dir.exists(caminho_dados)) list.files(caminho_dados, pattern = "\\.(dbf|DBF|dbc|DBC|rds|RDS)$", full.names = TRUE) else character(0)
      arquivos <- arquivos[!basename(arquivos) %in% c("mapa_bairros_belem.rds", "mapa_distritos_belem.rds", "mapa_ilhas_belem.rds", "estabelecimentos_pa.rds", "notificacoes_estabelecimentos.rds", "dados_historico_dengue_exemplo.rds")]
      
      if (length(arquivos) == 0) {
        removeNotification("msg_proc_dados")
        showModal(modalDialog(
          title = "Pasta 'dados/' Sem Novos Arquivos",
          tags$p("Nenhum novo arquivo .dbf ou .dbc foi localizado na pasta 'dados/'."),
          tags$div(
            class = "alert alert-info py-2 small",
            tags$b("Instrução:"),
            tags$br(),
            "Copie os arquivos de atualização do SINAN (ex: DENGON2026.dbf) para a pasta 'dados/' e clique neste botão novamente."
          ),
          easyClose = TRUE,
          footer = modalButton("Fechar")
        ))
        return()
      }
      
      if (file.exists("atualizar_dados.R")) {
        system2("Rscript", args = "atualizar_dados.R", stdout = TRUE, stderr = TRUE)
      }
      
      removeNotification("msg_proc_dados")
      
      # Recarrega a base em memória dinamicamente
      if (file.exists("dados_historico_dengue.rds")) {
        novo_hist <- readRDS("dados_historico_dengue.rds")
        HISTORICO_BASE <<- novo_hist
        dados_historico(novo_hist)
      }
      
      showModal(modalDialog(
        title = tags$span(tags$i(class = "fa-solid fa-circle-check text-success me-2"), "Auditoria & Ingestão Concluídas!"),
        tags$p(paste0("Foram processados ", length(arquivos), " arquivo(s) da pasta 'dados/'.")),
        tags$div(
          class = "alert alert-success py-2",
          tags$b("Base histórica municipal atualizada com sucesso!"),
          tags$br(),
          paste0("Total consolidado de notificações: ", format_num(nrow(HISTORICO_BASE)), " registros.")
        ),
        tags$p("O laudo da auditoria de qualidade foi registrado em 'relatorio_auditoria_dados.md'."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }, error = function(e) {
      removeNotification("msg_proc_dados")
      showNotification(paste("Erro durante processamento da pasta dados:", e$message), type = "error", duration = 12)
    })
  })
  
  # Base Unificada Consolidada (Histórico Salvo + Upload do Ano Vigente)
  dados_base <- reactive({
    dh <- dados_historico()
    du <- dados_upload()
    
    if ((is.null(dh) || nrow(dh) == 0) && (is.null(du) || nrow(du) == 0)) return(data.frame())
    if (!is.null(dh) && (is.null(du) || nrow(du) == 0)) return(dh)
    if ((is.null(dh) || nrow(dh) == 0) && !is.null(du)) return(du)
    
    anos_upload <- unique(du$ANO_NOTIF)
    dh_filtrado <- dh %>% filter(!ANO_NOTIF %in% anos_upload)
    
    bind_rows(dh_filtrado, du)
  })
  
  output$status_dados <- renderText({
    dh <- dados_historico()
    if (is.null(dh) || nrow(dh) == 0) return("Base histórica não localizada.")
    anos <- sort(unique(na.omit(dh$ANO_NOTIF)))
    tipo_base <- if (DADOS_MODO_DEMO) " [Modo Demo Sintético - LGPD]" else " [Base Oficial SINAN]"
    paste0("Série histórica ativa: ", min(anos), " a ", max(anos), " (", format_num(nrow(dh)), " notificações)", tipo_base, ".")
  })
  
  output$status_upload <- renderText({
    du <- dados_upload()
    if (is.null(du) || nrow(du) == 0) return("")
    paste0("+ Upload vigente ativo: ", format_num(nrow(du)), " casos (Ano: ", paste(sort(unique(du$ANO_NOTIF)), collapse = ", "), ")")
  })
  
  # Controles de Filtros Reativos Dinâmicos com suporte interativo a multi-anos
  output$filtro_anos_ui <- renderUI({
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) {
      return(p("Aguardando dados...", style = "font-size:0.8rem; color:#64748B;"))
    }
    
    anos_disp <- sort(unique(na.omit(as.numeric(df$ANO_NOTIF))), decreasing = TRUE)
    
    if (input$modo_ano == "individual") {
      selectInput(
        inputId = "sel_anos_individual",
        label = "Selecione o Ano de Análise:",
        choices = as.character(anos_disp),
        selected = as.character(anos_disp[1])
      )
    } else {
      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$span("Anos Selecionados:", style = "font-size:0.82rem; font-weight:600; color:#0000A1;"),
          tags$div(
            actionLink("btn_todos_anos", "Todos", class = "badge bg-primary text-decoration-none me-1"),
            actionLink("btn_ultimos_anos", "Últimos 3", class = "badge bg-secondary text-decoration-none me-1"),
            actionLink("btn_limpar_anos", "Limpar", class = "badge bg-light text-dark text-decoration-none")
          )
        ),
        selectizeInput(
          inputId = "sel_anos_multi",
          label = NULL,
          choices = as.character(anos_disp),
          selected = as.character(anos_disp[1:2]),
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = "Clique ou digite anos para adicionar..."
          )
        )
      )
    }
  })
  
  # Ações rápidas para seleção de múltiplos anos
  observeEvent(input$btn_todos_anos, {
    df <- dados_base()
    anos_disp <- sort(unique(na.omit(as.numeric(df$ANO_NOTIF))), decreasing = TRUE)
    updateSelectizeInput(session, "sel_anos_multi", selected = as.character(anos_disp))
  })
  
  observeEvent(input$btn_ultimos_anos, {
    df <- dados_base()
    anos_disp <- sort(unique(na.omit(as.numeric(df$ANO_NOTIF))), decreasing = TRUE)
    updateSelectizeInput(session, "sel_anos_multi", selected = as.character(head(anos_disp, 3)))
  })
  
  observeEvent(input$btn_limpar_anos, {
    updateSelectizeInput(session, "sel_anos_multi", selected = character(0))
  })
  
  # Vetor unificado de anos ativos
  anos_ativos <- reactive({
    df <- dados_base()
    anos_disp <- if (!is.null(df) && nrow(df) > 0) sort(unique(na.omit(as.numeric(df$ANO_NOTIF))), decreasing = TRUE) else 2026
    ano_padrao <- anos_disp[1]
    
    modo <- if (!is.null(input$modo_ano)) input$modo_ano else "individual"
    if (modo == "individual") {
      val <- if (!is.null(input$sel_anos_individual)) input$sel_anos_individual else as.character(ano_padrao)
      return(suppressWarnings(as.numeric(val)))
    } else {
      if (is.null(input$sel_anos_multi) || length(input$sel_anos_multi) == 0) return(NULL)
      return(suppressWarnings(as.numeric(input$sel_anos_multi)))
    }
  })
  
  output$filtro_bairros_ui <- renderUI({
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    bairros <- sort(unique(df$NM_BAIRRO))
    selectizeInput("sel_bairros", "Filtrar por Bairro(s):", choices = c("TODOS", bairros), selected = "TODOS", multiple = TRUE)
  })
  
  output$filtro_unidades_ui <- renderUI({
    df <- dados_base()
    unidades_base <- if(!is.null(df) && nrow(df) > 0) {
      unique(df$UNIDADE_NOTIF[!is.na(df$UNIDADE_NOTIF) & df$UNIDADE_NOTIF != "" & df$UNIDADE_NOTIF != "NÃO INFORMADO"])
    } else character(0)
    
    unidades_estab <- if(!is.null(NOTIFICACOES_ESTABELECIMENTOS) && nrow(NOTIFICACOES_ESTABELECIMENTOS) > 0) {
      unique(as.character(NOTIFICACOES_ESTABELECIMENTOS$nome_raw))
    } else character(0)
    
    unidades_todas <- sort(unique(c(unidades_base, unidades_estab)))
    if(length(unidades_todas) == 0) return(NULL)
    
    selectizeInput(
      inputId = "sel_unidade",
      label = "Unidade Notificadora:",
      choices = c("TODAS", unidades_todas),
      selected = "TODAS",
      multiple = TRUE,
      options = list(
        placeholder = "Digite para buscar a unidade...",
        plugins = list("remove_button"),
        maxOptions = 300
      )
    )
  })
  
  output$filtro_classificacao_ui <- renderUI({
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    classif <- sort(unique(df$CLASSI_FIN))
    selectizeInput("sel_classif", "Classificação do Caso:", choices = c("TODAS", classif), selected = "TODAS", multiple = TRUE)
  })

  output$filtro_sexo_ui <- renderUI({
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    sexos <- sort(unique(df$CS_SEXO))
    selectizeInput("sel_sexo", "Sexo:", choices = c("TODOS", sexos), selected = "TODOS", multiple = TRUE)
  })

  output$filtro_evolucao_ui <- renderUI({
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    evols <- sort(unique(df$EVOLUCAO))
    selectizeInput("sel_evolucao", "Evolução do Caso:", choices = c("TODAS", evols), selected = "TODAS", multiple = TRUE)
  })

  output$filtro_criterio_ui <- renderUI({
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    crits <- sort(unique(df$CRITERIO_FINAL))
    selectizeInput("sel_criterio", "Critério Diagnóstico:", choices = c("TODOS", crits), selected = "TODOS", multiple = TRUE)
  })

  output$filtro_sorotipo_ui <- renderUI({
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    col_s <- if("tp_sorotipo" %in% names(df)) "tp_sorotipo" else "RESUL_SORO"
    soros <- sort(unique(df[[col_s]]))
    selectizeInput("sel_sorotipo", "Sorotipo Viral:", choices = c("TODOS", soros), selected = "TODOS", multiple = TRUE)
  })
  
  # Dataset Filtrado Globalmente (acumula somas para todos os anos selecionados)
  df_filtrado <- reactive({
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # 1. Filtro Territorial
    if (input$filtro_abrangencia == "residencia") {
      df <- df %>% filter(COD_MUN_RES %in% c("150140", "1501400", "BELEM") | grepl("150140", COD_MUN_RES))
    }
    
    # 2. Filtro de Anos (acumulação dinâmica de todos os anos ativos)
    anos_sel <- anos_ativos()
    if (!is.null(anos_sel) && length(anos_sel) > 0) {
      anos_sel <- anos_sel[!is.na(anos_sel)]
      if (length(anos_sel) > 0) {
        df <- df %>% filter(as.numeric(ANO_NOTIF) %in% anos_sel)
      }
    }
    
    # 3. Filtro de Semanas Epidemiológicas
    if (!is.null(input$sel_semana)) {
      df <- df %>% filter(SEMANA_EPID >= as.numeric(input$sel_semana[1]), SEMANA_EPID <= as.numeric(input$sel_semana[2]))
    }
    
    # 4. Filtro de Bairros
    if (!is.null(input$sel_bairros) && !"TODOS" %in% input$sel_bairros) {
      df <- df %>% filter(NM_BAIRRO %in% input$sel_bairros)
    }

    # 4b. Filtro de Unidade Notificadora
    if (!is.null(input$sel_unidade) && !"TODAS" %in% input$sel_unidade) {
      df <- df %>% filter(UNIDADE_NOTIF %in% input$sel_unidade)
    }
    
    # 5. Filtro de Classificação
    if (!is.null(input$sel_classif) && !"TODAS" %in% input$sel_classif) {
      df <- df %>% filter(CLASSI_FIN %in% input$sel_classif)
    }

    # 6. Filtro de Sexo
    if (!is.null(input$sel_sexo) && !"TODOS" %in% input$sel_sexo) {
      df <- df %>% filter(CS_SEXO %in% input$sel_sexo)
    }

    # 7. Filtro de Evolução
    if (!is.null(input$sel_evolucao) && !"TODAS" %in% input$sel_evolucao) {
      df <- df %>% filter(EVOLUCAO %in% input$sel_evolucao)
    }

    # 8. Filtro de Critério
    if (!is.null(input$sel_criterio) && !"TODOS" %in% input$sel_criterio) {
      df <- df %>% filter(CRITERIO_FINAL %in% input$sel_criterio)
    }

    # 9. Filtro de Sorotipo
    if (!is.null(input$sel_sorotipo) && !"TODOS" %in% input$sel_sorotipo) {
      col_s <- if("tp_sorotipo" %in% names(df)) "tp_sorotipo" else "RESUL_SORO"
      df <- df %>% filter(.data[[col_s]] %in% input$sel_sorotipo)
    }
    
    return(df)
  })
  
  # Dataset Demográfico (acumula dados de todos os anos selecionados)
  df_demografico <- reactive({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    return(df)
  })
  
  # KPIs da Visão Geral
  output$kpi_notificados   <- renderText({ if(is.null(df_filtrado())) "0" else format_num(nrow(df_filtrado())) })
  output$kpi_descartados   <- renderText({ if(is.null(df_filtrado())) "0" else format_num(sum(df_filtrado()$CLASSI_FIN == "Descartado", na.rm = TRUE)) })
  output$kpi_provaveis     <- renderText({ 
    if(is.null(df_filtrado())) return("0")
    prov <- nrow(df_filtrado()) - sum(df_filtrado()$CLASSI_FIN == "Descartado", na.rm = TRUE)
    format_num(prov)
  })
  output$kpi_confirmados   <- renderText({ if(is.null(df_filtrado())) "0" else format_num(sum(df_filtrado()$CLASSI_FIN == "Confirmado", na.rm = TRUE)) })
  output$kpi_inconclusivos <- renderText({ if(is.null(df_filtrado())) "0" else format_num(sum(df_filtrado()$CLASSI_FIN == "Inconclusivo", na.rm = TRUE)) })
  output$kpi_obitos        <- renderText({ if(is.null(df_filtrado())) "0" else format_num(sum(df_filtrado()$EVOLUCAO == "Óbito por dengue", na.rm = TRUE)) })
  
  output$kpi_incidencia <- renderText({
    if(is.null(df_filtrado()) || is.null(input$populacao) || input$populacao <= 0) return("0,0")
    prov <- nrow(df_filtrado()) - sum(df_filtrado()$CLASSI_FIN == "Descartado", na.rm = TRUE)
    format_num((prov / input$populacao) * 100000, digits = 1)
  })
  
  output$kpi_letalidade <- renderText({
    if(is.null(df_filtrado())) return("0,00%")
    conf <- sum(df_filtrado()$CLASSI_FIN == "Confirmado", na.rm = TRUE)
    obitos <- sum(df_filtrado()$EVOLUCAO == "Óbito por dengue", na.rm = TRUE)
    if (conf == 0) return("0,00%")
    format_pct((obitos / conf) * 100, digits = 2)
  })
  
  # Funções de Estilização Plotly
  estilo_base <- function(p) {
    p <- plotly::layout(
      p,
      autosize = TRUE,
      margin = list(l = 50, r = 30, t = 30, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
    plotly::config(p, displayModeBar = TRUE, displaylogo = FALSE, responsive = TRUE)
  }
  
  estilo_rosca <- function(p) {
    p <- plotly::layout(
      p,
      autosize = TRUE,
      margin = list(l = 20, r = 20, t = 30, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.0, y = -0.1)
    )
    plotly::config(p, displayModeBar = TRUE, displaylogo = FALSE, responsive = TRUE)
  }
  
  funcao_vazia_plotly <- function(msg = "Sem dados para os filtros selecionados.", elem_id = NULL) {
    plotly::plot_ly(type = 'scatter', mode = 'markers') %>% plotly::layout(
      annotations = list(list(text = msg, showarrow = FALSE, font = list(size = 14, color = "#64748B"))),
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
  }
  
  # ============================================================================
  # 5. RENDERIZAÇÃO DOS GRÁFICOS
  # ============================================================================
  
  # 1. Curva Temporal / Comparação de Anos (Mantém linhas distintas por ano na seleção múltipla)
  output$plot_semanas_geral <- renderPlotly({
    df <- df_filtrado()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados para o período selecionado."))
    
    anos_unicos <- sort(unique(df$ANO_NOTIF))
    
    if (input$modo_ano == "cruzado" && length(anos_unicos) > 1) {
      df_grp <- df %>%
        group_by(ANO_NOTIF = as.factor(ANO_NOTIF), SEMANA_EPID) %>%
        summarise(Casos = n(), .groups = "drop")
      
      plotly::plot_ly(df_grp, x = ~SEMANA_EPID, y = ~Casos, color = ~ANO_NOTIF, colors = PALETTE_ANOS, type = 'scatter', mode = 'lines+markers') %>%
        plotly::layout(
          xaxis = list(title = "Semana Epidemiológica", dtick = 2),
          yaxis = list(title = "Total de Casos Notificados"),
          legend = list(title = list(text = "<b>Ano</b>"), orientation = "h", y = 1.1)
        ) %>% estilo_base()
    } else {
      df_grp <- df %>%
        group_by(SEMANA_EPID) %>%
        summarise(
          Notificados = n(),
          Confirmados = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
          .groups = "drop"
        )
      
      plotly::plot_ly(df_grp, x = ~SEMANA_EPID) %>%
        plotly::add_trace(y = ~Notificados, type = 'bar', name = "Notificados", marker = list(color = "#0000A1")) %>%
        plotly::add_trace(y = ~Confirmados, type = 'bar', name = "Confirmados", marker = list(color = "#3B82F6")) %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Semana Epidemiológica", dtick = 2),
          yaxis = list(title = "Nº de Casos")
        ) %>% estilo_base()
    }
  })
  
  # 2. Sexo (Acumulado da seleção)
  output$plot_sexo <- renderPlotly({
    df <- df_demografico()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados no período."))
    
    df_res <- df %>% count(Sexo = CS_SEXO, name = "Casos")
    plotly::plot_ly(df_res, labels = ~Sexo, values = ~Casos, type = "pie", hole = 0.5, marker = list(colors = PALETTE_PIE)) %>% estilo_rosca()
  })
  
  # 3. Raça/Cor (Acumulado da seleção)
  output$plot_raca <- renderPlotly({
    df <- df_demografico()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados no período."))
    
    df_res <- df %>% count(Raca = CS_RACA, name = "Casos")
    plotly::plot_ly(df_res, labels = ~Raca, values = ~Casos, type = "pie", hole = 0.5, marker = list(colors = PALETTE_PIE)) %>% estilo_rosca()
  })
  
  # 4. Faixa etária e sexo (Valores estritamente positivos no eixo X masculino)
  output$plot_piramide <- renderPlotly({
    df <- df_demografico()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados no período."))
    
    breaks_faixas <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf)
    labels_faixas <- c("0 - 9", "10 - 19", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80+")
    
    df_pir <- df %>%
      filter(CS_SEXO %in% c("Masculino", "Feminino")) %>%
      mutate(Faixa_Etaria = factor(cut(IDADE_ANOS, breaks = breaks_faixas, labels = labels_faixas, right = FALSE), levels = labels_faixas)) %>%
      count(Faixa_Etaria, Sexo = CS_SEXO, name = "Casos", .drop = FALSE) %>%
      mutate(Casos = ifelse(is.na(Casos), 0, Casos))
    
    if(nrow(df_pir) == 0 || sum(df_pir$Casos) == 0) return(funcao_vazia_plotly("Sem dados de idade/sexo para os anos selecionados."))
    
    df_masc <- df_pir %>% filter(Sexo == "Masculino") %>% mutate(Casos_Neg = -Casos)
    df_fem  <- df_pir %>% filter(Sexo == "Feminino")
    max_val <- max(c(df_masc$Casos, df_fem$Casos), na.rm = TRUE)
    if(max_val == 0) max_val <- 10
    
    passo <- ceiling(max_val / 4)
    if (passo == 0) passo <- 1
    ticks_pos <- seq(0, max_val, by = passo)
    vals_eixo <- c(-rev(ticks_pos[-1]), ticks_pos)
    rotulos_eixo <- c(rev(ticks_pos[-1]), ticks_pos)
    
    plotly::plot_ly() %>%
      plotly::add_trace(
        data = df_masc, x = ~Casos_Neg, y = ~Faixa_Etaria, type = 'bar', orientation = 'h', name = 'Masculino',
        marker = list(color = '#0000A1'), text = ~ifelse(Casos > 0, format_num(Casos), ""), 
        textposition = 'auto',
        hovertemplate = "<b>%{y}</b><br>Homens: %{text}<extra></extra>"
      ) %>%
      plotly::add_trace(
        data = df_fem, x = ~Casos, y = ~Faixa_Etaria, type = 'bar', orientation = 'h', name = 'Feminino',
        marker = list(color = '#EC4899'), text = ~ifelse(Casos > 0, format_num(Casos), ""), 
        textposition = 'auto',
        hovertemplate = "<b>%{y}</b><br>Mulheres: %{text}<extra></extra>"
      ) %>%
      plotly::layout(
        barmode = 'overlay',
        xaxis = list(
          title = "Nº de Casos Notificados",
          range = c(-max_val * 1.15, max_val * 1.15),
          tickmode = "array",
          tickvals = vals_eixo,
          ticktext = sapply(rotulos_eixo, format_num)
        ),
        yaxis = list(title = "Faixa Etária"),
        margin = list(l = 60, r = 30, t = 30, b = 50)
      ) %>% estilo_base()
  })
  
  # 5. Casos por bairro (Acumulado da seleção)
  output$plot_bairro <- renderPlotly({
    df <- df_demografico()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados no período."))
    
    qtd <- if(!is.null(input$max_bairros) && input$max_bairros > 0) input$max_bairros else 15
    df_bairro <- df %>%
      count(Bairro = NM_BAIRRO, name = "Casos") %>%
      arrange(desc(Casos)) %>%
      head(qtd)
    
    if(nrow(df_bairro) == 0) return(funcao_vazia_plotly("Nenhum dado de bairro no filtro."))
    
    plotly::plot_ly(df_bairro, x = ~Casos, y = ~reorder(Bairro, Casos), type = "bar", orientation = "h", marker = list(color = PALETTE_PRIMARY)) %>%
      plotly::layout(yaxis = list(automargin = TRUE, title = ""), xaxis = list(title = "Nº de Casos")) %>% estilo_base()
  })
  
  # 6. Classificação Final (Acumulado da seleção)
  output$plot_classificacao <- renderPlotly({
    df <- df_filtrado()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados no período."))
    
    df_res <- df %>% count(Classificacao = CLASSI_FIN, name = "Casos")
    plotly::plot_ly(df_res, labels = ~Classificacao, values = ~Casos, type = "pie", hole = 0.5, marker = list(colors = PALETTE_PIE)) %>% estilo_rosca()
  })
  
  # 7. Desfecho / Evolução (Acumulado da seleção)
  output$plot_evolucao <- renderPlotly({
    df <- df_filtrado()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados no período."))
    
    df_res <- df %>% count(Evolucao = EVOLUCAO, name = "Casos")
    plotly::plot_ly(df_res, labels = ~Evolucao, values = ~Casos, type = "pie", hole = 0.5, marker = list(colors = PALETTE_PIE)) %>% estilo_rosca()
  })
  
  # 8. Critério Diagnóstico (Acumulado da seleção)
  output$plot_criterio <- renderPlotly({
    df <- df_demografico()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados no período."))
    
    df_res <- df %>% count(Criterio = CRITERIO_FINAL, name = "Casos")
    plotly::plot_ly(df_res, labels = ~Criterio, values = ~Casos, type = "pie", hole = 0.5, marker = list(colors = PALETTE_PIE)) %>% estilo_rosca()
  })
  
  # 9. Casos por sorotipo (Acumulado da seleção - coleta da coluna tp_sorotipo)
  output$plot_sorotipo <- renderPlotly({
    df <- df_filtrado()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados no período."))
    
    col_s <- if("tp_sorotipo" %in% names(df)) "tp_sorotipo" else if("TP_SOROTIPO" %in% names(df)) "TP_SOROTIPO" else "RESUL_SORO"
    df_res <- df %>% count(Sorotipo = .data[[col_s]], name = "Casos")
    
    plotly::plot_ly(df_res, labels = ~Sorotipo, values = ~Casos, type = "pie", hole = 0.5, marker = list(colors = PALETTE_PIE)) %>% estilo_rosca()
  })
  
  # ============================================================================
  # 5.1 UNIDADES NOTIFICADORAS - PROCESSAMENTO CONSOLIDADO E RANKING
  # ============================================================================
  
  dados_ranking_unidades_completo <- reactive({
    anos_sel <- anos_ativos()
    df_ne <- NOTIFICACOES_ESTABELECIMENTOS
    du <- dados_upload()
    
    # Se houver upload de dados vigentes com unidades
    if (!is.null(du) && nrow(du) > 0 && "UNIDADE_NOTIF" %in% names(du)) {
      anos_upload <- unique(du$ANO_NOTIF)
      if (!is.null(df_ne) && nrow(df_ne) > 0) {
        df_ne <- df_ne %>% filter(!ano %in% anos_upload)
      }
      du_validas <- du %>%
        filter(!is.na(UNIDADE_NOTIF) & UNIDADE_NOTIF != "" & UNIDADE_NOTIF != "NÃO INFORMADO")
      if (nrow(du_validas) > 0) {
        cnes_up <- if("CNES" %in% names(du_validas)) du_validas$CNES else if("ID_UNIDADE_RAW" %in% names(du_validas)) du_validas$ID_UNIDADE_RAW else NA_character_
        du_agg <- data.frame(
          ano = du_validas$ANO_NOTIF,
          cnes = cnes_up,
          nome_raw = du_validas$UNIDADE_NOTIF,
          stringsAsFactors = FALSE
        ) %>%
          group_by(ano, cnes, nome_raw) %>%
          summarise(total = n(), .groups = "drop")
        df_ne <- bind_rows(df_ne, du_agg)
      }
    }
    
    if (is.null(df_ne) || nrow(df_ne) == 0) return(data.frame())
    
    # Filtro de anos ativos
    if (!is.null(anos_sel) && length(anos_sel) > 0) {
      anos_sel_num <- suppressWarnings(as.numeric(anos_sel))
      anos_sel_num <- anos_sel_num[!is.na(anos_sel_num)]
      if (length(anos_sel_num) > 0) {
        df_ne <- df_ne %>% filter(ano %in% anos_sel_num)
      }
    }
    
    if (nrow(df_ne) == 0) return(data.frame())
    
    # Padronização de nomes por CNES e consolidação
    resumo_estab <- df_ne %>%
      group_by(cnes, nome_raw) %>%
      summarise(total = sum(total, na.rm = TRUE), .groups = "drop")
    
    # Identifica o nome principal de cada CNES
    cnes_agrup <- resumo_estab %>%
      group_by(cnes) %>%
      summarise(total = sum(total), .groups = "drop")
    
    cnes_nomes <- sapply(cnes_agrup$cnes, function(cn) {
      if (is.na(cn) || cn == "") {
        sub_tmp <- resumo_estab %>% filter(is.na(cnes) | cnes == "")
        return(sub_tmp$nome_raw[which.max(sub_tmp$total)])
      }
      sub_tmp <- resumo_estab %>% filter(cnes == cn)
      sub_tmp$nome_raw[which.max(sub_tmp$total)]
    })
    
    cnes_agrup$Nome <- unname(cnes_nomes)
    
    # Se houver nomes repetidos sem CNES, agrupa por nome
    cnes_final <- cnes_agrup %>%
      group_by(Nome) %>%
      summarise(
        cnes = if(all(is.na(cnes))) NA_character_ else first(na.omit(cnes)),
        total = sum(total),
        .groups = "drop"
      ) %>%
      arrange(desc(total))
    
    # Filtro opcional do seletor da barra lateral
    if (!is.null(input$sel_unidade) && !"TODAS" %in% input$sel_unidade) {
      cnes_final <- cnes_final %>% filter(Nome %in% input$sel_unidade)
    }
    
    if (nrow(cnes_final) == 0) return(data.frame())
    
    total_geral <- sum(cnes_final$total)
    cnes_final$Posicao <- seq_len(nrow(cnes_final))
    cnes_final$Rotulo_Pos <- paste0(cnes_final$Posicao, "º")
    cnes_final$Pct <- if(total_geral > 0) round((cnes_final$total / total_geral) * 100, 1) else 0
    cnes_final$Nome_Ranking <- paste0(cnes_final$Rotulo_Pos, " - ", cnes_final$Nome)
    
    return(cnes_final)
  })
  
  # KPIs da Aba Unidades Notificadoras
  output$kpi_top_unidade <- renderText({
    rk <- dados_ranking_unidades_completo()
    if (is.null(rk) || nrow(rk) == 0) return("—")
    top1 <- rk[1, ]
    paste0(top1$Nome, " (", format_num(top1$total), ")")
  })
  
  output$kpi_total_notif_unidades <- renderText({
    rk <- dados_ranking_unidades_completo()
    if (is.null(rk) || nrow(rk) == 0) return("0")
    format_num(sum(rk$total, na.rm = TRUE))
  })
  
  output$kpi_qtd_unidades_ativas <- renderText({
    rk <- dados_ranking_unidades_completo()
    if (is.null(rk) || nrow(rk) == 0) return("0 estabelecimentos")
    paste0(format_num(nrow(rk)), " estabelecimentos")
  })
  
  output$kpi_concentracao_top3 <- renderText({
    rk <- dados_ranking_unidades_completo()
    if (is.null(rk) || nrow(rk) == 0) return("0,0%")
    total_geral <- sum(rk$total, na.rm = TRUE)
    if (total_geral == 0) return("0,0%")
    top3_total <- sum(head(rk$total, 3), na.rm = TRUE)
    format_pct((top3_total / total_geral) * 100, digits = 1)
  })
  
  output$subtitulo_anos_unidades <- renderText({
    anos <- anos_ativos()
    if (is.null(anos) || length(anos) == 0) return("Todos os anos da série histórica")
    if (length(anos) == 1) return(paste("Ano de Análise:", anos))
    paste0("Anos Selecionados (", length(anos), "): ", paste(sort(anos), collapse = ", "))
  })
  
  # 10. Estabelecimentos Notificadores - Gráfico Estilo Ranking (Maior para Menor)
  output$plot_unidades <- renderPlotly({
    rk <- dados_ranking_unidades_completo()
    if(is.null(rk) || nrow(rk) == 0) return(funcao_vazia_plotly("Sem registros de estabelecimentos para os filtros selecionados."))
    
    qtd <- if(!is.null(input$max_unidades) && input$max_unidades > 0) input$max_unidades else 15
    rk_plot <- head(rk, qtd)
    
    # Ordem visual: o 1º lugar (maior volume) fica no TOPO do gráfico
    # No Plotly horizontal, os níveis de fator invertidos colocam o 1º nível no topo
    rk_plot$Nome_Fator <- factor(rk_plot$Nome_Ranking, levels = rev(rk_plot$Nome_Ranking))
    rk_plot$Rotulo_Barra <- paste0(sapply(rk_plot$total, format_num), " (", rk_plot$Pct, "%)")
    
    # Cores com destaque especial para o Top 3
    cores_barras <- sapply(rk_plot$Posicao, function(pos) {
      if (pos == 1) "#0000A1"
      else if (pos == 2) "#2563EB"
      else if (pos == 3) "#3B82F6"
      else "#0284C7"
    })
    
    plotly::plot_ly(
      data = rk_plot,
      x = ~total,
      y = ~Nome_Fator,
      type = "bar",
      orientation = "h",
      marker = list(
        color = cores_barras,
        line = list(color = "#0F172A", width = 0.8)
      ),
      text = ~Rotulo_Barra,
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Código CNES: %{customdata[0]}<br>",
        "Notificações Registradas: <b>%{x}</b><br>",
        "Participação Relativa: <b>%{customdata[1]}%</b><extra></extra>"
      ),
      customdata = mapply(function(cn, pct) list(ifelse(is.na(cn) | cn == "", "Não informado", as.character(cn)), pct), rk_plot$cnes, rk_plot$Pct, SIMPLIFY = FALSE)
    ) %>%
      plotly::layout(
        yaxis = list(automargin = TRUE, title = "", tickfont = list(size = 11, color = "#1E293B")),
        xaxis = list(title = "Nº de Notificações Registradas", rangemode = "tozero"),
        margin = list(l = 340, r = 80, t = 30, b = 50)
      ) %>% estilo_base()
  })
  
  # Tabela Detalhada do Ranking com Barra de Rolagem
  output$tabela_ranking_unidades <- renderTable({
    rk <- dados_ranking_unidades_completo()
    if (is.null(rk) || nrow(rk) == 0) {
      return(data.frame(Mensagem = "Sem registros no período selecionado."))
    }
    
    rk %>%
      transmute(
        `Pos.` = Rotulo_Pos,
        `Estabelecimento de Saúde` = Nome,
        `CNES` = ifelse(is.na(cnes) | cnes == "", "—", as.character(cnes)),
        `Notificações` = sapply(total, format_num),
        `Participação` = paste0(format_num(Pct, digits = 1), "%")
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  # ============================================================================
  # 6. ANÁLISE ESPACIAL (MAPA DE BELÉM POR BAIRROS E POR DISTRITOS)
  # ============================================================================
  
  # Agregação dos dados por bairro oficial
  dados_mapa_bairros <- reactive({
    df <- df_filtrado()
    mapa <- MAPA_BELEM_SF
    if (is.null(mapa)) return(NULL)
    
    if (is.null(df) || nrow(df) == 0) {
      mapa$total_casos <- 0
      mapa$confirmados <- 0
      mapa$descartados <- 0
      mapa$provaveis   <- 0
      mapa$obitos      <- 0
      mapa$incidencia  <- 0
      return(mapa)
    }
    
    resumo_bairros <- df %>%
      filter(!is.na(NM_BAIRRO_MAPA) & NM_BAIRRO_MAPA != "") %>%
      group_by(nm_bairro = NM_BAIRRO_MAPA) %>%
      summarise(
        total_casos = n(),
        confirmados = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
        descartados = sum(CLASSI_FIN == "Descartado", na.rm = TRUE),
        provaveis   = total_casos - descartados,
        obitos      = sum(EVOLUCAO == "Óbito por dengue", na.rm = TRUE),
        .groups = "drop"
      )
    
    mapa_unido <- mapa %>%
      left_join(resumo_bairros, by = "nm_bairro") %>%
      mutate(
        total_casos = ifelse(is.na(total_casos), 0, total_casos),
        confirmados = ifelse(is.na(confirmados), 0, confirmados),
        descartados = ifelse(is.na(descartados), 0, descartados),
        provaveis   = ifelse(is.na(provaveis), 0, provaveis),
        obitos      = ifelse(is.na(obitos), 0, obitos),
        incidencia  = ifelse(!is.na(populacao_total) & populacao_total > 0, 
                             round((provaveis / populacao_total) * 100000, 1), 0),
        letalidade  = ifelse(confirmados > 0, round((obitos / confirmados) * 100, 2), 0)
      )
    
    return(mapa_unido)
  })
  
  # Agregação acumulada dos dados por Distrito Administrativo
  dados_mapa_distritos <- reactive({
    mb <- dados_mapa_bairros()
    md <- MAPA_DISTRITOS_SF
    if (is.null(mb) || is.null(md)) return(NULL)
    
    # Soma os dados dos bairros pertencentes a cada distrito
    resumo_distritos <- mb %>%
      st_drop_geometry() %>%
      filter(!is.na(sigla_distrito)) %>%
      group_by(sigla = sigla_distrito) %>%
      summarise(
        total_casos = sum(total_casos),
        confirmados = sum(confirmados),
        descartados = sum(descartados),
        provaveis   = sum(provaveis),
        obitos      = sum(obitos),
        .groups = "drop"
      )
    
    md_unido <- md %>%
      left_join(resumo_distritos, by = "sigla") %>%
      mutate(
        total_casos = ifelse(is.na(total_casos), 0, total_casos),
        confirmados = ifelse(is.na(confirmados), 0, confirmados),
        descartados = ifelse(is.na(descartados), 0, descartados),
        provaveis   = ifelse(is.na(provaveis), 0, provaveis),
        obitos      = ifelse(is.na(obitos), 0, obitos),
        incidencia  = ifelse(!is.na(populacao_2022) & populacao_2022 > 0, 
                             round((provaveis / populacao_2022) * 100000, 1), 0),
        letalidade  = ifelse(confirmados > 0, round((obitos / confirmados) * 100, 2), 0)
      )
    
    return(md_unido)
  })
  
  output$mapa_subtitulo_anos <- renderText({
    anos <- anos_ativos()
    if (is.null(anos) || length(anos) == 0) return("Nenhum Ano Ativo")
    if (length(anos) == 1) return(paste("Ano de Análise:", anos))
    paste0("Anos Acumulados (", length(anos), "): ", paste(sort(anos), collapse = ", "))
  })
  
  output$mapa_kpi_top_bairro <- renderText({
    mb <- dados_mapa_bairros()
    if (is.null(mb) || sum(mb$total_casos) == 0) return("—")
    top <- mb %>% st_drop_geometry() %>% arrange(desc(incidencia), desc(total_casos)) %>% head(1)
    paste0(top$nm_bairro, " (", format_num(top$incidencia, digits = 1), " por 100 mil hab.)")
  })
  
  output$mapa_kpi_bairros_atingidos <- renderText({
    mb <- dados_mapa_bairros()
    if (is.null(mb)) return("0 / 71")
    atingidos <- sum(mb$total_casos > 0)
    paste0(atingidos, " de ", nrow(mb), " (", round(100 * atingidos / nrow(mb), 0), "%)")
  })
  
  output$mapa_kpi_incidencia_media <- renderText({
    mb <- dados_mapa_bairros()
    if (is.null(mb) || sum(mb$total_casos) == 0) return("0,0 por 100 mil hab.")
    media_inc <- round(mean(mb$incidencia[mb$total_casos > 0], na.rm = TRUE), 1)
    if(is.na(media_inc)) media_inc <- 0
    paste0(format_num(media_inc, digits = 1), " por 100 mil hab.")
  })
  
  # Tabela 1: Todos os Bairros com Barra de Rolagem
  output$tabela_todos_bairros_mapa <- renderTable({
    mb <- dados_mapa_bairros()
    if (is.null(mb) || sum(mb$total_casos) == 0) {
      return(data.frame(Mensagem = "Sem registros no período selecionado."))
    }
    
    metrica_col <- if (input$metrica_mapa == "incidencia") "incidencia" else if (input$metrica_mapa == "confirmados") "confirmados" else if (input$metrica_mapa == "provaveis") "provaveis" else "total_casos"
    
    mb %>%
      st_drop_geometry() %>%
      arrange(desc(.data[[metrica_col]])) %>%
      transmute(
        Bairro = nm_bairro,
        Distrito = sigla_distrito,
        `População` = sapply(populacao_total, format_num),
        `Domicílios` = sapply(domicilios_ocupados, format_num),
        `Notif.` = sapply(total_casos, format_num),
        `Conf.` = sapply(confirmados, format_num),
        `Incidência (por 100 mil hab.)` = sapply(incidencia, function(v) format_num(v, digits = 1)),
        `Letalidade` = sapply(letalidade, function(v) paste0(format_num(v, digits = 2), "%"))
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  # Tabela 2: Distritos Administrativos de Belém
  output$tabela_distritos_mapa <- renderTable({
    md <- dados_mapa_distritos()
    if (is.null(md) || sum(md$total_casos) == 0) {
      return(data.frame(Mensagem = "Sem registros de distritos no período."))
    }
    
    md %>%
      st_drop_geometry() %>%
      arrange(desc(total_casos)) %>%
      transmute(
        DISTRITO = paste0(sigla, " - ", gsub("Distrito Administrativo d[eo] | \\(.*?\\)", "", nome)),
        `Bairros` = sapply(qtd_bairros, format_num),
        `População` = sapply(populacao_2022, format_num),
        `Domicílios` = sapply(domicilios_2022, format_num),
        `NOTIF.` = sapply(total_casos, format_num),
        `CONF.` = sapply(confirmados, format_num),
        `Incidência (por 100 mil hab.)` = sapply(incidencia, function(v) format_num(v, digits = 1)),
        `Letalidade (%)` = sapply(letalidade, function(v) format_pct(v, digits = 2))
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  output$mapa_tipo_legenda_rodape <- renderText({
    tipo_vis <- if (!is.null(input$tipo_visualizacao_mapa)) input$tipo_visualizacao_mapa else "coropletico"
    camada   <- if (!is.null(input$camada_mapa_ativa)) input$camada_mapa_ativa else "bairros"
    camada_txt <- if (camada == "distritos") "Distritos Administrativos" else "Bairros Oficiais"
    
    metrica_txt <- case_when(
      input$metrica_mapa == "incidencia"  ~ "Taxa de Incidência (por 100 mil hab.)",
      input$metrica_mapa == "confirmados" ~ "Casos Confirmados",
      input$metrica_mapa == "provaveis"   ~ "Casos Prováveis",
      TRUE                                ~ "Total de Notificações"
    )
    
    if (tipo_vis == "calor") {
      paste0("Visualização: Mapa de Calor (mancha térmica contínua de ", metrica_txt, " por ", camada_txt, ").")
    } else {
      paste0("Visualização: Mapa Coroplético de ", metrica_txt, " por ", camada_txt, " (fundo 100% branco e paleta suave de alto contraste).")
    }
  })
  
  # Paleta de cores com alto destaque e conforto visual
  CORES_MAPA_CONFORTAVEL <- c("#E0F2FE", "#93C5FD", "#FDE047", "#F97316", "#DC2626", "#7F1D1D")
  # Paleta térmica epidemiológica de alto contraste e destaque para o mapa de calor
  PALETA_CALOR_DESTAQUE  <- c("#1D4ED8", "#06B6D4", "#10B981", "#FACC15", "#F97316", "#EF4444", "#7F1D1D")
  TILE_BRANCO_LIMPO      <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
  
# Função reutilizável para renderização do Mapa Leaflet (utilizada na Análise Espacial e no Modo Apresentação)
  montar_mapa_leaflet_belem <- function(camada_forcada = NULL) {
    mb <- dados_mapa_bairros()
    md <- dados_mapa_distritos()
    milhas <- MAPA_ILHAS_SF
    
    if (is.null(mb) || is.null(md)) {
      return(
        leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
          addTiles(urlTemplate = TILE_BRANCO_LIMPO) %>%
          setView(lng = -48.47, lat = -1.35, zoom = 11)
      )
    }
    
    tipo_vis      <- if (!is.null(input$tipo_visualizacao_mapa)) input$tipo_visualizacao_mapa else "coropletico"
    camada_ativa  <- if (!is.null(camada_forcada)) camada_forcada else (if (!is.null(input$camada_mapa_ativa)) input$camada_mapa_ativa else "bairros")
    metrica_ativa <- if (!is.null(input$metrica_mapa)) input$metrica_mapa else "incidencia"
    
    # Textos e popups para as Ilhas de Belém (mantidas em branco com contornos suaves)
    if (!is.null(milhas) && nrow(milhas) > 0) {
      rotulos_ilhas <- paste0(milhas$nome_ilha, " (Área Insular - ", milhas$sigla_distrito, ")")
      popup_ilhas <- paste0(
        "<div style='font-family: system-ui, sans-serif; font-size: 13px; line-height: 1.5; min-width: 220px;'>",
        "<h6 style='margin:0 0 6px 0; font-weight:700; color:#0000A1; border-bottom: 2px solid #E2E8F0; padding-bottom: 4px;'>",
        milhas$nome_ilha, "</h6>",
        "<b>Distrito Administrativo:</b> ", milhas$nome_distrito, "<br/>",
        "<b>Área Territorial:</b> ", format_num(milhas$area_km2, digits = 2), " km²<br/>",
        "<b>Classificação:</b> Território Insular de Belém<br/>",
        "<small style='color:#64748B;'>Área insular sem configuração de bairro urbano oficial (mantida em branco para preservação da escala municipal).</small>",
        "</div>"
      )
    } else {
      rotulos_ilhas <- NULL
      popup_ilhas <- NULL
    }
    
    # --------------------------------------------------------------------------
    # MODO 1: MAPA DE CALOR (MANCHA TÉRMICA PONDERADA PELA MÉTRICA SELECIONADA)
    # --------------------------------------------------------------------------
    if (tipo_vis == "calor") {
      # Seleciona camada territorial para o mapa de calor
      if (camada_ativa == "distritos") {
        dados_territorio <- md
        nome_loc <- paste0(dados_territorio$nome, " (", dados_territorio$sigla, ")")
        pop_loc  <- dados_territorio$populacao_2022
        dom_loc  <- dados_territorio$domicilios_2022
      } else {
        dados_territorio <- mb
        nome_loc <- dados_territorio$nm_bairro
        pop_loc  <- dados_territorio$populacao_total
        dom_loc  <- dados_territorio$domicilios_ocupados
      }
      
      # Pondera a mancha de calor pelos dados selecionados na caixa "Dados"
      dados_territorio$valor_calor <- dados_territorio[[metrica_ativa]]
      
      df_calor <- dados_territorio %>%
        st_drop_geometry() %>%
        filter(valor_calor > 0 & !is.na(centroid_lat) & !is.na(centroid_lon))
      
      min_calor <- if(nrow(df_calor) > 0) min(df_calor$valor_calor, na.rm = TRUE) else 0
      max_calor <- if(nrow(df_calor) > 0) max(df_calor$valor_calor, na.rm = TRUE) else 1
      if (is.na(max_calor) || max_calor <= 0) max_calor <- 1
      
      # Rótulos informativos com a métrica selecionada
      valor_fmt <- if (metrica_ativa == "incidencia") {
        paste0(sapply(dados_territorio$incidencia, function(v) format_num(v, digits = 1)), " por 100 mil hab.")
      } else {
        paste0(sapply(dados_territorio[[metrica_ativa]], format_num), " casos")
      }
      
      rotulos_calor <- paste0(nome_loc, ": ", valor_fmt)
      
      min_fmt <- if (metrica_ativa == "incidencia") paste0(format_num(min_calor, digits = 1), " por 100 mil hab.") else paste0(format_num(min_calor), " casos")
      max_fmt <- if (metrica_ativa == "incidencia") paste0(format_num(max_calor, digits = 1), " por 100 mil hab.") else paste0(format_num(max_calor), " casos")
      
      popup_calor <- paste0(
        "<div style='font-family: system-ui, sans-serif; font-size: 13px; line-height: 1.5; min-width: 220px;'>",
        "<h6 style='margin:0 0 6px 0; font-weight:700; color:#0000A1; border-bottom: 2px solid #E2E8F0; padding-bottom: 4px;'>",
        nome_loc, "</h6>",
        "<div style='background:#EFF6FF; padding:4px 8px; border-radius:4px; margin-bottom:6px; border-left:3px solid #0000A1;'>",
        "<b>Métrica Ativa:</b> <span style='font-weight:700; color:#B91C1C;'>", valor_fmt, "</span>",
        "</div>",
        "<b>Taxa de Incidência:</b> <span style='font-weight:700; color:#B91C1C;'>", format_num(dados_territorio$incidencia, digits = 1), " por 100 mil hab.</span><br/>",
        "<b>Casos Prováveis:</b> <span style='font-weight:600; color:#D97706;'>", format_num(dados_territorio$provaveis), "</span><br/>",
        "<b>Casos Confirmados:</b> <span style='font-weight:600; color:#DC2626;'>", format_num(dados_territorio$confirmados), "</span><br/>",
        "<b>Total Notificados:</b> <span style='font-weight:600; color:#0000A1;'>", format_num(dados_territorio$total_casos), "</span><br/>",
        "<b>População:</b> ", format_num(pop_loc), " hab.<br/>",
        "<b>Domicílios:</b> ", format_num(dom_loc), " dom.",
        "</div>"
      )
      
      # Mapa de calor interativo com painel dedicado para captura de eventos
      mapa_heat <- leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
        addMapPane("polygonsPane", zIndex = 450) %>%
        addTiles(urlTemplate = TILE_BRANCO_LIMPO, group = "Fundo Branco Limpo") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron (Ruas Claras)") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satélite Esri")
      
      # 1. Aplica a mancha de calor contínua com paleta térmica de alto destaque
      if (nrow(df_calor) > 0) {
        mapa_heat <- mapa_heat %>%
          addHeatmap(
            data = df_calor,
            lng = ~centroid_lon, lat = ~centroid_lat, intensity = ~valor_calor,
            minOpacity = 0.20,
            blur = 20,
            max = max_calor,
            radius = 28,
            gradient = PALETA_CALOR_DESTAQUE,
            group = "Mancha de Calor"
          )
      }
      
      # 2. Na divisão por bairros, inclui as Ilhas em branco com contornos discretos
      if (camada_ativa == "bairros" && !is.null(milhas) && nrow(milhas) > 0) {
        mapa_heat <- mapa_heat %>%
          addPolygons(
            data = milhas,
            fillColor = "#FFFFFF",
            fillOpacity = 0.85,
            color = "#CBD5E1",
            weight = 0.85,
            dashArray = "",
            smoothFactor = 1.0,
            options = pathOptions(pane = "polygonsPane"),
            label = rotulos_ilhas,
            popup = popup_ilhas,
            group = "Ilhas e Áreas Insulares (em branco)"
          )
      }
      
      # 3. Contornos interativos dos polígonos sobrepostos ao calor para permitir clique e seleção
      mapa_heat <- mapa_heat %>%
        addPolygons(
          data = dados_territorio,
          fillColor = "#FFFFFF",
          fillOpacity = 0.04, # Transparência sutil que capta eventos de clique e hover
          color = if(camada_ativa == "distritos") "#0000A1" else "#94A3B8",
          weight = if(camada_ativa == "distritos") 1.4 else 0.85,
          opacity = if(camada_ativa == "distritos") 0.85 else 0.65,
          dashArray = "",
          smoothFactor = 1.0,
          options = pathOptions(pane = "polygonsPane"),
          label = rotulos_calor,
          popup = popup_calor,
          highlightOptions = highlightOptions(
            weight = 2.2,
            color = "#0284C7",
            dashArray = "",
            fillOpacity = 0.18,
            bringToFront = TRUE
          ),
          group = if(camada_ativa == "distritos") "Contornos dos Distritos" else "Contornos dos Bairros"
        )
      
      titulo_calor_legenda <- case_when(
        metrica_ativa == "incidencia"  ~ "Taxa de Incidência (por 100 mil hab.)",
        metrica_ativa == "confirmados" ~ "Casos Confirmados",
        metrica_ativa == "provaveis"   ~ "Casos Prováveis",
        TRUE                           ~ "Total Notificados"
      )
      
      html_legenda_calor <- paste0(
        "<div style='background:rgba(255,255,255,0.96); padding:9px 13px; border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.15); font-family:system-ui,-apple-system,sans-serif; font-size:12px; border:1px solid #CBD5E1;'>",
        "<b style='color:#0000A1; font-size:12px; display:block; margin-bottom:4px;'>Concentração Térmica: ", titulo_calor_legenda, "</b>",
        "<div style='display:flex; align-items:center; justify-content:space-between; gap:6px; margin-top:6px; margin-bottom:3px;'>",
        "<span style='font-size:10px; font-weight:600; color:#1D4ED8;'>Baixa (", min_fmt, ")</span>",
        "<div style='width:140px; height:12px; border-radius:4px; border:1px solid #94A3B8; background:linear-gradient(to right, #1D4ED8, #06B6D4, #10B981, #FACC15, #F97316, #EF4444, #7F1D1D);'></div>",
        "<span style='font-size:10px; font-weight:700; color:#7F1D1D;'>Alta (", max_fmt, ")</span>",
        "</div>",
        "<small style='color:#64748B; font-size:10px; display:block; text-align:center;'>Gradiente de mancha térmica de alto destaque e contraste</small>",
        "</div>"
      )
      
      mapa_heat <- mapa_heat %>%
        addControl(html = html_legenda_calor, position = "bottomright") %>%
        addLayersControl(
          baseGroups = c("Fundo Branco Limpo", "CartoDB Positron (Ruas Claras)", "OpenStreetMap", "Satélite Esri"),
          overlayGroups = c(
            if(camada_ativa == "distritos") "Contornos dos Distritos" else "Contornos dos Bairros",
            if(camada_ativa == "bairros" && !is.null(milhas) && nrow(milhas) > 0) "Ilhas e Áreas Insulares (em branco)" else NULL,
            if(nrow(df_calor) > 0) "Mancha de Calor" else NULL
          ),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      return(mapa_heat)
    }
    
    # --------------------------------------------------------------------------
    # MODO 2: MAPA COROPLÉTICO (ESTRITAMENTE COROPLÉTICO, SEM HEATMAP)
    # --------------------------------------------------------------------------
    if (camada_ativa == "distritos") {
      dados_poligonos <- md
      valores <- md[[metrica_ativa]]
    } else {
      dados_poligonos <- mb
      valores <- mb[[metrica_ativa]]
    }
    
    max_val <- max(valores, na.rm = TRUE)
    if (is.na(max_val) || max_val <= 0) max_val <- 1
    pal <- colorNumeric(palette = CORES_MAPA_CONFORTAVEL, domain = c(0, max_val))
    
    titulo_legenda <- case_when(
      metrica_ativa == "incidencia"  ~ "Incidência (por 100 mil hab.)",
      metrica_ativa == "confirmados" ~ "Casos Confirmados",
      metrica_ativa == "provaveis"   ~ "Casos Prováveis",
      TRUE                           ~ "Total Notificados"
    )
    
    # Bairros/distritos com valor zero recebem tom neutro limpo (#F8FAFC)
    fill_cores <- ifelse(valores == 0, "#F8FAFC", pal(valores))
    
    if (camada_ativa == "distritos") {
      popup_conteudo <- paste0(
        "<div style='font-family: system-ui, sans-serif; font-size: 13px; line-height: 1.5; min-width: 220px;'>",
        "<h6 style='margin:0 0 6px 0; font-weight:700; color:#0000A1; border-bottom: 2px solid #E2E8F0; padding-bottom: 4px;'>",
        md$nome, "</h6>",
        "<b>Sede Administrativa:</b> ", md$sede, "<br/>",
        "<b>Bairros Oficiais:</b> ", format_num(md$qtd_bairros), "<br/>",
        "<b>População (Censo 2022):</b> ", format_num(md$populacao_2022), " hab.<br/>",
        "<b>Domicílios (Censo 2022):</b> ", format_num(md$domicilios_2022), " dom.<br/>",
        "<b>Casos Notificados:</b> <span style='font-weight:600; color:#0000A1;'>", format_num(md$total_casos), "</span><br/>",
        "<b>Casos Prováveis:</b> <span style='font-weight:600; color:#D97706;'>", format_num(md$provaveis), "</span><br/>",
        "<b>Confirmados:</b> <span style='font-weight:600; color:#DC2626;'>", format_num(md$confirmados), "</span><br/>",
        "<b>Óbitos Dengue:</b> <span style='font-weight:600;'>", format_num(md$obitos), "</span><br/>",
        "<b>Taxa de Incidência:</b> <span style='font-weight:700; color:#B91C1C;'>", format_num(md$incidencia, digits = 1), " por 100 mil hab.</span><br/>",
        "<b>Taxa de Letalidade:</b> <span style='font-weight:700; color:#991B1B;'>", format_pct(md$letalidade, digits = 2), "</span>",
        "</div>"
      )
      rotulos <- paste0(md$sigla, " - ", md$nome, ": ", sapply(valores, function(v) format_num(v, digits = ifelse(metrica_ativa %in% c("incidencia", "letalidade"), 1, 0))), if(metrica_ativa == "incidencia") " por 100 mil hab." else " casos")
    } else {
      popup_conteudo <- paste0(
        "<div style='font-family: system-ui, sans-serif; font-size: 13px; line-height: 1.5; min-width: 210px;'>",
        "<h6 style='margin:0 0 6px 0; font-weight:700; color:#0000A1; border-bottom: 2px solid #E2E8F0; padding-bottom: 4px;'>",
        mb$nm_bairro, "</h6>",
        "<b>Distrito:</b> ", mb$nome_distrito, "<br/>",
        "<b>População (Censo 2022):</b> ", format_num(mb$populacao_total), " hab.<br/>",
        "<b>Domicílios (Censo 2022):</b> ", format_num(mb$domicilios_ocupados), " dom.<br/>",
        "<b>Densidade Demográfica:</b> ", format_num(mb$densidade_hab_km2), " hab./km²<br/>",
        "<b>Casos Notificados:</b> <span style='font-weight:600; color:#0000A1;'>", format_num(mb$total_casos), "</span><br/>",
        "<b>Casos Prováveis:</b> <span style='font-weight:600; color:#D97706;'>", format_num(mb$provaveis), "</span><br/>",
        "<b>Confirmados:</b> <span style='font-weight:600; color:#DC2626;'>", format_num(mb$confirmados), "</span><br/>",
        "<b>Óbitos Dengue:</b> <span style='font-weight:600;'>", format_num(mb$obitos), "</span><br/>",
        "<b>Taxa de Incidência:</b> <span style='font-weight:700; color:#B91C1C;'>", format_num(mb$incidencia, digits = 1), " por 100 mil hab.</span><br/>",
        "<b>Taxa de Letalidade:</b> <span style='font-weight:700; color:#991B1B;'>", format_pct(mb$letalidade, digits = 2), "</span>",
        "</div>"
      )
      rotulos <- paste0(mb$nm_bairro, ": ", sapply(valores, function(v) format_num(v, digits = ifelse(metrica_ativa %in% c("incidencia", "letalidade"), 1, 0))), if(metrica_ativa == "incidencia") " por 100 mil hab." else " casos")
    }
    
    # Mapa Coroplético 100% limpo com fundo branco transparente, bordas suaves e discretas
    mapa_coro <- leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      addMapPane("polygonsPane", zIndex = 450) %>%
      addTiles(urlTemplate = TILE_BRANCO_LIMPO, group = "Fundo Branco Limpo") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron (Ruas Claras)") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite Esri")
    
    # Adiciona as Ilhas em branco caso a camada ativa seja Bairros
    if (camada_ativa == "bairros" && !is.null(milhas) && nrow(milhas) > 0) {
      mapa_coro <- mapa_coro %>%
        addPolygons(
          data = milhas,
          fillColor = "#FFFFFF",
          fillOpacity = 0.90,
          color = "#CBD5E1",
          weight = 0.85,
          dashArray = "",
          smoothFactor = 1.0,
          options = pathOptions(pane = "polygonsPane"),
          label = rotulos_ilhas,
          popup = popup_ilhas,
          group = "Ilhas e Áreas Insulares (em branco)"
        )
    }
    
    # Polígonos de dados da camada ativa (bairros ou distritos)
    mapa_coro <- mapa_coro %>%
      addPolygons(
        data = dados_poligonos,
        fillColor = fill_cores,
        weight = if(camada_ativa == "distritos") 1.6 else 0.85,
        opacity = if(camada_ativa == "distritos") 0.9 else 0.7,
        color = if(camada_ativa == "distritos") "#0000A1" else "#94A3B8",
        dashArray = "",
        smoothFactor = 1.0,
        fillOpacity = 0.78,
        options = pathOptions(pane = "polygonsPane"),
        highlightOptions = highlightOptions(
          weight = 2.2,
          color = "#0284C7",
          dashArray = "",
          fillOpacity = 0.92,
          bringToFront = TRUE
        ),
        label = rotulos,
        popup = popup_conteudo,
        group = if(camada_ativa == "distritos") "Camada por Distrito" else "Camada por Bairro"
      ) %>%
      (function(map_inst) {
        if (camada_ativa == "distritos" && !is.null(md) && nrow(md) > 0) {
          NOMES_CURTOS_LEAF <- c(
            DAMOS = "Mosqueiro", DAOUT = "Outeiro", DAICO = "Icoaraci", DABEN = "Benguí",
            DAENT = "Entroncamento", DASAC = "Sacramenta", DABEL = "Belém", DAGUA = "Guamá"
          )
          md$rotulo_badge_leaf <- paste0("<b>", md$sigla, "</b><br><span style='font-size:10px;'>", NOMES_CURTOS_LEAF[md$sigla], "</span>")
          map_inst %>% addLabelOnlyMarkers(
            data = md,
            lng = ~centroid_lon, lat = ~centroid_lat,
            label = lapply(md$rotulo_badge_leaf, HTML),
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "center",
              textOnly = FALSE,
              style = list(
                "color" = "#0000A1",
                "font-family" = "system-ui, sans-serif",
                "text-align" = "center",
                "line-height" = "1.1",
                "background-color" = "rgba(255, 255, 255, 0.90)",
                "border" = "1px solid #94A3B8",
                "border-radius" = "5px",
                "padding" = "2px 6px",
                "box-shadow" = "0 1px 3px rgba(0,0,0,0.15)"
              )
            )
          )
        } else {
          map_inst
        }
      })() %>%
      addLegend(
        pal = pal,
        values = c(0, max_val),
        opacity = 0.88,
        title = titulo_legenda,
        position = "bottomright"
      ) %>%
      addLayersControl(
        baseGroups = c("Fundo Branco Limpo", "CartoDB Positron (Ruas Claras)", "OpenStreetMap", "Satélite Esri"),
        overlayGroups = c(
          if(camada_ativa == "distritos") "Camada por Distrito" else "Camada por Bairro",
          if(camada_ativa == "bairros" && !is.null(milhas) && nrow(milhas) > 0) "Ilhas e Áreas Insulares (em branco)" else NULL
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    return(mapa_coro)
  }
  
  output$mapa_dengue_belem <- renderLeaflet({
    montar_mapa_leaflet_belem()
  })
  
  # ============================================================================
  # 7. ANÁLISE EPIDEMIOLÓGICA (DIAGRAMA DE CONTROLE, INCIDÊNCIA, LETALIDADE, ANUAL)
  # ============================================================================
  
  # 7.1 Dataset para o Diagrama de Controle (Canal Endêmico)
  # Anos não epidêmicos de referência: 2015, 2016, 2017, 2018, 2019, 2022, 2023
  # Ano de observação em colunas: 2026
  # O Diagrama de Controle é o padrão municipal oficial e NÃO sofre alteração por filtros
  # da barra lateral (unidade notificadora, bairros, sexo, etc.), alterando-se exclusivamente
  # quando houver entrada/atualização de dados do ano observado (2026).
  dados_diagrama_controle <- reactive({
    df <- dados_base()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    anos_base <- c(2015, 2016, 2017, 2018, 2019, 2022, 2023)
    grid_base <- expand.grid(ANO_NOTIF = anos_base, SEMANA_EPID = 1:53)
    
    cont_base <- df %>%
      filter(as.numeric(ANO_NOTIF) %in% anos_base) %>%
      group_by(ANO_NOTIF = as.numeric(ANO_NOTIF), SEMANA_EPID = as.numeric(SEMANA_EPID)) %>%
      summarise(Casos = n(), .groups = "drop")
    
    diag_stat <- grid_base %>%
      left_join(cont_base, by = c("ANO_NOTIF", "SEMANA_EPID")) %>%
      mutate(Casos = ifelse(is.na(Casos), 0, Casos)) %>%
      group_by(SEMANA_EPID) %>%
      summarise(
        Media = round(mean(Casos), 1),
        Desvio = round(sd(Casos), 1),
        Limite_Sup = round(Media + 1.96 * Desvio, 1),
        Limite_Inf = round(pmax(0, Media - 1.96 * Desvio), 1),
        .groups = "drop"
      )
    
    obs_2026 <- df %>%
      filter(as.numeric(ANO_NOTIF) == 2026) %>%
      group_by(SEMANA_EPID = as.numeric(SEMANA_EPID)) %>%
      summarise(Observado_2026 = n(), .groups = "drop")
    
    diag_stat %>%
      left_join(obs_2026, by = "SEMANA_EPID") %>%
      mutate(Observado_2026 = ifelse(is.na(Observado_2026), 0, Observado_2026))
  })
  
  # Renderização do Diagrama de Controle
  output$plot_diagrama_controle <- renderPlotly({
    df_diag <- dados_diagrama_controle()
    if(is.null(df_diag) || nrow(df_diag) == 0) return(funcao_vazia_plotly("Sem dados para o Diagrama de Controle."))
    
    # Identifica até qual semana epidemiológica temos dados registrados em 2026
    df_base_tudo <- dados_base()
    sems_2026 <- df_base_tudo$SEMANA_EPID[df_base_tudo$ANO_NOTIF == 2026 & df_base_tudo$SEMANA_EPID <= 52]
    max_sem_2026 <- if(length(sems_2026) > 0) max(sems_2026, na.rm = TRUE) else 44
    if (is.na(max_sem_2026) || max_sem_2026 <= 0) max_sem_2026 <- 44
    
    df_obs <- df_diag %>% filter(SEMANA_EPID <= max_sem_2026)
    
    plotly::plot_ly() %>%
      # Limite Inferior
      plotly::add_trace(
        data = df_diag, x = ~SEMANA_EPID, y = ~Limite_Inf,
        type = 'scatter', mode = 'lines',
        line = list(color = '#10B981', dash = 'dash', width = 1.5),
        name = 'Limite Inferior',
        hoverinfo = 'skip'
      ) %>%
      # Limite Superior com faixa preenchida (Canal Endêmico)
      plotly::add_trace(
        data = df_diag, x = ~SEMANA_EPID, y = ~Limite_Sup,
        type = 'scatter', mode = 'lines',
        fill = 'tonexty',
        fillcolor = 'rgba(59, 130, 246, 0.12)',
        line = list(color = '#DC2626', width = 2.5),
        name = 'Limite Superior (Alerta Epidêmico)',
        hovertemplate = '<b>Semana %{x}</b><br>Limite Superior: %{y} casos<extra></extra>'
      ) %>%
      # Média Histórica dos Anos Não Epidêmicos
      plotly::add_trace(
        data = df_diag, x = ~SEMANA_EPID, y = ~Media,
        type = 'scatter', mode = 'lines',
        line = list(color = '#0000A1', dash = 'dot', width = 2),
        name = 'Média Esperada (Anos Não Epidêmicos)',
        hovertemplate = '<b>Semana %{x}</b><br>Média Esperada: %{y} casos<extra></extra>'
      ) %>%
      # Colunas observadas de 2026
      plotly::add_trace(
        data = df_obs, x = ~SEMANA_EPID, y = ~Observado_2026,
        type = 'bar',
        name = 'Observado 2026 (Semanal)',
        marker = list(
          color = ~ifelse(Observado_2026 > Limite_Sup, '#EF4444', '#3B82F6'),
          opacity = 0.85
        ),
        hovertemplate = '<b>Semana %{x}</b><br>Observado 2026: %{y} casos<extra></extra>'
      ) %>%
      plotly::layout(
        xaxis = list(title = "Semana Epidemiológica", dtick = 2, range = c(0.5, 53.5)),
        yaxis = list(title = "Nº de Notificações de Dengue"),
        legend = list(orientation = "h", x = 0.05, y = 1.15)
      ) %>% estilo_base()
  })
  
  # 7.2 Evolução de Incidência (Histograma Mensal conforme anos e filtros selecionados)
  output$plot_evolucao_incidencia <- renderPlotly({
    df <- df_filtrado()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados para incidência no período selecionado."))
    
    pop <- if (!is.null(input$populacao) && input$populacao > 0) input$populacao else 1303403
    
    df_mensal <- df %>%
      filter(!is.na(DT_NOTIFIC)) %>%
      mutate(
        Ano = as.integer(ANO_NOTIF),
        Mes = as.integer(month(DT_NOTIFIC)),
        Ano_Mes = sprintf("%04d-%02d", Ano, Mes)
      ) %>%
      group_by(Ano_Mes, Ano, Mes) %>%
      summarise(Casos = n(), .groups = "drop") %>%
      arrange(Ano_Mes) %>%
      mutate(Incidencia = round((Casos / pop) * 100000, 1))
    
    if(nrow(df_mensal) == 0) return(funcao_vazia_plotly("Sem dados mensais para os filtros ativos."))
    
    plotly::plot_ly(
      df_mensal, x = ~Ano_Mes, y = ~Incidencia,
      type = 'bar',
      marker = list(color = '#0000A1'),
      hovertemplate = '<b>%{x}</b><br>Incidência: %{y} por 100 mil hab.<br>Casos: %{customdata}<extra></extra>',
      customdata = ~Casos
    ) %>%
      plotly::layout(
        xaxis = list(title = "Mês / Ano", tickangle = -45, dtick = if(nrow(df_mensal) > 24) 4 else 1),
        yaxis = list(title = "Taxa de Incidência (por 100 mil hab.)")
      ) %>% estilo_base()
  })
  
  # 7.3 Evolução de Letalidade (Histograma Mensal conforme anos e filtros selecionados)
  output$plot_evolucao_letalidade <- renderPlotly({
    df <- df_filtrado()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados para letalidade no período selecionado."))
    
    df_mensal <- df %>%
      filter(!is.na(DT_NOTIFIC)) %>%
      mutate(
        Ano = as.integer(ANO_NOTIF),
        Mes = as.integer(month(DT_NOTIFIC)),
        Ano_Mes = sprintf("%04d-%02d", Ano, Mes)
      ) %>%
      group_by(Ano_Mes, Ano, Mes) %>%
      summarise(
        Casos = n(),
        Confirmados = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
        Obitos = sum(EVOLUCAO == "Óbito por dengue", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Ano_Mes) %>%
      mutate(Letalidade = ifelse(Confirmados > 0, round((Obitos / Confirmados) * 100, 2), 0))
    
    if(nrow(df_mensal) == 0) return(funcao_vazia_plotly("Sem dados mensais para os filtros ativos."))
    
    plotly::plot_ly(
      df_mensal, x = ~Ano_Mes, y = ~Letalidade,
      type = 'bar',
      marker = list(color = '#DC2626'),
      hovertemplate = '<b>%{x}</b><br>Letalidade: %{y}%<br>Óbitos: %{customdata[0]}<br>Confirmados: %{customdata[1]}<extra></extra>',
      customdata = mapply(function(o, c) list(o, c), df_mensal$Obitos, df_mensal$Confirmados, SIMPLIFY = FALSE)
    ) %>%
      plotly::layout(
        xaxis = list(title = "Mês / Ano", tickangle = -45, dtick = if(nrow(df_mensal) > 24) 4 else 1),
        yaxis = list(title = "Taxa de Letalidade (%)")
      ) %>% estilo_base()
  })
  
  # 7.4 Casos notificados, casos prováveis e confirmados por ano (interativo com filtros e seleção de anos)
  output$plot_confirmados_ano <- renderPlotly({
    df <- df_filtrado()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados para casos anuais no período selecionado."))
    
    df_ano <- df %>%
      mutate(Ano = as.character(ANO_NOTIF)) %>%
      group_by(Ano) %>%
      summarise(
        Notificados = n(),
        Confirmados = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
        Descartados = sum(CLASSI_FIN == "Descartado", na.rm = TRUE),
        Provaveis   = Notificados - Descartados,
        Provaveis_NaoConf = pmax(0, Provaveis - Confirmados),
        .groups = "drop"
      ) %>%
      arrange(as.numeric(Ano))
    
    if(nrow(df_ano) == 0) return(funcao_vazia_plotly("Nenhum registro para os filtros ativos."))
    
    plotly::plot_ly(df_ano, x = ~Ano) %>%
      plotly::add_trace(
        y = ~Confirmados,
        name = "Confirmados",
        type = 'bar',
        marker = list(color = '#DC2626'),
        hovertemplate = '<b>Ano %{x}</b><br>Confirmados: %{y}<extra></extra>'
      ) %>%
      plotly::add_trace(
        y = ~Provaveis_NaoConf,
        name = "Prováveis (Não Conf.)",
        type = 'bar',
        marker = list(color = '#F59E0B'),
        hovertemplate = '<b>Ano %{x}</b><br>Prováveis (Não Conf.): %{y}<br><b>Total Prováveis: %{customdata}</b><extra></extra>',
        customdata = ~sapply(Provaveis, format_num)
      ) %>%
      plotly::add_trace(
        y = ~Descartados,
        name = "Descartados",
        type = 'bar',
        marker = list(color = '#94A3B8'),
        hovertemplate = '<b>Ano %{x}</b><br>Descartados: %{y}<br><b>Total Notificados: %{customdata}</b><extra></extra>',
        customdata = ~sapply(Notificados, format_num)
      ) %>%
      plotly::layout(
        barmode = "stack",
        xaxis = list(title = "Ano de Notificação", type = "category"),
        yaxis = list(title = "Nº de Casos Notificados"),
        legend = list(orientation = "h", x = 0.05, y = 1.15, bgcolor = "rgba(255,255,255,0.7)"),
        hovermode = "x unified"
      ) %>% estilo_base()
  })

  # 7.5 Série histórica de óbitos (interativo com filtros e seleção de anos)
  output$plot_obitos_ano <- renderPlotly({
    df <- df_filtrado()
    if(is.null(df) || nrow(df) == 0) return(funcao_vazia_plotly("Sem dados para óbitos no período selecionado."))
    
    anos_sel <- sort(unique(as.numeric(na.omit(df$ANO_NOTIF))))
    if(length(anos_sel) == 0) return(funcao_vazia_plotly("Sem anos selecionados."))
    grid_anos <- data.frame(Ano = as.character(anos_sel))
    
    df_ob <- df %>%
      filter(EVOLUCAO == "Óbito por dengue") %>%
      group_by(Ano = as.character(ANO_NOTIF)) %>%
      summarise(Obitos = n(), .groups = "drop")
    
    df_ob_completo <- grid_anos %>%
      left_join(df_ob, by = "Ano") %>%
      mutate(Obitos = ifelse(is.na(Obitos), 0, Obitos)) %>%
      arrange(as.numeric(Ano))
    
    plotly::plot_ly(
      df_ob_completo,
      x = ~Ano,
      y = ~Obitos,
      type = 'bar',
      marker = list(
        color = '#B91C1C',
        line = list(color = '#7F1D1D', width = 1)
      ),
      text = ~Obitos,
      textposition = 'outside',
      hovertemplate = '<b>Ano %{x}</b><br>Óbitos Confirmados: %{y}<extra></extra>'
    ) %>%
      plotly::layout(
        xaxis = list(title = "Ano de Notificação", type = "category"),
        yaxis = list(title = "Nº de Óbitos por Dengue", rangemode = "tozero", dtick = 1)
      ) %>% estilo_base()
  })
  
  # ============================================================================
  # 8. DOWNLOAD DE TABELAS EXCEL (.XLSX)
  # ============================================================================
  gerar_download <- function(id_prefix, extrair_df_fun) {
    output[[paste0("dl_excel_", id_prefix)]] <- downloadHandler(
      filename = function() { paste0("dengue_belem_", id_prefix, "_", Sys.Date(), ".xlsx") },
      content = function(file) {
        df_exp <- extrair_df_fun()
        if(!is.null(df_exp)) {
          openxlsx::write.xlsx(df_exp, file, asTable = TRUE)
        }
      }
    )
  }
  
  gerar_download("semanas_geral", function() {
    req(df_filtrado())
    df_filtrado() %>% group_by(ANO = ANO_NOTIF, SEMANA = SEMANA_EPID) %>% summarise(Notificados = n(), Confirmados = sum(CLASSI_FIN == "Confirmado"), .groups = "drop")
  })
  gerar_download("sexo", function() { req(df_demografico()); df_demografico() %>% count(Sexo = CS_SEXO, name = "Casos") })
  gerar_download("raca", function() { req(df_demografico()); df_demografico() %>% count(Raca_Cor = CS_RACA, name = "Casos") })
  gerar_download("bairro", function() { req(df_demografico()); df_demografico() %>% count(Bairro = NM_BAIRRO, name = "Casos") %>% arrange(desc(Casos)) })
  gerar_download("piramide", function() { req(df_demografico()); df_demografico() %>% count(Idade = IDADE_ANOS, Sexo = CS_SEXO, name = "Casos") })
  gerar_download("classificacao", function() { req(df_filtrado()); df_filtrado() %>% count(Classificacao = CLASSI_FIN, name = "Casos") })
  gerar_download("evolucao", function() { req(df_filtrado()); df_filtrado() %>% count(Evolucao = EVOLUCAO, name = "Casos") })
  gerar_download("criterio", function() { req(df_demografico()); df_demografico() %>% count(Criterio = CRITERIO_FINAL, name = "Casos") })
  gerar_download("sorotipo", function() { 
    req(df_filtrado())
    col_s <- if("tp_sorotipo" %in% names(df_filtrado())) "tp_sorotipo" else "RESUL_SORO"
    df_filtrado() %>% count(Sorotipo = .data[[col_s]], name = "Casos") 
  })
  
  gerar_download("unidades", function() {
    rk <- dados_ranking_unidades_completo()
    req(rk)
    rk %>%
      transmute(
        `Posição` = Rotulo_Pos,
        `Estabelecimento Notificador` = Nome,
        `Código CNES` = ifelse(is.na(cnes) | cnes == "", "Não informado", as.character(cnes)),
        `Total de Notificações` = total,
        `Participação (%)` = Pct
      )
  })
  
  output$dl_excel_tabela_ranking_unidades <- downloadHandler(
    filename = function() { paste0("dengue_belem_ranking_unidades_notificadoras_", Sys.Date(), ".xlsx") },
    content = function(file) {
      rk <- dados_ranking_unidades_completo()
      if(!is.null(rk) && nrow(rk) > 0) {
        df_exp <- rk %>%
          transmute(
            `Posição` = Rotulo_Pos,
            `Estabelecimento Notificador` = Nome,
            `Código CNES` = ifelse(is.na(cnes) | cnes == "", "Não informado", as.character(cnes)),
            `Total de Notificações` = total,
            `Participação (%)` = Pct
          )
        openxlsx::write.xlsx(df_exp, file, asTable = TRUE)
      }
    }
  )
  
  gerar_download("diagrama_controle", function() {
    dados_diagrama_controle()
  })
  
  gerar_download("evolucao_incidencia", function() {
    df <- df_filtrado()
    req(df)
    pop <- if (!is.null(input$populacao) && input$populacao > 0) input$populacao else 1303403
    df %>%
      filter(!is.na(DT_NOTIFIC)) %>%
      mutate(Ano_Mes = sprintf("%04d-%02d", ANO_NOTIF, month(DT_NOTIFIC))) %>%
      group_by(`Mês/Ano` = Ano_Mes) %>%
      summarise(
        `Total Notificados` = n(),
        `Taxa Incidência (por 100 mil hab.)` = round((n() / pop) * 100000, 1),
        .groups = "drop"
      )
  })
  
  gerar_download("evolucao_letalidade", function() {
    df <- df_filtrado()
    req(df)
    df %>%
      filter(!is.na(DT_NOTIFIC)) %>%
      mutate(Ano_Mes = sprintf("%04d-%02d", ANO_NOTIF, month(DT_NOTIFIC))) %>%
      group_by(`Mês/Ano` = Ano_Mes) %>%
      summarise(
        `Total Notificados` = n(),
        `Casos Confirmados` = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
        `Óbitos por Dengue` = sum(EVOLUCAO == "Óbito por dengue", na.rm = TRUE),
        `Taxa Letalidade (%)` = ifelse(`Casos Confirmados` > 0, round((`Óbitos por Dengue` / `Casos Confirmados`) * 100, 2), 0),
        .groups = "drop"
      )
  })
  
  gerar_download("confirmados_ano", function() {
    df <- df_filtrado()
    req(df)
    df %>%
      mutate(Ano = as.character(ANO_NOTIF)) %>%
      group_by(Ano) %>%
      summarise(
        `Total Notificados` = n(),
        `Casos Prováveis`   = `Total Notificados` - sum(CLASSI_FIN == "Descartado", na.rm = TRUE),
        `Casos Confirmados` = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
        `Casos Descartados` = sum(CLASSI_FIN == "Descartado", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(as.numeric(Ano))
  })

  gerar_download("obitos_ano", function() {
    df <- df_filtrado()
    req(df)
    anos_serie <- sort(unique(as.numeric(na.omit(df$ANO_NOTIF))))
    grid_anos <- data.frame(Ano = as.character(anos_serie))
    df_ob <- df %>%
      filter(EVOLUCAO == "Óbito por dengue") %>%
      group_by(Ano = as.character(ANO_NOTIF)) %>%
      summarise(`Óbitos por Dengue` = n(), .groups = "drop")
    grid_anos %>%
      left_join(df_ob, by = "Ano") %>%
      mutate(`Óbitos por Dengue` = ifelse(is.na(`Óbitos por Dengue`), 0, `Óbitos por Dengue`)) %>%
      arrange(as.numeric(Ano))
  })
  
  output$dl_excel_mapa_bairros <- downloadHandler(
    filename = function() { paste0("dengue_belem_dados_espaciais_todos_bairros_", Sys.Date(), ".xlsx") },
    content = function(file) {
      mb <- dados_mapa_bairros()
      if(!is.null(mb)) {
        df_export <- mb %>% st_drop_geometry() %>% select(
          `Código Bairro` = cd_bairro,
          Bairro = nm_bairro,
          `Distrito Administrativo` = nome_distrito,
          `Sigla Distrito` = sigla_distrito,
          `População Censo 2022` = populacao_total,
          `Domicílios (Censo 2022)` = domicilios_ocupados,
          `Área (km²)` = area_km2,
          `Densidade (hab/km²)` = densidade_hab_km2,
          `Total Notificados` = total_casos,
          `Casos Prováveis` = provaveis,
          `Casos Confirmados` = confirmados,
          `Casos Descartados` = descartados,
          `Óbitos por Dengue` = obitos,
          `Taxa Incidência (por 100 mil hab.)` = incidencia,
          `Letalidade (%)` = letalidade
        ) %>% arrange(desc(`Taxa Incidência (por 100 mil hab.)`))
        openxlsx::write.xlsx(df_export, file, asTable = TRUE)
      }
    }
  )
  output$dl_excel_mapa_distritos <- downloadHandler(
    filename = function() { paste0("dengue_belem_distritos_administrativos_", Sys.Date(), ".xlsx") },
    content = function(file) {
      md <- dados_mapa_distritos()
      if(!is.null(md)) {
        df_export <- md %>% st_drop_geometry() %>% select(
          Sigla = sigla,
          `Distrito Administrativo` = nome,
          `Sede` = sede,
          `População (Censo 2022)` = populacao_2022,
          `Domicílios (Censo 2022)` = domicilios_2022,
          `Qtd Bairros` = qtd_bairros,
          `Área (km²)` = area_km2,
          `Total Notificados` = total_casos,
          `Casos Prováveis` = provaveis,
          `Casos Confirmados` = confirmados,
          `Casos Descartados` = descartados,
          `Óbitos por Dengue` = obitos,
          `Taxa Incidência (por 100 mil hab.)` = incidencia,
          `Letalidade Geral (%)` = letalidade
        ) %>% arrange(desc(total_casos))
        openxlsx::write.xlsx(df_export, file, asTable = TRUE)
      }
    }
  )
  
# ============================================================================
  # 7. MODO APRESENTAÇÃO EXECUTIVA DINÂMICA (ONLINE, PPTX & PDF)
  # ============================================================================
  
  # 7.1 Catálogo dos Gráficos Estruturados na Sequência Lógica das Abas
  GRAFICOS_CATALOGO <- list(
    # Aba 1: Visão Geral
    g_cartoes = list(
      id = "g_cartoes", aba = "Visão Geral", num_aba = 1,
      titulo = "Indicadores Estratégicos & Dados de Destaque",
      rotulo_curto = "Dados de Destaque (Cartões)"
    ),
    g_semanas = list(
      id = "g_semanas", aba = "Visão Geral", num_aba = 1,
      titulo = "Curva Epidêmica Semanal & Indicadores Gerais",
      rotulo_curto = "Curva Epidêmica Semanal"
    ),
    
    # Aba 2: Análise Espacial
    g_mapa_bairros = list(
      id = "g_mapa_bairros", aba = "Análise Espacial", num_aba = 2,
      titulo = "Mapa Territorial da Dengue — Divisão por Bairros",
      rotulo_curto = "Mapa Territorial por Bairro"
    ),
    g_mapa_distritos = list(
      id = "g_mapa_distritos", aba = "Análise Espacial", num_aba = 2,
      titulo = "Mapa Territorial da Dengue — Divisão por Distrito",
      rotulo_curto = "Mapa Territorial por Distrito"
    ),
    g_bairros = list(
      id = "g_bairros", aba = "Análise Espacial", num_aba = 2,
      titulo = "Vigilância Territorial & Bairros Críticos",
      rotulo_curto = "Top Bairros Notificados"
    ),
    g_distritos = list(
      id = "g_distritos", aba = "Análise Espacial", num_aba = 2,
      titulo = "Síntese por Distrito Administrativo",
      rotulo_curto = "Tabela de Distritos"
    ),
    
    # Aba 3: Sociodemográfico
    g_piramide = list(
      id = "g_piramide", aba = "Sociodemográfico", num_aba = 3,
      titulo = "Perfil Demográfico & Pirâmide Etária",
      rotulo_curto = "Pirâmide Etária"
    ),
    g_sexo_raca = list(
      id = "g_sexo_raca", aba = "Sociodemográfico", num_aba = 3,
      titulo = "Perfil por Raça/Cor Declarada",
      rotulo_curto = "Distribuição Raça/Cor"
    ),
    
    # Aba 4: Diagnóstico & Desfechos
    g_classificacao = list(
      id = "g_classificacao", aba = "Diagnóstico & Desfechos", num_aba = 4,
      titulo = "Classificação Final dos Casos",
      rotulo_curto = "Classificação Final"
    ),
    g_criterio = list(
      id = "g_criterio", aba = "Diagnóstico & Desfechos", num_aba = 4,
      titulo = "Critério de Confirmação / Diagnóstico",
      rotulo_curto = "Critério Diagnóstico"
    ),
    g_sorotipo = list(
      id = "g_sorotipo", aba = "Diagnóstico & Desfechos", num_aba = 4,
      titulo = "Distribuição de Casos por Sorotipo Viral",
      rotulo_curto = "Casos por Sorotipo"
    ),
    g_evolucao = list(
      id = "g_evolucao", aba = "Diagnóstico & Desfechos", num_aba = 4,
      titulo = "Evolução Clínica & Desfechos",
      rotulo_curto = "Evolução & Desfechos"
    ),
    
    # Aba 5: Unidades Notificadoras
    g_unidades = list(
      id = "g_unidades", aba = "Unidades Notificadoras", num_aba = 5,
      titulo = "Rede Notificadora de Saúde (Top 10)",
      rotulo_curto = "Top Estabelecimentos"
    ),
    
    # Aba 6: Análise Epidemiológica
    g_canal = list(
      id = "g_canal", aba = "Análise Epidemiológica", num_aba = 6,
      titulo = "Canal Endêmico & Diagrama de Controle (Ano Vigente: 2026)",
      rotulo_curto = "Canal Endêmico (Ano Vigente 2026)"
    ),
    g_evol_incidencia = list(
      id = "g_evol_incidencia", aba = "Análise Epidemiológica", num_aba = 6,
      titulo = "Evolução Histórica da Taxa de Incidência",
      rotulo_curto = "Evolução da Incidência"
    ),
    g_evol_letalidade = list(
      id = "g_evol_letalidade", aba = "Análise Epidemiológica", num_aba = 6,
      titulo = "Evolução Histórica da Taxa de Letalidade",
      rotulo_curto = "Evolução da Letalidade"
    ),
    g_casos_obitos_ano = list(
      id = "g_casos_obitos_ano", aba = "Análise Epidemiológica", num_aba = 6,
      titulo = "Série Anual de Notificados, Confirmados e Óbitos",
      rotulo_curto = "Série Anual de Casos/Óbitos"
    )
  )
  
  # 7.2 Reatividade dos Gráficos Selecionados (Sempre na Ordem Lógica das Abas)
  graficos_selecionados <- reactive({
    todos_ids <- names(GRAFICOS_CATALOGO)
    sel <- character(0)
    for (gid in todos_ids) {
      val <- input[[paste0("sel_", gid)]]
      if (isTRUE(val)) {
        sel <- c(sel, gid)
      }
    }
    if (length(sel) == 0) sel <- "g_cartoes" # Garantir ao menos 1
    return(sel)
  })
  
  # 7.3 Ações Rápidas da Caixinha Lateral
  observeEvent(input$btn_sel_todos, {
    for (gid in names(GRAFICOS_CATALOGO)) {
      updateCheckboxInput(session, paste0("sel_", gid), value = TRUE)
    }
  })
  observeEvent(input$btn_sel_padrao, {
    padrao_ids <- c("g_cartoes", "g_semanas", "g_mapa_bairros", "g_bairros", "g_piramide", "g_classificacao", "g_unidades", "g_canal")
    for (gid in names(GRAFICOS_CATALOGO)) {
      updateCheckboxInput(session, paste0("sel_", gid), value = (gid %in% padrao_ids))
    }
  })
  observeEvent(input$btn_limpar_todos, {
    for (gid in names(GRAFICOS_CATALOGO)) {
      updateCheckboxInput(session, paste0("sel_", gid), value = (gid == "g_cartoes"))
    }
  })
  
  # Alternar exibição da caixinha lateral
  observeEvent(input$btn_toggle_caixinha, {
    shinyjs::toggle(id = "caixinha_lateral_graficos", anim = TRUE)
    shinyjs::toggleClass(id = "area_apresentacao_slides", class = "col-lg-12")
    shinyjs::toggleClass(id = "area_apresentacao_slides", class = "col-lg-9")
  })
  
  # Badges de contagem
  output$txt_badge_sel <- renderText({
    sel <- graficos_selecionados()
    paste0(length(sel), " sel.")
  })
  output$txt_badge_sel2 <- renderText({
    sel <- graficos_selecionados()
    paste0(length(sel), " de ", length(GRAFICOS_CATALOGO))
  })
  
  # Badges individuais com numeração dinâmica de slide
  observe({
    sel <- graficos_selecionados()
    n_tot <- length(sel)
    for (gid in names(GRAFICOS_CATALOGO)) {
      local({
        id_local <- gid
        output[[paste0("badge_", id_local)]] <- renderText({
          idx <- match(id_local, sel)
          if (!is.na(idx)) {
            paste0("Slide ", idx, " de ", n_tot, " • Aba ", GRAFICOS_CATALOGO[[id_local]]$num_aba)
          } else {
            ""
          }
        })
      })
    }
  })
  
  # 7.4 Atualização Dinâmica do Dropdown de Navegação
  observe({
    sel <- graficos_selecionados()
    choices_vec <- c(
      "Slide Inicial (Capa Executiva)" = "g_capa",
      setNames(
        sel,
        sapply(seq_along(sel), function(i) {
          info <- GRAFICOS_CATALOGO[[sel[i]]]
          paste0("Slide ", i, " de ", length(sel), ": ", info$rotulo_curto)
        })
      )
    )
    todos_slides <- c("g_capa", sel)
    atual <- isolate(input$sel_slide_apres)
    novo_sel <- if (!is.null(atual) && atual %in% todos_slides) atual else "g_capa"
    updateSelectInput(session, "sel_slide_apres", choices = choices_vec, selected = novo_sel)
  })
  
  # Navegação de Slides
  observeEvent(input$btn_slide_prox, {
    sel <- graficos_selecionados()
    todos_slides <- c("g_capa", sel)
    atual <- input$sel_slide_apres
    idx <- match(atual, todos_slides)
    if (!is.na(idx) && idx < length(todos_slides)) {
      updateSelectInput(session, "sel_slide_apres", selected = todos_slides[idx + 1])
    }
  })
  observeEvent(input$btn_slide_ant, {
    sel <- graficos_selecionados()
    todos_slides <- c("g_capa", sel)
    atual <- input$sel_slide_apres
    idx <- match(atual, todos_slides)
    if (!is.na(idx) && idx > 1) {
      updateSelectInput(session, "sel_slide_apres", selected = todos_slides[idx - 1])
    }
  })
  observeEvent(input$btn_fullscreen_apres, {
    # Tratado nativamente no cliente pelo onclick do botão para garantir execução na ativação do usuário
  })
  
  # 7.5 KPIs Executivos
  output$apres_kpi_total <- renderText({
    df <- df_filtrado()
    format_num(if (!is.null(df)) nrow(df) else 0)
  })
  output$apres_kpi_total_dest <- renderText({
    df <- df_filtrado()
    format_num(if (!is.null(df)) nrow(df) else 0)
  })
  output$apres_kpi_confirmados <- renderText({
    df <- df_filtrado()
    format_num(if (!is.null(df)) sum(df$CLASSI_FIN == "Confirmado", na.rm = TRUE) else 0)
  })
  output$apres_kpi_confirmados_dest <- renderText({
    df <- df_filtrado()
    format_num(if (!is.null(df)) sum(df$CLASSI_FIN == "Confirmado", na.rm = TRUE) else 0)
  })
  output$apres_kpi_provaveis_dest <- renderText({
    df <- df_filtrado()
    if (is.null(df)) return("0")
    prov <- nrow(df) - sum(df$CLASSI_FIN == "Descartado", na.rm = TRUE)
    format_num(prov)
  })
  output$apres_kpi_inconclusivos_dest <- renderText({
    df <- df_filtrado()
    format_num(if (!is.null(df)) sum(df$CLASSI_FIN == "Inconclusivo", na.rm = TRUE) else 0)
  })
  output$apres_kpi_descartados_dest <- renderText({
    df <- df_filtrado()
    format_num(if (!is.null(df)) sum(df$CLASSI_FIN == "Descartado", na.rm = TRUE) else 0)
  })
  output$apres_kpi_obitos_dest <- renderText({
    df <- df_filtrado()
    format_num(if (!is.null(df)) sum(df$EVOLUCAO == "Óbito por dengue", na.rm = TRUE) else 0)
  })
  output$apres_kpi_incidencia <- renderText({
    df <- df_filtrado()
    pop <- if (!is.null(input$populacao) && input$populacao > 0) input$populacao else 1303403
    format_num(if (!is.null(df)) (nrow(df) / pop) * 100000 else 0, digits = 1)
  })
  output$apres_kpi_incidencia_dest <- renderText({
    df <- df_filtrado()
    pop <- if (!is.null(input$populacao) && input$populacao > 0) input$populacao else 1303403
    format_num(if (!is.null(df)) (nrow(df) / pop) * 100000 else 0, digits = 1)
  })
  output$apres_kpi_letalidade <- renderText({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return("0,00%")
    conf <- sum(df$CLASSI_FIN == "Confirmado", na.rm = TRUE)
    ob <- sum(df$EVOLUCAO == "Óbito por dengue", na.rm = TRUE)
    format_pct(if (conf > 0) (ob / conf) * 100 else 0, digits = 2)
  })
  output$apres_kpi_letalidade_dest <- renderText({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return("0,00%")
    conf <- sum(df$CLASSI_FIN == "Confirmado", na.rm = TRUE)
    ob <- sum(df$EVOLUCAO == "Óbito por dengue", na.rm = TRUE)
    format_pct(if (conf > 0) (ob / conf) * 100 else 0, digits = 2)
  })
  
  output$apres_kpi_top_bairro <- renderText({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return("Nenhum")
    tb <- df %>% filter(!is.na(NM_BAIRRO), NM_BAIRRO != "") %>% count(NM_BAIRRO, sort = TRUE)
    if (nrow(tb) > 0) paste0(tb$NM_BAIRRO[1], " (", format_num(tb$n[1]), ")") else "Nenhum"
  })
  output$apres_kpi_top_incidencia_bairro <- renderText({
    mb <- dados_mapa_bairros()
    if (is.null(mb) || nrow(mb) == 0) return("Nenhum")
    top_inc <- mb %>% st_drop_geometry() %>% arrange(desc(incidencia)) %>% head(1)
    if (nrow(top_inc) > 0) paste0(top_inc$nm_bairro, " (", format_num(top_inc$incidencia, 1), " por 100 mil hab.)") else "Nenhum"
  })
  output$apres_kpi_bairros_atingidos <- renderText({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return("0 de 71")
    n_b <- length(unique(na.omit(df$NM_BAIRRO[df$NM_BAIRRO != ""])))
    paste0(n_b, " de 71 (", format_pct((n_b / 71) * 100, 1), ")")
  })
  
  # Insights Dinâmicos por Slide
  output$apres_insight_cartoes <- renderText({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return("Sem dados no recorte selecionado.")
    total <- nrow(df)
    desc <- sum(df$CLASSI_FIN == "Descartado", na.rm = TRUE)
    prov <- total - desc
    conf <- sum(df$CLASSI_FIN == "Confirmado", na.rm = TRUE)
    ob <- sum(df$EVOLUCAO == "Óbito por dengue", na.rm = TRUE)
    pop <- if (!is.null(input$populacao) && input$populacao > 0) input$populacao else 1303403
    incid <- round((prov / pop) * 100000, 1)
    let <- if (conf > 0) round((ob / conf) * 100, 2) else 0
    class_risco <- if (incid < 100) "Baixo Risco (<100 por 100 mil hab.)" else if (incid < 300) "Médio Risco (100 a 300 por 100 mil hab.)" else "Alto Risco (>300 por 100 mil hab.)"
    paste0("Foram registradas ", format_num(total), " notificações no município de Belém. Do total, ", format_num(prov), " representam casos prováveis (", format_pct((prov / max(total, 1)) * 100, 1), ") e ", format_num(conf), " foram confirmados. A taxa de incidência acumulada é de ", format_num(incid, 1), " casos por 100 mil habitantes (classificação de ", class_risco, "). A taxa de letalidade hospitalar e geral encontra-se em ", format_pct(let, 2), " (com ", format_num(ob), " óbitos registrados).")
  })
  
  output$apres_insight_slide1 <- renderText({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return("Sem dados no recorte selecionado.")
    total <- nrow(df)
    conf <- sum(df$CLASSI_FIN == "Confirmado", na.rm = TRUE)
    pop <- if (!is.null(input$populacao) && input$populacao > 0) input$populacao else 1303403
    incid <- round((total / pop) * 100000, 1)
    class_risco <- if (incid < 100) "Baixo Risco (<100 por 100 mil hab.)" else if (incid < 300) "Médio Risco (100 a 300 por 100 mil hab.)" else "Alto Risco (>300 por 100 mil hab.)"
    paste0("Foram registradas ", format_num(total), " notificações no recorte selecionado, resultando em taxa de incidência estimada em ", format_num(incid, 1), " casos por 100 mil habitantes (classificação de ", class_risco, "). A taxa de confirmação laboratorial/clínica é de ", format_pct((conf / max(total, 1)) * 100, 1), ".")
  })
  
  output$apres_legenda_mapa_bairros <- renderText({
    tipo_txt <- if (!is.null(input$tipo_visualizacao_mapa) && input$tipo_visualizacao_mapa == "calor") "Mapa de Calor (Mancha Térmica)" else "Mapa Coroplético"
    metrica_txt <- case_when(
      input$metrica_mapa == "incidencia"  ~ "Taxa de Incidência (por 100 mil hab.)",
      input$metrica_mapa == "confirmados" ~ "Casos Confirmados",
      input$metrica_mapa == "provaveis"   ~ "Casos Prováveis",
      TRUE                                ~ "Total Notificados"
    )
    paste0("Configuração Espacial Ativa: ", tipo_txt, " • Métrica: ", metrica_txt, " • Camada Territorial: Divisão por Bairros e Ilhas de Belém")
  })
  
  output$apres_insight_mapa_bairros <- renderText({
    mb <- dados_mapa_bairros()
    if (is.null(mb) || nrow(mb) == 0) return("Sem dados territoriais.")
    top_inc <- mb %>% st_drop_geometry() %>% arrange(desc(incidencia)) %>% head(1)
    top_casos <- mb %>% st_drop_geometry() %>% arrange(desc(total_casos)) %>% head(1)
    paste0("O bairro com maior número absoluto de casos é ", top_casos$nm_bairro, " (", format_num(top_casos$total_casos), " notificações), enquanto ", top_inc$nm_bairro, " lidera a taxa de incidência ponderada com ", format_num(top_inc$incidencia, 1), " casos/100 mil hab. As áreas insulares permanecem destacadas para vigilância contínua.")
  })
  
  output$apres_legenda_mapa_distritos <- renderText({
    tipo_txt <- if (!is.null(input$tipo_visualizacao_mapa) && input$tipo_visualizacao_mapa == "calor") "Mapa de Calor (Mancha Térmica)" else "Mapa Coroplético"
    metrica_txt <- case_when(
      input$metrica_mapa == "incidencia"  ~ "Taxa de Incidência (por 100 mil hab.)",
      input$metrica_mapa == "confirmados" ~ "Casos Confirmados",
      input$metrica_mapa == "provaveis"   ~ "Casos Prováveis",
      TRUE                                ~ "Total Notificados"
    )
    paste0("Configuração Espacial Ativa: ", tipo_txt, " • Métrica: ", metrica_txt, " • Camada Territorial: 8 Distritos Administrativos de Belém")
  })
  
  output$apres_insight_mapa_distritos <- renderText({
    md <- dados_mapa_distritos()
    if (is.null(md) || nrow(md) == 0) return("Sem dados distritais.")
    top_d <- md %>% st_drop_geometry() %>% arrange(desc(total_casos)) %>% head(1)
    top_i <- md %>% st_drop_geometry() %>% arrange(desc(incidencia)) %>% head(1)
    paste0("O distrito ", top_d$nome, " (", top_d$sigla, ") concentra o maior volume notificado do município (", format_num(top_d$total_casos), " casos), enquanto ", top_i$nome, " (", top_i$sigla, ") apresenta a maior taxa de incidência (", format_num(top_i$incidencia, 1), " por 100 mil hab.).")
  })
  
  output$apres_insight_slide2 <- renderText({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return("Sem dados territoriais.")
    tb <- df %>% filter(!is.na(NM_BAIRRO), NM_BAIRRO != "") %>% count(NM_BAIRRO, sort = TRUE)
    if (nrow(tb) >= 3) {
      top3 <- paste(tb$NM_BAIRRO[1:3], collapse = ", ")
      pct3 <- round((sum(tb$n[1:3]) / sum(tb$n)) * 100, 1)
      paste0("Os bairros ", top3, " lideram as notificações e concentram ", format_pct(pct3, 1), " de todos os registros de Belém. Recomenda-se foco prioritário das brigadas do CCZ nestas áreas.")
    } else {
      "Distribuição territorial dentro dos parâmetros habituais no período analisado."
    }
  })
  
  output$apres_insight_slide3 <- renderText({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return("Sem dados sociodemográficos.")
    fem <- sum(df$CS_SEXO == "Feminino", na.rm = TRUE)
    masc <- sum(df$CS_SEXO == "Masculino", na.rm = TRUE)
    tot <- max(fem + masc, 1)
    paste0("O perfil por sexo aponta ", format_pct((fem / tot) * 100, 1), " no sexo feminino e ", format_pct((masc / tot) * 100, 1), " no masculino. A pirâmide etária indica maior prevalência na população de 20 a 49 anos, demandando campanhas específicas em polos comerciais e de serviços.")
  })
  
  output$apres_insight_slide4 <- renderText({
    df <- df_filtrado()
    if (is.null(df) || nrow(df) == 0) return("Sem dados de desfecho.")
    ob <- sum(df$EVOLUCAO == "Óbito por dengue", na.rm = TRUE)
    conf <- sum(df$CLASSI_FIN == "Confirmado", na.rm = TRUE)
    let <- ifelse(conf > 0, round((ob / conf) * 100, 2), 0)
    meta_status <- if (let <= 0.05) "dentro da meta rigorosa recomendada (≤ 0,05%)" else "sob monitoramento prioritário com reforço dos protocolos de hidratação precoce"
    paste0("Registrados ", format_num(ob), " óbitos por dengue entre confirmados (letalidade geral de ", format_pct(let, 2), "), patamar que se encontra ", meta_status, ".")
  })
  
  output$apres_insight_slide5 <- renderText({
    rk <- dados_ranking_unidades_completo()
    if (is.null(rk) || nrow(rk) == 0) return("Sem dados de estabelecimentos notificadores.")
    top1 <- rk$Nome[1]
    pct3 <- round(sum(rk$pct[1:min(3, nrow(rk))]), 1)
    paste0("A unidade com maior fluxo notificador é ", top1, ". Os 3 principais serviços concentram ", format_pct(pct3, 1), " dos atendimentos, reforçando a importância do suporte contínuo de testes rápidos e soro nas UPAs de referência.")
  })
  
  output$apres_insight_slide6 <- renderText({
    paste0("O Canal Endêmico acompanha desvios frente à distribuição histórica municipal de Belém (2015-2023). Notificações acima do Limite Superior Vermelho sinalizam situação de alerta epidêmico imediato.")
  })
  
  # 7.6 Gráficos e Mapas da Apresentação
  output$plot_apres_semanas <- renderPlotly({
    df <- df_filtrado()
    req(df)
    df_sem <- df %>%
      filter(!is.na(SEMANA_EPID), SEMANA_EPID >= 1, SEMANA_EPID <= 53) %>%
      group_by(SEMANA_EPID, Ano = as.character(ANO_NOTIF)) %>%
      summarise(Casos = n(), .groups = "drop")
    req(nrow(df_sem) > 0)
    
    anos_unicos <- sort(unique(df_sem$Ano))
    cores_ano <- PALETTE_ANOS[1:min(length(anos_unicos), length(PALETTE_ANOS))]
    names(cores_ano) <- anos_unicos[1:length(cores_ano)]
    
    plot_ly(df_sem, x = ~SEMANA_EPID, y = ~Casos, color = ~Ano, colors = cores_ano,
            type = 'scatter', mode = 'lines+markers',
            marker = list(size = 6), line = list(width = 2.5),
            hovertemplate = '<b>Ano %{fullData.name}</b><br>Semana %{x}<br>Casos: %{y:,}<extra></extra>') %>%
      layout(
        xaxis = list(title = "Semana Epidemiológica", dtick = 2, range = c(0.5, 53.5)),
        yaxis = list(title = "Total de Notificações Registradas"),
        legend = list(orientation = "h", x = 0.05, y = 1.12),
        margin = list(t = 30, b = 40, l = 50, r = 20)
      ) %>%
      estilo_base()
  })
  
  # MAPAS LEAFLET EXECUTIVOS (ACOMPANHAM SELEÇÃO DA ABA DE ANÁLISE ESPACIAL)
  output$plot_apres_mapa_bairros <- renderLeaflet({
    montar_mapa_leaflet_belem("bairros")
  })
  
  output$plot_apres_mapa_distritos <- renderLeaflet({
    montar_mapa_leaflet_belem("distritos")
  })
  
  output$plot_apres_bairros <- renderPlotly({
    df <- df_filtrado()
    req(df)
    df_b <- df %>%
      filter(!is.na(NM_BAIRRO), NM_BAIRRO != "") %>%
      count(Bairro = NM_BAIRRO, sort = TRUE) %>%
      head(12)
    req(nrow(df_b) > 0)
    
    plot_ly(df_b, x = ~n, y = ~reorder(Bairro, n), type = 'bar', orientation = 'h',
            marker = list(color = "#0000A1"),
            hovertemplate = '<b>%{y}</b><br>Notificações: %{x:,}<extra></extra>') %>%
      layout(
        xaxis = list(title = "Total Notificado"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 150, r = 20, t = 10, b = 40)
      ) %>%
      estilo_base()
  })
  
  output$tabela_apres_distritos <- renderTable({
    md <- dados_mapa_distritos()
    req(md)
    md %>%
      st_drop_geometry() %>%
      transmute(
        Distrito = sigla,
        População = format_num(populacao_2022),
        Casos = format_num(total_casos),
        `Incidência (por 100 mil hab.)` = format_num(incidencia, 1),
        `Letalidade` = format_pct(letalidade, 1)
      ) %>%
      arrange(desc(as.numeric(gsub("\\\\.", "", Casos))))
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs")
  
  output$plot_apres_piramide <- renderPlotly({
    df <- df_filtrado()
    req(df)
    df_pir <- df %>%
      filter(!is.na(IDADE_ANOS), IDADE_ANOS >= 0, IDADE_ANOS <= 110, CS_SEXO %in% c("Feminino", "Masculino")) %>%
      mutate(Faixa = cut(IDADE_ANOS, breaks = c(seq(0, 80, 10), 120), right = FALSE, labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"))) %>%
      count(Faixa, CS_SEXO)
    req(nrow(df_pir) > 0)
    
    df_fem <- df_pir %>% filter(CS_SEXO == "Feminino")
    df_masc <- df_pir %>% filter(CS_SEXO == "Masculino")
    max_val <- max(c(df_fem$n, df_masc$n), na.rm = TRUE)
    
    plot_ly() %>%
      add_bars(data = df_fem, x = ~n, y = ~Faixa, name = "Feminino", marker = list(color = "#EC4899"), orientation = 'h',
               hovertemplate = '<b>Feminino %{y}</b><br>Casos: %{x:,}<extra></extra>') %>%
      add_bars(data = df_masc, x = ~(-n), y = ~Faixa, name = "Masculino", marker = list(color = "#3B82F6"), orientation = 'h',
               hovertemplate = '<b>Masculino %{y}</b><br>Casos: %{customdata:,}<extra></extra>', customdata = ~n) %>%
      layout(
        barmode = 'overlay',
        xaxis = list(title = "Nº de Notificações", range = c(-max_val * 1.15, max_val * 1.15),
                     tickvals = seq(-max_val, max_val, length.out = 5),
                     ticktext = abs(round(seq(-max_val, max_val, length.out = 5)))),
        yaxis = list(title = "Faixa Etária"),
        legend = list(orientation = "h", x = 0.3, y = 1.12),
        margin = list(l = 60, r = 20, t = 20, b = 40)
      ) %>%
      estilo_base()
  })
  
  output$plot_apres_sexo_raca <- renderPlotly({
    df <- df_filtrado()
    req(df)
    df_raca <- df %>%
      filter(!is.na(CS_RACA), CS_RACA != "") %>%
      count(Raca = CS_RACA, sort = TRUE)
    req(nrow(df_raca) > 0)
    
    plot_ly(df_raca, x = ~n, y = ~reorder(Raca, n), type = 'bar', orientation = 'h',
            marker = list(color = PALETTE_PIE[1:nrow(df_raca)]),
            hovertemplate = '<b>%{y}</b><br>Casos: %{x:,}<extra></extra>') %>%
      layout(
        title = list(text = "Distribuição por Raça/Cor Declarada", font = list(size = 13, color = "#0000A1")),
        xaxis = list(title = "Total Notificado"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 100, r = 20, t = 40, b = 40)
      ) %>%
      estilo_base()
  })
  
  output$plot_apres_classificacao <- renderPlotly({
    df <- df_filtrado()
    req(df)
    df_cla <- df %>%
      filter(!is.na(CLASSI_FIN), CLASSI_FIN != "") %>%
      count(Classificacao = CLASSI_FIN, sort = TRUE)
    req(nrow(df_cla) > 0)
    
    cores_cla <- c("Confirmado" = "#EF4444", "Descartado" = "#64748B", "Inconclusivo" = "#0EA5E9", "Suspeito" = "#F59E0B")
    
    plot_ly(df_cla, labels = ~Classificacao, values = ~n, type = 'pie', hole = 0.55,
            marker = list(colors = cores_cla[df_cla$Classificacao]),
            textinfo = 'label+percent',
            hovertemplate = '<b>%{label}</b><br>Total: %{value:,}<br>Percentual: %{percent}<extra></extra>') %>%
      layout(
        title = list(text = "Classificação Final dos Casos", font = list(size = 13, color = "#0000A1")),
        legend = list(orientation = "h", x = 0.1, y = -0.1),
        margin = list(t = 40, b = 40, l = 20, r = 20)
      ) %>%
      estilo_base()
  })
  
  # CRITÉRIO DE CONFIRMAÇÃO / DIAGNÓSTICO
  output$plot_apres_criterio <- renderPlotly({
    df <- df_filtrado()
    req(df)
    df_res <- df %>% filter(!is.na(CRITERIO_FINAL), CRITERIO_FINAL != "") %>% count(Criterio = CRITERIO_FINAL, name = "Casos", sort = TRUE)
    req(nrow(df_res) > 0)
    plot_ly(df_res, labels = ~Criterio, values = ~Casos, type = "pie", hole = 0.55,
            marker = list(colors = PALETTE_PIE[1:min(nrow(df_res), length(PALETTE_PIE))]),
            textinfo = 'label+percent',
            hovertemplate = '<b>%{label}</b><br>Casos: %{value:,}<br>Proporção: %{percent}<extra></extra>') %>%
      layout(
        title = list(text = "Critério de Confirmação / Diagnóstico", font = list(size = 13, color = "#0000A1")),
        legend = list(orientation = "h", x = 0.1, y = -0.1),
        margin = list(t = 40, b = 40, l = 20, r = 20)
      ) %>% estilo_base()
  })
  
  # CASOS POR SOROTIPO VIRAL
  output$plot_apres_sorotipo <- renderPlotly({
    df <- df_filtrado()
    req(df)
    col_s <- if("tp_sorotipo" %in% names(df)) "tp_sorotipo" else if("TP_SOROTIPO" %in% names(df)) "TP_SOROTIPO" else "RESUL_SORO"
    df_res <- df %>% filter(!is.na(.data[[col_s]]), .data[[col_s]] != "") %>% count(Sorotipo = .data[[col_s]], name = "Casos", sort = TRUE)
    if(nrow(df_res) == 0) return(funcao_vazia_plotly("Sem dados de sorotipo para os filtros ativos."))
    plot_ly(df_res, labels = ~Sorotipo, values = ~Casos, type = "pie", hole = 0.55,
            marker = list(colors = PALETTE_PIE[1:min(nrow(df_res), length(PALETTE_PIE))]),
            textinfo = 'label+percent',
            hovertemplate = '<b>%{label}</b><br>Casos: %{value:,}<br>Proporção: %{percent}<extra></extra>') %>%
      layout(
        title = list(text = "Casos por Sorotipo Viral (DENV)", font = list(size = 13, color = "#0000A1")),
        legend = list(orientation = "h", x = 0.1, y = -0.1),
        margin = list(t = 40, b = 40, l = 20, r = 20)
      ) %>% estilo_base()
  })
  
  output$plot_apres_evolucao <- renderPlotly({
    df <- df_filtrado()
    req(df)
    df_ev <- df %>%
      filter(!is.na(EVOLUCAO), EVOLUCAO != "") %>%
      count(Desfecho = EVOLUCAO, sort = TRUE)
    req(nrow(df_ev) > 0)
    
    cores_ev <- c("Cura" = "#10B981", "Óbito por dengue" = "#EF4444", "Óbito por outras causas" = "#F59E0B", "Em investigação" = "#0EA5E9")
    
    plot_ly(df_ev, x = ~n, y = ~reorder(Desfecho, n), type = 'bar', orientation = 'h',
            marker = list(color = cores_ev[df_ev$Desfecho]),
            hovertemplate = '<b>%{y}</b><br>Casos: %{x:,}<extra></extra>') %>%
      layout(
        title = list(text = "Evolução Clínica e Desfechos", font = list(size = 13, color = "#0000A1")),
        xaxis = list(title = "Total de Pacientes"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 150, r = 20, t = 40, b = 40)
      ) %>%
      estilo_base()
  })
  
  output$plot_apres_unidades <- renderPlotly({
    rk <- dados_ranking_unidades_completo()
    req(rk)
    df_top <- rk %>% head(10)
    req(nrow(df_top) > 0)
    
    plot_ly(df_top, x = ~total, y = ~reorder(Nome, total), type = 'bar', orientation = 'h',
            marker = list(color = "#10B981"),
            hovertemplate = '<b>%{y}</b><br>Notificações: %{x:,}<br>Participação: %{customdata}%<extra></extra>',
            customdata = ~Pct) %>%
      layout(
        title = list(text = "Top 10 Estabelecimentos Notificadores", font = list(size = 13, color = "#0000A1")),
        xaxis = list(title = "Total Notificado"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 220, r = 20, t = 40, b = 40)
      ) %>%
      estilo_base()
  })
  
  # DIAGRAMA DE CONTROLE (SEMPRE ANO VIGENTE 2026 E LINHA DE BASE MUNICIPAL)
  output$plot_apres_canal <- renderPlotly({
    df_diag <- dados_diagrama_controle()
    req(df_diag)
    
    df_base_tudo <- dados_base()
    sems_2026 <- df_base_tudo$SEMANA_EPID[df_base_tudo$ANO_NOTIF == 2026 & df_base_tudo$SEMANA_EPID <= 52]
    max_sem_2026 <- if(length(sems_2026) > 0) max(sems_2026, na.rm = TRUE) else 44
    if (is.na(max_sem_2026) || max_sem_2026 <= 0) max_sem_2026 <- 44
    df_obs <- df_diag %>% filter(SEMANA_EPID <= max_sem_2026)
    
    plot_ly() %>%
      add_trace(
        data = df_diag, x = ~SEMANA_EPID, y = ~Limite_Inf,
        type = 'scatter', mode = 'lines',
        line = list(color = '#10B981', dash = 'dash', width = 1.5),
        name = 'Limite Inferior', hoverinfo = 'skip'
      ) %>%
      add_trace(
        data = df_diag, x = ~SEMANA_EPID, y = ~Limite_Sup,
        type = 'scatter', mode = 'lines',
        fill = 'tonexty', fillcolor = 'rgba(59, 130, 246, 0.12)',
        line = list(color = '#DC2626', width = 2.5),
        name = 'Limite Superior (Alerta Epidêmico)',
        hovertemplate = '<b>Semana %{x}</b><br>Limite Superior: %{y} casos<extra></extra>'
      ) %>%
      add_trace(
        data = df_diag, x = ~SEMANA_EPID, y = ~Media,
        type = 'scatter', mode = 'lines',
        line = list(color = '#0000A1', dash = 'dot', width = 2),
        name = 'Média Esperada (2015-2023)',
        hovertemplate = '<b>Semana %{x}</b><br>Média Esperada: %{y} casos<extra></extra>'
      ) %>%
      add_trace(
        data = df_obs, x = ~SEMANA_EPID, y = ~Observado_2026,
        type = 'bar', name = 'Observado 2026 (Ano Vigente)',
        marker = list(
          color = ~ifelse(Observado_2026 > Limite_Sup, '#EF4444', '#3B82F6'),
          opacity = 0.85
        ),
        hovertemplate = '<b>Semana %{x}</b><br>Observado 2026: %{y} casos<extra></extra>'
      ) %>%
      layout(
        xaxis = list(title = "Semana Epidemiológica", dtick = 2, range = c(0.5, 53.5)),
        yaxis = list(title = "Nº de Notificações de Dengue"),
        legend = list(orientation = "h", x = 0.05, y = 1.15),
        margin = list(t = 30, b = 40, l = 50, r = 20)
      ) %>% estilo_base()
  })
  
  # EVOLUÇÃO DE INCIDÊNCIA
  output$plot_apres_evol_incidencia <- renderPlotly({
    df <- df_filtrado()
    req(df)
    pop <- if (!is.null(input$populacao) && input$populacao > 0) input$populacao else 1303403
    df_mensal <- df %>%
      filter(!is.na(DT_NOTIFIC)) %>%
      mutate(Ano = as.integer(ANO_NOTIF), Mes = as.integer(month(DT_NOTIFIC)), Ano_Mes = sprintf("%04d-%02d", Ano, Mes)) %>%
      group_by(Ano_Mes) %>%
      summarise(Casos = n(), .groups = "drop") %>%
      arrange(Ano_Mes) %>%
      mutate(Incidencia = round((Casos / pop) * 100000, 1))
    req(nrow(df_mensal) > 0)
    plot_ly(df_mensal, x = ~Ano_Mes, y = ~Incidencia, type = 'bar', marker = list(color = '#0000A1'),
            hovertemplate = '<b>%{x}</b><br>Incidência: %{y} por 100 mil hab.<extra></extra>') %>%
      layout(xaxis = list(title = "Mês / Ano", tickangle = -45), yaxis = list(title = "Taxa de Incidência (por 100 mil hab.)")) %>%
      estilo_base()
  })
  
  # EVOLUÇÃO DE LETALIDADE
  output$plot_apres_evol_letalidade <- renderPlotly({
    df <- df_filtrado()
    req(df)
    df_mensal <- df %>%
      filter(!is.na(DT_NOTIFIC)) %>%
      mutate(Ano = as.integer(ANO_NOTIF), Mes = as.integer(month(DT_NOTIFIC)), Ano_Mes = sprintf("%04d-%02d", Ano, Mes)) %>%
      group_by(Ano_Mes) %>%
      summarise(Confirmados = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE), Obitos = sum(EVOLUCAO == "Óbito por dengue", na.rm = TRUE), .groups = "drop") %>%
      arrange(Ano_Mes) %>%
      mutate(Letalidade = ifelse(Confirmados > 0, round((Obitos / Confirmados) * 100, 2), 0))
    req(nrow(df_mensal) > 0)
    plot_ly(df_mensal, x = ~Ano_Mes, y = ~Letalidade, type = 'bar', marker = list(color = '#DC2626'),
            hovertemplate = '<b>%{x}</b><br>Letalidade: %{y}%<extra></extra>') %>%
      layout(xaxis = list(title = "Mês / Ano", tickangle = -45), yaxis = list(title = "Taxa de Letalidade (%)")) %>%
      estilo_base()
  })
  
  # SÉRIE ANUAL DE CASOS E ÓBITOS
  output$plot_apres_casos_ano <- renderPlotly({
    df <- df_filtrado()
    req(df)
    df_ano <- df %>%
      mutate(Ano = as.character(ANO_NOTIF)) %>%
      group_by(Ano) %>%
      summarise(
        Confirmados = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
        Descartados = sum(CLASSI_FIN == "Descartado", na.rm = TRUE),
        Notificados = n(),
        Provaveis = pmax(0, Notificados - Descartados - Confirmados),
        .groups = "drop"
      ) %>% arrange(as.numeric(Ano))
    req(nrow(df_ano) > 0)
    plot_ly(df_ano, x = ~Ano) %>%
      add_bars(y = ~Confirmados, name = "Confirmados", marker = list(color = '#DC2626')) %>%
      add_bars(y = ~Provaveis, name = "Prováveis", marker = list(color = '#F59E0B')) %>%
      add_bars(y = ~Descartados, name = "Descartados", marker = list(color = '#94A3B8')) %>%
      layout(barmode = "stack", xaxis = list(title = "Ano"), yaxis = list(title = "Total de Casos")) %>%
      estilo_base()
  })
  
  # ----------------------------------------------------------------------------
  # FUNÇÕES DE GERAÇÃO PPTX E PDF EXECUTIVOS (CONFORME GRÁFICOS SELECIONADOS)
  # ----------------------------------------------------------------------------
  # ============================================================================
  # 7.6 MOTOR EXECUTIVO UNIFICADO DE EXPORTAÇÃO (PPTX E PDF PADRONIZADOS)
  # ============================================================================
  
  # Função compartilhada para desenhar o Slide Inicial (Capa Executiva Corporativa)
  desenhar_capa_executiva <- function() {
    grid.newpage()
    grid.rect(gp = gpar(fill = "#0000A1", col = NA))
    grid.text("Atualização Epidemiológica de Dengue", y = 0.62, gp = gpar(col = "white", fontsize = 26, fontface = "bold"))
    grid.text("Belém - Pará", y = 0.50, gp = gpar(col = "#93C5FD", fontsize = 20, fontface = "bold"))
    grid.text(paste0("Secretaria Municipal de Saúde - Centro de Informações em Vigilância em Saúde - CIEVS/ Monitoramento de Epidemiológico da Dengue\nEmissão Oficial: ", format(Sys.Date(), "%d/%m/%Y")), y = 0.38, gp = gpar(col = "#E2E8F0", fontsize = 13))
  }

  # Função compartilhada para desenhar o layout corporativo padronizado do slide
  desenhar_slide_executivo <- function(titulo, subtitulo, plot_obj, insight) {
    grid.newpage()
    # 1. Barra Superior Corporativa Azul SESMA (sem badges ou descrições no topo)
    grid.rect(x = 0.5, y = 0.94, width = 1, height = 0.12, gp = gpar(fill = "#0000A1", col = NA))
    grid.text(titulo, x = 0.04, y = 0.96, just = "left", gp = gpar(col = "white", fontsize = 14, fontface = "bold"))
    grid.text(subtitulo, x = 0.04, y = 0.92, just = "left", gp = gpar(col = "#93C5FD", fontsize = 10))
    
    # 2. Área Central do Gráfico
    pushViewport(viewport(x = 0.5, y = 0.49, width = 0.92, height = 0.70))
    print(plot_obj, newpage = FALSE)
    upViewport()
    
    # 3. Caixa Inferior da Nota Técnica Executiva
    grid.rect(x = 0.5, y = 0.06, width = 0.92, height = 0.07, gp = gpar(fill = "#F1F5F9", col = "#CBD5E1"))
    grid.text(paste("Nota Técnica Executiva:", insight), x = 0.05, y = 0.06, just = "left", gp = gpar(col = "#0000A1", fontsize = 9, fontface = "italic"))
  }

  # Função geradora de conteúdo individual de cada slide
  construir_conteudo_slide <- function(gid, i, df, pop) {
    total_n <- nrow(df)
    desc <- sum(df$CLASSI_FIN == "Descartado", na.rm = TRUE)
    prov <- total_n - desc
    conf <- sum(df$CLASSI_FIN == "Confirmado", na.rm = TRUE)
    inconc <- sum(df$CLASSI_FIN == "Inconclusivo", na.rm = TRUE)
    ob <- sum(df$EVOLUCAO == "Óbito por dengue", na.rm = TRUE)
    incid <- round((total_n / pop) * 100000, 1)
    let <- ifelse(conf > 0, round((ob / conf) * 100, 2), 0)
    
    tipo_vis      <- if (!is.null(input$tipo_visualizacao_mapa)) input$tipo_visualizacao_mapa else "coropletico"
    metrica_ativa <- if (!is.null(input$metrica_mapa)) input$metrica_mapa else "incidencia"
    titulo_legenda <- case_when(
      metrica_ativa == "incidencia"  ~ "Incidência (por 100 mil hab.)",
      metrica_ativa == "confirmados" ~ "Casos Confirmados",
      metrica_ativa == "provaveis"   ~ "Casos Prováveis",
      TRUE                           ~ "Total Notificados"
    )
    milhas <- MAPA_ILHAS_SF
    
    NOMES_DISTRITOS_MAPA <- c(
      DAMOS = "Mosqueiro", DAOUT = "Outeiro", DAICO = "Icoaraci", DABEN = "Benguí",
      DAENT = "Entroncamento", DASAC = "Sacramenta", DABEL = "Belém", DAGUA = "Guamá"
    )

    titulo <- ""
    subtitulo <- ""
    p <- NULL
    insight <- ""

    if (gid == "g_cartoes") {
      titulo <- paste0(i, ". INDICADORES ESTRATÉGICOS & DADOS DE DESTAQUE")
      subtitulo <- "Consolidação de Notificações, Incidência e Mortalidade em Belém"
      insight <- "Quadro global para acompanhamento do panorama epidemiológico do município."
      
      df_kpis <- data.frame(
        Indicador = c("Total Notificados", "Casos Prováveis", "Confirmados", "Inconclusivos",
                      "Descartados", "Óbitos por Dengue", "Taxa Incidência (por 100 mil hab.)", "Letalidade Geral (%)"),
        Valor = c(format_num(total_n), format_num(prov), format_num(conf), format_num(inconc),
                  format_num(desc), format_num(ob), paste0(format_num(incid, 1), " por 100 mil hab."), format_pct(let, 2)),
        x = c(1, 2, 3, 4, 1, 2, 3, 4),
        y = c(2, 2, 2, 2, 1, 1, 1, 1),
        cor = c("#0000A1", "#D97706", "#DC2626", "#0284C7", "#64748B", "#7F1D1D", "#B91C1C", "#991B1B")
      )
      p <- ggplot(df_kpis, aes(x = x, y = y)) +
        geom_tile(fill = "#F8FAFC", color = "#CBD5E1", linewidth = 0.8, width = 0.92, height = 0.85) +
        geom_text(aes(label = Indicador), y = df_kpis$y + 0.18, fontface = "bold", size = 4.2, color = "#64748B") +
        geom_text(aes(label = Valor, color = cor), y = df_kpis$y - 0.10, fontface = "bold", size = 6.8, show.legend = FALSE) +
        scale_color_identity() +
        scale_x_continuous(limits = c(0.4, 4.6)) + scale_y_continuous(limits = c(0.4, 2.6)) +
        theme_void()

    } else if (gid == "g_semanas") {
      titulo <- paste0(i, ". CURVA EPIDÊMICA & INDICADORES GERAIS")
      subtitulo <- "Distribuição das Notificações por Semana Epidemiológica"
      insight <- "Monitoramento contínuo das semanas de maior circulação viral na Região Metropolitana."
      
      p <- df %>% filter(!is.na(SEMANA_EPID), SEMANA_EPID >= 1, SEMANA_EPID <= 53) %>%
        count(SEMANA_EPID, ANO_NOTIF) %>%
        ggplot(aes(x = SEMANA_EPID, y = n, color = factor(ANO_NOTIF))) +
        geom_line(linewidth = 1) + geom_point(size = 1.8) +
        scale_color_manual(values = PALETTE_ANOS) +
        labs(title = paste0("Distribuição por Semana Epidemiológica | Notificados: ", format_num(total_n), " | Incidência: ", incid, " por 100 mil hab."),
             x = "Semana Epidemiológica", y = "Total de Notificações", color = "Ano") +
        theme_minimal(base_size = 12) + theme(legend.position = "bottom")

    } else if (gid == "g_mapa_bairros") {
      titulo <- paste0(i, ". MAPA TERRITORIAL DA DENGUE — BAIRROS")
      subtitulo <- "Distribuição Espacial Conforme Configuração Ativa"
      insight <- "Mapeamento para direcionamento das equipes de controle vetorial do CCZ."
      
      mb <- dados_mapa_bairros()
      if (!is.null(mb) && nrow(mb) > 0) {
        p <- ggplot() +
          (if (!is.null(milhas) && nrow(milhas) > 0) geom_sf(data = milhas, fill = "#FFFFFF", color = "#CBD5E1", linewidth = 0.3) else NULL) +
          geom_sf(data = mb, aes(fill = .data[[metrica_ativa]]), color = "#94A3B8", linewidth = 0.3) +
          scale_fill_gradientn(colors = CORES_MAPA_CONFORTAVEL, name = titulo_legenda) +
          labs(title = paste0("Mapa Territorial por Bairros — ", titulo_legenda),
               subtitle = paste0("Configuração ativa: ", ifelse(tipo_vis == "calor", "Mancha Térmica/Calor", "Coroplético"), " | Métrica: ", metrica_ativa)) +
          theme_void(base_size = 11) + theme(legend.position = "right", plot.title = element_text(face = "bold", color = "#0000A1"))
      }

    } else if (gid == "g_mapa_distritos") {
      titulo <- paste0(i, ". MAPA TERRITORIAL DA DENGUE — DISTRITOS")
      subtitulo <- "Distribuição Espacial por Distrito Administrativo com Identificação Enquadrada"
      insight <- "Consolidação por distrito para alocação de recursos regionais da SESMA."
      
      md <- dados_mapa_distritos()
      if (!is.null(md) && nrow(md) > 0) {
        md$nome_curto <- NOMES_DISTRITOS_MAPA[md$sigla]
        md$rotulo_mapa <- paste0(md$sigla, "
", md$nome_curto)
        
        p <- ggplot() +
          (if (!is.null(milhas) && nrow(milhas) > 0) geom_sf(data = milhas, fill = "#FFFFFF", color = "#CBD5E1", linewidth = 0.3) else NULL) +
          geom_sf(data = md, aes(fill = .data[[metrica_ativa]]), color = "#0000A1", linewidth = 0.6) +
          geom_label(data = md, aes(x = centroid_lon, y = centroid_lat, label = rotulo_mapa), size = 2.5, fontface = "bold",
                        fill = "#FFFFFF", color = "#0000A1", alpha = 0.92, linewidth = 0.25,
                        label.padding = unit(0.14, "lines")) +
          scale_fill_gradientn(colors = CORES_MAPA_CONFORTAVEL, name = titulo_legenda) +
          labs(title = paste0("Mapa Territorial por Distrito — ", titulo_legenda),
               subtitle = paste0("Configuração ativa: ", ifelse(tipo_vis == "calor", "Mancha Térmica/Calor", "Coroplético"), " | Métrica: ", metrica_ativa)) +
          theme_void(base_size = 11) + theme(legend.position = "right", plot.title = element_text(face = "bold", color = "#0000A1"))
      }

    } else if (gid == "g_bairros") {
      titulo <- paste0(i, ". VIGILÂNCIA TERRITORIAL & BAIRROS CRÍTICOS")
      subtitulo <- "Concentração de Casos Notificados por Bairro Urbano"
      insight <- "Identificação das microáreas prioritárias para ações de bloqueio e nebulização."
      
      df_b <- df %>% filter(!is.na(NM_BAIRRO), NM_BAIRRO != "") %>%
        count(NM_BAIRRO, sort = TRUE) %>% head(12)
      p <- ggplot(df_b, aes(x = n, y = reorder(NM_BAIRRO, n))) +
        geom_col(fill = "#0000A1", width = 0.7) +
        geom_text(aes(label = n), hjust = -0.2, fontface = "bold", size = 3.8, color = "#0000A1") +
        labs(title = "Top 12 Bairros com Maior Volume de Notificações", x = "Total de Notificações", y = "Bairro") +
        theme_minimal(base_size = 12) + theme(panel.grid.major.y = element_blank())

    } else if (gid == "g_distritos") {
      titulo <- paste0(i, ". SÍNTESE POR DISTRITO ADMINISTRATIVO")
      subtitulo <- "Consolidação Distrital de Belém (Casos e Incidência)"
      insight <- "Visão agregada por macroregião para planejamento das Unidades Básicas de Saúde."
      
      md <- dados_mapa_distritos()
      if (!is.null(md)) {
        md$nome_curto <- NOMES_DISTRITOS_MAPA[md$sigla]
        df_dist <- md %>% st_drop_geometry() %>% head(8) %>%
          mutate(rotulo_eixo = paste0(sigla, " (", nome_curto, ")"))
        p <- ggplot(df_dist, aes(x = reorder(rotulo_eixo, total_casos), y = total_casos)) +
          geom_col(fill = "#0284C7", width = 0.7) + coord_flip() +
          geom_text(aes(label = paste0(total_casos, " (", round(incidencia, 1), " por 100 mil hab.)")), hjust = -0.1, fontface = "bold", size = 3.8, color = "#0284C7") +
          labs(title = "Distribuição por Distrito Administrativo (Total de Casos e Taxa por 100 mil hab.)", x = "Distrito", y = "Casos") +
          theme_minimal(base_size = 12)
      }

    } else if (gid == "g_piramide") {
      titulo <- paste0(i, ". PERFIL DEMOGRÁFICO & FAIXAS ETÁRIAS")
      subtitulo <- "Distribuição Cruzada de Sexo e Faixa Etária"
      insight <- "Análise da vulnerabilidade demográfica para direcionar campanhas educativas."
      
      df_pir <- df %>% filter(!is.na(IDADE_ANOS), IDADE_ANOS >= 0, IDADE_ANOS <= 110, CS_SEXO %in% c("Feminino", "Masculino")) %>%
        mutate(Faixa = cut(IDADE_ANOS, breaks = c(seq(0, 80, 10), 120), right = FALSE, labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"))) %>%
        count(Faixa, CS_SEXO) %>% mutate(Qtd = ifelse(CS_SEXO == "Masculino", -n, n))
      p <- ggplot(df_pir, aes(x = Faixa, y = Qtd, fill = CS_SEXO)) +
        geom_col(width = 0.8) + coord_flip() +
        scale_fill_manual(values = c("Masculino" = "#0000A1", "Feminino" = "#EC4899")) +
        scale_y_continuous(labels = function(x) abs(x)) +
        labs(title = "Pirâmide Etária dos Casos Notificados", x = "Faixa Etária", y = "Número de Notificações", fill = "Sexo") +
        theme_minimal(base_size = 12) + theme(legend.position = "bottom")

    } else if (gid == "g_sexo_raca") {
      titulo <- paste0(i, ". PERFIL POR RAÇA/COR DECLARADA")
      subtitulo <- "Autodeclaração Étnico-Racial dos Casos"
      insight <- "Indicador de equidade e cobertura do preenchimento das fichas do SINAN."
      
      df_r <- df %>% filter(!is.na(CS_RACA), CS_RACA != "") %>% count(CS_RACA, sort = TRUE)
      p <- ggplot(df_r, aes(x = reorder(CS_RACA, n), y = n)) +
        geom_col(fill = "#3B82F6", width = 0.6) + coord_flip() +
        geom_text(aes(label = n), hjust = -0.15, fontface = "bold", size = 4) +
        labs(title = "Distribuição dos Casos Notificados por Raça/Cor", x = "Raça / Cor", y = "Casos") +
        theme_minimal(base_size = 12)

    } else if (gid == "g_classificacao") {
      titulo <- paste0(i, ". CLASSIFICAÇÃO CLÍNICA DOS CASOS")
      subtitulo <- "Proporção de Casos Confirmados e Descartados"
      insight <- "Avaliação da acurácia e capacidade diagnóstica da rede de vigilância municipal."
      
      df_cl <- df %>% filter(!is.na(CLASSI_FIN)) %>% count(CLASSI_FIN)
      p <- ggplot(df_cl, aes(x = "", y = n, fill = CLASSI_FIN)) +
        geom_col(width = 1, color = "white") + coord_polar("y", start = 0) +
        scale_fill_manual(values = PALETTE_PIE) +
        labs(title = "Classificação Final dos Casos Notificados", fill = "Classificação") +
        theme_void(base_size = 12) + theme(legend.position = "right")

    } else if (gid == "g_criterio") {
      titulo <- paste0(i, ". CRITÉRIO DE CONFIRMAÇÃO DIAGNÓSTICA")
      subtitulo <- "Proporção de Casos Laboratoriais vs. Clínico-Epidemiológicos"
      insight <- "Reflete a cobertura da rede de laboratórios públicos (LACEN/PA) na confirmação etiológica."
      
      col_crit <- if ("CRITERIO_FINAL" %in% names(df)) "CRITERIO_FINAL" else if ("CRITERIO_DIAGNOSTICO" %in% names(df)) "CRITERIO_DIAGNOSTICO" else if ("CRITERIO" %in% names(df)) "CRITERIO" else names(df)[1]
      df_crit <- df %>% filter(!is.na(.data[[col_crit]]), .data[[col_crit]] != "") %>% count(Criterio = .data[[col_crit]], sort = TRUE)
      if (nrow(df_crit) > 0) {
        p <- ggplot(df_crit, aes(x = reorder(Criterio, n), y = n)) +
          geom_col(fill = "#0EA5E9", width = 0.6) + coord_flip() +
          geom_text(aes(label = paste0(n, " (", round(n/sum(n)*100, 1), "%)")), hjust = -0.1, fontface = "bold", size = 4) +
          labs(title = "Critério de Confirmação dos Casos", x = "Critério", y = "Notificações") +
          theme_minimal(base_size = 12)
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Sem registros de critério diagnóstico para o período filtrado.", size = 5, color = "#64748B", fontface = "italic") +
          theme_void()
      }

    } else if (gid == "g_sorotipo") {
      titulo <- paste0(i, ". DISTRIBUIÇÃO POR SOROTIPO VIRAL")
      subtitulo <- "Identificação dos Sorotipos Circulantes (DENV 1-4)"
      insight <- "Vigilância virológica para monitoramento da introdução de novos sorotipos hiperendêmicos."
      
      col_soro <- if ("tp_sorotipo" %in% names(df)) "tp_sorotipo" else if ("TP_SOROTIPO" %in% names(df)) "TP_SOROTIPO" else if ("RESUL_SORO" %in% names(df)) "RESUL_SORO" else if ("SOROTIPO" %in% names(df)) "SOROTIPO" else names(df)[1]
      df_soro <- df %>% filter(!is.na(.data[[col_soro]]), .data[[col_soro]] != "", .data[[col_soro]] != "NÃO INFORMADO") %>% count(Sorotipo = .data[[col_soro]], sort = TRUE)
      if (nrow(df_soro) > 0) {
        p <- ggplot(df_soro, aes(x = Sorotipo, y = n, fill = Sorotipo)) +
          geom_col(width = 0.6, show.legend = FALSE) +
          scale_fill_manual(values = PALETTE_PIE) +
          geom_text(aes(label = n), vjust = -0.3, fontface = "bold", size = 4) +
          labs(title = "Sorotipos Virais Identificados em Belém", x = "Sorotipo Viral", y = "Isolamentos / Notificações") +
          theme_minimal(base_size = 12)
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Sem registros de isolamento viral / sorotipo para o período filtrado.", size = 5, color = "#64748B", fontface = "italic") +
          theme_void()
      }

    } else if (gid == "g_evolucao") {
      titulo <- paste0(i, ". EVOLUÇÃO CLÍNICA & DESFECHOS")
      subtitulo <- "Monitoramento de Óbitos e Cura"
      insight <- "Indicador sentinela para mensurar a precocidade do manejo clínico e hidratação venosa."
      
      df_ev <- df %>% filter(!is.na(EVOLUCAO), EVOLUCAO != "") %>% count(EVOLUCAO)
      p <- ggplot(df_ev, aes(x = "", y = n, fill = EVOLUCAO)) +
        geom_col(width = 1, color = "white") + coord_polar("y", start = 0) +
        scale_fill_manual(values = c("Cura" = "#10B981", "Óbito por dengue" = "#DC2626", "Óbito por outras causas" = "#F59E0B", "Em investigação" = "#64748B")) +
        labs(title = "Desfecho Clínico e Evolução dos Pacientes", fill = "Evolução") +
        theme_void(base_size = 12) + theme(legend.position = "right")

    } else if (gid == "g_unidades") {
      titulo <- paste0(i, ". REDE NOTIFICADORA DE SAÚDE")
      subtitulo <- "Desempenho dos Principais Estabelecimentos Notificadores"
      insight <- "Monitoramento dos pontos focais da rede hospitalar e pronto-atendimentos da capital."
      
      col_unid <- if ("UNIDADE_NOTIF" %in% names(df)) "UNIDADE_NOTIF" else if ("ID_UNIDADE" %in% names(df)) "ID_UNIDADE" else names(df)[1]
      df_u <- df %>%
        filter(!is.na(.data[[col_unid]]), .data[[col_unid]] != "", .data[[col_unid]] != "NÃO INFORMADO") %>%
        count(Nome = .data[[col_unid]], sort = TRUE) %>%
        head(10)
      if (nrow(df_u) > 0) {
        p <- ggplot(df_u, aes(x = n, y = reorder(Nome, n))) +
          geom_col(fill = "#059669", width = 0.7) + coord_flip() +
          geom_text(aes(label = n), hjust = -0.15, fontface = "bold", size = 3.6, color = "#059669") +
          labs(title = "Top 10 Estabelecimentos com Mais Notificações", x = "Total de Notificações", y = "Estabelecimento") +
          theme_minimal(base_size = 11) + theme(panel.grid.major.y = element_blank())
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Sem notificações com estabelecimento identificado para os filtros ativos.", size = 5, color = "#64748B", fontface = "italic") +
          theme_void()
      }

    } else if (gid == "g_canal") {
      titulo <- paste0(i, ". CANAL ENDÊMICO & DIAGRAMA DE CONTROLE (ANO VIGENTE 2026)")
      subtitulo <- "Faixas Endêmicas Municipais com Barras em Azul para o Ano Vigente"
      insight <- "Curva de alarme epidemiológico baseada na série não epidêmica do SINAN."
      
      df_base_tudo <- dados_base()
      anos_base <- c(2015, 2016, 2017, 2018, 2019, 2022, 2023)
      grid_b <- expand.grid(ANO_NOTIF = anos_base, SEMANA_EPID = 1:53)
      cont_b <- df_base_tudo %>%
        filter(as.numeric(ANO_NOTIF) %in% anos_base) %>%
        group_by(ANO_NOTIF = as.numeric(ANO_NOTIF), SEMANA_EPID = as.numeric(SEMANA_EPID)) %>%
        summarise(Casos = n(), .groups = "drop")
      diag_st <- grid_b %>%
        left_join(cont_b, by = c("ANO_NOTIF", "SEMANA_EPID")) %>%
        mutate(Casos = ifelse(is.na(Casos), 0, Casos)) %>%
        group_by(SEMANA_EPID) %>%
        summarise(Media = mean(Casos), Limite_Sup = Media + 1.96 * sd(Casos), .groups = "drop")
      obs_26 <- df_base_tudo %>%
        filter(as.numeric(ANO_NOTIF) == 2026) %>%
        group_by(SEMANA_EPID = as.numeric(SEMANA_EPID)) %>%
        summarise(Casos_2026 = n(), .groups = "drop")
      df_canal_plot <- diag_st %>% left_join(obs_26, by = "SEMANA_EPID") %>% mutate(Casos_2026 = ifelse(is.na(Casos_2026), 0, Casos_2026))
      
      p <- ggplot(df_canal_plot, aes(x = SEMANA_EPID)) +
        geom_ribbon(aes(ymin = 0, ymax = Limite_Sup, fill = "Faixa Endêmica"), alpha = 0.25) +
        geom_line(aes(y = Media, color = "Média Histórica"), linewidth = 1, linetype = "dotted") +
        geom_line(aes(y = Limite_Sup, color = "Limite Superior Epidêmico"), linewidth = 1.1) +
        geom_col(aes(y = Casos_2026, fill = "Observado 2026 (Ano Vigente)"), alpha = 0.90, width = 0.7) +
        scale_fill_manual(values = c("Faixa Endêmica" = "#BFDBFE", "Observado 2026 (Ano Vigente)" = "#0000A1")) +
        scale_color_manual(values = c("Média Histórica" = "#0000A1", "Limite Superior Epidêmico" = "#DC2626")) +
        labs(title = "Canal Endêmico da Dengue em Belém (Ano Vigente: 2026)", x = "Semana Epidemiológica", y = "Casos Esperados / Notificados", fill = "", color = "") +
        theme_minimal(base_size = 12) + theme(legend.position = "bottom")

    } else if (gid == "g_evol_incidencia") {
      titulo <- paste0(i, ". EVOLUÇÃO HISTÓRICA DA INCIDÊNCIA")
      subtitulo <- "Taxa Mensal de Incidência por 100 mil Habitantes"
      insight <- "Análise do comportamento sazonal de transmissão e picos de demanda."
      
      df_mensal <- df %>% filter(!is.na(DT_NOTIFIC)) %>%
        mutate(Ano = as.integer(ANO_NOTIF), Mes = as.integer(month(DT_NOTIFIC)), Ano_Mes = sprintf("%04d-%02d", Ano, Mes)) %>%
        group_by(Ano_Mes) %>% summarise(Casos = n(), .groups = "drop") %>% arrange(Ano_Mes) %>%
        mutate(Incidencia = round((Casos / pop) * 100000, 1))
      p <- ggplot(df_mensal, aes(x = Ano_Mes, y = Incidencia)) +
        geom_col(fill = "#0000A1", width = 0.7) +
        labs(title = "Evolução da Taxa de Incidência (por 100 mil hab.)", x = "Mês / Ano", y = "Incidência (por 100 mil hab.)") +
        theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

    } else if (gid == "g_evol_letalidade") {
      titulo <- paste0(i, ". EVOLUÇÃO HISTÓRICA DA LETALIDADE")
      subtitulo <- "Monitoramento de Óbitos Evitáveis"
      insight <- "Avaliação da adesão aos protocolos de hidratação e identificação de sinais de alarme."
      
      df_mensal <- df %>% filter(!is.na(DT_NOTIFIC)) %>%
        mutate(Ano = as.integer(ANO_NOTIF), Mes = as.integer(month(DT_NOTIFIC)), Ano_Mes = sprintf("%04d-%02d", Ano, Mes)) %>%
        group_by(Ano_Mes) %>%
        summarise(Confirmados = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE), Obitos = sum(EVOLUCAO == "Óbito por dengue", na.rm = TRUE), .groups = "drop") %>%
        arrange(Ano_Mes) %>% mutate(Letalidade = ifelse(Confirmados > 0, round((Obitos / Confirmados) * 100, 2), 0))
      p <- ggplot(df_mensal, aes(x = Ano_Mes, y = Letalidade)) +
        geom_col(fill = "#DC2626", width = 0.7) +
        labs(title = "Evolução Histórica da Taxa de Letalidade (%)", x = "Mês / Ano", y = "Letalidade (%)") +
        theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

    } else if (gid == "g_casos_obitos_ano") {
      titulo <- paste0(i, ". SÉRIE ANUAL DE CASOS")
      subtitulo <- "Comparativo Anual Consolidado"
      insight <- "Visão global dos ciclos epidêmicos da dengue na capital paraense."
      
      df_ano <- df %>% mutate(Ano = as.character(ANO_NOTIF)) %>%
        group_by(Ano) %>%
        summarise(Confirmados = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
                  Descartados = sum(CLASSI_FIN == "Descartado", na.rm = TRUE),
                  Notificados = n(),
                  Provaveis = pmax(0, Notificados - Descartados - Confirmados),
                  .groups = "drop") %>%
        tidyr::pivot_longer(cols = c("Confirmados", "Provaveis", "Descartados"), names_to = "Categoria", values_to = "Qtd")
      p <- ggplot(df_ano, aes(x = Ano, y = Qtd, fill = Categoria)) +
        geom_col() +
        scale_fill_manual(values = c("Confirmados" = "#DC2626", "Provaveis" = "#F59E0B", "Descartados" = "#94A3B8")) +
        labs(title = "Série Histórica Anual de Notificados, Prováveis e Confirmados", x = "Ano", y = "Total de Casos", fill = "") +
        theme_minimal(base_size = 12) + theme(legend.position = "bottom")
    }

    return(list(titulo = titulo, subtitulo = subtitulo, p = p, insight = insight))
  }

  # 7.7 Geração da Apresentação em PPTX (Padronizada com o Layout do PDF)
  gerar_apresentacao_pptx <- function(df, pop, caminho_arquivo, graficos_sel = c("g_cartoes")) {
    doc <- read_pptx()
    sz <- slide_size(doc)
    tmps <- c()
    on.exit({
      for (f in tmps) { if (file.exists(f)) unlink(f) }
    }, add = TRUE)
    
    # Slide 1: Capa Executiva Corporativa (idêntica ao PDF)
    tryCatch({
      t_capa <- tempfile(fileext = ".png")
      tmps <- c(tmps, t_capa)
      png(t_capa, width = round(sz$width * 150), height = round(sz$height * 150), res = 150)
      desenhar_capa_executiva()
      dev.off()
      
      doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
      doc <- ph_with(doc, external_img(t_capa, width = sz$width, height = sz$height),
                     location = ph_location(left = 0, top = 0, width = sz$width, height = sz$height))
    }, error = function(e) {
      message("Erro gerando capa PPTX: ", e$message)
    })
    
    # Slides de Conteúdo com Layout Idêntico ao PDF
    for (i in seq_along(graficos_sel)) {
      gid <- graficos_sel[i]
      tryCatch({
        conteudo <- construir_conteudo_slide(gid, i, df, pop)
        if (!is.null(conteudo$p)) {
          t_img <- tempfile(fileext = ".png")
          tmps <- c(tmps, t_img)
          png(t_img, width = round(sz$width * 150), height = round(sz$height * 150), res = 150)
          desenhar_slide_executivo(conteudo$titulo, conteudo$subtitulo, conteudo$p, conteudo$insight)
          dev.off()
          
          doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
          doc <- ph_with(doc, external_img(t_img, width = sz$width, height = sz$height),
                         location = ph_location(left = 0, top = 0, width = sz$width, height = sz$height))
        }
      }, error = function(e) {
        message("Erro gerando slide PPTX [", gid, "]: ", e$message)
      })
    }
    
    print(doc, target = caminho_arquivo)
  }
  
  # 7.8 Geração da Apresentação em PDF
  gerar_apresentacao_pdf <- function(df, pop, caminho_arquivo, graficos_sel = c("g_cartoes")) {
    pdf(caminho_arquivo, width = 11.69, height = 8.27, paper = "a4r")
    on.exit(dev.off(), add = TRUE)
    
    # Slide 1: Capa Executiva Corporativa
    tryCatch({
      desenhar_capa_executiva()
    }, error = function(e) {
      message("Erro gerando capa PDF: ", e$message)
    })
    
    # Slides de Conteúdo
    for (i in seq_along(graficos_sel)) {
      gid <- graficos_sel[i]
      tryCatch({
        conteudo <- construir_conteudo_slide(gid, i, df, pop)
        if (!is.null(conteudo$p)) {
          desenhar_slide_executivo(conteudo$titulo, conteudo$subtitulo, conteudo$p, conteudo$insight)
        }
      }, error = function(e) {
        message("Erro gerando slide PDF [", gid, "]: ", e$message)
      })
    }
  }
  
  output$dl_apresentacao_pptx <- downloadHandler(
    filename = function() {
      paste0("apresentacao_executiva_dengue_belem_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      df <- df_filtrado()
      if (is.null(df) || nrow(df) == 0) df <- dados_base()
      req(df)
      pop <- if (!is.null(input$populacao) && !is.na(input$populacao) && input$populacao > 0) as.numeric(input$populacao) else 1303403
      sel <- graficos_selecionados()
      if (is.null(sel) || length(sel) == 0) sel <- c("g_cartoes", "g_semanas")
      gerar_apresentacao_pptx(df, pop, file, graficos_sel = sel)
    },
    contentType = "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  )
  
  output$dl_apresentacao_pdf <- downloadHandler(
    filename = function() {
      paste0("apresentacao_executiva_dengue_belem_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      df <- df_filtrado()
      if (is.null(df) || nrow(df) == 0) df <- dados_base()
      req(df)
      pop <- if (!is.null(input$populacao) && !is.na(input$populacao) && input$populacao > 0) as.numeric(input$populacao) else 1303403
      sel <- graficos_selecionados()
      if (is.null(sel) || length(sel) == 0) sel <- c("g_cartoes", "g_semanas")
      gerar_apresentacao_pdf(df, pop, file, graficos_sel = sel)
    },
    contentType = "application/pdf"
  )
}

# ==============================================================================
# 5. EXECUÇÃO DO SHINY APP
# ==============================================================================
shinyApp(ui = ui, server = server)
