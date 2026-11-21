# ==============================================================================
# EXPORTADOR DE DADOS EPIDEMIOLÓGICOS PARA PORTAL WEB (GITHUB PAGES)
# Painel Epidemiológico da Dengue - Belém / SESMA / CIEVS
# ==============================================================================
# Este script extrai os dados agregados da base oficial ou sintética
# e gera o arquivo 'dados_dengue_publico.json' 100% livre de dados pessoais (LGPD).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(jsonlite)
})

cat("Gerando dados agregados para GitHub Pages (dados_dengue_publico.json)...\n")

caminho_base <- if (file.exists("dados_historico_dengue.rds")) "dados_historico_dengue.rds" else "dados_historico_dengue_exemplo.rds"
df <- readRDS(caminho_base)

pop_belem <- 1303403
anos_disp <- sort(unique(na.omit(as.numeric(df$ANO_NOTIF))), decreasing = TRUE)
anos_base_canal <- c(2015, 2016, 2017, 2018, 2019, 2022, 2023)

d26 <- df %>% filter(ANO_NOTIF == 2026)
max_se_2026 <- if (nrow(d26) > 0) max(d26$SEMANA_EPID[d26$SEMANA_EPID <= 52], na.rm = TRUE) else 44

# 1. Metadados Gerais
meta <- list(
  municipio           = "Belém - PA",
  secretaria          = "Secretaria Municipal de Saúde (SESMA)",
  setor               = "Centro de Informações Estratégicas em Vigilância em Saúde (CIEVS)",
  data_atualizacao    = format(Sys.time(), "%d/%m/%Y %H:%M"),
  ano_vigente         = 2026,
  max_semana_vigente  = max_se_2026,
  total_notificacoes  = nrow(df),
  populacao_referencia = pop_belem,
  serie_historica     = paste0(min(anos_disp), "–", max(anos_disp))
)

# 2. KPIs por Ano
calc_kpis <- function(sub_df) {
  if (nrow(sub_df) == 0) return(list())
  notif   <- nrow(sub_df)
  conf    <- sum(sub_df$CLASSI_FIN == "Confirmado", na.rm = TRUE)
  desc    <- sum(sub_df$CLASSI_FIN == "Descartado", na.rm = TRUE)
  inv     <- sum(sub_df$CLASSI_FIN == "Em Investigação/Suspeito", na.rm = TRUE)
  inconc  <- sum(sub_df$CLASSI_FIN == "Inconclusivo", na.rm = TRUE)
  prov    <- pmax(0, notif - desc)
  obit    <- sum(sub_df$EVOLUCAO == "Óbito por dengue", na.rm = TRUE)
  incid   <- round((conf / pop_belem) * 100000, 1)
  letal   <- if (conf > 0) round((obit / conf) * 100, 2) else 0
  
  list(
    notificados    = notif,
    confirmados    = conf,
    descartados    = desc,
    investigacao   = inv,
    inconclusivos  = inconc,
    provaveis      = prov,
    obitos         = obit,
    incidencia     = incid,
    letalidade     = letal
  )
}

kpis_por_ano <- list()
for (a in as.character(anos_disp)) {
  kpis_por_ano[[a]] <- calc_kpis(df %>% filter(ANO_NOTIF == as.numeric(a)))
}
kpis_por_ano[["TODOS"]] <- calc_kpis(df)

# 3. Série Semanal por Ano (Semanas 1 a 53)
serie_semanal <- list()
for (a in as.character(anos_disp)) {
  df_a <- df %>% filter(ANO_NOTIF == as.numeric(a))
  max_s <- if (a == "2026") max_se_2026 else 53
  
  sems_df <- df_a %>%
    filter(SEMANA_EPID <= max_s) %>%
    group_by(semana = as.numeric(SEMANA_EPID)) %>%
    summarise(
      notificados  = n(),
      confirmados  = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
      descartados  = sum(CLASSI_FIN == "Descartado", na.rm = TRUE),
      investigacao = sum(CLASSI_FIN == "Em Investigação/Suspeito", na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    arrange(semana)
  
  serie_semanal[[a]] <- sems_df
}

# 4. Diagrama de Controle (Canal Endêmico Municipal)
grid_base <- expand.grid(ANO_NOTIF = anos_base_canal, SEMANA_EPID = 1:52)
cont_base <- df %>%
  filter(ANO_NOTIF %in% anos_base_canal, SEMANA_EPID <= 52) %>%
  group_by(ANO_NOTIF, SEMANA_EPID) %>%
  summarise(Casos = n(), .groups = "drop")

diag_stat <- grid_base %>%
  left_join(cont_base, by = c("ANO_NOTIF", "SEMANA_EPID")) %>%
  mutate(Casos = ifelse(is.na(Casos), 0, Casos)) %>%
  group_by(semana = SEMANA_EPID) %>%
  summarise(
    media       = round(mean(Casos), 1),
    desvio      = round(sd(Casos), 1),
    limite_sup  = round(media + 1.96 * desvio, 1),
    limite_inf  = round(pmax(0, media - 1.96 * desvio), 1),
    .groups     = "drop"
  )

obs_26 <- d26 %>%
  filter(SEMANA_EPID <= 52) %>%
  group_by(semana = SEMANA_EPID) %>%
  summarise(observado_2026 = n(), .groups = "drop")

canal_endemico <- diag_stat %>%
  left_join(obs_26, by = "semana") %>%
  mutate(
    observado_2026 = ifelse(semana <= max_se_2026, ifelse(is.na(observado_2026), 0, observado_2026), NA)
  )

# 5. Distribuição por Bairros de Belém
bairros_df <- df %>%
  filter(NM_BAIRRO_MAPA != "NÃO INFORMADO") %>%
  group_by(bairro = NM_BAIRRO_MAPA) %>%
  summarise(
    total       = n(),
    total_2026  = sum(ANO_NOTIF == 2026, na.rm = TRUE),
    confirmados = sum(CLASSI_FIN == "Confirmado", na.rm = TRUE),
    descartados = sum(CLASSI_FIN == "Descartado", na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(desc(total_2026), desc(total))

# 6. Pirâmide Etária por Ano
faixas_rotulos <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

calc_piramide <- function(sub_df) {
  sub_df <- sub_df %>%
    filter(!is.na(IDADE_ANOS), IDADE_ANOS >= 0, IDADE_ANOS <= 110, CS_SEXO %in% c("Feminino", "Masculino")) %>%
    mutate(faixa = cut(IDADE_ANOS, breaks = c(seq(0, 80, 10), 120), right = FALSE, labels = faixas_rotulos))
  
  pir <- sub_df %>%
    group_by(faixa, CS_SEXO) %>%
    summarise(casos = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = CS_SEXO, values_from = casos, values_fill = 0)
  
  if (!"Masculino" %in% names(pir)) pir$Masculino <- 0
  if (!"Feminino" %in% names(pir)) pir$Feminino <- 0
  
  pir %>% arrange(match(faixa, faixas_rotulos))
}

piramide_por_ano <- list()
for (a in as.character(c(2026, 2025, 2024))) {
  if (a %in% as.character(anos_disp)) {
    piramide_por_ano[[a]] <- calc_piramide(df %>% filter(ANO_NOTIF == as.numeric(a)))
  }
}
piramide_por_ano[["TODOS"]] <- calc_piramide(df)

# 7. Raça / Cor (2026 e Geral)
raca_df <- df %>%
  group_by(raca = CS_RACA) %>%
  summarise(
    total = n(),
    total_2026 = sum(ANO_NOTIF == 2026, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_2026))

# 8. Sorotipo e Critério
sorotipo_df <- df %>%
  filter(!tp_sorotipo %in% c("Não Realizado/Sem Info", NA)) %>%
  group_by(sorotipo = tp_sorotipo) %>%
  summarise(
    total = n(),
    total_2026 = sum(ANO_NOTIF == 2026, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total))

criterio_df <- df %>%
  group_by(criterio = CRITERIO_FINAL) %>%
  summarise(
    total = n(),
    total_2026 = sum(ANO_NOTIF == 2026, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total))

# 9. Ranking de Unidades Notificadoras (Top 20)
ranking_df <- df %>%
  filter(!is.na(UNIDADE_NOTIF), UNIDADE_NOTIF != "", UNIDADE_NOTIF != "NÃO INFORMADO") %>%
  group_by(unidade = UNIDADE_NOTIF) %>%
  summarise(
    total = n(),
    total_2026 = sum(ANO_NOTIF == 2026, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_2026), desc(total)) %>%
  head(25)

# Consolidação final do objeto JSON
dados_web <- list(
  meta             = meta,
  anos_disponiveis = anos_disp,
  kpis             = kpis_por_ano,
  serie_semanal    = serie_semanal,
  canal_endemico   = canal_endemico,
  bairros          = bairros_df,
  piramide         = piramide_por_ano,
  raca_cor         = raca_df,
  sorotipo         = sorotipo_df,
  criterio         = criterio_df,
  ranking_unidades = ranking_df
)

caminho_saida <- "dados_dengue_publico.json"
write_json(dados_web, caminho_saida, pretty = TRUE, auto_unbox = TRUE)
cat(paste0("Arquivo '", caminho_saida, "' gerado com sucesso! (", round(file.info(caminho_saida)$size / 1024, 1), " KB)\n"))
