# Painel Epidemiológico da Dengue — Belém / SESMA / CIEVS

[![R Version](https://img.shields.io/badge/R-%3E%3D%204.2.0-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Interactive%20Dashboard-blue.svg)](https://shiny.posit.co/)
[![BSlib](https://img.shields.io/badge/BSlib-Modern%20Theme-007bc2.svg)](https://rstudio.github.io/bslib/)
[![LGPD](https://img.shields.io/badge/LGPD-Compliant-success.svg)](https://www.planalto.gov.br/ccivil_03/_ato2015-2018/2018/lei/l13709.htm)
[![Licença](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

Dashboard analítico e epidemiológico de alta performance desenvolvido em **R Shiny** para a **Secretaria Municipal de Saúde de Belém (SESMA)** e o **Centro de Informações Estratégicas em Vigilância em Saúde (CIEVS)**. O sistema consolida a vigilância da Dengue no município de Belém (PA), abrangendo a série histórica municipal (2015–2025) e o ano de monitoramento ativo (**2026**).

---

## 🔒 Conformidade com a LGPD e Proteção de Dados em Saúde

> [!IMPORTANT]
> **Aviso de Privacidade e Sigilo em Saúde Pública:**
> Em estrita conformidade com a **Lei Geral de Proteção de Dados Pessoais (Lei Federal nº 13.709/2018 - LGPD)**, especialmente no que tange a **dados pessoais sensíveis de saúde (Art. 5º, inciso II e Art. 11)**, bem como com as normas éticas de sigilo médico do **Sistema de Informação de Agravos de Notificação (SINAN / Ministério da Saúde)**:
> 
> * **Nenhum dado pessoal nominal, endereço residencial ou microdado confidencial de pacientes é disponibilizado neste repositório.**
> * Todos os arquivos brutos do SINAN (`.dbf` e `.dbc`) e o banco consolidado restrito (`dados_historico_dengue.rds`) estão permanentemente bloqueados pelo `.gitignore` e **nunca** serão enviados ao GitHub.
> * Para fins de teste, desenvolvimento e auditoria de código, este repositório disponibiliza um conjunto de **dados sintéticos e 100% fictícios** (`dados_historico_dengue_exemplo.rds`), gerados pelo script [gerar_dados_exemplo.R](gerar_dados_exemplo.R).
> * O código da aplicação detecta automaticamente o ambiente: caso a base oficial restrita não esteja presente na máquina do usuário, o sistema entra em **Modo Demonstração (LGPD)**, preservando integralmente o funcionamento da interface sem comprometer a privacidade de terceiros.

---

## 🎯 Funcionalidades Principais

O painel é estruturado em 7 eixos estratégicos de análise:

1. **Visão Geral & Indicadores-Chave (KPIs):**
   * Total de notificações, casos prováveis, casos confirmados, descartados, inconclusivos e óbitos.
   * Cálculo em tempo real da **Taxa de Incidência** (por 100 mil habitantes) e da **Taxa de Letalidade Geral**.
   * Curva epidemiológica semanal interativa com alternância entre ano individual e série multi-anos.

2. **Análise Espacial & Georreferenciamento:**
   * Mapas coropléticos e interativos de Belém via **Leaflet** com polígonos oficiais por **Bairros**, **Distritos Administrativos** (DAGUA, DASAC, DABEL, DAMOS, DAENT, DAICO, DAOUT, DASC) e **Ilhas de Belém**.
   * Tabelas de rolagem com densidade de incidência e classificação epidemiológica territorial.

3. **Sociodemográfico:**
   * **Pirâmide Etária** estratificada por sexo e faixas decenais.
   * Distribuição por Raça/Cor segundo o padrão IBGE/SINAN.

4. **Diagnóstico & Desfechos:**
   * Proporção de confirmação por Critério Laboratorial vs. Clínico-Epidemiológico.
   * Monitoramento de sorotipos virais circulantes (**DENV-1**, **DENV-2**, etc.).
   * Desfechos clínicos (Cura, Óbito por dengue, Óbito por outras causas).

5. **Rede Notificadora (CNES):**
   * Ranking completo das Unidades Notificadoras (Hospitais de Referência, UPAs, Unidades Básicas de Saúde).
   * Exportação do ranking institucional detalhado diretamente para planilha Excel (`.xlsx`).

6. **Análise Epidemiológica Avançada (Canal Endêmico):**
   * **Diagrama de Controle Municipal** oficial baseado na série histórica de anos não epidêmicos (2015–2019, 2022–2023).
   * Faixa endêmica interativa com Limite Inferior, Média Histórica Esperada e Limite Superior de Alerta Epidêmico ($\mu + 1{,}96 \times \sigma$).
   * Acompanhamento semanal das notificações do ano corrente (2026) com destaque visual de alerta.

7. **Modo Apresentação Executiva (Exportação Automática):**
   * Ferramenta de geração de apresentações oficiais para reuniões de gabinete e boletins epidemiológicos.
   * Exportação com um clique para **Microsoft PowerPoint (`.pptx`)** e **Adobe PDF (`.pdf`)**, contendo capas executivas e lâminas temáticas padronizadas.

---

## 📂 Estrutura de Arquivos do Repositório

```text
├── dengue.R                           # Aplicação principal R Shiny (UI & Server)
├── gerar_dados_exemplo.R              # Gerador de base sintética para demonstração segura
├── dados_historico_dengue_exemplo.rds # Base sintética 100% fictícia (LGPD Compliant)
├── estabelecimentos_pa.rds            # Catálogo de Unidades Notificadoras do Pará (CNES público)
├── notificacoes_estabelecimentos.rds   # Estatísticas consolidadas por unidade de saúde
├── mapa_bairros_belem.rds             # Polígonos espaciais oficiais dos Bairros de Belém (SF)
├── mapa_distritos_belem.rds           # Polígonos espaciais dos Distritos Administrativos de Belém (SF)
├── mapa_ilhas_belem.rds               # Polígonos espaciais das Ilhas de Belém (SF)
├── www/
│   └── logo_sesma.png                 # Logotipo institucional SESMA / Prefeitura de Belém
├── .gitignore                         # Regras rigorosas de bloqueio de dados confidenciais
├── LICENSE                            # Licença de uso
└── README.md                          # Documentação técnica do projeto
```

---

## 🛠️ Requisitos e Instalação

### Pré-requisitos
* **R** versão $\ge 4.2.0$ ([Download R](https://cloud.r-project.org/))
* **RStudio Desktop** (opcional, recomendado) ([Download RStudio](https://posit.co/download/rstudio-desktop/))

### Instalação dos Pacotes R
Abra o R ou RStudio e execute o comando abaixo para instalar todas as dependências:

```r
pacotes <- c(
  "shiny", "bslib", "foreign", "dplyr", "plotly", "lubridate", 
  "bsicons", "shinyjs", "base64enc", "openxlsx", "leaflet", "sf", 
  "leaflet.extras", "officer", "gridExtra", "grid", "ggplot2"
)

novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if (length(novos_pacotes) > 0) {
  install.packages(novos_pacotes, repos = "https://cloud.r-project.org")
}
```

---

## 🚀 Como Executar o Painel

### 1. Execução Rápida (Modo Demonstração / Dados Sintéticos)
Clone este repositório e execute diretamente:

```bash
git clone https://github.com/SEU_USUARIO/painel-dengue-belem.git
cd painel-dengue-belem
```

No console do R ou terminal:

```r
shiny::runApp("dengue.R", launch.browser = TRUE)
```

O aplicativo iniciará automaticamente no seu navegador padrão utilizando a base sintética `dados_historico_dengue_exemplo.rds`.

---

### 2. Execução Institucional (Equipe Técnica Autorizada SESMA / CIEVS)
Para os profissionais autorizados pela vigilância epidemiológica municipal que possuem acesso aos arquivos oficiais:

1. Obtenha a base compilada oficial `dados_historico_dengue.rds` através dos canais internos e seguros do CIEVS/SESMA.
2. Cole o arquivo `dados_historico_dengue.rds` na raiz do projeto (ou dentro de uma pasta `data/`).
3. Inicie o painel:
   ```r
   shiny::runApp("dengue.R", launch.browser = TRUE)
   ```
4. O painel detectará a base oficial e mudará automaticamente o indicador superior para **`[Base Oficial SINAN]`**.
5. Se desejar atualizar com novas notificações no ano vigente, utilize o botão lateral **"Upload DBF/DBC Ano Vigente"** dentro do próprio painel.

---

## 📊 Geração de Dados Sintéticos Adicionais

Caso queira gerar uma nova amostra sintética ou alterar o volume de registros para testes de carga, execute:

```bash
Rscript gerar_dados_exemplo.R
```

O script criará um novo arquivo `dados_historico_dengue_exemplo.rds` com distribuições estocásticas realistas, preservando a compatibilidade total com todos os gráficos do painel.

---

## 📜 Licença e Termos de Uso

Este projeto está licenciado sob os termos da licença [MIT](LICENSE).

**Ressalva de Uso:** O código deste sistema destina-se a fins de saúde pública, pesquisa epidemiológica e transparência da gestão municipal. É expressamente vedado o uso deste código para fins que infrinjam os direitos de privacidade individuais estabelecidos pela LGPD (Lei nº 13.709/2018).

---

**Secretaria Municipal de Saúde de Belém (SESMA)**  
*Centro de Informações Estratégicas em Vigilância em Saúde (CIEVS)*  
*Belém — Pará — Brasil*
