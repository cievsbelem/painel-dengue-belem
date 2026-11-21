/**
 * PAINEL EPIDEMIOLÓGICO DA DENGUE - BELÉM / SESMA / CIEVS
 * Lógica da Interface Web (GitHub Pages / Portal Público)
 * Consome 'dados_dengue_publico.json' (100% Agregado / LGPD Compliant)
 */

let DADOS_GLOBAIS = null;
let GEOJSON_BAIRROS = null;
let MAPA_LEAFLET = null;
let CAMADA_GEOJSON = null;

let chartSemanalInstance = null;
let chartCanalInstance = null;
let chartPiramideInstance = null;
let chartRacaInstance = null;
let chartCriterioInstance = null;

// Inicialização ao carregar a página
document.addEventListener("DOMContentLoaded", async () => {
  try {
    const res = await fetch("dados_dengue_publico.json");
    DADOS_GLOBAIS = await res.json();
    
    // Tenta carregar GeoJSON de Bairros para o mapa
    try {
      const resGeo = await fetch("bairros_belem.geojson");
      GEOJSON_BAIRROS = await resGeo.json();
    } catch (e) {
      console.warn("GeoJSON de bairros não localizado. O mapa exibirá modo simplificado.");
    }
    
    inicializarInterface();
  } catch (err) {
    console.error("Erro ao carregar dados_dengue_publico.json:", err);
    document.getElementById("loading-spinner").innerHTML = `
      <div class="alert alert-danger">
        <h5>Erro ao carregar os dados epidemiológicos</h5>
        <p>Não foi possível acessar 'dados_dengue_publico.json'. Certifique-se de executar 'Rscript exportar_dados_web.R'.</p>
      </div>
    `;
  }
});

function formatarNumero(num) {
  if (num === null || num === undefined || isNaN(num)) return "0";
  return Number(num).toLocaleString("pt-BR");
}

function inicializarInterface() {
  document.getElementById("loading-spinner").style.display = "none";
  document.getElementById("conteudo-painel").style.display = "block";
  
  // Atualiza metadados
  const meta = DADOS_GLOBAIS.meta;
  document.getElementById("meta-data-atualizacao").textContent = meta.data_atualizacao;
  document.getElementById("meta-semana-vigente").textContent = `SE ${meta.max_semana_vigente} / ${meta.ano_vigente}`;
  document.getElementById("meta-total-notif").textContent = formatarNumero(meta.total_notificacoes);
  
  // Preenche o seletor de anos
  const selectAno = document.getElementById("select-ano");
  selectAno.innerHTML = "";
  DADOS_GLOBAIS.anos_disponiveis.forEach(ano => {
    const opt = document.createElement("option");
    opt.value = ano;
    opt.textContent = ano;
    if (ano == meta.ano_vigente) opt.selected = true;
    selectAno.appendChild(opt);
  });
  
  const optTodos = document.createElement("option");
  optTodos.value = "TODOS";
  optTodos.textContent = "Série Histórica Completa";
  selectAno.appendChild(optTodos);
  
  selectAno.addEventListener("change", (e) => {
    atualizarAnoSelecionado(e.target.value);
  });
  
  // Inicializa com o ano vigente
  atualizarAnoSelecionado(meta.ano_vigente.toString());
  
  // Renderiza Canal Endêmico (Fixo no Ano Vigente)
  renderizarCanalEndemico();
  
  // Renderiza Mapa e Tabelas
  inicializarMapaLeaflet();
  renderizarTabelaBairros();
  renderizarRankingUnidades();
}

function atualizarAnoSelecionado(ano) {
  const kpis = DADOS_GLOBAIS.kpis[ano] || DADOS_GLOBAIS.kpis["TODOS"];
  
  document.getElementById("kpi-notificados").textContent = formatarNumero(kpis.notificados);
  document.getElementById("kpi-confirmados").textContent = formatarNumero(kpis.confirmados);
  document.getElementById("kpi-descartados").textContent = formatarNumero(kpis.descartados);
  document.getElementById("kpi-investigacao").textContent = formatarNumero(kpis.investigacao);
  document.getElementById("kpi-obitos").textContent = formatarNumero(kpis.obitos);
  document.getElementById("kpi-incidencia").textContent = kpis.incidencia.toLocaleString("pt-BR", { minimumFractionDigits: 1 }) + " /100k";
  document.getElementById("kpi-letalidade").textContent = kpis.letalidade.toLocaleString("pt-BR", { minimumFractionDigits: 2 }) + "%";
  
  renderizarGraficoSemanal(ano);
  renderizarPiramide(ano);
  renderizarRacaCor(ano);
  renderizarCriterios(ano);
  atualizarCoresMapa(ano);
}

// 1. Gráfico Semanal (Notificados vs Confirmados)
function renderizarGraficoSemanal(ano) {
  const ctx = document.getElementById("chart-semanal").getContext("2d");
  const dadosAno = DADOS_GLOBAIS.serie_semanal[ano] || [];
  
  const labels = dadosAno.map(d => `SE ${d.semana}`);
  const notificados = dadosAno.map(d => d.notificados);
  const confirmados = dadosAno.map(d => d.confirmados);
  
  if (chartSemanalInstance) chartSemanalInstance.destroy();
  
  chartSemanalInstance = new Chart(ctx, {
    type: "bar",
    data: {
      labels: labels,
      datasets: [
        {
          label: "Casos Notificados",
          data: notificados,
          backgroundColor: "#1E3A8A",
          borderRadius: 4
        },
        {
          label: "Casos Confirmados",
          data: confirmados,
          backgroundColor: "#3B82F6",
          borderRadius: 4
        }
      ]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { position: "top" },
        tooltip: {
          mode: "index",
          intersect: false
        }
      },
      scales: {
        x: { grid: { display: false } },
        y: { beginAtZero: true, title: { display: true, text: "Nº de Casos" } }
      }
    }
  });
}

// 2. Canal Endêmico / Diagrama de Controle Oficial
function renderizarCanalEndemico() {
  const ctx = document.getElementById("chart-canal").getContext("2d");
  const canal = DADOS_GLOBAIS.canal_endemico || [];
  
  const labels = canal.map(d => `SE ${d.semana}`);
  const media = canal.map(d => d.media);
  const limiteSup = canal.map(d => d.limite_sup);
  const limiteInf = canal.map(d => d.limite_inf);
  const observado = canal.map(d => d.observado_2026);
  
  if (chartCanalInstance) chartCanalInstance.destroy();
  
  chartCanalInstance = new Chart(ctx, {
    type: "bar",
    data: {
      labels: labels,
      datasets: [
        {
          type: "line",
          label: "Limite Superior (Alerta Epidêmico)",
          data: limiteSup,
          borderColor: "#DC2626",
          backgroundColor: "rgba(220, 38, 38, 0.08)",
          fill: "+1",
          borderWidth: 2,
          pointRadius: 0
        },
        {
          type: "line",
          label: "Média Histórica Esperada",
          data: media,
          borderColor: "#1E3A8A",
          borderDash: [5, 5],
          borderWidth: 2,
          pointRadius: 0,
          fill: false
        },
        {
          type: "line",
          label: "Limite Inferior",
          data: limiteInf,
          borderColor: "#10B981",
          borderDash: [2, 2],
          borderWidth: 1.5,
          pointRadius: 0,
          fill: false
        },
        {
          type: "bar",
          label: "Observado 2026 (Semanal)",
          data: observado,
          backgroundColor: canal.map(d => (d.observado_2026 > d.limite_sup) ? "#EF4444" : "#2563EB"),
          borderRadius: 4
        }
      ]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { position: "top" }
      },
      scales: {
        x: { grid: { display: false } },
        y: { beginAtZero: true, title: { display: true, text: "Notificações de Dengue" } }
      }
    }
  });
}

// 3. Pirâmide Etária
function renderizarPiramide(ano) {
  const ctx = document.getElementById("chart-piramide").getContext("2d");
  const pir = DADOS_GLOBAIS.piramide[ano] || DADOS_GLOBAIS.piramide["2026"] || [];
  
  const faixas = pir.map(d => d.faixa);
  const masc = pir.map(d => -Math.abs(d.Masculino || 0));
  const fem = pir.map(d => Math.abs(d.Feminino || 0));
  
  if (chartPiramideInstance) chartPiramideInstance.destroy();
  
  chartPiramideInstance = new Chart(ctx, {
    type: "bar",
    data: {
      labels: faixas,
      datasets: [
        {
          label: "Masculino",
          data: masc,
          backgroundColor: "#1E3A8A",
          borderRadius: 4
        },
        {
          label: "Feminino",
          data: fem,
          backgroundColor: "#EC4899",
          borderRadius: 4
        }
      ]
    },
    options: {
      indexAxis: "y",
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        tooltip: {
          callbacks: {
            label: (ctx) => `${ctx.dataset.label}: ${Math.abs(ctx.raw)} casos`
          }
        }
      },
      scales: {
        x: {
          ticks: {
            callback: (v) => Math.abs(v)
          },
          title: { display: true, text: "Volume de Casos" }
        }
      }
    }
  });
}

// 4. Raça / Cor
function renderizarRacaCor(ano) {
  const ctx = document.getElementById("chart-raca").getContext("2d");
  const raca = DADOS_GLOBAIS.raca_cor || [];
  
  const labels = raca.map(d => d.raca);
  const valores = raca.map(d => (ano == "2026") ? d.total_2026 : d.total);
  
  if (chartRacaInstance) chartRacaInstance.destroy();
  
  chartRacaInstance = new Chart(ctx, {
    type: "doughnut",
    data: {
      labels: labels,
      datasets: [{
        data: valores,
        backgroundColor: ["#1E3A8A", "#3B82F6", "#F59E0B", "#10B981", "#8B5CF6", "#64748B"]
      }]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { position: "right" }
      }
    }
  });
}

// 5. Critério de Confirmação
function renderizarCriterios(ano) {
  const ctx = document.getElementById("chart-criterio").getContext("2d");
  const crit = DADOS_GLOBAIS.criterio || [];
  
  const labels = crit.map(d => d.criterio);
  const valores = crit.map(d => (ano == "2026") ? d.total_2026 : d.total);
  
  if (chartCriterioInstance) chartCriterioInstance.destroy();
  
  chartCriterioInstance = new Chart(ctx, {
    type: "pie",
    data: {
      labels: labels,
      datasets: [{
        data: valores,
        backgroundColor: ["#10B981", "#3B82F6", "#F59E0B", "#94A3B8"]
      }]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { position: "right" }
      }
    }
  });
}

// 6. Mapa Interativo de Belém com Bairros (Leaflet)
function inicializarMapaLeaflet() {
  const mapContainer = document.getElementById("mapa-belem");
  if (!mapContainer) return;
  
  MAPA_LEAFLET = L.map("mapa-belem").setView([-1.42, -48.47], 11);
  
  L.tileLayer("https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png", {
    maxZoom: 18,
    attribution: "&copy; OpenStreetMap & CartoDB"
  }).addTo(MAPA_LEAFLET);
  
  if (GEOJSON_BAIRROS) {
    atualizarCoresMapa("2026");
  }
}

function getCorCasos(d) {
  return d > 150 ? "#7F1D1D" :
         d > 80  ? "#DC2626" :
         d > 40  ? "#F97316" :
         d > 20  ? "#FBBF24" :
         d > 0   ? "#93C5FD" :
                   "#E2E8F0";
}

function atualizarCoresMapa(ano) {
  if (!MAPA_LEAFLET || !GEOJSON_BAIRROS) return;
  
  if (CAMADA_GEOJSON) MAPA_LEAFLET.removeLayer(CAMADA_GEOJSON);
  
  const mapCasos = {};
  DADOS_GLOBAIS.bairros.forEach(b => {
    mapCasos[b.bairro.toUpperCase()] = (ano == "2026") ? b.total_2026 : b.total;
  });
  
  CAMADA_GEOJSON = L.geoJson(GEOJSON_BAIRROS, {
    style: function(feature) {
      const nome = (feature.properties.nm_bairro || "").toUpperCase();
      const casos = mapCasos[nome] || 0;
      return {
        fillColor: getCorCasos(casos),
        weight: 1.2,
        opacity: 1,
        color: "#ffffff",
        fillOpacity: 0.75
      };
    },
    onEachFeature: function(feature, layer) {
      const nome = feature.properties.nm_bairro || "Bairro";
      const casos = mapCasos[nome.toUpperCase()] || 0;
      layer.bindTooltip(`<b>${nome}</b><br>Casos Notificados: ${formatarNumero(casos)}`, { sticky: true });
    }
  }).addTo(MAPA_LEAFLET);
}

// 7. Tabela de Bairros
function renderizarTabelaBairros() {
  const tbody = document.getElementById("tbody-bairros");
  if (!tbody) return;
  
  tbody.innerHTML = "";
  DADOS_GLOBAIS.bairros.forEach((b, idx) => {
    const tr = document.createElement("tr");
    tr.innerHTML = `
      <td><span class="badge bg-light text-dark border">${idx + 1}</span></td>
      <td class="fw-semibold">${b.bairro}</td>
      <td class="text-end fw-bold text-primary">${formatarNumero(b.total_2026)}</td>
      <td class="text-end text-muted">${formatarNumero(b.total)}</td>
    `;
    tbody.appendChild(tr);
  });
}

// 8. Ranking de Unidades Notificadoras
function renderizarRankingUnidades() {
  const tbody = document.getElementById("tbody-unidades");
  if (!tbody) return;
  
  tbody.innerHTML = "";
  DADOS_GLOBAIS.ranking_unidades.forEach((u, idx) => {
    const tr = document.createElement("tr");
    tr.innerHTML = `
      <td><span class="badge bg-primary">${idx + 1}º</span></td>
      <td class="fw-semibold">${u.unidade}</td>
      <td class="text-end fw-bold text-primary">${formatarNumero(u.total_2026)}</td>
      <td class="text-end text-muted">${formatarNumero(u.total)}</td>
    `;
    tbody.appendChild(tr);
  });
}
