/**
 * PAINEL EPIDEMIOLÓGICO DA DENGUE — BELÉM / SESMA / CIEVS
 * Lógica do Portal Web Oficial com Plotly.js e Leaflet
 * Consome 'dados_dengue_publico.json' (100% LGPD Compliant)
 */

let DADOS_GLOBAIS = null;
let GEOJSON_BAIRROS = null;
let MAPA_LEAFLET = null;
let CAMADA_GEOJSON = null;
let ANO_SELECIONADO = "2026";

// Configurações Globais do Plotly
const PLOTLY_CONFIG = {
  responsive: true,
  displayModeBar: true,
  displaylogo: false,
  modeBarButtonsToRemove: ['lasso2d', 'select2d'],
  toImageButtonOptions: {
    format: 'png',
    filename: 'grafico_dengue_belem',
    height: 600,
    width: 1000,
    scale: 2
  }
};

const FONT_FAMILY = "'Plus Jakarta Sans', sans-serif";

// Inicialização imediata
document.addEventListener("DOMContentLoaded", async () => {
  try {
    const ts = new Date().getTime();
    const res = await fetch(`dados_dengue_publico.json?t=${ts}`);
    if (!res.ok) throw new Error("Status HTTP: " + res.status);
    DADOS_GLOBAIS = await res.json();
    
    // Tenta carregar GeoJSON de Bairros para o mapa
    try {
      const resGeo = await fetch(`bairros_belem.geojson?v=2`);
      if (resGeo.ok) {
        GEOJSON_BAIRROS = await resGeo.json();
      }
    } catch (e) {
      console.warn("GeoJSON de bairros não localizado. O mapa funcionará em modo simplificado.");
    }
    
    inicializarInterface();
  } catch (err) {
    console.error("Erro ao carregar dados_dengue_publico.json:", err);
    document.getElementById("loading-spinner").innerHTML = `
      <div class="alert alert-danger mx-auto" style="max-width: 650px;">
        <h5 class="fw-bold"><i class="bi bi-exclamation-triangle-fill me-2"></i>Erro ao carregar os dados epidemiológicos</h5>
        <p class="mb-0">Não foi possível ler 'dados_dengue_publico.json'. Execute o comando R para gerar os dados atualizados.</p>
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
    opt.textContent = (ano == meta.ano_vigente) ? `${ano} (Ano Vigente)` : ano;
    if (ano == meta.ano_vigente) opt.selected = true;
    selectAno.appendChild(opt);
  });
  
  const optTodos = document.createElement("option");
  optTodos.value = "TODOS";
  optTodos.textContent = "Série Histórica Completa (2014–2026)";
  selectAno.appendChild(optTodos);
  
  selectAno.addEventListener("change", (e) => {
    ANO_SELECIONADO = e.target.value;
    atualizarDadosAno(ANO_SELECIONADO);
  });

  // Evento de ajuste do Plotly e Leaflet ao trocar de abas
  document.querySelectorAll('button[data-bs-toggle="tab"]').forEach(tabBtn => {
    tabBtn.addEventListener('shown.bs.tab', (e) => {
      const targetId = e.target.getAttribute('data-bs-target');
      if (targetId === '#tab-visao') {
        Plotly.Plots.resize('plot-semanal');
      } else if (targetId === '#tab-canal') {
        Plotly.Plots.resize('plot-canal');
      } else if (targetId === '#tab-espacial') {
        if (MAPA_LEAFLET) {
          setTimeout(() => MAPA_LEAFLET.invalidateSize(), 50);
        }
      } else if (targetId === '#tab-sociodemo') {
        Plotly.Plots.resize('plot-piramide');
        Plotly.Plots.resize('plot-raca');
      } else if (targetId === '#tab-rede') {
        Plotly.Plots.resize('plot-criterio');
      }
    });
  });

  // Busca na tabela de Bairros
  const inputBusca = document.getElementById("busca-bairro");
  if (inputBusca) {
    inputBusca.addEventListener("input", (e) => {
      const termo = e.target.value.toLowerCase();
      const linhas = document.querySelectorAll("#tbody-bairros tr");
      linhas.forEach(tr => {
        const txt = tr.children[1].textContent.toLowerCase();
        tr.style.display = txt.includes(termo) ? "" : "none";
      });
    });
  }

  // Inicializa mapa e tabelas
  inicializarMapaLeaflet();
  renderizarTabelaBairros();
  renderizarRankingUnidades();

  // Renderiza ano inicial
  atualizarDadosAno(ANO_SELECIONADO);
  
  // Renderiza canal endêmico (fixo ano 2026)
  renderizarPlotCanal();
}

function atualizarDadosAno(ano) {
  const kpis = DADOS_GLOBAIS.kpis[ano] || DADOS_GLOBAIS.kpis["TODOS"] || {};
  
  document.getElementById("kpi-notificados").textContent = formatarNumero(kpis.notificados);
  document.getElementById("kpi-provaveis").textContent   = formatarNumero(kpis.provaveis);
  document.getElementById("kpi-confirmados").textContent  = formatarNumero(kpis.confirmados);
  document.getElementById("kpi-descartados").textContent  = formatarNumero(kpis.descartados);
  document.getElementById("kpi-investigacao").textContent = formatarNumero(kpis.investigacao);
  
  const incid = (kpis.incidencia !== undefined) ? Number(kpis.incidencia).toLocaleString("pt-BR", { minimumFractionDigits: 1 }) : "0,0";
  document.getElementById("kpi-incidencia").textContent = incid;
  
  document.getElementById("kpi-obitos").textContent = formatarNumero(kpis.obitos);
  const letal = (kpis.letalidade !== undefined) ? Number(kpis.letalidade).toLocaleString("pt-BR", { minimumFractionDigits: 2 }) : "0,00";
  document.getElementById("kpi-letalidade").textContent = `Letalidade: ${letal}%`;

  // Renderiza Gráficos Plotly para o ano
  renderizarPlotSemanal(ano);
  renderizarPlotPiramide(ano);
  renderizarPlotRaca(ano);
  renderizarPlotCriterio(ano);

  // Atualiza cores do Mapa
  atualizarCoresMapa(ano);
}

// -----------------------------------------------------------------------------
// 1. Curva Semanal de Notificados vs Confirmados (Plotly)
// -----------------------------------------------------------------------------
function renderizarPlotSemanal(ano) {
  const dados = DADOS_GLOBAIS.serie_semanal[ano] || [];
  
  const x = dados.map(d => `SE ${d.semana}`);
  const yNotif = dados.map(d => d.notificados);
  const yConf = dados.map(d => d.confirmados);
  
  const traceNotif = {
    x: x,
    y: yNotif,
    name: 'Casos Notificados',
    type: 'bar',
    marker: { color: '#1E3A8A' }
  };
  
  const traceConf = {
    x: x,
    y: yConf,
    name: 'Casos Confirmados',
    type: 'bar',
    marker: { color: '#3B82F6' }
  };
  
  const layout = {
    font: { family: FONT_FAMILY },
    barmode: 'group',
    margin: { l: 50, r: 20, t: 30, b: 60 },
    legend: { orientation: 'h', x: 0, y: 1.12 },
    xaxis: {
      title: 'Semana Epidemiológica',
      tickangle: -45,
      showgrid: false
    },
    yaxis: {
      title: 'Número de Casos',
      zeroline: true,
      gridcolor: '#F1F5F9'
    },
    hovermode: 'x unified',
    paper_bgcolor: 'transparent',
    plot_bgcolor: 'transparent'
  };
  
  Plotly.react('plot-semanal', [traceNotif, traceConf], layout, PLOTLY_CONFIG);
}

// -----------------------------------------------------------------------------
// 2. Canal Endêmico / Diagrama de Controle Oficial (Plotly)
// -----------------------------------------------------------------------------
function renderizarPlotCanal() {
  const canal = DADOS_GLOBAIS.canal_endemico || [];
  
  const x = canal.map(d => `SE ${d.semana}`);
  const limiteSup = canal.map(d => d.limite_sup);
  const media = canal.map(d => d.media);
  const limiteInf = canal.map(d => d.limite_inf);
  const obs = canal.map(d => (d.observado_2026 !== null && d.observado_2026 !== undefined) ? d.observado_2026 : null);
  
  // Cores das barras do ano vigente: vermelho se ultrapassa o limite superior
  const barColors = canal.map(d => {
    if (d.observado_2026 !== null && d.observado_2026 > d.limite_sup) return '#DC2626';
    return '#2563EB';
  });

  const traceSup = {
    x: x,
    y: limiteSup,
    name: 'Limite Superior (Alerta Epidêmico)',
    type: 'scatter',
    mode: 'lines',
    line: { color: '#DC2626', width: 2 },
    hoverinfo: 'y+name'
  };

  const traceMedia = {
    x: x,
    y: media,
    name: 'Média Histórica Esperada',
    type: 'scatter',
    mode: 'lines',
    line: { color: '#1E3A8A', width: 2, dash: 'dash' },
    hoverinfo: 'y+name'
  };

  const traceInf = {
    x: x,
    y: limiteInf,
    name: 'Limite Inferior',
    type: 'scatter',
    mode: 'lines',
    line: { color: '#10B981', width: 1.5, dash: 'dot' },
    hoverinfo: 'y+name'
  };

  const traceObs = {
    x: x,
    y: obs,
    name: 'Observado 2026 (Semanal)',
    type: 'bar',
    marker: { color: barColors },
    hoverinfo: 'y+name'
  };

  const layout = {
    font: { family: FONT_FAMILY },
    margin: { l: 50, r: 20, t: 30, b: 60 },
    legend: { orientation: 'h', x: 0, y: 1.15 },
    xaxis: {
      title: 'Semana Epidemiológica (2026)',
      tickangle: -45,
      showgrid: false
    },
    yaxis: {
      title: 'Notificações de Dengue',
      zeroline: true,
      gridcolor: '#F1F5F9'
    },
    paper_bgcolor: 'transparent',
    plot_bgcolor: 'transparent'
  };

  Plotly.react('plot-canal', [traceObs, traceSup, traceMedia, traceInf], layout, PLOTLY_CONFIG);
}

// -----------------------------------------------------------------------------
// 3. Pirâmide Etária por Sexo (Plotly)
// -----------------------------------------------------------------------------
function renderizarPlotPiramide(ano) {
  const dados = (DADOS_GLOBAIS.piramide && DADOS_GLOBAIS.piramide[ano]) 
    ? DADOS_GLOBAIS.piramide[ano] 
    : (DADOS_GLOBAIS.piramide["2026"] || []);

  const faixas = dados.map(d => d.faixa);
  const masc = dados.map(d => -Math.abs(d.Masculino || 0));
  const fem  = dados.map(d => Math.abs(d.Feminino || 0));

  const traceMasc = {
    y: faixas,
    x: masc,
    name: 'Masculino',
    type: 'bar',
    orientation: 'h',
    marker: { color: '#1E3A8A' },
    customdata: masc.map(v => Math.abs(v)),
    hovertemplate: 'Masculino: %{customdata} casos<extra></extra>'
  };

  const traceFem = {
    y: faixas,
    x: fem,
    name: 'Feminino',
    type: 'bar',
    orientation: 'h',
    marker: { color: '#EC4899' },
    customdata: fem,
    hovertemplate: 'Feminino: %{customdata} casos<extra></extra>'
  };

  const maxVal = Math.max(...masc.map(v => Math.abs(v)), ...fem) * 1.15 || 500;

  const layout = {
    font: { family: FONT_FAMILY },
    barmode: 'relative',
    margin: { l: 60, r: 20, t: 30, b: 50 },
    legend: { orientation: 'h', x: 0.3, y: 1.12 },
    xaxis: {
      title: 'Volume de Casos',
      range: [-maxVal, maxVal],
      tickvals: [-maxVal, -Math.round(maxVal/2), 0, Math.round(maxVal/2), maxVal],
      ticktext: [Math.round(maxVal), Math.round(maxVal/2), 0, Math.round(maxVal/2), Math.round(maxVal)],
      gridcolor: '#F1F5F9'
    },
    yaxis: {
      title: 'Faixa Etária'
    },
    paper_bgcolor: 'transparent',
    plot_bgcolor: 'transparent'
  };

  Plotly.react('plot-piramide', [traceMasc, traceFem], layout, PLOTLY_CONFIG);
}

// -----------------------------------------------------------------------------
// 4. Raça / Cor (Plotly Donut)
// -----------------------------------------------------------------------------
function renderizarPlotRaca(ano) {
  const dados = (DADOS_GLOBAIS.raca_cor && DADOS_GLOBAIS.raca_cor[ano]) 
    ? DADOS_GLOBAIS.raca_cor[ano] 
    : (DADOS_GLOBAIS.raca_cor["TODOS"] || []);

  const labels = dados.map(d => d.raca);
  const values = dados.map(d => d.total);

  const trace = {
    labels: labels,
    values: values,
    type: 'pie',
    hole: 0.45,
    marker: {
      colors: ['#1E3A8A', '#3B82F6', '#F59E0B', '#10B981', '#8B5CF6', '#94A3B8']
    },
    textinfo: 'percent',
    hoverinfo: 'label+value+percent'
  };

  const layout = {
    font: { family: FONT_FAMILY },
    margin: { l: 20, r: 20, t: 20, b: 20 },
    legend: { orientation: 'v', x: 1, y: 0.5 },
    paper_bgcolor: 'transparent',
    plot_bgcolor: 'transparent'
  };

  Plotly.react('plot-raca', [trace], layout, PLOTLY_CONFIG);
}

// -----------------------------------------------------------------------------
// 5. Critério de Confirmação (Plotly Donut)
// -----------------------------------------------------------------------------
function renderizarPlotCriterio(ano) {
  const dados = (DADOS_GLOBAIS.criterio && DADOS_GLOBAIS.criterio[ano]) 
    ? DADOS_GLOBAIS.criterio[ano] 
    : (DADOS_GLOBAIS.criterio["TODOS"] || []);

  const labels = dados.map(d => d.criterio);
  const values = dados.map(d => d.total);

  const trace = {
    labels: labels,
    values: values,
    type: 'pie',
    hole: 0.45,
    marker: {
      colors: ['#10B981', '#3B82F6', '#F59E0B', '#CBD5E1']
    },
    textinfo: 'percent',
    hoverinfo: 'label+value+percent'
  };

  const layout = {
    font: { family: FONT_FAMILY },
    margin: { l: 20, r: 20, t: 20, b: 20 },
    legend: { orientation: 'v', x: 1, y: 0.5 },
    paper_bgcolor: 'transparent',
    plot_bgcolor: 'transparent'
  };

  Plotly.react('plot-criterio', [trace], layout, PLOTLY_CONFIG);
}

// -----------------------------------------------------------------------------
// 6. Mapa Leaflet dos Bairros de Belém
// -----------------------------------------------------------------------------
function inicializarMapaLeaflet() {
  const mapEl = document.getElementById("mapa-belem");
  if (!mapEl) return;

  MAPA_LEAFLET = L.map("mapa-belem", {
    zoomControl: true,
    scrollWheelZoom: true
  }).setView([-1.42, -48.47], 11);

  L.tileLayer("https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png", {
    maxZoom: 18,
    attribution: "&copy; OpenStreetMap &copy; CARTO"
  }).addTo(MAPA_LEAFLET);

  if (GEOJSON_BAIRROS) {
    atualizarCoresMapa(ANO_SELECIONADO);
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

  if (CAMADA_GEOJSON) {
    MAPA_LEAFLET.removeLayer(CAMADA_GEOJSON);
  }

  const mapaCasos = {};
  if (DADOS_GLOBAIS && DADOS_GLOBAIS.bairros) {
    DADOS_GLOBAIS.bairros.forEach(b => {
      mapaCasos[b.bairro.toUpperCase()] = (ano == "2026") ? b.total_2026 : b.total;
    });
  }

  CAMADA_GEOJSON = L.geoJson(GEOJSON_BAIRROS, {
    style: function (feature) {
      const nome = (feature.properties.nm_bairro || feature.properties.bairro || "").toUpperCase();
      const casos = mapaCasos[nome] || 0;
      return {
        fillColor: getCorCasos(casos),
        weight: 1.2,
        opacity: 1,
        color: "#ffffff",
        fillOpacity: 0.78
      };
    },
    onEachFeature: function (feature, layer) {
      const nome = feature.properties.nm_bairro || feature.properties.bairro || "Bairro";
      const casos = mapaCasos[nome.toUpperCase()] || 0;
      layer.bindTooltip(
        `<b>${nome}</b><br>Casos Notificados: <b>${formatarNumero(casos)}</b>`,
        { sticky: true }
      );
    }
  }).addTo(MAPA_LEAFLET);
}

// -----------------------------------------------------------------------------
// 7. Tabelas de Bairros e Ranking de Unidades
// -----------------------------------------------------------------------------
function renderizarTabelaBairros() {
  const tbody = document.getElementById("tbody-bairros");
  if (!tbody || !DADOS_GLOBAIS.bairros) return;

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

function renderizarRankingUnidades() {
  const tbody = document.getElementById("tbody-unidades");
  if (!tbody || !DADOS_GLOBAIS.ranking_unidades) return;

  tbody.innerHTML = "";
  DADOS_GLOBAIS.ranking_unidades.forEach((u, idx) => {
    const tr = document.createElement("tr");
    tr.innerHTML = `
      <td><span class="badge bg-primary-subtle text-primary border px-2">${idx + 1}º</span></td>
      <td class="fw-semibold">${u.unidade}</td>
      <td class="text-end fw-bold text-primary">${formatarNumero(u.total_2026)}</td>
      <td class="text-end text-muted">${formatarNumero(u.total)}</td>
    `;
    tbody.appendChild(tr);
  });
}
