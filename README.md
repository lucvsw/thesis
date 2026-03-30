# Does Metro Access Increase Income? Evidence from Brasília

Tese de Doutorado em Economia — Universidade Católica de Brasília
**Autor:** Lucas Dutra de Paulo

## Sobre

Estima o efeito causal da proximidade ao Metrô de Brasília sobre o crescimento de renda per capita dos setores censitários entre 2000 e 2010, usando o alinhamento planejado e descartado do projeto IMT (1986–1987) como instrumento (2SLS).

## Reproduzindo o pipeline

O projeto usa [`{targets}`](https://docs.ropensci.org/targets/). Para rodar:

```r
targets::tar_make()
```

Os resultados ficam disponíveis via `targets::tar_read(<nome_do_target>)`.

## Dados externos (não incluídos)

Os rasters do MapBiomas são grandes demais para o repositório. Para reproduzir os targets de urbanização:

1. Acesse [mapbiomas.org](https://mapbiomas.org) → Coleção 9 → classe **Área Urbanizada (24)**
2. Baixe os anos **2000** e **2010**, recortados para o Distrito Federal
3. Salve em `dados/MapBiomas - Urbanizacao/2000.tif` e `2010.tif`

## Estrutura

```
R/
├── 1_shapefiles.R              # carrega shapefiles (estações, linhas, RAs, rodovias)
├── 2_malha_censitaria.R        # une shapefiles com dados tabulares do censo
├── 3_compatibilidade.R         # harmoniza setores 2000 ↔ 2010
├── 4_novas_variaveis.R         # distâncias, dummies de exposição, variações
├── 4b_urbanizacao_mapbiomas.R  # proporção de área urbanizada por setor
├── 5_analise_covariaveis.R     # importância das covariáveis
├── 6_regressoes.R              # modelos 2SLS principais
├── 7_robustez_amostragem.R     # robustez: amostra alternativa
├── 8_robustez_threshold.R      # robustez: thresholds alternativos
├── 9_robustez_intensidade.R    # robustez: variável contínua de intensidade
├── 10_heterogeneidade_RAs.R    # heterogeneidade por Região Administrativa
├── 11_sorting.R                # sorting: população, domicílios, renda, apartamentos
├── 12_regressoes_urbanizacao.R # efeito sobre urbanização
└── 13_regressoes_mecanismos.R  # mecanismos (composição / gentrificação)
```
