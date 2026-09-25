# Data

```
data/
├── raw/
│   ├── census/
│   │   ├── 2000/            IBGE census tables by census tract, Federal District
│   │   └── 2010/
│   ├── shapefiles/          Subway, planned alignment, administrative regions, highways
│   └── mapbiomas/           External rasters (not versioned, see below)
└── processed/
    ├── census_final.gpkg    Analysis dataset (census tract × year), with geometry
    └── census_final.csv     Same table without geometry
```

## Sources

| Data | Files | Source |
|---|---|---|
| Census 2000, universe results by census tract | `raw/census/2000/` (`Basico`, `Domicilio`, `Instrucao1`, `Morador`, `Pessoa1`, `Responsavel1`) | IBGE, Censo Demográfico 2000, aggregates by census tract (*Agregados por Setores Censitários*), Distrito Federal |
| Census 2010, universe results by census tract | `raw/census/2010/` (`Basico`, `Domicilio01`, `Domicilio02`, `Pessoa13`, `Responsavel02`, `ResponsavelRenda`) | IBGE, Censo Demográfico 2010, aggregates by census tract, Distrito Federal |
| Census-tract boundaries, 2000 and 2010 | downloaded when the pipeline runs | IBGE, through the [`geobr`](https://ipeagit.github.io/geobr/) package |
| Subway lines and stations; administrative regions | `raw/shapefiles/subway_lines`, `subway_stations`, `administrative_regions` | GeoPortal/DF |
| Planned and discarded subway alignment (the instrument) | `raw/shapefiles/planned_alignment` | Feasibility study by the Mauá Institute of Technology (IMT), 1986–87 |
| Highways | `raw/shapefiles/highways` | DER-DF (per the shapefile's own `rod_fonte` field), scale 1:10,000 |
| Urban land cover | not included | MapBiomas, see below |
| Maximum floor-area ratio (CfAM) by administrative region | values are written in `R/18_heterogeneity_cfam.R` | Annex V of the PDOT/2009 (Complementary Law 803/2009) |

The census tables are the unmodified files distributed by IBGE, under IBGE's original (Portuguese) file names: *Basico* = basic tract information, *Domicilio* = households, *Instrucao* = education, *Morador* = residents, *Pessoa* = persons, *Responsavel* = household heads. The shapefiles were renamed from their original Portuguese names (`estacao_de_metro`, `linha_de_metro`, `projeto_metro`, `regioes_administrativas`, `rodovias`); their attribute fields keep the source names and are renamed to English when imported by `R/1_shapefiles.R`.

## MapBiomas rasters (external)

The urban land-cover outcome (Table 8, column 4 of the paper) uses MapBiomas Collection 9 land-cover rasters (class 24, urban infrastructure). The national rasters (about 1 GB per year) are not versioned. Run

```r
source("data/raw/mapbiomas/download_mapbiomas.R")
```

from the repository root to read only the Federal District window from the public MapBiomas bucket (about 1 MB per year) and save it as `data/raw/mapbiomas/2000.tif` and `2010.tif`. The files come from `https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_<year>.tif`.

## Analysis dataset (`data/processed/`)

`census_final` is the target of the same name in the pipeline (`_targets.R`): 5,366 rows (census tracts observed in 2000 and 2010, after harmonizing the two tract maps, see Appendix A of the paper) and 130 columns. Because geography and exposure do not vary over time, use one year of rows (for example `year == 2010`) for tract-level analyses. Three tracts fall outside every administrative region; they carry `ra_id = "outsideRA"`. The main variables are:

| Variable | Description |
|---|---|
| `code_tract` | Harmonized census-tract identifier |
| `year` | Census year (2000 or 2010) |
| `income_per_capita` | Per-capita income, R$ (nominal): monthly income declared by household heads divided by residents |
| `dlog_income_per_capita` | Outcome: log(income 2010) − log(income 2000); defined on the 2010 rows |
| `dist_station` | Distance (m) from the tract centroid to the nearest subway station |
| `dist_planned` | Distance (m) from the tract centroid to the nearest segment of the planned IMT alignment |
| `dummy_500m` … `dummy_2500m` | Exposure *D*: 1 if `dist_station` is at most 500, 1,000, 1,500, 2,000 or 2,500 m |
| `dummyp_500m` … `dummyp_2500m` | Instrument *Z*: 1 if `dist_planned` is at most the same thresholds |
| `int_*`, `intp_*` | Continuous versions of exposure and instrument: `max(0, 1 − distance / threshold)` |
| `dummy_subway_5km` … `dummy_subway_20km` | Estimation-sample indicators: 1 if `dist_station` is at most 5, 7.5, 10, 12.5, 15 or 20 km (the paper's baseline sample is 10 km) |
| `ra_id` | Administrative region (RA) of the tract, used for fixed effects and clustering |
| `dummy_RA_*` | Administrative-region indicators |
| `pop`, `share_illiterate`, `share_over_65`, `share_college_complete` | Population, illiteracy share, share aged 65 or older, and higher-education share (baseline controls, 2000 values) |
| `dist_cbd`, `dist_highway` | Distances (m) to the central business district and to the nearest highway (baseline controls) |
| `households`, `apartments`, `share_apartments` | Households, apartments, and apartment share (urban-structure outcomes) |
| `share_women`, `share_large_family`, `share_illiterate_heads`, `share_high_income`, `share_working_age` | Composition outcomes (Hypothesis 2) |
| `heads_positive_income`, `mean_income_positive_heads` | Household heads with positive income, and their mean income (labor-market margins) |
| `cfa_b`, `cfa_m` | Basic and maximum floor-area ratio (*coeficiente de aproveitamento*, CfAB/CfAM), assigned tract by tract for a few administrative regions |
| `dlog_*`, `d_*` | Log changes (`dlog_`) and level changes (`d_`) between 2000 and 2010 |

Other columns are constructed in `R/2_census_data.R` and `R/4_new_variables.R`.
