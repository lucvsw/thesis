# Function to merge the 2010 tracts that form one tract in 2000: it removes the merged 2010 tracts and names the merged 2010 tract after the original 2000 tract, which is renamed to avoid duplication with a possible 2010 tract that has the same code as the 2000 tract but is not part of the merged tracts.

# I need to look at QGIS again to do the harmonization
merge_tracts_across_years <- function(df, tracts_2010, tract_2000, new_code) {
  
  df %>%
    # Remove the 2010 tracts that will be merged
    filter(!(year == 2010 & code_tract %in% tracts_2010)) %>%
    
    # Remove the 2000 tract that will be renamed
    filter(!(year == 2000 & code_tract == tract_2000)) %>%
    
    # Add the merged row of the 2010 tracts
    bind_rows(
      df %>%
        filter(year == 2010 & code_tract %in% tracts_2010) %>%
        mutate(geom = st_make_valid(geom)) %>%
        summarise(
          across(
            .cols = where(is.numeric) & !c(code_tract, year, geom),  # sum all numeric columns except code and year
            .fns  = ~sum(.x, na.rm = TRUE)
          ),
          geom = st_union(geom),
          .groups = "drop"
        ) %>%
        mutate(
          code_tract = new_code,
          year = 2010
        ),
      
      # Rename the 2000 tract to the new code
      df %>%
        filter(year == 2000 & code_tract == tract_2000) %>%
        mutate(code_tract = new_code)
    )
}

# Function to build the harmonized 2010 tract map
harmonize_tracts <- function(census_sf_to_harmonize) {
# Merge the 2010 tracts that make up 2000 tract 530010805070149 and replace them with the new tract
  census_sf_harmonized <- census_sf_to_harmonize
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070167", "530010805070224", "530010805070225"),
  tract_2000   = "530010805070149",
  new_code  = "5300108050701491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070148 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070166", "530010805070213"),
  tract_2000   = "530010805070148",
  new_code  = "5300108050701481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070146 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070211", "530010805070164"),
  tract_2000   = "530010805070146",
  new_code  = "5300108050701461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070145 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070163", "530010805070210"),
  tract_2000   = "530010805070145",
  new_code  = "5300108050701451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070147 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070165", "530010805070212", "530010805070172", "530010805070173"),
  tract_2000   = "530010805070147",
  new_code  = "5300108050701471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200076 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200119", "530010805200179", "530010805200182", "530010805200120", "530010805200121", "530010805200122", "530010805200123", "530010805200126", "530010805200124", "530010805200125", "530010805200181"),
  tract_2000   = "530010805200076",
  new_code  = "5300108052000761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180172 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180236", "530010805180235", "530010805180234", "530010805180313", "530010805180312", "530010805180238", "530010805180237", "530010805180314"),
  tract_2000   = "530010805180172",
  new_code  = "5300108051801721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150389 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150511", "530010805150510", "530010805150470", "530010805150469", "530010805150457"),
  tract_2000   = "530010805150389",
  new_code  = "5300108051503891"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120059 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120074", "530010805120083"),
  tract_2000   = "530010805120059",
  new_code  = "5300108051200591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110166 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110193", "530010805110205"),
  tract_2000   = "530010805110166",
  new_code  = "5300108051101661"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110165 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110192", "530010805110293", "530010805110292"),
  tract_2000   = "530010805110165",
  new_code  = "5300108051101651"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110160-0160 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110187", "530010805110287"),
  tract_2000   = "530010805110160-0160",
  new_code  = "530010805110160-01601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110162 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110189", "530010805110276"),
  tract_2000   = "530010805110162",
  new_code  = "5300108051101621"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110158 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110289", "530010805110185"),
  tract_2000   = "530010805110158",
  new_code  = "5300108051101581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110184 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110184", "530010805110288", "530010805110204"),
  tract_2000   = "530010805110184",
  new_code  = "5300108051101841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100152 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100232", "530010805100231", "530010805100228"),
  tract_2000   = "530010805100152",
  new_code  = "5300108051001521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110185 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110291", "530010805110175"),
  tract_2000   = "530010805110185",
  new_code  = "5300108051101851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110175 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110201", "530010805110283", "530010805110282"),
  tract_2000   = "530010805110175",
  new_code  = "5300108051101751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110170 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110290", "530010805110197", "530010805110198", "530010805110285"),
  tract_2000   = "530010805110170",
  new_code  = "5300108051101701"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110172 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110199", "530010805110277", "530010805110279", "530010805110298", "530010805110278"),
  tract_2000   = "530010805110172",
  new_code  = "5300108051101721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120058 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120073", "530010805120082", "530010805120100"),
  tract_2000   = "530010805120058",
  new_code  = "5300108051200581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120075", "530010805120084", "530010805120077"),
  tract_2000   = "530010805120060",
  new_code  = "5300108051200601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120072", "530010805120081", "530010805120096", "530010805120080"),
  tract_2000   = "530010805120057",
  new_code  = "5300108051200571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120061 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120076", "530010805120085"),
  tract_2000   = "530010805120061",
  new_code  = "5300108051200611"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120056 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120098", "530010805120079", "530010805120097", "530010805120078", "530010805120071"),
  tract_2000   = "530010805120056",
  new_code  = "5300108051200561"
)

###### Merge the 2010 tracts that make up 2000 tract 530010805110182 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110182", "530010805110203"),
  tract_2000   = "530010805110182",
  new_code  = "5300108051101821"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300126", "530010805300089", "530010805300127", "530010805300092", "530010805300128"),
  tract_2000   = "530010805300060",
  new_code  = "5300108053000601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250116 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250197", "530010805250196", "530010805250129"),
  tract_2000   = "530010805250116",
  new_code  = "5300108052501161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250119 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250189", "530010805250191", "530010805250192", "530010805250190", "530010805250126"),
  tract_2000   = "530010805250119",
  new_code  = "5300108052501191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300059 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300088", "530010805300143", "530010805300152", "530010805300151", "530010805300091"),
  tract_2000   = "530010805300059",
  new_code  = "5300108053000591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300058 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300087"),
  tract_2000   = "530010805300058",
  new_code  = "5300108053000581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300086"),
  tract_2000   = "530010805300057",
  new_code  = "5300108053000571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300054 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300122", "530010805300124", "530010805300085", "530010805300158", "530010805300084", "530010805300107", "530010805300106", "530010805300105", "530010805300104", "530010805300123", "530010805300113", "530010805300148", "530010805300114", "530010805300150", "530010805300115", "530010805300117", "530010805300118", "530010805300156", "530010805300116", "530010805300112", "530010805300110", "530010805300147", "530010805300111", "530010805300109", "530010805300108", "530010805300125", "530010805300149"),
  tract_2000   = "530010805300054",
  new_code  = "5300108053000541"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300047 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300146", "530010805300145", "530010805300155", "530010805300154", "530010805300078", "530010805300079", "530010805300081", "530010805300119", "530010805300120", "530010805300082", "530010805300080", "530010805300101", "530010805300103", "530010805300102", "530010805300093", "530010805300153", "530010805300129", "530010805300131", "530010805300142", "530010805300083", "530010805300130", "530010805300132", "530010805300133", "530010805300138", "530010805300137", "530010805300141", "530010805300136", "530010805300140", "530010805300139", "530010805300134", "530010805300135", "530010805300121", "530010805300077", "530010805300100", "530010805300099", "530010805300144"),
  tract_2000   = "530010805300047",
  new_code  = "5300108053000471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120048 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120069", "530010805120087", "530010805120086", "530010805120095", "530010805120070", "530010805120066", "530010805120065"),
  tract_2000   = "530010805120048",
  new_code  = "5300108051200481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120051 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120062", "530010805120090"),
  tract_2000   = "530010805120051",
  new_code  = "5300108051200511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120052 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120091", "530010805120063"),
  tract_2000   = "530010805120052",
  new_code  = "5300108051200521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120053 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120093", "530010805120092", "530010805120064"),
  tract_2000   = "530010805120053",
  new_code  = "5300108051200531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120049 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120061", "530010805120089"),
  tract_2000   = "530010805120049",
  new_code  = "5300108051200491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120047 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120068", "530010805120101", "530010805120099"),
  tract_2000   = "530010805120047",
  new_code  = "5300108051200471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120046 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120067", "530010805120094", "530010805120102"),
  tract_2000   = "530010805120046",
  new_code  = "5300108051200461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100121 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100121", "530010805100282"),
  tract_2000   = "530010805100121",
  new_code  = "5300108051001211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100122 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100122", "530010805100283"),
  tract_2000   = "530010805100122",
  new_code  = "5300108051001221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100120 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100120", "530010805100146"),
  tract_2000   = "530010805100120",
  new_code  = "5300108051001201"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100154 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100230", "530010805100325"),
  tract_2000   = "530010805100154",
  new_code  = "5300108051001541"
)

# Merge the 2010 tracts that make up 2000 tract 530010805090055 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805090084", "530010805090093", "530010805090094", "530010805090083", "530010805090095", "530010805090099", "530010805090098", "530010805090064", "530010805090097", "530010805090096"),
  tract_2000   = "530010805090055",
  new_code  = "5300108050900551"
)

# Merge the 2010 tracts that make up 2000 tract 530010805090053 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805090076", "530010805090077", "530010805090065", "530010805090062", "530010805090075", "530010805090078", "530010805090080", "530010805090079", "530010805090081", "530010805090092", "530010805090067", "530010805090086", "530010805090087", "530010805090088", "530010805090068", "530010805090089", "530010805090069", "530010805090090"),
  tract_2000   = "530010805090053",
  new_code  = "5300108050900531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805090061 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805090074", "530010805090061"),
  tract_2000   = "530010805090061",
  new_code  = "5300108050900611"
)

# Merge the 2010 tracts that make up 2000 tract 530010805090060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805090060", "530010805090073"),
  tract_2000   = "530010805090060",
  new_code  = "5300108050900601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150391 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150468", "530010805150608", "530010805150607", "530010805150606", "530010805150467", "530010805150522", "530010805150459"),
  tract_2000   = "530010805150391",
  new_code  = "5300108051503911"
)

# Merge the 2010 tracts that make up 2000 tract 530010805090058 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805090085", "530010805090066", "530010805090072", "530010805090058", "530010805090101"),
  tract_2000   = "530010805090058",
  new_code  = "5300108050900581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805090057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805090071", "530010805090057"),
  tract_2000   = "530010805090057",
  new_code  = "5300108050900571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250115 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250195", "530010805250128", "530010805250193", "530010805250194"),
  tract_2000   = "530010805250115",
  new_code  = "5300108052501151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070143 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070207", "530010805070208", "530010805070217", "530010805070178", "530010805070219", "530010805070206", "530010805070161", "530010805070175", "530010805070215", "530010805070216"),
  tract_2000   = "530010805070143",
  new_code  = "5300108050701431"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070144 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070162", "530010805070209", "530010805070221", "530010805070220", "530010805070177", "530010805070176"),
  tract_2000   = "530010805070144",
  new_code  = "5300108050701441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070156 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070174", "530010805070214", "530010805070179", "530010805070218"),
  tract_2000   = "530010805070156",
  new_code  = "5300108050701561"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200074 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200176", "530010805200118", "530010805200180", "530010805200178"),
  tract_2000   = "530010805200074",
  new_code  = "5300108052000741"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200075 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200183", "530010805200075"),
  tract_2000   = "530010805200075",
  new_code  = "5300108052000751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130056 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130124", "530010805130123", "530010805130056"),
  tract_2000   = "530010805130056",
  new_code  = "5300108051300561"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130057", "530010805130125", "530010805130126"),
  tract_2000   = "530010805130057",
  new_code  = "5300108051300571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130022", "530010805130099", "530010805130107", "530010805130100", "530010805130101", "530010805130102", "530010805130103", "530010805130104", "530010805130108", "530010805130106", "530010805130105"),
  tract_2000   = "530010805130022",
  new_code  = "5300108051300221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130021 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130096", "530010805130095", "530010805130094", "530010805130093", "530010805130092", "530010805130091", "530010805130090", "530010805130089", "530010805130021", "530010805130086", "530010805130088", "530010805130087", "530010805130098", "530010805130097"),
  tract_2000   = "530010805130021",
  new_code  = "5300108051300211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140028 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140028", "530010805140063", "530010805140064"),
  tract_2000   = "530010805140028",
  new_code  = "5300108051400281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140029 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140053", "530010805140073", "530010805140065", "530010805140029"),
  tract_2000   = "530010805140029",
  new_code  = "5300108051400291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140030 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140030", "530010805140066"),
  tract_2000   = "530010805140030",
  new_code  = "5300108051400301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140034 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140034", "530010805140069"),
  tract_2000   = "530010805140034",
  new_code  = "5300108051400341"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140033 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140033", "530010805140068"),
  tract_2000   = "530010805140033",
  new_code  = "5300108051400331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140032 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140032", "530010805140067"),
  tract_2000   = "530010805140032",
  new_code  = "5300108051400321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230046 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230060", "530010805230061", "530010805230046"),
  tract_2000   = "530010805230046",
  new_code  = "5300108052300461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250120 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250199", "530010805250130", "530010805250127", "530010805250200", "530010805250201"),
  tract_2000   = "530010805250120",
  new_code  = "5300108052501201"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070151 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070169", "530010805070223", "530010805070222"),
  tract_2000   = "530010805070151",
  new_code  = "5300108050701511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250113 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250177", "530010805250178", "530010805250179", "530010805250113", "530010805250172", "530010805250173", "530010805250174", "530010805250175", "530010805250176", "530010805250123", "530010805250180", "530010805250181", "530010805250182", "530010805250183", "530010805250184", "530010805250185", "530010805250186", "530010805250187", "530010805250188"),
  tract_2000   = "530010805250113",
  new_code  = "5300108052501131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100135 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100207", "530010805100338", "530010805100337", "530010805100336", "530010805100335", "530010805100298", "530010805100206", "530010805100332", "530010805100333"),
  tract_2000   = "530010805100135",
  new_code  = "5300108051001351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100139-0139 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100235", "530010805100339", "530010805100340", "530010805100308", "530010805100211"),
  tract_2000   = "530010805100139-0139",
  new_code  = "530010805100139-01391"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100138-0138 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100210"),
  tract_2000   = "530010805100138-0138",
  new_code  = "530010805100138-01381"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100137-0137 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100300", "530010805100209", "530010805100299"),
  tract_2000   = "530010805100137-0137",
  new_code  = "530010805100137-01371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100136-0136 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100208", "530010805100341", "530010805100334"),
  tract_2000   = "530010805100136-0136",
  new_code  = "530010805100136-01361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230029 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230053", "530010805230029", "530010805230054", "530010805230052"),
  tract_2000   = "530010805230029",
  new_code  = "5300108052300291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060286 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060424", "530010805060449", "530010805060286", "530010805060446"),
  tract_2000   = "530010805060286",
  new_code  = "5300108050602861"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060288 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060288"),
  tract_2000   = "530010805060288",
  new_code  = "5300108050602881"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060287 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060287"),
  tract_2000   = "530010805060287",
  new_code  = "5300108050602871"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210070", "530010805210072", "530010805210058", "530010805210069", "530010805210022", "530010805210068", "530010805210071"),
  tract_2000   = "530010805210022",
  new_code  = "5300108052100221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210023 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210059", "530010805210056", "530010805210057", "530010805210055", "530010805210050", "530010805210074", "530010805210073", "530010805210023", "530010805210048", "530010805210047", "530010805210045", "530010805210044", "530010805210043", "530010805210046", "530010805210054", "530010805210053", "530010805210052", "530010805210051", "530010805210049"),
  tract_2000   = "530010805210023",
  new_code  = "5300108052100231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210025 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210025"),
  tract_2000   = "530010805210025",
  new_code  = "5300108052100251"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210026 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210026", "530010805210060"),
  tract_2000   = "530010805210026",
  new_code  = "5300108052100261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210027 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210075", "530010805210027", "530010805210061"),
  tract_2000   = "530010805210027",
  new_code  = "5300108052100271"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210028 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210028", "530010805210035"),
  tract_2000   = "530010805210028",
  new_code  = "5300108052100281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210029 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210029"),
  tract_2000   = "530010805210029",
  new_code  = "5300108052100291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210030 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210036", "530010805210030"),
  tract_2000   = "530010805210030",
  new_code  = "5300108052100301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100123 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100327", "530010805100123", "530010805100326", "530010805100159", "530010805100286", "530010805100160", "530010805100161", "530010805100162", "530010805100163", "530010805100164", "530010805100165", "530010805100166", "530010805100158", "530010805100157", "530010805100156", "530010805100155", "530010805100154", "530010805100153", "530010805100152", "530010805100147", "530010805100148", "530010805100150", "530010805100149", "530010805100149", "530010805100169", "530010805100287", "530010805100187", "530010805100192", "530010805100193", "530010805100194", "530010805100294", "530010805100295", "530010805100293", "530010805100190", "530010805100292", "530010805100189", "530010805100291", "530010805100188", "530010805100290", "530010805100167", "530010805100168", "530010805100186", "530010805100185", "530010805100184", "530010805100183", "530010805100191", "530010805100289", "530010805100174", "530010805100173", "530010805100172", "530010805100170", "530010805100171", "530010805100288", "530010805100178", "530010805100176", "530010805100175", "530010805100177", "530010805100182", "530010805100181", "530010805100180", "530010805100179", "530010805100195", "530010805100296", "530010805100328"),
  tract_2000   = "530010805100123",
  new_code  = "5300108051001231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100118 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100118", "530010805100145", "530010805100285"),
  tract_2000   = "530010805100118",
  new_code  = "5300108051001181"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100133 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100197", "530010805100196"),
  tract_2000   = "530010805100133",
  new_code  = "5300108051001331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100117 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100117", "530010805100144", "530010805100143", "530010805100142", "530010805100141", "530010805100140", "530010805100284", "530010805100139", "530010805100138", "530010805100137"),
  tract_2000   = "530010805100117",
  new_code  = "5300108051001171"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100106 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100272", "530010805100271", "530010805100270", "530010805100269", "530010805100106"),
  tract_2000   = "530010805100106",
  new_code  = "5300108051001061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100105 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100105", "530010805100268"),
  tract_2000   = "530010805100105",
  new_code  = "5300108051001051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100147-0147 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100219", "530010805100310", "530010805100309", "530010805100306", "530010805100227", "530010805100223", "530010805100222", "530010805100226", "530010805100305", "530010805100314", "530010805100221", "530010805100220", "530010805100304"),
  tract_2000   = "530010805100147-0147",
  new_code  = "530010805100147-01471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100145 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100321", "530010805100217", "530010805100322", "530010805100323", "530010805100324"),
  tract_2000   = "530010805100145",
  new_code  = "5300108051001451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100146 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100218", "530010805100303"),
  tract_2000   = "530010805100146",
  new_code  = "5300108051001461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100144 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100302", "530010805100216", "530010805100215", "530010805100301", "530010805100234", "530010805100307"),
  tract_2000   = "530010805100144",
  new_code  = "5300108051001441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100141 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100331", "530010805100213"),
  tract_2000   = "530010805100141",
  new_code  = "5300108051001411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100132 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100203", "530010805100317"),
  tract_2000   = "530010805100132",
  new_code  = "5300108051001321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100129 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100311", "530010805100312", "530010805100200"),
  tract_2000   = "530010805100129",
  new_code  = "5300108051001291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100127 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100318", "530010805100198", "530010805100199"),
  tract_2000   = "530010805100127",
  new_code  = "5300108051001271"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100126 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100329", "530010805100205"),
  tract_2000   = "530010805100126",
  new_code  = "5300108051001261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210021 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210067", "530010805210021", "530010805210066"),
  tract_2000   = "530010805210021",
  new_code  = "5300108052100211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230041 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230041", "530010805230059", "530010805230058"),
  tract_2000   = "530010805230041",
  new_code  = "5300108052300411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230040 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230040", "530010805230057"),
  tract_2000   = "530010805230040",
  new_code  = "5300108052300401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230035 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230035", "530010805230056"),
  tract_2000   = "530010805230035",
  new_code  = "5300108052300351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230026 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230026", "530010805230051", "530010805230027", "530010805230028"),
  tract_2000   = "530010805230026",
  new_code  = "5300108052300261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230023 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230023", "530010805230050"),
  tract_2000   = "530010805230023",
  new_code  = "5300108052300231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060269 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060445", "530010805060441", "530010805060442", "530010805060443", "530010805060444", "530010805060440", "530010805060439", "530010805060437", "530010805060438", "530010805060436", "530010805060269"),
  tract_2000   = "530010805060269",
  new_code  = "5300108050602691"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060267 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060267", "530010805060359"),
  tract_2000   = "530010805060267",
  new_code  = "5300108050602671"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060268 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060268", "530010805060423"),
  tract_2000   = "530010805060268",
  new_code  = "5300108050602681"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210010 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210010", "530010805210032"),
  tract_2000   = "530010805210010",
  new_code  = "5300108052100101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210011 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210011", "530010805210038"),
  tract_2000   = "530010805210011",
  new_code  = "5300108052100111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210009 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210031", "530010805210009"),
  tract_2000   = "530010805210009",
  new_code  = "5300108052100091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210013", "530010805210039"),
  tract_2000   = "530010805210013",
  new_code  = "5300108052100131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210015 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210015", "530010805210040"),
  tract_2000   = "530010805210015",
  new_code  = "5300108052100151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210017 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210017", "530010805210033"),
  tract_2000   = "530010805210017",
  new_code  = "5300108052100171"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210019 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210062", "530010805210063", "530010805210064", "530010805210065", "530010805210019", "530010805210034"),
  tract_2000   = "530010805210019",
  new_code  = "5300108052100191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805210002 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805210002", "530010805210037"),
  tract_2000   = "530010805210002",
  new_code  = "5300108052100021"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060284 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060447", "530010805060284"),
  tract_2000   = "530010805060284",
  new_code  = "5300108050602841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080242 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080656", "530010805080242"),
  tract_2000   = "530010805080242",
  new_code  = "5300108050802421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160115 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160115", "530010805160217", "530010805160218"),
  tract_2000   = "530010805160115",
  new_code  = "5300108051601151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160140 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160208", "530010805160140", "530010805160203", "530010805160204", "530010805160206", "530010805160207", "530010805160205"),
  tract_2000   = "530010805160140",
  new_code  = "5300108051601401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160112 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160112", "530010805160173", "530010805160215", "530010805160174"),
  tract_2000   = "530010805160112",
  new_code  = "5300108051601121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160113 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160113", "530010805160220", "530010805160194", "530010805160193", "530010805160195", "530010805160192"),
  tract_2000   = "530010805160113",
  new_code  = "5300108051601131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080239 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080727", "530010805080515", "530010805080516", "530010805080726", "530010805080239", "530010805080654", "530010805080514", "530010805080702"),
  tract_2000   = "530010805080239",
  new_code  = "5300108050802391"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080486 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080707", "530010805080694", "530010805080486", "530010805080708", "530010805080709", "530010805080547"),
  tract_2000   = "530010805080486",
  new_code  = "5300108050804861"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080238 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080725", "530010805080724", "530010805080513", "530010805080653", "530010805080238"),
  tract_2000   = "530010805080238",
  new_code  = "5300108050802381"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080237 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080511", "530010805080722", "530010805080237", "530010805080512", "530010805080652", "530010805080723"),
  tract_2000   = "530010805080237",
  new_code  = "5300108050802371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080485 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080543", "530010805080544", "530010805080706", "530010805080729", "530010805080485", "530010805080545", "530010805080546"),
  tract_2000   = "530010805080485",
  new_code  = "5300108050804851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080487 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080549", "530010805080712", "530010805080695", "530010805080548", "530010805080711", "530010805080710", "530010805080487"),
  tract_2000   = "530010805080487",
  new_code  = "5300108050804871"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080488 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080488", "530010805080550", "530010805080551"),
  tract_2000   = "530010805080488",
  new_code  = "5300108050804881"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080489 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080553", "530010805080730", "530010805080552", "530010805080696", "530010805080489"),
  tract_2000   = "530010805080489",
  new_code  = "5300108050804891"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080241 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080241", "530010805080655", "530010805080518", "530010805080703"),
  tract_2000   = "530010805080241",
  new_code  = "5300108050802411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080235 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080509", "530010805080508", "530010805080507", "530010805080505", "530010805080506", "530010805080651", "530010805080235"),
  tract_2000   = "530010805080235",
  new_code  = "5300108050802351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080481 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080481", "530010805080542"),
  tract_2000   = "530010805080481",
  new_code  = "5300108050804811"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080478 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080478", "530010805080539", "530010805080693"),
  tract_2000   = "530010805080478",
  new_code  = "5300108050804781"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080234 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080234", "530010805080504", "530010805080501", "530010805080502"),
  tract_2000   = "530010805080234",
  new_code  = "5300108050802341"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080479 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080540", "530010805080479", "530010805080503", "530010805080739"),
  tract_2000   = "530010805080479",
  new_code  = "5300108050804791"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080480 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080541", "530010805080480"),
  tract_2000   = "530010805080480",
  new_code  = "5300108050804801"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080236 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080718", "530010805080236", "530010805080510", "530010805080701"),
  tract_2000   = "530010805080236",
  new_code  = "5300108050802361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080483 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080492", "530010805080483"),
  tract_2000   = "530010805080483",
  new_code  = "5300108050804831"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080484 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080735", "530010805080484", "530010805080738", "530010805080737", "530010805080736"),
  tract_2000   = "530010805080484",
  new_code  = "5300108050804841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140041 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140041", "530010805140071"),
  tract_2000   = "530010805140041",
  new_code  = "5300108051400411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080176 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080635", "530010805080176"),
  tract_2000   = "530010805080176",
  new_code  = "5300108050801761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140042 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140042", "530010805140072"),
  tract_2000   = "530010805140042",
  new_code  = "5300108051400421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080033 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080567", "530010805080566", "530010805080033"),
  tract_2000   = "530010805080033",
  new_code  = "5300108050800331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080032 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080032", "530010805080564", "530010805080565"),
  tract_2000   = "530010805080032",
  new_code  = "5300108050800321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080034 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080034", "530010805080568", "530010805080569"),
  tract_2000   = "530010805080034",
  new_code  = "5300108050800341"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080035 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080035", "530010805080570"),
  tract_2000   = "530010805080035",
  new_code  = "5300108050800351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080036 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080572", "530010805080571", "530010805080036"),
  tract_2000   = "530010805080036",
  new_code  = "5300108050800361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080037 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080573", "530010805080037"),
  tract_2000   = "530010805080037",
  new_code  = "5300108050800371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080038 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080495", "530010805080699", "530010805080574", "530010805080038"),
  tract_2000   = "530010805080038",
  new_code  = "5300108050800381"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080040 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080040", "530010805080577", "530010805080576", "530010805080578"),
  tract_2000   = "530010805080040",
  new_code  = "5300108050800401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080041 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080041", "530010805080497"),
  tract_2000   = "530010805080041",
  new_code  = "5300108050800411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080042 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080042", "530010805080579"),
  tract_2000   = "530010805080042",
  new_code  = "5300108050800421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140019 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140019", "530010805140056", "530010805140057"),
  tract_2000   = "530010805140019",
  new_code  = "5300108051400191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080491 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080734", "530010805080733", "530010805080732", "530010805080731"),
  tract_2000   = "530010805080491",
  new_code  = "5300108050804911"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080476 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080491", "530010805080476"),
  tract_2000   = "530010805080476",
  new_code  = "5300108050804761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080490 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080490", "530010805080697"),
  tract_2000   = "530010805080490",
  new_code  = "5300108050804901"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080039 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080039", "530010805080575", "530010805080496", "530010805080700"),
  tract_2000   = "530010805080039",
  new_code  = "5300108050800391"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080559", "530010805080560", "530010805080558", "530010805080013"),
  tract_2000   = "530010805080013",
  new_code  = "5300108050800131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080030 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080563", "530010805080030"),
  tract_2000   = "530010805080030",
  new_code  = "5300108050800301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110145 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110268", "530010805110145"),
  tract_2000   = "530010805110145",
  new_code  = "5300108051101451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110144 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110267", "530010805110144"),
  tract_2000   = "530010805110144",
  new_code  = "5300108051101441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110157 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110273", "530010805110157", "530010805110272"),
  tract_2000   = "530010805110157",
  new_code  = "5300108051101571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110146 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110146", "530010805110269"),
  tract_2000   = "530010805110146",
  new_code  = "5300108051101461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110142 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110266", "530010805110142", "530010805110264", "530010805110265"),
  tract_2000   = "530010805110142",
  new_code  = "5300108051101421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110174 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110200", "530010805110281", "530010805110280"),
  tract_2000   = "530010805110174",
  new_code  = "5300108051101741"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110112 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110112", "530010805110245", "530010805110246"),
  tract_2000   = "530010805110112",
  new_code  = "5300108051101121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110127 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110127", "530010805110255"),
  tract_2000   = "530010805110127",
  new_code  = "5300108051101271"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110128 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110256", "530010805110128"),
  tract_2000   = "530010805110128",
  new_code  = "5300108051101281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110130 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110130", "530010805110258"),
  tract_2000   = "530010805110130",
  new_code  = "5300108051101301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110131 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110131", "530010805110259"),
  tract_2000   = "530010805110131",
  new_code  = "5300108051101311"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110101 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110101", "530010805110242"),
  tract_2000   = "530010805110101",
  new_code  = "5300108051101011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110104 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110104", "530010805110244"),
  tract_2000   = "530010805110104",
  new_code  = "5300108051101041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110100 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110241", "530010805110100"),
  tract_2000   = "530010805110100",
  new_code  = "5300108051101001"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110099 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110239", "530010805110240", "530010805110099"),
  tract_2000   = "530010805110099",
  new_code  = "5300108051100991"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110084 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110084", "530010805110234"),
  tract_2000   = "530010805110084",
  new_code  = "5300108051100841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110081 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110231", "530010805110081", "530010805110232"),
  tract_2000   = "530010805110081",
  new_code  = "5300108051100811"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110001 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110001", "530010805110206", "530010805110207"),
  tract_2000   = "530010805110001",
  new_code  = "5300108051100011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110019 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110161", "530010805110019"),
  tract_2000   = "530010805110019",
  new_code  = "5300108051100191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070139 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070157", "530010805070139"),
  tract_2000   = "530010805070139",
  new_code  = "5300108050701391"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070137 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070199", "530010805070137"),
  tract_2000   = "530010805070137",
  new_code  = "5300108050701371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070136 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070156", "530010805070136"),
  tract_2000   = "530010805070136",
  new_code  = "5300108050701361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070130 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070155", "530010805070130"),
  tract_2000   = "530010805070130",
  new_code  = "5300108050701301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070127 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070127", "530010805070198"),
  tract_2000   = "530010805070127",
  new_code  = "5300108050701271"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070126 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070154", "530010805070126"),
  tract_2000   = "530010805070126",
  new_code  = "5300108050701261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070121 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070153", "530010805070121"),
  tract_2000   = "530010805070121",
  new_code  = "5300108050701211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070119 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070152", "530010805070119"),
  tract_2000   = "530010805070119",
  new_code  = "5300108050701191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070118 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070118", "530010805070151"),
  tract_2000   = "530010805070118",
  new_code  = "5300108050701181"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070110 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070110", "530010805070226"),
  tract_2000   = "530010805070110",
  new_code  = "5300108050701101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070109 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070109", "530010805070150", "530010805070204"),
  tract_2000   = "530010805070109",
  new_code  = "5300108050701091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070098 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070098", "530010805070196"),
  tract_2000   = "530010805070098",
  new_code  = "5300108050700981"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070097 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070097", "530010805070195"),
  tract_2000   = "530010805070097",
  new_code  = "5300108050700971"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070088 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070088", "530010805070193"),
  tract_2000   = "530010805070088",
  new_code  = "5300108050700881"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070087 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070087", "530010805070192"),
  tract_2000   = "530010805070087",
  new_code  = "5300108050700871"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070081 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070081", "530010805070191"),
  tract_2000   = "530010805070081",
  new_code  = "5300108050700811"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070080 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070080", "530010805070190"),
  tract_2000   = "530010805070080",
  new_code  = "5300108050700801"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070079 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070079", "530010805070189"),
  tract_2000   = "530010805070079",
  new_code  = "5300108050700791"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070071 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070071", "530010805070146", "530010805070203"),
  tract_2000   = "530010805070071",
  new_code  = "5300108050700711"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070070 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070070", "530010805070188"),
  tract_2000   = "530010805070070",
  new_code  = "5300108050700701"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070077 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070077", "530010805070147"),
  tract_2000   = "530010805070077",
  new_code  = "5300108050700771"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070063 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070063", "530010805070143", "530010805070200", "530010805070186", "530010805070201"),
  tract_2000   = "530010805070063",
  new_code  = "5300108050700631"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070064 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070064", "530010805070144"),
  tract_2000   = "530010805070064",
  new_code  = "5300108050700641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070058 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070058", "530010805070184"),
  tract_2000   = "530010805070058",
  new_code  = "5300108050700581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070059 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070185", "530010805070059"),
  tract_2000   = "530010805070059",
  new_code  = "5300108050700591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070158", "530010805070060"),
  tract_2000   = "530010805070060",
  new_code  = "5300108050700601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070045 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070142", "530010805070045"),
  tract_2000   = "530010805070045",
  new_code  = "5300108050700451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070004 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070004", "530010805070181"),
  tract_2000   = "530010805070004",
  new_code  = "5300108050700041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070007 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070007", "530010805070182"),
  tract_2000   = "530010805070007",
  new_code  = "5300108050700071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070003 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070003", "530010805070180"),
  tract_2000   = "530010805070003",
  new_code  = "5300108050700031"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070142 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070159", "530010805070160", "530010805070205"),
  tract_2000   = "530010805070142",
  new_code  = "5300108050701421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250005 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250005", "530010805250131"),
  tract_2000   = "530010805250005",
  new_code  = "5300108052500051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250016 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250016", "530010805250140"),
  tract_2000   = "530010805250016",
  new_code  = "5300108052500161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250012", "530010805250136"),
  tract_2000   = "530010805250012",
  new_code  = "5300108052500121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250013", "530010805250137"),
  tract_2000   = "530010805250013",
  new_code  = "5300108052500131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250014 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250014", "530010805250138"),
  tract_2000   = "530010805250014",
  new_code  = "5300108052500141"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250010 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250010", "530010805250135"),
  tract_2000   = "530010805250010",
  new_code  = "5300108052500101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250007 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250007", "530010805250132"),
  tract_2000   = "530010805250007",
  new_code  = "5300108052500071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250008 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250008", "530010805250133"),
  tract_2000   = "530010805250008",
  new_code  = "5300108052500081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250021 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250021", "530010805250143"),
  tract_2000   = "530010805250021",
  new_code  = "5300108052500211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250026 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250026", "530010805250146"),
  tract_2000   = "530010805250026",
  new_code  = "5300108052500261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250025 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250145", "530010805250025"),
  tract_2000   = "530010805250025",
  new_code  = "5300108052500251"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250035 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250035", "530010805250147"),
  tract_2000   = "530010805250035",
  new_code  = "5300108052500351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250054 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250054", "530010805250156", "530010805250157", "530010805250158", "530010805250159", "530010805250160"),
  tract_2000   = "530010805250054",
  new_code  = "5300108052500541"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250052 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250052", "530010805250114"),
  tract_2000   = "530010805250052",
  new_code  = "5300108052500521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250057", "530010805250161", "530010805250162"),
  tract_2000   = "530010805250057",
  new_code  = "5300108052500571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250058 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250058", "530010805250163"),
  tract_2000   = "530010805250058",
  new_code  = "5300108052500581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250065 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250065", "530010805250115"),
  tract_2000   = "530010805250065",
  new_code  = "5300108052500651"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250064 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250064", "530010805250164"),
  tract_2000   = "530010805250064",
  new_code  = "5300108052500641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250073 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250117", "530010805250073"),
  tract_2000   = "530010805250073",
  new_code  = "5300108052500731"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250072 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250072", "530010805250116"),
  tract_2000   = "530010805250072",
  new_code  = "5300108052500721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250085 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250085", "530010805250119"),
  tract_2000   = "530010805250085",
  new_code  = "5300108052500851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250091 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250091", "530010805250120"),
  tract_2000   = "530010805250091",
  new_code  = "5300108052500911"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250092 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250092", "530010805250198"),
  tract_2000   = "530010805250092",
  new_code  = "5300108052500921"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250094 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250094", "530010805250121"),
  tract_2000   = "530010805250094",
  new_code  = "5300108052500941"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250095 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250095", "530010805250122"),
  tract_2000   = "530010805250095",
  new_code  = "5300108052500951"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250084 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250084", "530010805250118"),
  tract_2000   = "530010805250084",
  new_code  = "5300108052500841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250082 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250082", "530010805250169"),
  tract_2000   = "530010805250082",
  new_code  = "5300108052500821"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250097 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250097", "530010805250170"),
  tract_2000   = "530010805250097",
  new_code  = "5300108052500971"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250098 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250098", "530010805250171"),
  tract_2000   = "530010805250098",
  new_code  = "5300108052500981"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250079 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250168", "530010805250079"),
  tract_2000   = "530010805250079",
  new_code  = "5300108052500791"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250075 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250166", "530010805250075"),
  tract_2000   = "530010805250075",
  new_code  = "5300108052500751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250076 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250167", "530010805250076"),
  tract_2000   = "530010805250076",
  new_code  = "5300108052500761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250069 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250069", "530010805250165"),
  tract_2000   = "530010805250069",
  new_code  = "5300108052500691"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250049 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250049", "530010805250155"),
  tract_2000   = "530010805250049",
  new_code  = "5300108052500491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250048 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250048", "530010805250154"),
  tract_2000   = "530010805250048",
  new_code  = "5300108052500481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250047 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250047", "530010805250153"),
  tract_2000   = "530010805250047",
  new_code  = "5300108052500471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250043 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250043", "530010805250149"),
  tract_2000   = "530010805250043",
  new_code  = "5300108052500431"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250046 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250046", "530010805250152"),
  tract_2000   = "530010805250046",
  new_code  = "5300108052500461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250045 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250045", "530010805250151"),
  tract_2000   = "530010805250045",
  new_code  = "5300108052500451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250044 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250044", "530010805250150"),
  tract_2000   = "530010805250044",
  new_code  = "5300108052500441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250040 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250040", "530010805250148"),
  tract_2000   = "530010805250040",
  new_code  = "5300108052500401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250022", "530010805250144"),
  tract_2000   = "530010805250022",
  new_code  = "5300108052500221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250020 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250020", "530010805250142"),
  tract_2000   = "530010805250020",
  new_code  = "5300108052500201"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250019 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250019", "530010805250141"),
  tract_2000   = "530010805250019",
  new_code  = "5300108052500191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250015 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250015", "530010805250139"),
  tract_2000   = "530010805250015",
  new_code  = "5300108052500151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805250009 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805250009", "530010805250134"),
  tract_2000   = "530010805250009",
  new_code  = "5300108052500091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300040 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300070", "530010805300040"),
  tract_2000   = "530010805300040",
  new_code  = "5300108053000401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300042 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300071", "530010805300072", "530010805300042"),
  tract_2000   = "530010805300042",
  new_code  = "5300108053000421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300043 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300074", "530010805300073", "530010805300043"),
  tract_2000   = "530010805300043",
  new_code  = "5300108053000431"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300044 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300044", "530010805300097"),
  tract_2000   = "530010805300044",
  new_code  = "5300108053000441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300045 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300075", "530010805300045"),
  tract_2000   = "530010805300045",
  new_code  = "5300108053000451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300036 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300036", "530010805300068", "530010805300098"),
  tract_2000   = "530010805300036",
  new_code  = "5300108053000361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300035 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300035", "530010805300067"),
  tract_2000   = "530010805300035",
  new_code  = "5300108053000351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300034 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300034", "530010805300066"),
  tract_2000   = "530010805300034",
  new_code  = "5300108053000341"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300033 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300065", "530010805300033"),
  tract_2000   = "530010805300033",
  new_code  = "5300108053000331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300032 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300064", "530010805300032"),
  tract_2000   = "530010805300032",
  new_code  = "5300108053000321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300031 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300031", "530010805300096", "530010805300063"),
  tract_2000   = "530010805300031",
  new_code  = "5300108053000311"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300030 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300030", "530010805300062"),
  tract_2000   = "530010805300030",
  new_code  = "5300108053000301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300029 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300029", "530010805300061"),
  tract_2000   = "530010805300029",
  new_code  = "5300108053000291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300023 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300023", "530010805300059"),
  tract_2000   = "530010805300023",
  new_code  = "5300108053000231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300026 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300060", "530010805300026"),
  tract_2000   = "530010805300026",
  new_code  = "5300108053000261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300010 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300010", "530010805300054"),
  tract_2000   = "530010805300010",
  new_code  = "5300108053000101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300008 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300094", "530010805300008"),
  tract_2000   = "530010805300008",
  new_code  = "5300108053000081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300007 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300007", "530010805300053"),
  tract_2000   = "530010805300007",
  new_code  = "5300108053000071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300004 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300004", "530010805300052"),
  tract_2000   = "530010805300004",
  new_code  = "5300108053000041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300055", "530010805300013"),
  tract_2000   = "530010805300013",
  new_code  = "5300108053000131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300015 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300056", "530010805300015"),
  tract_2000   = "530010805300015",
  new_code  = "5300108053000151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300016 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300016", "530010805300095"),
  tract_2000   = "530010805300016",
  new_code  = "5300108053000161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300022", "530010805300058"),
  tract_2000   = "530010805300022",
  new_code  = "5300108053000221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300019 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300019", "530010805300057"),
  tract_2000   = "530010805300019",
  new_code  = "5300108053000191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300003 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300051", "530010805300050", "530010805300049", "530010805300003"),
  tract_2000   = "530010805300003",
  new_code  = "5300108053000031"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300002 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300048", "530010805300002"),
  tract_2000   = "530010805300002",
  new_code  = "5300108053000021"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230013", "530010805230049"),
  tract_2000   = "530010805230013",
  new_code  = "5300108052300131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230012", "530010805230048"),
  tract_2000   = "530010805230012",
  new_code  = "5300108052300121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805230010 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805230010", "530010805230047"),
  tract_2000   = "530010805230010",
  new_code  = "5300108052300101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060282 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060282", "530010805060376"),
  tract_2000   = "530010805060282",
  new_code  = "5300108050602821"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060265 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060358", "530010805060265"),
  tract_2000   = "530010805060265",
  new_code  = "5300108050602651"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100113 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100279", "530010805100278", "530010805100275", "530010805100113"),
  tract_2000   = "530010805100113",
  new_code  = "5300108051001131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100111 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100111", "530010805100136"),
  tract_2000   = "530010805100111",
  new_code  = "5300108051001111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100116 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100116", "530010805100281"),
  tract_2000   = "530010805100116",
  new_code  = "5300108051001161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100115 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100280", "530010805100115"),
  tract_2000   = "530010805100115",
  new_code  = "5300108051001151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100108 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100274", "530010805100273", "530010805100108"),
  tract_2000   = "530010805100108",
  new_code  = "5300108051001081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100104 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100104", "530010805100266", "530010805100267"),
  tract_2000   = "530010805100104",
  new_code  = "5300108051001041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100104 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100264", "530010805100265", "530010805100263", "530010805100103"),
  tract_2000   = "530010805100103",
  new_code  = "5300108051001031"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100102 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100102", "530010805100260", "530010805100261", "530010805100262"),
  tract_2000   = "530010805100102",
  new_code  = "5300108051001021"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100082 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100134", "530010805100082"),
  tract_2000   = "530010805100082",
  new_code  = "5300108051000821"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100083 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100083", "530010805100251", "530010805100252", "530010805100253"),
  tract_2000   = "530010805100083",
  new_code  = "5300108051000831"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100092 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100092", "530010805100255"),
  tract_2000   = "530010805100092",
  new_code  = "5300108051000921"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100093 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100093", "530010805100256"),
  tract_2000   = "530010805100093",
  new_code  = "5300108051000931"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100096 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100096", "530010805100257"),
  tract_2000   = "530010805100096",
  new_code  = "5300108051000961"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100098 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100135", "530010805100098"),
  tract_2000   = "530010805100098",
  new_code  = "5300108051000981"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100100 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100100", "530010805100259", "530010805100258"),
  tract_2000   = "530010805100100",
  new_code  = "5300108051001001"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100079 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100079", "530010805100250"),
  tract_2000   = "530010805100079",
  new_code  = "5300108051000791"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100072 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100072", "530010805100249"),
  tract_2000   = "530010805100072",
  new_code  = "5300108051000721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100068 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100068", "530010805100248"),
  tract_2000   = "530010805100068",
  new_code  = "5300108051000681"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100069 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100069", "530010805100132", "530010805100133"),
  tract_2000   = "530010805100069",
  new_code  = "5300108051000691"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100067 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100067", "530010805100319", "530010805100320"),
  tract_2000   = "530010805100067",
  new_code  = "5300108051000671"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100066 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100066", "530010805100131", "530010805100247"),
  tract_2000   = "530010805100066",
  new_code  = "5300108051000661"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100065 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100065", "530010805100246", "530010805100130"),
  tract_2000   = "530010805100065",
  new_code  = "5300108051000651"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100049 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100049", "530010805100126", "530010805100313", "530010805100225", "530010805100224", "530010805100127"),
  tract_2000   = "530010805100049",
  new_code  = "5300108051000491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100048 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100241", "530010805100048"),
  tract_2000   = "530010805100048",
  new_code  = "5300108051000481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100051 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100242", "530010805100051"),
  tract_2000   = "530010805100051",
  new_code  = "5300108051000511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100053 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100053", "530010805100243"),
  tract_2000   = "530010805100053",
  new_code  = "5300108051000531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100054 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100244", "530010805100054"),
  tract_2000   = "530010805100054",
  new_code  = "5300108051000541"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100055 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100055", "530010805100315", "530010805100316"),
  tract_2000   = "530010805100055",
  new_code  = "5300108051000551"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100060", "530010805100245"),
  tract_2000   = "530010805100060",
  new_code  = "5300108051000601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100059 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100059", "530010805100129", "530010805100128"),
  tract_2000   = "530010805100059",
  new_code  = "5300108051000591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100015 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100237", "530010805100015"),
  tract_2000   = "530010805100015",
  new_code  = "5300108051000151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100006 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100236", "530010805100006"),
  tract_2000   = "530010805100006",
  new_code  = "5300108051000061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100016 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100238", "530010805100016"),
  tract_2000   = "530010805100016",
  new_code  = "5300108051000161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100027 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100240", "530010805100027"),
  tract_2000   = "530010805100027",
  new_code  = "5300108051000271"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100017 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100017", "530010805100125"),
  tract_2000   = "530010805100017",
  new_code  = "5300108051000171"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110096 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110096", "530010805110158"),
  tract_2000   = "530010805110096",
  new_code  = "5300108051100961"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110095 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110095", "530010805110238"),
  tract_2000   = "530010805110095",
  new_code  = "5300108051100951"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110092 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110092", "530010805110286"),
  tract_2000   = "530010805110092",
  new_code  = "5300108051100921"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110091 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110091", "530010805110173", "530010805110237"),
  tract_2000   = "530010805110091",
  new_code  = "5300108051100911"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110089 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110089", "530010805110171"),
  tract_2000   = "530010805110089",
  new_code  = "5300108051100891"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110090 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110090", "530010805110172"),
  tract_2000   = "530010805110090",
  new_code  = "5300108051100901"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110088 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110088", "530010805110170"),
  tract_2000   = "530010805110088",
  new_code  = "5300108051100881"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110087 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110087", "530010805110236"),
  tract_2000   = "530010805110087",
  new_code  = "5300108051100871"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110086 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110086", "530010805110235"),
  tract_2000   = "530010805110086",
  new_code  = "5300108051100861"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110085 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110085", "530010805110169"),
  tract_2000   = "530010805110085",
  new_code  = "5300108051100851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110082 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110082", "530010805110168"),
  tract_2000   = "530010805110082",
  new_code  = "5300108051100821"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110083 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110083", "530010805110233"),
  tract_2000   = "530010805110083",
  new_code  = "5300108051100831"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110080 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110080", "530010805110230"),
  tract_2000   = "530010805110080",
  new_code  = "5300108051100801"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110076 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110076", "530010805110229"),
  tract_2000   = "530010805110076",
  new_code  = "5300108051100761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110074 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110074", "530010805110227"),
  tract_2000   = "530010805110074",
  new_code  = "5300108051100741"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110075 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110075", "530010805110228"),
  tract_2000   = "530010805110075",
  new_code  = "5300108051100751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110073 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110073", "530010805110167", "530010805110226"),
  tract_2000   = "530010805110073",
  new_code  = "5300108051100731"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110072 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110072", "530010805110166"),
  tract_2000   = "530010805110072",
  new_code  = "5300108051100721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110070 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110070", "530010805110225"),
  tract_2000   = "530010805110070",
  new_code  = "5300108051100701"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110067 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110165", "530010805110275", "530010805110067"),
  tract_2000   = "530010805110067",
  new_code  = "5300108051100671"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110012", "530010805110214"),
  tract_2000   = "530010805110012",
  new_code  = "5300108051100121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110009 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110009", "530010805110212"),
  tract_2000   = "530010805110009",
  new_code  = "5300108051100091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110013", "530010805110215"),
  tract_2000   = "530010805110013",
  new_code  = "5300108051100131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110011 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110011", "530010805110213"),
  tract_2000   = "530010805110011",
  new_code  = "5300108051100111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110008 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110008", "530010805110211"),
  tract_2000   = "530010805110008",
  new_code  = "5300108051100081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110007 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110007", "530010805110210"),
  tract_2000   = "530010805110007",
  new_code  = "5300108051100071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110005 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110005", "530010805110209"),
  tract_2000   = "530010805110005",
  new_code  = "5300108051100051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110002 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110002", "530010805110208"),
  tract_2000   = "530010805110002",
  new_code  = "5300108051100021"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110035 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110035", "530010805110162"),
  tract_2000   = "530010805110035",
  new_code  = "5300108051100351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110036 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110036", "530010805110217"),
  tract_2000   = "530010805110036",
  new_code  = "5300108051100361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110041 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110041", "530010805110218", "530010805110219"),
  tract_2000   = "530010805110041",
  new_code  = "5300108051100411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110042 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110042", "530010805110220"),
  tract_2000   = "530010805110042",
  new_code  = "5300108051100421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110043 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110043", "530010805110163"),
  tract_2000   = "530010805110043",
  new_code  = "5300108051100431"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110048 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110048", "530010805110164"),
  tract_2000   = "530010805110048",
  new_code  = "5300108051100481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110063 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110063", "530010805110284"),
  tract_2000   = "530010805110063",
  new_code  = "5300108051100631"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110116 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110116", "530010805110159"),
  tract_2000   = "530010805110116",
  new_code  = "5300108051101161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110117 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110117", "530010805110160", "530010805110247", "530010805110274"),
  tract_2000   = "530010805110117",
  new_code  = "5300108051101171"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110123 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110123", "530010805110251"),
  tract_2000   = "530010805110123",
  new_code  = "5300108051101231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110124 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110124", "530010805110252"),
  tract_2000   = "530010805110124",
  new_code  = "5300108051101241"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110119 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110119", "530010805110248"),
  tract_2000   = "530010805110119",
  new_code  = "5300108051101191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110140 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110140", "530010805110263"),
  tract_2000   = "530010805110140",
  new_code  = "5300108051101401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110137 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110137", "530010805110262"),
  tract_2000   = "530010805110137",
  new_code  = "5300108051101371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110136 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110136", "530010805110261"),
  tract_2000   = "530010805110136",
  new_code  = "5300108051101361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110133 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110133", "530010805110260"),
  tract_2000   = "530010805110133",
  new_code  = "5300108051101331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110061 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110061", "530010805110224"),
  tract_2000   = "530010805110061",
  new_code  = "5300108051100611"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110057", "530010805110222"),
  tract_2000   = "530010805110057",
  new_code  = "5300108051100571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110055 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110055", "530010805110221"),
  tract_2000   = "530010805110055",
  new_code  = "5300108051100551"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110148 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110148", "530010805110270"),
  tract_2000   = "530010805110148",
  new_code  = "5300108051101481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110149 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110149", "530010805110271"),
  tract_2000   = "530010805110149",
  new_code  = "5300108051101491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120001 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120001", "530010805120046"),
  tract_2000   = "530010805120001",
  new_code  = "5300108051200011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120004 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120004", "530010805120047"),
  tract_2000   = "530010805120004",
  new_code  = "5300108051200041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120006 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120049", "530010805120050", "530010805120006"),
  tract_2000   = "530010805120006",
  new_code  = "5300108051200061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120008 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120008", "530010805120051"),
  tract_2000   = "530010805120008",
  new_code  = "5300108051200081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120010 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120010", "530010805120052"),
  tract_2000   = "530010805120010",
  new_code  = "5300108051200101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120012", "530010805120054"),
  tract_2000   = "530010805120012",
  new_code  = "5300108051200121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120011 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120053", "530010805120011"),
  tract_2000   = "530010805120011",
  new_code  = "5300108051200111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120018 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120018", "530010805120055"),
  tract_2000   = "530010805120018",
  new_code  = "5300108051200181"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120024 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120024", "530010805120058"),
  tract_2000   = "530010805120024",
  new_code  = "5300108051200241"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120023 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120023", "530010805120057"),
  tract_2000   = "530010805120023",
  new_code  = "5300108051200231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120022", "530010805120056"),
  tract_2000   = "530010805120022",
  new_code  = "5300108051200221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120037 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120037", "530010805120060"),
  tract_2000   = "530010805120037",
  new_code  = "5300108051200371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120031 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120031", "530010805120059"),
  tract_2000   = "530010805120031",
  new_code  = "5300108051200311"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170063 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170063", "530010805170141", "530010805170142", "530010805170099", "530010805170100", "530010805170143"),
  tract_2000   = "530010805170063",
  new_code  = "5300108051700631"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170078 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170118", "530010805170078", "530010805170119", "530010805170120"),
  tract_2000   = "530010805170078",
  new_code  = "5300108051700781"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170011 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170011", "530010805170082"),
  tract_2000   = "530010805170011",
  new_code  = "5300108051700111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170077 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170117", "530010805170077", "530010805170135"),
  tract_2000   = "530010805170077",
  new_code  = "5300108051700771"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170076 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170076", "530010805170129"),
  tract_2000   = "530010805170076",
  new_code  = "5300108051700761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170062 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170062", "530010805170096", "530010805170097", "530010805170098"),
  tract_2000   = "530010805170062",
  new_code  = "5300108051700621"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170061 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170061", "530010805170126"),
  tract_2000   = "530010805170061",
  new_code  = "5300108051700611"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170060", "530010805170095"),
  tract_2000   = "530010805170060",
  new_code  = "5300108051700601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170059 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170059", "530010805170125"),
  tract_2000   = "530010805170059",
  new_code  = "5300108051700591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170058 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170058", "530010805170094"),
  tract_2000   = "530010805170058",
  new_code  = "5300108051700581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170057", "530010805170124"),
  tract_2000   = "530010805170057",
  new_code  = "5300108051700571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170064 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170064", "530010805170101"),
  tract_2000   = "530010805170064",
  new_code  = "5300108051700641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170065 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170109", "530010805170110", "530010805170105", "530010805170106", "530010805170108", "530010805170102", "530010805170107", "530010805170134", "530010805170065", "530010805170127", "530010805170133", "530010805170132", "530010805170103", "530010805170104"),
  tract_2000   = "530010805170065",
  new_code  = "5300108051700651"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170075 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170075", "530010805170128"),
  tract_2000   = "530010805170075",
  new_code  = "5300108051700751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170072 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170114", "530010805170115", "530010805170116", "530010805170116", "530010805170072"),
  tract_2000   = "530010805170072",
  new_code  = "5300108051700721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170068 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170111", "530010805170068"),
  tract_2000   = "530010805170068",
  new_code  = "5300108051700681"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170069 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170112", "530010805170069"),
  tract_2000   = "530010805170069",
  new_code  = "5300108051700691"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170070 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170113", "530010805170070"),
  tract_2000   = "530010805170070",
  new_code  = "5300108051700701"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170053 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170053", "530010805170093"),
  tract_2000   = "530010805170053",
  new_code  = "5300108051700531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170052 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170052", "530010805170136", "530010805170137", "530010805170138", "530010805170139", "530010805170140"),
  tract_2000   = "530010805170052",
  new_code  = "5300108051700521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170050 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170050", "530010805170092"),
  tract_2000   = "530010805170050",
  new_code  = "5300108051700501"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170049 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170049", "530010805170091"),
  tract_2000   = "530010805170049",
  new_code  = "5300108051700491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170048 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170048", "530010805170090"),
  tract_2000   = "530010805170048",
  new_code  = "5300108051700481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170046 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170046", "530010805170123"),
  tract_2000   = "530010805170046",
  new_code  = "5300108051700461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170045 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170045", "530010805170089"),
  tract_2000   = "530010805170045",
  new_code  = "5300108051700451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170028 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170122", "530010805170028"),
  tract_2000   = "530010805170028",
  new_code  = "5300108051700281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170029 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170029", "530010805170087"),
  tract_2000   = "530010805170029",
  new_code  = "5300108051700291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170030 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170030", "530010805170088"),
  tract_2000   = "530010805170030",
  new_code  = "5300108051700301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170020 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170121", "530010805170020"),
  tract_2000   = "530010805170020",
  new_code  = "5300108051700201"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170022", "530010805170086"),
  tract_2000   = "530010805170022",
  new_code  = "5300108051700221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170017 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170017", "530010805170085"),
  tract_2000   = "530010805170017",
  new_code  = "5300108051700171"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170012", "530010805170083"),
  tract_2000   = "530010805170012",
  new_code  = "5300108051700121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170013", "530010805170084"),
  tract_2000   = "530010805170013",
  new_code  = "5300108051700131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170079 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170079", "530010805170144", "530010805170147", "530010805170149", "530010805170151", "530010805170150", "530010805170145", "530010805170146"),
  tract_2000   = "530010805170079",
  new_code  = "5300108051700791"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170080 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170080", "530010805170130"),
  tract_2000   = "530010805170080",
  new_code  = "5300108051700801"
)

# Merge the 2010 tracts that make up 2000 tract 530010805170081 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805170081", "530010805170131"),
  tract_2000   = "530010805170081",
  new_code  = "5300108051700811"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060235 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060235", "530010805060375", "530010805060435"),
  tract_2000   = "530010805060235",
  new_code  = "5300108050602351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060234 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060234", "530010805060413"),
  tract_2000   = "530010805060234",
  new_code  = "5300108050602341"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060171 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060369", "530010805060171"),
  tract_2000   = "530010805060171",
  new_code  = "5300108050601711"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060170 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060368", "530010805060170"),
  tract_2000   = "530010805060170",
  new_code  = "5300108050601701"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060175 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060175", "530010805060408"),
  tract_2000   = "530010805060175",
  new_code  = "5300108050601751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060179 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060179", "530010805060409"),
  tract_2000   = "530010805060179",
  new_code  = "5300108050601791"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060180 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060180", "530010805060410"),
  tract_2000   = "530010805060180",
  new_code  = "5300108050601801"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060184 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060371", "530010805060184"),
  tract_2000   = "530010805060184",
  new_code  = "5300108050601841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060185 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060372", "530010805060185"),
  tract_2000   = "530010805060185",
  new_code  = "5300108050601851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060224 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060224", "530010805060411"),
  tract_2000   = "530010805060224",
  new_code  = "5300108050602241"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060216 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060373", "530010805060216"),
  tract_2000   = "530010805060216",
  new_code  = "5300108050602161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060169 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060367", "530010805060169"),
  tract_2000   = "530010805060169",
  new_code  = "5300108050601691"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060159 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060366", "530010805060159"),
  tract_2000   = "530010805060159",
  new_code  = "5300108050601591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060156 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060156", "530010805060407"),
  tract_2000   = "530010805060156",
  new_code  = "5300108050601561"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060151 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060151", "530010805060404"),
  tract_2000   = "530010805060151",
  new_code  = "5300108050601511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060152 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060152", "530010805060405"),
  tract_2000   = "530010805060152",
  new_code  = "5300108050601521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060153 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060153", "530010805060406"),
  tract_2000   = "530010805060153",
  new_code  = "5300108050601531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060149 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060149", "530010805060403"),
  tract_2000   = "530010805060149",
  new_code  = "5300108050601491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060145 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060145", "530010805060364"),
  tract_2000   = "530010805060145",
  new_code  = "5300108050601451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060144 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060363", "530010805060144"),
  tract_2000   = "530010805060144",
  new_code  = "5300108050601441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060141 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060141", "530010805060361"),
  tract_2000   = "530010805060141",
  new_code  = "5300108050601411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060126 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060126", "530010805060400"),
  tract_2000   = "530010805060126",
  new_code  = "5300108050601261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060122 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060122", "530010805060399"),
  tract_2000   = "530010805060122",
  new_code  = "5300108050601221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060116 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060116", "530010805060360"),
  tract_2000   = "530010805060116",
  new_code  = "5300108050601161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060115 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060115", "530010805060398"),
  tract_2000   = "530010805060115",
  new_code  = "5300108050601151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060114 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060114", "530010805060397"),
  tract_2000   = "530010805060114",
  new_code  = "5300108050601141"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060128 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060128", "530010805060401"),
  tract_2000   = "530010805060128",
  new_code  = "5300108050601281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060146 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060146", "530010805060365"),
  tract_2000   = "530010805060146",
  new_code  = "5300108050601461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060183 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060370", "530010805060183"),
  tract_2000   = "530010805060183",
  new_code  = "5300108050601831"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060147 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060147", "530010805060402"),
  tract_2000   = "530010805060147",
  new_code  = "5300108050601471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060005 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060005", "530010805060377", "530010805060378"),
  tract_2000   = "530010805060005",
  new_code  = "5300108050600051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060238 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060416", "530010805060414", "530010805060415", "530010805060238"),
  tract_2000   = "530010805060238",
  new_code  = "5300108050602381"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060113 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060113", "530010805060335"),
  tract_2000   = "530010805060113",
  new_code  = "5300108050601131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060111 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060111", "530010805060396"),
  tract_2000   = "530010805060111",
  new_code  = "5300108050601111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060006 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060006", "530010805060379"),
  tract_2000   = "530010805060006",
  new_code  = "5300108050600061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060112 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060112", "530010805060334"),
  tract_2000   = "530010805060112",
  new_code  = "5300108050601121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060109 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060333", "530010805060109"),
  tract_2000   = "530010805060109",
  new_code  = "5300108050601091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060009 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060009", "530010805060294", "530010805060295"),
  tract_2000   = "530010805060009",
  new_code  = "5300108050600091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060103 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060103", "530010805060395"),
  tract_2000   = "530010805060103",
  new_code  = "5300108050601031"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060011 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060011", "530010805060380"),
  tract_2000   = "530010805060011",
  new_code  = "5300108050600111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060239 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060239", "530010805060336", "530010805060417"),
  tract_2000   = "530010805060239",
  new_code  = "5300108050602391"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060099 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060099", "530010805060332"),
  tract_2000   = "530010805060099",
  new_code  = "5300108050600991"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060016 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060016", "530010805060296", "530010805060425", "530010805060297"),
  tract_2000   = "530010805060016",
  new_code  = "5300108050600161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060021 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060021", "530010805060381"),
  tract_2000   = "530010805060021",
  new_code  = "5300108050600211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060022", "530010805060382"),
  tract_2000   = "530010805060022",
  new_code  = "5300108050600221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060243 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060243", "530010805060338"),
  tract_2000   = "530010805060243",
  new_code  = "5300108050602431"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060245 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060245", "530010805060339"),
  tract_2000   = "530010805060245",
  new_code  = "5300108050602451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060032 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060032", "530010805060300"),
  tract_2000   = "530010805060032",
  new_code  = "5300108050600321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060034 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060034", "530010805060304"),
  tract_2000   = "530010805060034",
  new_code  = "5300108050600341"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060033 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060033", "530010805060302", "530010805060303", "530010805060301"),
  tract_2000   = "530010805060033",
  new_code  = "5300108050600331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060089 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060089", "530010805060330"),
  tract_2000   = "530010805060089",
  new_code  = "5300108050600891"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060087 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060087", "530010805060392"),
  tract_2000   = "530010805060087",
  new_code  = "5300108050600871"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060084 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060084", "530010805060329"),
  tract_2000   = "530010805060084",
  new_code  = "5300108050600841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060078 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060078", "530010805060325", "530010805060326"),
  tract_2000   = "530010805060078",
  new_code  = "5300108050600781"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060038 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060038", "530010805060383"),
  tract_2000   = "530010805060038",
  new_code  = "5300108050600381"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060037 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060305", "530010805060306", "530010805060307", "530010805060037"),
  tract_2000   = "530010805060037",
  new_code  = "5300108050600371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060250 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060250", "530010805060340"),
  tract_2000   = "530010805060250",
  new_code  = "5300108050602501"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060251 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060251", "530010805060418"),
  tract_2000   = "530010805060251",
  new_code  = "5300108050602511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060079 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060079", "530010805060327", "530010805060328"),
  tract_2000   = "530010805060079",
  new_code  = "5300108050600791"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060075 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060320", "530010805060321", "530010805060428", "530010805060390", "530010805060075", "530010805060319", "530010805060324", "530010805060427", "530010805060322", "530010805060323", "530010805060429"),
  tract_2000   = "530010805060075",
  new_code  = "5300108050600751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060074 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060316", "530010805060389", "530010805060074", "530010805060317", "530010805060318", "530010805060426"),
  tract_2000   = "530010805060074",
  new_code  = "5300108050600741"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060072 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060072", "530010805060315"),
  tract_2000   = "530010805060072",
  new_code  = "5300108050600721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060064 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060064", "530010805060314"),
  tract_2000   = "530010805060064",
  new_code  = "5300108050600641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060062 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060062", "530010805060388"),
  tract_2000   = "530010805060062",
  new_code  = "5300108050600621"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060059 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060059", "530010805060312"),
  tract_2000   = "530010805060059",
  new_code  = "5300108050600591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060058 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060058", "530010805060387"),
  tract_2000   = "530010805060058",
  new_code  = "5300108050600581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060261 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060261", "530010805060357"),
  tract_2000   = "530010805060261",
  new_code  = "5300108050602611"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060057", "530010805060311"),
  tract_2000   = "530010805060057",
  new_code  = "5300108050600571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060257 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060257", "530010805060352"),
  tract_2000   = "530010805060257",
  new_code  = "5300108050602571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060053 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060053", "530010805060386"),
  tract_2000   = "530010805060053",
  new_code  = "5300108050600531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060052 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060052", "530010805060385"),
  tract_2000   = "530010805060052",
  new_code  = "5300108050600521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060253 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060253", "530010805060346"),
  tract_2000   = "530010805060253",
  new_code  = "5300108050602531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060255 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060350", "530010805060351", "530010805060255", "530010805060348", "530010805060347", "530010805060349"),
  tract_2000   = "530010805060255",
  new_code  = "5300108050602551"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060260 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060260", "530010805060356"),
  tract_2000   = "530010805060260",
  new_code  = "5300108050602601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060263 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060263", "530010805060421"),
  tract_2000   = "530010805060263",
  new_code  = "5300108050602631"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060252 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060345", "530010805060434", "530010805060430", "530010805060431", "530010805060432", "530010805060433", "530010805060342", "530010805060344", "530010805060341", "530010805060343", "530010805060252"),
  tract_2000   = "530010805060252",
  new_code  = "5300108050602521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180003 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180003", "530010805180172"),
  tract_2000   = "530010805180003",
  new_code  = "5300108051800031"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180002 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180002", "530010805180239"),
  tract_2000   = "530010805180002",
  new_code  = "5300108051800021"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180006 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180006", "530010805180173"),
  tract_2000   = "530010805180006",
  new_code  = "5300108051800061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180007 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180007", "530010805180174"),
  tract_2000   = "530010805180007",
  new_code  = "5300108051800071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180009 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180009", "530010805180176"),
  tract_2000   = "530010805180009",
  new_code  = "5300108051800091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180008 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180008", "530010805180175"),
  tract_2000   = "530010805180008",
  new_code  = "5300108051800081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180010 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180010", "530010805180240", "530010805180252"),
  tract_2000   = "530010805180010",
  new_code  = "5300108051800101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180015 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180015", "530010805180243"),
  tract_2000   = "530010805180015",
  new_code  = "5300108051800151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180022", "530010805180244"),
  tract_2000   = "530010805180022",
  new_code  = "5300108051800221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180027 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180027", "530010805180246"),
  tract_2000   = "530010805180027",
  new_code  = "5300108051800271"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180028 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180028", "530010805180247"),
  tract_2000   = "530010805180028",
  new_code  = "5300108051800281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180033 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180033", "530010805180182"),
  tract_2000   = "530010805180033",
  new_code  = "5300108051800331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180171 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180292", "530010805180293", "530010805180291", "530010805180282", "530010805180286", "530010805180288", "530010805180285", "530010805180284", "530010805180283", "530010805180287", "530010805180289", "530010805180290", "530010805180281", "530010805180171"),
  tract_2000   = "530010805180171",
  new_code  = "5300108051801711"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180050 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180050", "530010805180255"),
  tract_2000   = "530010805180050",
  new_code  = "5300108051800501"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180051 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180186", "530010805180294", "530010805180051"),
  tract_2000   = "530010805180051",
  new_code  = "5300108051800511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180057", "530010805180258"),
  tract_2000   = "530010805180057",
  new_code  = "5300108051800571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180067 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180067", "530010805180260"),
  tract_2000   = "530010805180067",
  new_code  = "5300108051800671"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180001 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180233", "530010805180232", "530010805180231", "530010805180311", "530010805180230", "530010805180229", "530010805180228", "530010805180310", "530010805180227", "530010805180226", "530010805180309", "530010805180225", "530010805180224", "530010805180223", "530010805180222", "530010805180308", "530010805180221", "530010805180220", "530010805180219", "530010805180217", "530010805180304", "530010805180216", "530010805180303", "530010805180218", "530010805180305", "530010805180306", "530010805180307", "530010805180215", "530010805180302", "530010805180214", "530010805180212", "530010805180300", "530010805180301", "530010805180211", "530010805180213", "530010805180210", "530010805180001"),
  tract_2000   = "530010805180001",
  new_code  = "5300108051800011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180013", "530010805180242"),
  tract_2000   = "530010805180013",
  new_code  = "5300108051800131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180021 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180021", "530010805180178"),
  tract_2000   = "530010805180021",
  new_code  = "5300108051800211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180025 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180025", "530010805180245"),
  tract_2000   = "530010805180025",
  new_code  = "5300108051800251"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180029 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180029", "530010805180179"),
  tract_2000   = "530010805180029",
  new_code  = "5300108051800291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180032 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180032", "530010805180248"),
  tract_2000   = "530010805180032",
  new_code  = "5300108051800321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180036 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180036", "530010805180183"),
  tract_2000   = "530010805180036",
  new_code  = "5300108051800361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180041 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180041", "530010805180251"),
  tract_2000   = "530010805180041",
  new_code  = "5300108051800411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180040 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180040", "530010805180250"),
  tract_2000   = "530010805180040",
  new_code  = "5300108051800401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180045 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180045", "530010805180184"),
  tract_2000   = "530010805180045",
  new_code  = "5300108051800451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180046 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180046", "530010805180185"),
  tract_2000   = "530010805180046",
  new_code  = "5300108051800461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180049 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180049", "530010805180254"),
  tract_2000   = "530010805180049",
  new_code  = "5300108051800491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180047 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180047", "530010805180253"),
  tract_2000   = "530010805180047",
  new_code  = "5300108051800471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180054 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180054", "530010805180187"),
  tract_2000   = "530010805180054",
  new_code  = "5300108051800541"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180056 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180056", "530010805180257"),
  tract_2000   = "530010805180056",
  new_code  = "5300108051800561"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180055 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180055", "530010805180256"),
  tract_2000   = "530010805180055",
  new_code  = "5300108051800551"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180059 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180059", "530010805180188"),
  tract_2000   = "530010805180059",
  new_code  = "5300108051800591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180062 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180062", "530010805180189"),
  tract_2000   = "530010805180062",
  new_code  = "5300108051800621"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180063 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180063", "530010805180190", "530010805180191", "530010805180295"),
  tract_2000   = "530010805180063",
  new_code  = "5300108051800631"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180064 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180064", "530010805180192"),
  tract_2000   = "530010805180064",
  new_code  = "5300108051800641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180065 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180065", "530010805180193", "530010805180259"),
  tract_2000   = "530010805180065",
  new_code  = "5300108051800651"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180072 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180072", "530010805180195"),
  tract_2000   = "530010805180072",
  new_code  = "5300108051800721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180071 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180071", "530010805180194", "530010805180261"),
  tract_2000   = "530010805180071",
  new_code  = "5300108051800711"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180074 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180074", "530010805180262"),
  tract_2000   = "530010805180074",
  new_code  = "5300108051800741"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180075 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180075", "530010805180263", "530010805180264"),
  tract_2000   = "530010805180075",
  new_code  = "5300108051800751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180012", "530010805180241"),
  tract_2000   = "530010805180012",
  new_code  = "5300108051800121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180030 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180030", "530010805180180"),
  tract_2000   = "530010805180030",
  new_code  = "5300108051800301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180031 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180031", "530010805180181"),
  tract_2000   = "530010805180031",
  new_code  = "5300108051800311"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180035 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180035", "530010805180249"),
  tract_2000   = "530010805180035",
  new_code  = "5300108051800351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180164 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180164", "530010805180209"),
  tract_2000   = "530010805180164",
  new_code  = "5300108051801641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180160 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180160", "530010805180208"),
  tract_2000   = "530010805180160",
  new_code  = "5300108051801601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180159 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180159", "530010805180207"),
  tract_2000   = "530010805180159",
  new_code  = "5300108051801591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180157 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180157", "530010805180280"),
  tract_2000   = "530010805180157",
  new_code  = "5300108051801571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180156 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180156", "530010805180206", "530010805180299"),
  tract_2000   = "530010805180156",
  new_code  = "5300108051801561"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180155 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180155", "530010805180279"),
  tract_2000   = "530010805180155",
  new_code  = "5300108051801551"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180146 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180146", "530010805180278"),
  tract_2000   = "530010805180146",
  new_code  = "5300108051801461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180144 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180144", "530010805180277"),
  tract_2000   = "530010805180144",
  new_code  = "5300108051801441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180145 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180145", "530010805180205", "530010805180298"),
  tract_2000   = "530010805180145",
  new_code  = "5300108051801451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180138 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180138", "530010805180204"),
  tract_2000   = "530010805180138",
  new_code  = "5300108051801381"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180137 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180137", "530010805180276"),
  tract_2000   = "530010805180137",
  new_code  = "5300108051801371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180132 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180132", "530010805180203"),
  tract_2000   = "530010805180132",
  new_code  = "5300108051801321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180129 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180129", "530010805180202", "530010805180297"),
  tract_2000   = "530010805180129",
  new_code  = "5300108051801291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180122 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180122", "530010805180274"),
  tract_2000   = "530010805180122",
  new_code  = "5300108051801221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180123 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180123", "530010805180275"),
  tract_2000   = "530010805180123",
  new_code  = "5300108051801231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180113 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180113", "530010805180273"),
  tract_2000   = "530010805180113",
  new_code  = "5300108051801131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180114 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180114", "530010805180201"),
  tract_2000   = "530010805180114",
  new_code  = "5300108051801141"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180108 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180108", "530010805180272"),
  tract_2000   = "530010805180108",
  new_code  = "5300108051801081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180104 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180104", "530010805180200"),
  tract_2000   = "530010805180104",
  new_code  = "5300108051801041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180100 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180100", "530010805180271"),
  tract_2000   = "530010805180100",
  new_code  = "5300108051801001"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180099 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180099", "530010805180270"),
  tract_2000   = "530010805180099",
  new_code  = "5300108051800991"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180096 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180096", "530010805180199", "530010805180296"),
  tract_2000   = "530010805180096",
  new_code  = "5300108051800961"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180097 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180097", "530010805180269"),
  tract_2000   = "530010805180097",
  new_code  = "5300108051800971"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180090 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180090", "530010805180268"),
  tract_2000   = "530010805180090",
  new_code  = "5300108051800901"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180089 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180089", "530010805180198"),
  tract_2000   = "530010805180089",
  new_code  = "5300108051800891"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180088 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180088", "530010805180267"),
  tract_2000   = "530010805180088",
  new_code  = "5300108051800881"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180082 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180082", "530010805180197", "530010805180266"),
  tract_2000   = "530010805180082",
  new_code  = "5300108051800821"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180081 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180081", "530010805180196"),
  tract_2000   = "530010805180081",
  new_code  = "5300108051800811"
)

# Merge the 2010 tracts that make up 2000 tract 530010805180077 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805180077", "530010805180265"),
  tract_2000   = "530010805180077",
  new_code  = "5300108051800771"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130001 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130001", "530010805130058", "530010805130073"),
  tract_2000   = "530010805130001",
  new_code  = "5300108051300011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130010 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130063", "530010805130080", "530010805130010", "530010805130078"),
  tract_2000   = "530010805130010",
  new_code  = "5300108051300101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130009 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130009", "530010805130062", "530010805130077"),
  tract_2000   = "530010805130009",
  new_code  = "5300108051300091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130008 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130008", "530010805130061", "530010805130127"),
  tract_2000   = "530010805130008",
  new_code  = "5300108051300081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130011 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130064", "530010805130065", "530010805130128", "530010805130011"),
  tract_2000   = "530010805130011",
  new_code  = "5300108051300111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130016 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130016", "530010805130083", "530010805130084"),
  tract_2000   = "530010805130016",
  new_code  = "5300108051300161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130017 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130017", "530010805130085", "530010805130068", "530010805130129", "530010805130069", "530010805130130"),
  tract_2000   = "530010805130017",
  new_code  = "5300108051300171"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130018 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130018", "530010805130071", "530010805130070"),
  tract_2000   = "530010805130018",
  new_code  = "5300108051300181"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130020 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130020", "530010805130072"),
  tract_2000   = "530010805130020",
  new_code  = "5300108051300201"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130002 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130002", "530010805130074", "530010805130075"),
  tract_2000   = "530010805130002",
  new_code  = "5300108051300021"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130004 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130004", "530010805130076"),
  tract_2000   = "530010805130004",
  new_code  = "5300108051300041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130007 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130007", "530010805130060"),
  tract_2000   = "530010805130007",
  new_code  = "5300108051300071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130006 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130006", "530010805130059"),
  tract_2000   = "530010805130006",
  new_code  = "5300108051300061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130012", "530010805130066"),
  tract_2000   = "530010805130012",
  new_code  = "5300108051300121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130013", "530010805130081"),
  tract_2000   = "530010805130013",
  new_code  = "5300108051300131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130014 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130014", "530010805130082"),
  tract_2000   = "530010805130014",
  new_code  = "5300108051300141"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130015 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130015", "530010805130067"),
  tract_2000   = "530010805130015",
  new_code  = "5300108051300151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140027 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140027", "530010805140062"),
  tract_2000   = "530010805140027",
  new_code  = "5300108051400271"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140026 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140026", "530010805140061"),
  tract_2000   = "530010805140026",
  new_code  = "5300108051400261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140025 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140025", "530010805140052"),
  tract_2000   = "530010805140025",
  new_code  = "5300108051400251"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140024 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140024", "530010805140051", "530010805140059", "530010805140060"),
  tract_2000   = "530010805140024",
  new_code  = "5300108051400241"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140010 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140010", "530010805140055"),
  tract_2000   = "530010805140010",
  new_code  = "5300108051400101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140009 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140009", "530010805140045", "530010805140046"),
  tract_2000   = "530010805140009",
  new_code  = "5300108051400091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140005 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140005", "530010805140043"),
  tract_2000   = "530010805140005",
  new_code  = "5300108051400051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140006 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140006", "530010805140044"),
  tract_2000   = "530010805140006",
  new_code  = "5300108051400061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140014 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140014", "530010805140048"),
  tract_2000   = "530010805140014",
  new_code  = "5300108051400141"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140017 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140017", "530010805140049"),
  tract_2000   = "530010805140017",
  new_code  = "5300108051400171"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140012", "530010805140047"),
  tract_2000   = "530010805140012",
  new_code  = "5300108051400121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140022", "530010805140050", "530010805140058"),
  tract_2000   = "530010805140022",
  new_code  = "5300108051400221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805190004 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805190004", "530010805190021"),
  tract_2000   = "530010805190004",
  new_code  = "5300108051900041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805190015 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805190015", "530010805190022"),
  tract_2000   = "530010805190015",
  new_code  = "5300108051900151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805190001 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805190001", "530010805190020"),
  tract_2000   = "530010805190001",
  new_code  = "5300108051900011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805140004 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805140004", "530010805140054"),
  tract_2000   = "530010805140004",
  new_code  = "5300108051400041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160108 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160108", "530010805160191"),
  tract_2000   = "530010805160108",
  new_code  = "5300108051601081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160085 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160213", "530010805160166", "530010805160212", "530010805160165", "530010805160164", "530010805160210", "530010805160188", "530010805160211", "530010805160185", "530010805160186", "530010805160187", "530010805160085"),
  tract_2000   = "530010805160085",
  new_code  = "5300108051600851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160114 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160114", "530010805160196"),
  tract_2000   = "530010805160114",
  new_code  = "5300108051601141"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160105 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160105", "530010805160170"),
  tract_2000   = "530010805160105",
  new_code  = "5300108051601051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160107 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160107", "530010805160172"),
  tract_2000   = "530010805160107",
  new_code  = "5300108051601071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160106 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160106", "530010805160171", "530010805160190"),
  tract_2000   = "530010805160106",
  new_code  = "5300108051601061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160104 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160104", "530010805160169", "530010805160189"),
  tract_2000   = "530010805160104",
  new_code  = "5300108051601041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160101 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160101", "530010805160167"),
  tract_2000   = "530010805160101",
  new_code  = "5300108051601011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160103 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160103", "530010805160216", "530010805160168"),
  tract_2000   = "530010805160103",
  new_code  = "5300108051601031"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160082 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160082", "530010805160163"),
  tract_2000   = "530010805160082",
  new_code  = "5300108051600821"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160080 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160080", "530010805160162"),
  tract_2000   = "530010805160080",
  new_code  = "5300108051600801"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160077 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160077", "530010805160160"),
  tract_2000   = "530010805160077",
  new_code  = "5300108051600771"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160076 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160076", "530010805160157", "530010805160209", "530010805160158", "530010805160159"),
  tract_2000   = "530010805160076",
  new_code  = "5300108051600761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160064 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160064", "530010805160155"),
  tract_2000   = "530010805160064",
  new_code  = "5300108051600641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160070 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160070", "530010805160184"),
  tract_2000   = "530010805160070",
  new_code  = "5300108051600701"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160060", "530010805160154"),
  tract_2000   = "530010805160060",
  new_code  = "5300108051600601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160053 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160153", "530010805160181", "530010805160053"),
  tract_2000   = "530010805160053",
  new_code  = "5300108051600531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160061 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160061", "530010805160183"),
  tract_2000   = "530010805160061",
  new_code  = "5300108051600611"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160075 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160075", "530010805160156"),
  tract_2000   = "530010805160075",
  new_code  = "5300108051600751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160051 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160214", "530010805160180", "530010805160051"),
  tract_2000   = "530010805160051",
  new_code  = "5300108051600511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160109 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160182", "530010805160219", "530010805160109", "530010805160221"),
  tract_2000   = "530010805160109",
  new_code  = "5300108051601091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160129 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160129", "530010805160200"),
  tract_2000   = "530010805160129",
  new_code  = "5300108051601291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160032 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160032", "530010805160177"),
  tract_2000   = "530010805160032",
  new_code  = "5300108051600321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160034 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160034", "530010805160178"),
  tract_2000   = "530010805160034",
  new_code  = "5300108051600341"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160035 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160035", "530010805160145"),
  tract_2000   = "530010805160035",
  new_code  = "5300108051600351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160037 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160037", "530010805160146"),
  tract_2000   = "530010805160037",
  new_code  = "5300108051600371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160039 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160039", "530010805160148"),
  tract_2000   = "530010805160039",
  new_code  = "5300108051600391"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160040 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160040", "530010805160149"),
  tract_2000   = "530010805160040",
  new_code  = "5300108051600401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160038 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160038", "530010805160147"),
  tract_2000   = "530010805160038",
  new_code  = "5300108051600381"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160018 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160018", "530010805160144"),
  tract_2000   = "530010805160018",
  new_code  = "5300108051600181"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160005 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160005", "530010805160142"),
  tract_2000   = "530010805160005",
  new_code  = "5300108051600051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160006 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160006", "530010805160143"),
  tract_2000   = "530010805160006",
  new_code  = "5300108051600061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160007 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160007", "530010805160175"),
  tract_2000   = "530010805160007",
  new_code  = "5300108051600071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160022", "530010805160176"),
  tract_2000   = "530010805160022",
  new_code  = "5300108051600221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160046 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160046", "530010805160179"),
  tract_2000   = "530010805160046",
  new_code  = "5300108051600461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160045 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160045", "530010805160152"),
  tract_2000   = "530010805160045",
  new_code  = "5300108051600451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160044 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160044", "530010805160151"),
  tract_2000   = "530010805160044",
  new_code  = "5300108051600441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160043 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160043", "530010805160150"),
  tract_2000   = "530010805160043",
  new_code  = "5300108051600431"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160119 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160119", "530010805160198"),
  tract_2000   = "530010805160119",
  new_code  = "5300108051601191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160118 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160118", "530010805160197"),
  tract_2000   = "530010805160118",
  new_code  = "5300108051601181"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160130 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160201", "530010805160202", "530010805160130"),
  tract_2000   = "530010805160130",
  new_code  = "5300108051601301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805160121 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805160121", "530010805160199"),
  tract_2000   = "530010805160121",
  new_code  = "5300108051601211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150386 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150455", "530010805150554", "530010805150471", "530010805150473", "530010805150472", "530010805150476", "530010805150477", "530010805150474", "530010805150475", "530010805150555", "530010805150483", "530010805150481", "530010805150482", "530010805150479", "530010805150480", "530010805150478", "530010805150580", "530010805150556", "530010805150557", "530010805150558", "530010805150559", "530010805150560", "530010805150561", "530010805150562", "530010805150563", "530010805150564", "530010805150565", "530010805150566", "530010805150567", "530010805150568", "530010805150570", "530010805150571", "530010805150572", "530010805150573", "530010805150575", "530010805150574", "530010805150576", "530010805150577", "530010805150578", "530010805150579", "530010805150581", "530010805150582", "530010805150576", "530010805150569", "530010805150583", "530010805150585", "530010805150586", "530010805150584", "530010805150587", "530010805150588", "530010805150590", "530010805150591", "530010805150592", "530010805150593", "530010805150594", "530010805150595", "530010805150599", "530010805150600", "530010805150596", "530010805150597", "530010805150598", "530010805150589", "530010805150536", "530010805150602", "530010805150603", "530010805150604", "530010805150601", "530010805150605", "530010805150550"),
  tract_2000   = "530010805150386",
  new_code  = "5300108051503861"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150387 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150529", "530010805150387", "530010805150528", "530010805150533", "530010805150534", "530010805150526", "530010805150531", "530010805150525", "530010805150527", "530010805150532", "530010805150523", "530010805150524", "530010805150530", "530010805150535"),
  tract_2000   = "530010805150387",
  new_code  = "5300108051503871"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130053 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130053", "530010805130122"),
  tract_2000   = "530010805130053",
  new_code  = "5300108051300531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130052 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130120", "530010805130121", "530010805130052", "530010805130119"),
  tract_2000   = "530010805130052",
  new_code  = "5300108051300521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130047 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130047", "530010805130115", "530010805130116"),
  tract_2000   = "530010805130047",
  new_code  = "5300108051300471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130039 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130039", "530010805130111", "530010805130112"),
  tract_2000   = "530010805130039",
  new_code  = "5300108051300391"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130033 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130033", "530010805130109"),
  tract_2000   = "530010805130033",
  new_code  = "5300108051300331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130036 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130036", "530010805130110"),
  tract_2000   = "530010805130036",
  new_code  = "5300108051300361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130044 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130044", "530010805130114"),
  tract_2000   = "530010805130044",
  new_code  = "5300108051300441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130041 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130041", "530010805130113"),
  tract_2000   = "530010805130041",
  new_code  = "5300108051300411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130049 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130049", "530010805130118"),
  tract_2000   = "530010805130049",
  new_code  = "5300108051300491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805130048 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805130048", "530010805130117"),
  tract_2000   = "530010805130048",
  new_code  = "5300108051300481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200019 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200019", "530010805200136"),
  tract_2000   = "530010805200019",
  new_code  = "5300108052000191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200020 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200020", "530010805200137"),
  tract_2000   = "530010805200020",
  new_code  = "5300108052000201"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200021 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200021", "530010805200138"),
  tract_2000   = "530010805200021",
  new_code  = "5300108052000211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200091 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200091", "530010805200171"),
  tract_2000   = "530010805200091",
  new_code  = "5300108052000911"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200090 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200090", "530010805200170"),
  tract_2000   = "530010805200090",
  new_code  = "5300108052000901"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200088 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200088", "530010805200169"),
  tract_2000   = "530010805200088",
  new_code  = "5300108052000881"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200087 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200087", "530010805200168"),
  tract_2000   = "530010805200087",
  new_code  = "5300108052000871"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200084 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200084", "530010805200167"),
  tract_2000   = "530010805200084",
  new_code  = "5300108052000841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200022 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200022", "530010805200102"),
  tract_2000   = "530010805200022",
  new_code  = "5300108052000221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200024 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200024", "530010805200104"),
  tract_2000   = "530010805200024",
  new_code  = "5300108052000241"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200023 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200023", "530010805200103"),
  tract_2000   = "530010805200023",
  new_code  = "5300108052000231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200016 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200016", "530010805200079"),
  tract_2000   = "530010805200016",
  new_code  = "5300108052000161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200015 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200015", "530010805200135"),
  tract_2000   = "530010805200015",
  new_code  = "5300108052000151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200025 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200025", "530010805200105"),
  tract_2000   = "530010805200025",
  new_code  = "5300108052000251"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200026 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200026", "530010805200106"),
  tract_2000   = "530010805200026",
  new_code  = "5300108052000261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200014 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200014", "530010805200134"),
  tract_2000   = "530010805200014",
  new_code  = "5300108052000141"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200013 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200013", "530010805200133"),
  tract_2000   = "530010805200013",
  new_code  = "5300108052000131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200027 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200027", "530010805200107"),
  tract_2000   = "530010805200027",
  new_code  = "5300108052000271"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200012", "530010805200132"),
  tract_2000   = "530010805200012",
  new_code  = "5300108052000121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200028 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200028", "530010805200139"),
  tract_2000   = "530010805200028",
  new_code  = "5300108052000281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200011 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200011", "530010805200078"),
  tract_2000   = "530010805200011",
  new_code  = "5300108052000111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200029 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200029", "530010805200108"),
  tract_2000   = "530010805200029",
  new_code  = "5300108052000291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200010 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200010", "530010805200131"),
  tract_2000   = "530010805200010",
  new_code  = "5300108052000101"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200030 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200030", "530010805200140"),
  tract_2000   = "530010805200030",
  new_code  = "5300108052000301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200009 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200009", "530010805200077"),
  tract_2000   = "530010805200009",
  new_code  = "5300108052000091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200031 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200031", "530010805200109"),
  tract_2000   = "530010805200031",
  new_code  = "5300108052000311"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200008 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200008", "530010805200130"),
  tract_2000   = "530010805200008",
  new_code  = "5300108052000081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200032 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200032", "530010805200141"),
  tract_2000   = "530010805200032",
  new_code  = "5300108052000321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200033 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200033", "530010805200110"),
  tract_2000   = "530010805200033",
  new_code  = "5300108052000331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200034 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200142", "530010805200034", "530010805200143", "530010805200144"),
  tract_2000   = "530010805200034",
  new_code  = "5300108052000341"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200007 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200007", "530010805200129"),
  tract_2000   = "530010805200007",
  new_code  = "5300108052000071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200006 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200006", "530010805200128"),
  tract_2000   = "530010805200006",
  new_code  = "5300108052000061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200004 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200004", "530010805200127"),
  tract_2000   = "530010805200004",
  new_code  = "5300108052000041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200003 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200003", "530010805200177"),
  tract_2000   = "530010805200003",
  new_code  = "5300108052000031"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200002 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200002", "530010805200076"),
  tract_2000   = "530010805200002",
  new_code  = "5300108052000021"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200001 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200001", "530010805200074"),
  tract_2000   = "530010805200001",
  new_code  = "5300108052000011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200035 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200145", "530010805200035"),
  tract_2000   = "530010805200035",
  new_code  = "5300108052000351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200037 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200147", "530010805200037"),
  tract_2000   = "530010805200037",
  new_code  = "5300108052000371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200038 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200038", "530010805200111"),
  tract_2000   = "530010805200038",
  new_code  = "5300108052000381"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200039 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200039", "530010805200112"),
  tract_2000   = "530010805200039",
  new_code  = "5300108052000391"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200040 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200148", "530010805200040"),
  tract_2000   = "530010805200040",
  new_code  = "5300108052000401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200041 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200149", "530010805200041", "530010805200150"),
  tract_2000   = "530010805200041",
  new_code  = "5300108052000411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200042 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200151", "530010805200042"),
  tract_2000   = "530010805200042",
  new_code  = "5300108052000421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200050 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200050", "530010805200158"),
  tract_2000   = "530010805200050",
  new_code  = "5300108052000501"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200049 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200049", "530010805200157"),
  tract_2000   = "530010805200049",
  new_code  = "5300108052000491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200045 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200152", "530010805200045"),
  tract_2000   = "530010805200045",
  new_code  = "5300108052000451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200072 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200072", "530010805200166"),
  tract_2000   = "530010805200072",
  new_code  = "5300108052000721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200048 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200155", "530010805200048", "530010805200156"),
  tract_2000   = "530010805200048",
  new_code  = "5300108052000481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200047 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200154", "530010805200047"),
  tract_2000   = "530010805200047",
  new_code  = "5300108052000471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200059 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200059", "530010805200163"),
  tract_2000   = "530010805200059",
  new_code  = "5300108052000591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200057", "530010805200161"),
  tract_2000   = "530010805200057",
  new_code  = "5300108052000571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200056 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200056", "530010805200160"),
  tract_2000   = "530010805200056",
  new_code  = "5300108052000561"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200058 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200058", "530010805200162"),
  tract_2000   = "530010805200058",
  new_code  = "5300108052000581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200100 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200100", "530010805200116"),
  tract_2000   = "530010805200100",
  new_code  = "5300108052001001"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200098 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200098", "530010805200175"),
  tract_2000   = "530010805200098",
  new_code  = "5300108052000981"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200092 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200092", "530010805200172"),
  tract_2000   = "530010805200092",
  new_code  = "5300108052000921"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200101 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200101", "530010805200117"),
  tract_2000   = "530010805200101",
  new_code  = "5300108052001011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200097 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200097", "530010805200115"),
  tract_2000   = "530010805200097",
  new_code  = "5300108052000971"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200093 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200093", "530010805200173"),
  tract_2000   = "530010805200093",
  new_code  = "5300108052000931"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200096 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200096", "530010805200114"),
  tract_2000   = "530010805200096",
  new_code  = "5300108052000961"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200094 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200094", "530010805200113"),
  tract_2000   = "530010805200094",
  new_code  = "5300108052000941"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200095 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200095", "530010805200174"),
  tract_2000   = "530010805200095",
  new_code  = "5300108052000951"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200060", "530010805200164"),
  tract_2000   = "530010805200060",
  new_code  = "5300108052000601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200054 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200054", "530010805200159"),
  tract_2000   = "530010805200054",
  new_code  = "5300108052000541"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200066 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200066", "530010805200165"),
  tract_2000   = "530010805200066",
  new_code  = "5300108052000661"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080180 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080180", "530010805080636"),
  tract_2000   = "530010805080180",
  new_code  = "5300108050801801"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080173 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080173", "530010805080633"),
  tract_2000   = "530010805080173",
  new_code  = "5300108050801731"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080174 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080174", "530010805080634"),
  tract_2000   = "530010805080174",
  new_code  = "5300108050801741"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080181 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080181", "530010805080637", "530010805080640", "530010805080638", "530010805080639", "530010805080641"),
  tract_2000   = "530010805080181",
  new_code  = "5300108050801811"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080171 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080171", "530010805080631", "530010805080632"),
  tract_2000   = "530010805080171",
  new_code  = "5300108050801711"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080182 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080182", "530010805080642"),
  tract_2000   = "530010805080182",
  new_code  = "5300108050801821"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080166 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080166", "530010805080630"),
  tract_2000   = "530010805080166",
  new_code  = "5300108050801661"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080162 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080162", "530010805080629"),
  tract_2000   = "530010805080162",
  new_code  = "5300108050801621"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080156 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080156", "530010805080628"),
  tract_2000   = "530010805080156",
  new_code  = "5300108050801561"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080143 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080143", "530010805080621"),
  tract_2000   = "530010805080143",
  new_code  = "5300108050801431"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080144 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080144", "530010805080622"),
  tract_2000   = "530010805080144",
  new_code  = "5300108050801441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080145 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080145", "530010805080623"),
  tract_2000   = "530010805080145",
  new_code  = "5300108050801451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080149 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080149", "530010805080626"),
  tract_2000   = "530010805080149",
  new_code  = "5300108050801491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080148 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080148", "530010805080625"),
  tract_2000   = "530010805080148",
  new_code  = "5300108050801481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080128 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080128", "530010805080618", "530010805080617"),
  tract_2000   = "530010805080128",
  new_code  = "5300108050801281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080106 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080106", "530010805080607", "530010805080608"),
  tract_2000   = "530010805080106",
  new_code  = "5300108050801061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080051 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080051", "530010805080583", "530010805080584", "530010805080051"),
  tract_2000   = "530010805080051",
  new_code  = "5300108050800511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080050 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080050", "530010805080582"),
  tract_2000   = "530010805080050",
  new_code  = "5300108050800501"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080142 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080142", "530010805080620"),
  tract_2000   = "530010805080142",
  new_code  = "5300108050801421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080141 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080141", "530010805080619"),
  tract_2000   = "530010805080141",
  new_code  = "5300108050801411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080121 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080121", "530010805080613"),
  tract_2000   = "530010805080121",
  new_code  = "5300108050801211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080146 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080146", "530010805080624"),
  tract_2000   = "530010805080146",
  new_code  = "5300108050801461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080057 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080057", "530010805080589"),
  tract_2000   = "530010805080057",
  new_code  = "5300108050800571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080062 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080062", "530010805080591"),
  tract_2000   = "530010805080062",
  new_code  = "5300108050800621"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080060", "530010805080590"),
  tract_2000   = "530010805080060",
  new_code  = "5300108050800601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080085 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080085", "530010805080597"),
  tract_2000   = "530010805080085",
  new_code  = "5300108050800851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080097 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080097", "530010805080603"),
  tract_2000   = "530010805080097",
  new_code  = "5300108050800971"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080098 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080604", "530010805080605", "530010805080098"),
  tract_2000   = "530010805080098",
  new_code  = "5300108050800981"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080115 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080115", "530010805080611"),
  tract_2000   = "530010805080115",
  new_code  = "5300108050801151"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080072 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080072", "530010805080595"),
  tract_2000   = "530010805080072",
  new_code  = "5300108050800721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080089 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080089", "530010805080598", "530010805080599"),
  tract_2000   = "530010805080089",
  new_code  = "5300108050800891"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080092 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080092", "530010805080601"),
  tract_2000   = "530010805080092",
  new_code  = "5300108050800921"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080095 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080095", "530010805080602"),
  tract_2000   = "530010805080095",
  new_code  = "5300108050800951"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080123 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080123", "530010805080612"),
  tract_2000   = "530010805080123",
  new_code  = "5300108050801231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080124 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080124", "530010805080614"),
  tract_2000   = "530010805080124",
  new_code  = "5300108050801241"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080125 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080125", "530010805080615", "530010805080616"),
  tract_2000   = "530010805080125",
  new_code  = "5300108050801251"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080107 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080107", "530010805080609", "530010805080610"),
  tract_2000   = "530010805080107",
  new_code  = "5300108050801071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080091 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080091", "530010805080600"),
  tract_2000   = "530010805080091",
  new_code  = "5300108050800911"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080075 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080075", "530010805080716", "530010805080717"),
  tract_2000   = "530010805080075",
  new_code  = "5300108050800751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080063 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080063", "530010805080592"),
  tract_2000   = "530010805080063",
  new_code  = "5300108050800631"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080055 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080055", "530010805080588"),
  tract_2000   = "530010805080055",
  new_code  = "5300108050800551"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080076 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080076", "530010805080596"),
  tract_2000   = "530010805080076",
  new_code  = "5300108050800761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080066 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080066", "530010805080594"),
  tract_2000   = "530010805080066",
  new_code  = "5300108050800661"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080064 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080064", "530010805080593"),
  tract_2000   = "530010805080064",
  new_code  = "5300108050800641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080053 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080053", "530010805080587"),
  tract_2000   = "530010805080053",
  new_code  = "5300108050800531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080052 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080052", "530010805080586"),
  tract_2000   = "530010805080052",
  new_code  = "5300108050800521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080049 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080049", "530010805080581"),
  tract_2000   = "530010805080049",
  new_code  = "5300108050800491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080047 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080047", "530010805080580"),
  tract_2000   = "530010805080047",
  new_code  = "5300108050800471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080002 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080002", "530010805080713", "530010805080714", "530010805080715"),
  tract_2000   = "530010805080002",
  new_code  = "5300108050800021"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080005 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080005", "530010805080556"),
  tract_2000   = "530010805080005",
  new_code  = "5300108050800051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080019 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080019", "530010805080493"),
  tract_2000   = "530010805080019",
  new_code  = "5300108050800191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080023 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080023", "530010805080494"),
  tract_2000   = "530010805080023",
  new_code  = "5300108050800231"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080020 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080020", "530010805080740"),
  tract_2000   = "530010805080020",
  new_code  = "5300108050800201"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080021 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080021", "530010805080562"),
  tract_2000   = "530010805080021",
  new_code  = "5300108050800211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080189 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080189", "530010805080643"),
  tract_2000   = "530010805080189",
  new_code  = "5300108050801891"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080195 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080195", "530010805080498"),
  tract_2000   = "530010805080195",
  new_code  = "5300108050801951"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080464 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080464", "530010805080690", "530010805080465", "530010805080534", "530010805080535", "530010805080555", "530010805080536", "530010805080537", "530010805080538", "530010805080561", "530010805080698"),
  tract_2000   = "530010805080464",
  new_code  = "5300108050804641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080200 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080200", "530010805080644"),
  tract_2000   = "530010805080200",
  new_code  = "5300108050802001"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080228 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080228", "530010805080650"),
  tract_2000   = "530010805080228",
  new_code  = "5300108050802281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080224 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080224", "530010805080649"),
  tract_2000   = "530010805080224",
  new_code  = "5300108050802241"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080222 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080222", "530010805080648"),
  tract_2000   = "530010805080222",
  new_code  = "5300108050802221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080221 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080221", "530010805080647"),
  tract_2000   = "530010805080221",
  new_code  = "5300108050802211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080202 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080202", "530010805080645"),
  tract_2000   = "530010805080202",
  new_code  = "5300108050802021"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080203 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080203", "530010805080646"),
  tract_2000   = "530010805080203",
  new_code  = "5300108050802031"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080230 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080230", "530010805080499"),
  tract_2000   = "530010805080230",
  new_code  = "5300108050802301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080245 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080245", "530010805080519"),
  tract_2000   = "530010805080245",
  new_code  = "5300108050802451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080247 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080247", "530010805080520"),
  tract_2000   = "530010805080247",
  new_code  = "5300108050802471"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080254 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080254", "530010805080660"),
  tract_2000   = "530010805080254",
  new_code  = "5300108050802541"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080255 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080255", "530010805080661"),
  tract_2000   = "530010805080255",
  new_code  = "5300108050802551"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080263 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080263", "530010805080662"),
  tract_2000   = "530010805080263",
  new_code  = "5300108050802631"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080301 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080301", "530010805080670"),
  tract_2000   = "530010805080301",
  new_code  = "5300108050803011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080303 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080303", "530010805080671"),
  tract_2000   = "530010805080303",
  new_code  = "5300108050803031"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080324 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080324", "530010805080672"),
  tract_2000   = "530010805080324",
  new_code  = "5300108050803241"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080250 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080250", "530010805080658", "530010805080521"),
  tract_2000   = "530010805080250",
  new_code  = "5300108050802501"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080252 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080252", "530010805080522"),
  tract_2000   = "530010805080252",
  new_code  = "5300108050802521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080253 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080659", "530010805080253"),
  tract_2000   = "530010805080253",
  new_code  = "5300108050802531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080265 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080265", "530010805080663"),
  tract_2000   = "530010805080265",
  new_code  = "5300108050802651"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080266 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080266", "530010805080664"),
  tract_2000   = "530010805080266",
  new_code  = "5300108050802661"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080268 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080268", "530010805080523"),
  tract_2000   = "530010805080268",
  new_code  = "5300108050802681"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080270 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080270", "530010805080665"),
  tract_2000   = "530010805080270",
  new_code  = "5300108050802701"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080272 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080272", "530010805080666"),
  tract_2000   = "530010805080272",
  new_code  = "5300108050802721"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080283 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080667", "530010805080283"),
  tract_2000   = "530010805080283",
  new_code  = "5300108050802831"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080293 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080293", "530010805080668"),
  tract_2000   = "530010805080293",
  new_code  = "5300108050802931"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080300 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080300", "530010805080669"),
  tract_2000   = "530010805080300",
  new_code  = "5300108050803001"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080308 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080524", "530010805080308"),
  tract_2000   = "530010805080308",
  new_code  = "5300108050803081"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080332 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080332", "530010805080525"),
  tract_2000   = "530010805080332",
  new_code  = "5300108050803321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080349 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080349", "530010805080674", "530010805080529"),
  tract_2000   = "530010805080349",
  new_code  = "5300108050803491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080341 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080341", "530010805080673"),
  tract_2000   = "530010805080341",
  new_code  = "5300108050803411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080346 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080346", "530010805080528"),
  tract_2000   = "530010805080346",
  new_code  = "5300108050803461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080345 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080345", "530010805080526", "530010805080527"),
  tract_2000   = "530010805080345",
  new_code  = "5300108050803451"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080357 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080357", "530010805080675"),
  tract_2000   = "530010805080357",
  new_code  = "5300108050803571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080364 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080530", "530010805080531", "530010805080364", "530010805080704", "530010805080705"),
  tract_2000   = "530010805080364",
  new_code  = "5300108050803641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080375 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080375", "530010805080532"),
  tract_2000   = "530010805080375",
  new_code  = "5300108050803751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080397 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080397", "530010805080679"),
  tract_2000   = "530010805080397",
  new_code  = "5300108050803971"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080425 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080425", "530010805080680"),
  tract_2000   = "530010805080425",
  new_code  = "5300108050804251"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080369 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080369", "530010805080676"),
  tract_2000   = "530010805080369",
  new_code  = "5300108050803691"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080394 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080394", "530010805080677"),
  tract_2000   = "530010805080394",
  new_code  = "5300108050803941"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080395 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080395", "530010805080678"),
  tract_2000   = "530010805080395",
  new_code  = "5300108050803951"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080396 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080396", "530010805080720"),
  tract_2000   = "530010805080396",
  new_code  = "5300108050803961"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150163 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150163", "530010805150426"),
  tract_2000   = "530010805150163",
  new_code  = "5300108051501631"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150162 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150424", "530010805150425", "530010805150162"),
  tract_2000   = "530010805150162",
  new_code  = "5300108051501621"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150104 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150104", "530010805150412"),
  tract_2000   = "530010805150104",
  new_code  = "5300108051501041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150019 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150019", "530010805150401"),
  tract_2000   = "530010805150019",
  new_code  = "5300108051500191"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150016 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150016", "530010805150485"),
  tract_2000   = "530010805150016",
  new_code  = "5300108051500161"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150012 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150012", "530010805150484"),
  tract_2000   = "530010805150012",
  new_code  = "5300108051500121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150009 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150009", "530010805150400"),
  tract_2000   = "530010805150009",
  new_code  = "5300108051500091"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150033 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150402", "530010805150033"),
  tract_2000   = "530010805150033",
  new_code  = "5300108051500331"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150064 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150064", "530010805150487"),
  tract_2000   = "530010805150064",
  new_code  = "5300108051500641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150061 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150061", "530010805150486"),
  tract_2000   = "530010805150061",
  new_code  = "5300108051500611"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150060 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150060", "530010805150406"),
  tract_2000   = "530010805150060",
  new_code  = "5300108051500601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150084 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150084", "530010805150407"),
  tract_2000   = "530010805150084",
  new_code  = "5300108051500841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150085 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150085", "530010805150408"),
  tract_2000   = "530010805150085",
  new_code  = "5300108051500851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150086 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150086", "530010805150409"),
  tract_2000   = "530010805150086",
  new_code  = "5300108051500861"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150090 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150090", "530010805150488"),
  tract_2000   = "530010805150090",
  new_code  = "5300108051500901"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150122 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150122", "530010805150493"),
  tract_2000   = "530010805150122",
  new_code  = "5300108051501221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150114 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150114", "530010805150492"),
  tract_2000   = "530010805150114",
  new_code  = "5300108051501141"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150113 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150113", "530010805150414"),
  tract_2000   = "530010805150113",
  new_code  = "5300108051501131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150112 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150112", "530010805150413"),
  tract_2000   = "530010805150112",
  new_code  = "5300108051501121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150111 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150111", "530010805150491"),
  tract_2000   = "530010805150111",
  new_code  = "5300108051501111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150128 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150128", "530010805150386"),
  tract_2000   = "530010805150128",
  new_code  = "5300108051501281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150130 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150552", "530010805150553", "530010805150549", "530010805150551", "530010805150546", "530010805150547", "530010805150548", "530010805150130"),
  tract_2000   = "530010805150130",
  new_code  = "5300108051501301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150131 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150518", "530010805150519", "530010805150537", "530010805150514", "530010805150516", "530010805150517", "530010805150131", "530010805150515"),
  tract_2000   = "530010805150131",
  new_code  = "5300108051501311"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150132 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150132", "530010805150415"),
  tract_2000   = "530010805150132",
  new_code  = "5300108051501321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150151 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150151", "530010805150420"),
  tract_2000   = "530010805150151",
  new_code  = "5300108051501511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150150 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150150", "530010805150419"),
  tract_2000   = "530010805150150",
  new_code  = "5300108051501501"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150149 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150149", "530010805150418"),
  tract_2000   = "530010805150149",
  new_code  = "5300108051501491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150157 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150157", "530010805150421"),
  tract_2000   = "530010805150157",
  new_code  = "5300108051501571"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150160 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150160", "530010805150422"),
  tract_2000   = "530010805150160",
  new_code  = "5300108051501601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150161 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150161", "530010805150423"),
  tract_2000   = "530010805150161",
  new_code  = "5300108051501611"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080240 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080517", "530010805080719", "530010805080728", "530010805080240"),
  tract_2000   = "530010805080240",
  new_code  = "5300108050802401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080430 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080430", "530010805080681"),
  tract_2000   = "530010805080430",
  new_code  = "5300108050804301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080435 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080435", "530010805080683"),
  tract_2000   = "530010805080435",
  new_code  = "5300108050804351"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080443 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080443", "530010805080684"),
  tract_2000   = "530010805080443",
  new_code  = "5300108050804431"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080446 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080446", "530010805080685"),
  tract_2000   = "530010805080446",
  new_code  = "5300108050804461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080449 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080449", "530010805080686"),
  tract_2000   = "530010805080449",
  new_code  = "5300108050804491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080452 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080452", "530010805080687"),
  tract_2000   = "530010805080452",
  new_code  = "5300108050804521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080454 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080454", "530010805080688"),
  tract_2000   = "530010805080454",
  new_code  = "5300108050804541"
)

# Merge the 2010 tracts that make up 2000 tract 530010805080460 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805080460", "530010805080689"),
  tract_2000   = "530010805080460",
  new_code  = "5300108050804601"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150164 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150164", "530010805150427"),
  tract_2000   = "530010805150164",
  new_code  = "5300108051501641"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150165 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150165", "530010805150428"),
  tract_2000   = "530010805150165",
  new_code  = "5300108051501651"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150166 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150429", "530010805150430", "530010805150521", "530010805150166"),
  tract_2000   = "530010805150166",
  new_code  = "5300108051501661"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150222 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150222", "530010805150439"),
  tract_2000   = "530010805150222",
  new_code  = "5300108051502221"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150313 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150313", "530010805150512"),
  tract_2000   = "530010805150313",
  new_code  = "5300108051503131"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150314 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150314", "530010805150392", "530010805150501"),
  tract_2000   = "530010805150314",
  new_code  = "5300108051503141"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150312 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150312", "530010805150391"),
  tract_2000   = "530010805150312",
  new_code  = "5300108051503121"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150317 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150317", "530010805150502"),
  tract_2000   = "530010805150317",
  new_code  = "5300108051503171"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150320 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150320", "530010805150393"),
  tract_2000   = "530010805150320",
  new_code  = "5300108051503201"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150377 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150377", "530010805150508"),
  tract_2000   = "530010805150377",
  new_code  = "5300108051503771"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150380 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150380", "530010805150509"),
  tract_2000   = "530010805150380",
  new_code  = "5300108051503801"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150361 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150361", "530010805150506"),
  tract_2000   = "530010805150361",
  new_code  = "5300108051503611"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150362 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150362", "530010805150507"),
  tract_2000   = "530010805150362",
  new_code  = "5300108051503621"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150337 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150337", "530010805150503"),
  tract_2000   = "530010805150337",
  new_code  = "5300108051503371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150300 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150300", "530010805150390"),
  tract_2000   = "530010805150300",
  new_code  = "5300108051503001"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150297 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150297", "530010805150389"),
  tract_2000   = "530010805150297",
  new_code  = "5300108051502971"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150352 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150352", "530010805150504"),
  tract_2000   = "530010805150352",
  new_code  = "5300108051503521"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150353 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150353", "530010805150505"),
  tract_2000   = "530010805150353",
  new_code  = "5300108051503531"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150348 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150348", "530010805150396"),
  tract_2000   = "530010805150348",
  new_code  = "5300108051503481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150349 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150349", "530010805150397"),
  tract_2000   = "530010805150349",
  new_code  = "5300108051503491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150342 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150342", "530010805150395"),
  tract_2000   = "530010805150342",
  new_code  = "5300108051503421"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150307 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150307", "530010805150500"),
  tract_2000   = "530010805150307",
  new_code  = "5300108051503071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150305 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150305", "530010805150499"),
  tract_2000   = "530010805150305",
  new_code  = "5300108051503051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150279 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150279", "530010805150451"),
  tract_2000   = "530010805150279",
  new_code  = "5300108051502791"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150280 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150280", "530010805150452"),
  tract_2000   = "530010805150280",
  new_code  = "5300108051502801"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150283 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150283", "530010805150496"),
  tract_2000   = "530010805150283",
  new_code  = "5300108051502831"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150284 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150284", "530010805150453"),
  tract_2000   = "530010805150284",
  new_code  = "5300108051502841"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150285 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150285", "530010805150454"),
  tract_2000   = "530010805150285",
  new_code  = "5300108051502851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150286 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150286", "530010805150388"),
  tract_2000   = "530010805150286",
  new_code  = "5300108051502861"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150291 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150291", "530010805150497"),
  tract_2000   = "530010805150291",
  new_code  = "5300108051502911"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150293 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150293", "530010805150498"),
  tract_2000   = "530010805150293",
  new_code  = "5300108051502931"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150268 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150268", "530010805150446"),
  tract_2000   = "530010805150268",
  new_code  = "5300108051502681"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150269 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150269", "530010805150447"),
  tract_2000   = "530010805150269",
  new_code  = "5300108051502691"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150276 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150276", "530010805150448"),
  tract_2000   = "530010805150276",
  new_code  = "5300108051502761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150277 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150277", "530010805150449"),
  tract_2000   = "530010805150277",
  new_code  = "5300108051502771"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150278 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150278", "530010805150450"),
  tract_2000   = "530010805150278",
  new_code  = "5300108051502781"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150244 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150244", "530010805150442"),
  tract_2000   = "530010805150244",
  new_code  = "5300108051502441"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150248 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150248", "530010805150495"),
  tract_2000   = "530010805150248",
  new_code  = "5300108051502481"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150249 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150249", "530010805150443"),
  tract_2000   = "530010805150249",
  new_code  = "5300108051502491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150251 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150251", "530010805150444"),
  tract_2000   = "530010805150251",
  new_code  = "5300108051502511"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150258 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150258", "530010805150445"),
  tract_2000   = "530010805150258",
  new_code  = "5300108051502581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150229 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150229", "530010805150440"),
  tract_2000   = "530010805150229",
  new_code  = "5300108051502291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150232 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150232", "530010805150441"),
  tract_2000   = "530010805150232",
  new_code  = "5300108051502321"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150241 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150241", "530010805150540"),
  tract_2000   = "530010805150241",
  new_code  = "5300108051502411"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150206 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150206", "530010805150438"),
  tract_2000   = "530010805150206",
  new_code  = "5300108051502061"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150207 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150207", "530010805150494"),
  tract_2000   = "530010805150207",
  new_code  = "5300108051502071"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150201 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150201", "530010805150435"),
  tract_2000   = "530010805150201",
  new_code  = "5300108051502011"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150204 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150204", "530010805150436"),
  tract_2000   = "530010805150204",
  new_code  = "5300108051502041"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150205 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150205", "530010805150437"),
  tract_2000   = "530010805150205",
  new_code  = "5300108051502051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150176 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150520", "530010805150176"),
  tract_2000   = "530010805150176",
  new_code  = "5300108051501761"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150182 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150182", "530010805150434"),
  tract_2000   = "530010805150182",
  new_code  = "5300108051501821"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150169 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150169", "530010805150431"),
  tract_2000   = "530010805150169",
  new_code  = "5300108051501691"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150170 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150170", "530010805150432"),
  tract_2000   = "530010805150170",
  new_code  = "5300108051501701"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150171 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150171", "530010805150433"),
  tract_2000   = "530010805150171",
  new_code  = "5300108051501711"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150175 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150175", "530010805150541", "530010805150542", "530010805150543", "530010805150544", "530010805150545", "530010805150538", "530010805150539"),
  tract_2000   = "530010805150175",
  new_code  = "5300108051501751"
)

# Merge the 2010 tracts that make up 2000 tract 530010805150390 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805150458", "530010805150464", "530010805150465", "530010805150462", "530010805150463", "530010805150466", "530010805150460", "530010805150461"),
  tract_2000   = "530010805150390",
  new_code  = "5300108051503901"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070152-0155 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070170", "530010805070171"),
  tract_2000   = "530010805070152-0155",
  new_code  = "530010805070152-01551"
)

# Merge the 2010 tracts that make up 2000 tract 530010805100140 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805100233", "530010805100212"),
  tract_2000   = "530010805100140",
  new_code  = "5300108051001401"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110129 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110129", "530010805110257"),
  tract_2000   = "530010805110129",
  new_code  = "5300108051101291"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110126 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110126", "530010805110253", "530010805110254"),
  tract_2000   = "530010805110126",
  new_code  = "5300108051101261"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110121 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110121", "530010805110250"),
  tract_2000   = "530010805110121",
  new_code  = "5300108051101211"
)

# Merge the 2010 tracts that make up 2000 tract 530010805110059 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805110059", "530010805110223"),
  tract_2000   = "530010805110059",
  new_code  = "5300108051100591"
)

# Merge the 2010 tracts that make up 2000 tract 530010805300037 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805300037", "530010805300069"),
  tract_2000   = "530010805300037",
  new_code  = "5300108053000371"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070111 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070111", "530010805070227"),
  tract_2000   = "530010805070111",
  new_code  = "5300108050701111"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070085 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070085", "530010805070148"),
  tract_2000   = "530010805070085",
  new_code  = "5300108050700851"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070095 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070095", "530010805070149"),
  tract_2000   = "530010805070095",
  new_code  = "5300108050700951"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070094 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070094", "530010805070194"),
  tract_2000   = "530010805070094",
  new_code  = "5300108050700941"
)

# Merge the 2010 tracts that make up 2000 tract 530010805070099 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805070099", "530010805070197"),
  tract_2000   = "530010805070099",
  new_code  = "5300108050700991"
)

# Merge the 2010 tracts that make up 2000 tract 530010805120005 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805120005", "530010805120048"),
  tract_2000   = "530010805120005",
  new_code  = "5300108051200051"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200036 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200146", "530010805200036"),
  tract_2000   = "530010805200036",
  new_code  = "5300108052000361"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200046 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200153", "530010805200046"),
  tract_2000   = "530010805200046",
  new_code  = "5300108052000461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200017 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200017", "530010805200080"),
  tract_2000   = "530010805200017",
  new_code  = "5300108052000171"
)

# Merge the 2010 tracts that make up 2000 tract 530010805200018 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805200018", "530010805200081"),
  tract_2000   = "530010805200018",
  new_code  = "5300108052000181"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060093 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060093", "530010805060394"),
  tract_2000   = "530010805060093",
  new_code  = "5300108050600931"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060028 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060028", "530010805060298"),
  tract_2000   = "530010805060028",
  new_code  = "5300108050600281"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060091 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060091", "530010805060393"),
  tract_2000   = "530010805060091",
  new_code  = "5300108050600911"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060030 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060030", "530010805060299"),
  tract_2000   = "530010805060030",
  new_code  = "5300108050600301"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060083 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060083", "530010805060391"),
  tract_2000   = "530010805060083",
  new_code  = "5300108050600831"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060049 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060049", "530010805060384"),
  tract_2000   = "530010805060049",
  new_code  = "5300108050600491"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060050 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060050", "530010805060310"),
  tract_2000   = "530010805060050",
  new_code  = "5300108050600501"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060046 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060046", "530010805060308"),
  tract_2000   = "530010805060046",
  new_code  = "5300108050600461"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060258 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060258", "530010805060353", "530010805060354", "530010805060355"),
  tract_2000   = "530010805060258",
  new_code  = "5300108050602581"
)

# Merge the 2010 tracts that make up 2000 tract 530010805060262 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805060262", "530010805060419", "530010805060420"),
  tract_2000   = "530010805060262",
  new_code  = "5300108050602621"
)

# Merge the 2010 tracts that make up 2000 tract 530010805090054 and replace them with the new tract
census_sf_harmonized <- merge_tracts_across_years(
  df = census_sf_harmonized,
  tracts_2010 = c("530010805090063", "530010805090082", "530010805090100", "530010805090053", "530010805090054", "530010805090055", "530010805090091", "530010805090052"),
  tract_2000   = "530010805090054",
  new_code  = "5300108050900541"
)

return(census_sf_harmonized)
}

#
# st_write(census_sf_harmonized, "census_sf_harmonized.geojson", delete_dsn = TRUE)

# Drop irrelevant observations
#census_sf_harmonized <- census_sf_harmonized %>%
#  filter(!(code_tract %in% c("530010805070001-0142", "530010805250001-0113", "530010805200001-0073")))
