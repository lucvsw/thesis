# This script takes the census-tract tables provided by IBGE and joins them with the tract shapefiles of both years

# Getting the data frames to be merged with the 2000 census shapefile
get_census_tables_2000 <- function() {
  # Basic table, 2000
  census_2000_basic <- read_excel(here("data", "raw", "census", "2000", "Basico_DF.XLS"))
  
  # Household heads table, 2000
  census_2000_head <- read_excel(here("data", "raw", "census", "2000", "Responsavel1_DF.XLS"))
  
  # Persons table, 2000
  census_2000_person <- read_excel(here("data", "raw", "census", "2000", "Pessoa1_DF.XLS"))
  
  # Education table, 2000
  census_2000_education <- read_excel(here("data", "raw", "census", "2000", "Instrucao1_DF.XLS"))
  
  # Households table, 2000
  census_2000_household <- read_excel(here("data", "raw", "census", "2000", "Domicilio_DF.XLS"))

  # Residents table, 2000 (total and by sex) — for share_women
  census_2000_resident <- read_excel(
    here("data", "raw", "census", "2000", "Morador_DF.XLS")
  )

  return(list(
    census_2000_basic = census_2000_basic,
    census_2000_head = census_2000_head,
    census_2000_person = census_2000_person,
    census_2000_education = census_2000_education,
    census_2000_household = census_2000_household,
    census_2000_resident = census_2000_resident
  ))
}

# Merging the data with the 2000 shapefile
join_tables_sf_2000 <- function(census_sf_2000_full, data_list) {
  # Extract the objects from the list
  census_2000_basic <- data_list$census_2000_basic
  census_2000_head <- data_list$census_2000_head
  census_2000_person <- data_list$census_2000_person
  census_2000_education <- data_list$census_2000_education
  census_2000_household <- data_list$census_2000_household
  census_2000_resident <- data_list$census_2000_resident
  
  census_2000_full <- census_sf_2000_full %>%
    left_join(census_2000_basic %>% dplyr::select(Cod_setor, Var01, Var02, Var03, Var05, Var06, Var12),
              by = c("code_tract" = "Cod_setor")) %>%
    left_join(
      census_2000_head %>%
        dplyr::transmute(
          Cod_setor, V0595, V0611, V0577, V0402, V0509,
          high_income_heads = as.numeric(V0608) + as.numeric(V0609) + as.numeric(V0610)
        ),
      by = c("code_tract" = "Cod_setor")) %>%
    left_join(
      census_2000_person %>%
        dplyr::transmute(
          Cod_setor, V1461, V1462, V1463, V1464,
          pop_working_age     = as.numeric(V1451) + as.numeric(V1452) + as.numeric(V1453) +
                          as.numeric(V1454) + as.numeric(V1455) + as.numeric(V1456) +
                          as.numeric(V1457) + as.numeric(V1458) + as.numeric(V1459) +
                          as.numeric(V1460),
          total_person1 = as.numeric(V1330)
        ),
      by = c("code_tract" = "Cod_setor")) %>% 
    left_join(census_2000_education %>% dplyr::select(Cod_setor, V2249),
              by = c("code_tract" = "Cod_setor")) %>%
    left_join(
      census_2000_household %>%
        transmute(
          Cod_setor,
          V0007,
          hh_5plus = as.numeric(V0060) + as.numeric(V0061) + as.numeric(V0062) +
                      as.numeric(V0063) + as.numeric(V0064) + as.numeric(V0065)
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    left_join(
      census_2000_resident %>%
        transmute(
          Cod_setor = as.character(Cod_setor),
          total_residents = as.numeric(V0237),
          male_residents = as.numeric(V0292)
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    rename(
      households = Var01,
      income_total = Var02,
      income_per_head = Var03,
      heads_positive_income = Var05,
      mean_income_positive_heads = Var06,
      pop = Var12,
      higher_ed = V0595,
      no_income = V0611,
      age_65 = V1461,
      age_70 = V1462,
      age_75 = V1463,
      age_80 = V1464,
      illiterate = V2249,
      apartments = V0007,
      college_complete = V0577,
      heads_total = V0402,
      heads_literate = V0509
    ) %>%
    mutate(year = 2000)
  
  return(census_2000_full)
}

###############################################################################

# Getting the data frames to be merged with the 2010 tract shapefile
get_census_tables_2010 <- function() {
  # Basic table, 2010
  census_2010_basic <- read_excel(here("data", "raw", "census", "2010", "Basico_DF.XLS"))
  census_2010_basic <- census_2010_basic %>%
    mutate(Cod_setor = as.character(Cod_setor))
  
  # Household heads/income table
  census_2010_head <- read_excel(here("data", "raw", "census", "2010", "ResponsavelRenda_DF.XLS"))
  census_2010_head <- census_2010_head %>%
    mutate(Cod_setor = as.character(Cod_setor))
  
  # Households table (structure and type)
  census_2010_household <- read_excel(here("data", "raw", "census", "2010", "Domicilio01_DF.XLS"))
  census_2010_household <- census_2010_household %>%
    mutate(Cod_setor = as.character(Cod_setor))

  # Residents table, 2010 (total and by sex) — for share_women
  base2010 <- here("data", "raw", "census", "2010")
  census_2010_household02 <- read_excel(file.path(base2010, "Domicilio02_DF.xls"))
  census_2010_household02 <- census_2010_household02 %>%
    mutate(Cod_setor = as.character(Cod_setor))

  # Household heads table, 2010 (illiteracy) — for share_illiterate_heads
  census_2010_head02 <- read_excel(file.path(base2010, "Responsavel02_DF.xls"))
  census_2010_head02 <- census_2010_head02 %>%
    mutate(Cod_setor = as.character(Cod_setor))

  # Persons by age table, 2010 (elderly) — for share_over_65 in 2010
  census_2010_person13 <- read_excel(file.path(base2010, "Pessoa13_DF.xls"))
  census_2010_person13 <- census_2010_person13 %>%
    mutate(Cod_setor = as.character(Cod_setor))

  return(list(
    census_2010_basic = census_2010_basic,
    census_2010_head = census_2010_head,
    census_2010_household = census_2010_household,
    census_2010_household02 = census_2010_household02,
    census_2010_head02 = census_2010_head02,
    census_2010_person13 = census_2010_person13
  ))
}

# Merging the data with the 2010 shapefile
join_tables_sf_2010 <- function(census_sf_2010, data_list, census_2000_full) {
  census_2010_basic <- data_list$census_2010_basic
  census_2010_head <- data_list$census_2010_head
  census_2010_household <- data_list$census_2010_household
  census_2010_household02 <- data_list$census_2010_household02
  census_2010_head02 <- data_list$census_2010_head02
  census_2010_person13 <- data_list$census_2010_person13
  
  # Adding the data to the 2010 shapefile
  census_2010_full <- census_sf_2010 %>%
    left_join(census_2010_basic %>% dplyr::select(Cod_setor, V001, V002),
              by = c("code_tract" = "Cod_setor")) %>%
    left_join(
      census_2010_head %>%
        dplyr::transmute(
          Cod_setor, V021, V022,
          high_income_heads  = suppressWarnings(
            as.numeric(ifelse(V007 == "X", "0", V007)) +
            as.numeric(ifelse(V008 == "X", "0", V008)) +
            as.numeric(ifelse(V009 == "X", "0", V009))
          ),
          heads_income_total = suppressWarnings(as.numeric(ifelse(V020 == "X", "0", V020)))
        ),
      by = c("code_tract" = "Cod_setor")) %>%
    left_join(
      census_2010_household %>%
        transmute(
          Cod_setor,
          V005,
          hh_5plus = suppressWarnings(
            as.numeric(V054) + as.numeric(V055) + as.numeric(V056) +
            as.numeric(V057) + as.numeric(V058) + as.numeric(V059)
          )
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    left_join(
      census_2010_household02 %>%
        transmute(
          Cod_setor,
          total_residents = suppressWarnings(as.numeric(V001)),
          male_residents = suppressWarnings(as.numeric(V045))
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    left_join(
      census_2010_head02 %>%
        transmute(
          Cod_setor,
          heads_total = suppressWarnings(as.numeric(V001)),
          heads_literate = suppressWarnings(as.numeric(V093))
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    left_join(
      census_2010_person13 %>%
        transmute(
          Cod_setor,
          elderly_total    = suppressWarnings(
            rowSums(across(all_of(paste0("V", formatC(99:134, width = 3, flag = "0"))),
                           as.numeric), na.rm = TRUE)
          ),
          pop_working_age_total = suppressWarnings(
            rowSums(across(all_of(paste0("V", formatC(49:98, width = 3, flag = "0"))),
                           as.numeric), na.rm = TRUE)
          ),
          total_person13  = suppressWarnings(as.numeric(V001))
        ),
      by = c("code_tract" = "Cod_setor")
    ) %>%
    rename(households = V001,
           pop = V002,
           heads_positive_income = V021,
           income_total = V022,
           apartments = V005) %>%
    mutate(
      year = 2010,
      # Some rows of the variables below have the value "X"; replace "X" with "0" (as a string), then convert the values of these variables to numbers
      income_total = parse_number(
        if_else(income_total == "X", "0", income_total),
        locale = locale(decimal_mark = ",")
      ),
      heads_positive_income = parse_number(
        if_else(heads_positive_income == "X", "0", heads_positive_income),
        locale = locale(decimal_mark = ",")
      ),
      apartments = parse_number(
        if_else(apartments == "X", "0", apartments),
        locale = locale(decimal_mark = ",")
      ),
    )
  
  # Check which columns exist in 2000 but not in 2010
  missing_columns <- setdiff(names(census_2000_full), names(census_2010_full))
  
  # Create these columns in census_2010_full filled with NA
  for (col in missing_columns) {
    census_2010_full[[col]] <- NA
  }
  
  return(census_2010_full)
}

# Merge the 2000 and 2010 census data and turn them into a two-year time series
prepare_for_harmonization <- function(census_2000_full, census_2010_full) {
  # Merge the data
  census_panel <- bind_rows(census_2010_full, census_2000_full)
  
  # Remove duplicates
  census_sf_to_harmonize <- census_panel %>%
    distinct(code_tract, year, pop, geom, .keep_all = TRUE) # If two rows have exactly the same values in these columns, one of them is removed
  
  return(census_sf_to_harmonize)
}

