risks_table_cleaned <- 
  risks_table_mapped %>% 
  
  # Oldest model year in CY2012 data should be 2013
  filter(vehicle_year <= 2013) %>% 
  
  # Bucket older cars where data is thin.  Perhaps a different cutoff?
  mutate(vehicle_year = 
           if_else(vehicle_year < 1980, "<1980", as.character(vehicle_year)) %>%
           as.factor()) %>% 
  
  # The Jurídica and Sem Informação levels are missing claims/premiums.  
  # Group together for now into 'other'
  mutate(sex = 
           sex %>%
           as.factor() %>% 
           fct_collapse(male = 'Masculino',
                        female = 'Feminino',
                        other = c('Jurídica','Sem Informação')) %>% 
           fct_explicit_na('unknown')) %>% 
  
  # Translate age ranges
  mutate(age_range = 
           age_range %>% 
           as.factor() %>% 
           fct_recode(`18 to 25` = 'Entre 18 e 25 anos',
                      `26 to 35` = 'Entre 26 e 35 anos',
                      `36 to 45` = 'Entre 36 e 45 anos',
                      `46 to 55` = 'Entre 46 e 55 anos',
                      `56+` = 'Maior que 55 anos',
                      unknown = 'Não informada')) %>% 
  
  # Translate vehicle types
  mutate(vehicle_category = 
           vehicle_category %>% 
           as.factor() %>% 
           fct_recode(passenger_domestic = 'Passeio nacional',
                      passenger_import = 'Passeio importado',
                      pickup = 'Pick-up (nacional e importado)',
                      cargo_van = 'Veículo de Carga (nacional e importado)',
                      motorcycle = 'Motocicleta (nacional e importado)',
                      bus = 'Ônibus (nacional e importado)',
                      utility = 'Utilitários (nacional e importado)',
                      other = 'Outros')) %>% 
  
  # further refine region into state and subregion components
  separate(region, c('state_abbrev', 'subregion'), sep = " - ")


## Add 'all coverages' claim counts and amounts

risks_table_agg <-
  risks_table_cleaned %>% 
  mutate(claim_amount_all = select(., starts_with("claim_amount")) %>% rowSums,
         claim_count_all = select(., starts_with("claim_count")) %>% rowSums) %>% 
  
  # add risk id.  move this to risks_table_mapped definition?
  mutate(risk_id = row_number()) %>% 

  # reorder columns
  select(-starts_with("claim_count"), everything()) %>% 
  select(-starts_with("claim_amount"), everything())
  


