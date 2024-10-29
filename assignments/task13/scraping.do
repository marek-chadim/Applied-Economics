insheet using numbeo_cost_of_living_all_countries.csv, clear names 
log using stata_codebook, text replace
codebook, c
log close
save scraping, replace