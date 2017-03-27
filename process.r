### Process ###
process_ahgm = function(ahgm_raw) {
  
  ### Clone list
  ahgm_processed = ahgm_raw
  cat("processing AHGM \n")
  
  ### Trim and tolower everything
  ahgm_processed = lapply(ahgm_processed, function(x) import_string_normalize(x))
  cat("*** All Datasets string normalized \n")
  
  ### Add tenure from ct codes & inc_code descriptions
  ahgm_processed = process_codes(ahgm_processed)
  cat(" Tenure codes added to CT \n")
  
  ### Individual processing
  cat("processing AHGM \n")
  ahgm_live_processed = process_ahgm_live(ahgm_processed$ahgm)
  cat("processing ahgm_uprn \n")
  ahgm_uprn_processed = process_ahgm_uprn(ahgm_processed$ahgm_uprn)
  cat("processing benefits \n")
  benefits_processed = process_benefits(ahgm_processed$benefits)
  cat("processing ct \n")
  ct_processed = process_ct(ahgm_processed$ct)
  cat("processing finref_uprn \n")
  finref_uprn_processed = process_finref_uprn(ahgm_processed$finref_uprn)
  cat("processing ctr \n")
  ctr_processed = process_ctr(ahgm_processed$ctr)
  cat("processing hb_household \n")
  hb_household_processed = process_hb_household(ahgm_processed$hb_household)
  cat("processing hb_income \n")
  hb_income_processed = process_hb_income(ahgm_processed$hb_income)
  cat("processing hb_member \n")
  hb_member_processed = process_hb_member(ahgm_processed$hb_member)
  
  return(list(ahgm = ahgm_live_processed, ahgm_uprn = ahgm_uprn_processed, benefits = benefits_processed, finref_uprn = finref_uprn_processed, ct = ct_processed, ctr= ctr_processed, hb_income=hb_income_processed, hb_member = hb_member_processed, hb_household = hb_household_processed, income_codes = ahgm_processed$income_codes))
}

process_ahgm_live = function(ahgm){
  ahgm_live_processed = ahgm
  
  ### fix colnames
  colnames(ahgm_live_processed) = c("plot", "forename", "surname", "house_no", "flat", "street", "postcode", "age", "health", "ward", "neighbourhood_no", "neighbourhood_name")
  
  ### plot to char
  ahgm_live_processed$plot = as.character(ahgm_live_processed$plot)
  
  return(ahgm_live_processed)
}

process_ahgm_uprn = function(ahgm_uprn){
  ahgm_uprn_processed = ahgm_uprn
  
  ### subset and rename columns
  ahgm_uprn_processed = ahgm_uprn_processed %>% select(PLOT_NO, UPRN, STREET_NAM, POSTCODE, POST_TOWN, ADDRESS, EASTING, NORTHING, CLASS_DESC) %>% `colnames<-`(c("plot", "uprn", "street_name", "postcode", "post_town", "address", "easting", "northing", "house_type"))
  
  ### uprn and plot as character
  ahgm_uprn_processed[,c("plot", "uprn")] = lapply(ahgm_uprn_processed[,c("plot", "uprn")], function(x) as.character(x))
  
  ### Return
  return(ahgm_uprn_processed)
}

process_benefits = function(benefits){
  benefits_processed = benefits
  
  ### dob
  benefits_processed$claimant_dob = as.Date(benefits_processed$claimant_dob, "%d.%m.%Y")
  
  ### cols to character
  benefits_processed[,c("claim_id", "property_ref")] = lapply(benefits_processed[,c("claim_id", "property_ref")], function(x) as.character(x))
  
  return(benefits_processed)
}

process_ct = function(ct){
  ct_processed = ct
  
  ### Colnames
  colnames(ct_processed) = c("property_ref", "lead_name", "street", "addr1", "addr2", "addr3", "addr4", "postcode", "owner_code", "owner_name", "owner_addr1", "owner_addr2", "owner_addr3", "owner_addr4", "owner_postcode", "schedule_code", "schedule_name", "schedule_addr1", "schedule_addr2", "schedule_addr3", "schedule_addr4", "schedule_postcode", "unoccupied_allowance", "disc_type", "allowance", "owner_code_tenure", "schedule_code_tenure")
  
  ### as character
  ct_processed[,c("property_ref", "owner_code")] = lapply(ct_processed[,c("property_ref", "owner_code")], function(x) as.character(x))
  

  
  
  return(ct_processed)
}

process_finref_uprn = function(finref_uprn){
  finref_uprn_processed = finref_uprn
  
  ### as character
  finref_uprn_processed = as.data.frame(lapply(finref_uprn_processed, function(x) as.character(x)), stringsAsFactors = FALSE)
  
  return(finref_uprn_processed)
}

process_ctr = function(ctr){
  ctr_processed = ctr
  
  ### DOB
  ctr_processed$claimant_dob = as.Date(ctr_processed$claimant_dob, "%d.%m.%Y")
  
  ### as numeric amounts
  ctr_processed[,c("ctr_amt", "ctax_charge", "dla_high", "dla_med", "dla_low", "ata")] = as.data.frame(lapply(ctr_processed[,c("ctr_amt", "ctax_charge", "dla_high", "dla_med", "dla_low", "ata")], as.numeric), stringsAsFactors = FALSE)
  
  ### property_ref as character
  ctr_processed$property_ref = as.character(ctr_processed$property_ref)
  
  ### total DLA
  ctr_processed$dla_total = ctr_processed$dla_high + ctr_processed$dla_med + ctr_processed$dla_low
  
  ### chop of last claim_ref digit
  ctr_processed$claim_id8 = str_match(ctr_processed$claim_ref, "(.+)(.)$")[,2]
  
  ### Tag claimants over 70
  age_cutoff = seq(as.Date(Sys.Date()), length=2, by="-70 years")[2]
  ctr_processed %>% mutate(claimant_over_70 = claimant_dob <= age_cutoff) %>% head()
  
  return(ctr_processed)
}

process_hb_income = function(hb_income){
  hb_income_processed = hb_income
  
  incomes_formula = function(incomes_row){
    # incomes_row = data.frame(t(incomes_row), stringsAsFactors=FALSE)
    if(incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 0){
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_len"] * 7
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 0) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) * 7
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 2) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_period"]
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 2) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"])
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 3) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_period"] / 52
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 3) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) * 12 / 52
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 4) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) * 2 / incomes_row["freq_period"] / 52
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 4) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) * 2 / 52
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 5) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_period"] / 52
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 5) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / 52
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 14) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_period"] / 52
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 14) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / 52
    } else {
      -1
    }
  }
  
  ### Apply the formula row wise
  weekly_income = apply(hb_income_processed[,c("inc_amt", "ni_amt", "tax_amt", "freq_len", "freq_period")], 1, incomes_formula)
  
  ### add it to our DF
  hb_income_processed$weekly_income_calculated = weekly_income
  
  ### claim_id to char
  hb_income_processed$claim_id = as.character(hb_income_processed$claim_id)
  
  return(hb_income_processed)
}

process_hb_household = function(hb_household){
  hb_household_processed = hb_household
  
  return(hb_household_processed)
}

process_hb_member = function(hb_member){
  hb_member_processed = hb_member
  
  ### int to char for certain cols
  hb_member_processed[,c("claim_id", "house_id", "member_id")] = lapply(hb_member_processed[,c("claim_id", "house_id", "member_id")], function(x) as.character(x))
  
  ### dob
  hb_member_processed$birth_date = as.Date(hb_member_processed$birth_date, "%Y/%m/%d")
  
  return(hb_member_processed)
}

process_codes = function(ahgm_processed){
  ### Add schedule and owner tenure to ct
  
  ### Owner code tenure
  ahgm_processed$ct$owner_code_tenure = ahgm_processed$owner_codes$Tenure[match(ahgm_processed$ct$Owner.Code, ahgm_processed$owner_codes$Owner.Code)]
  
  ### Schedule code tenure 
  ahgm_processed$ct$schedule_code_tenure = ahgm_processed$schedule_codes$Tenure[match(ahgm_processed$ct$Schedule.Code, ahgm_processed$schedule_codes$Code)]
  
  ### add income description to hb_income
  ahgm_processed$hb_income = left_join(ahgm_processed$hb_income, ahgm_processed$income_codes, by=c("inc_code" = "Code"))
  
  ## Return 
  return(ahgm_processed)
}
