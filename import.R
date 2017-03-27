### Import ###

import_ahgm = function(){
  ahgm = read.csv("../data/import/ahgm_extract_2016_12_05.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  cat("ahgm imported \n")
  ahgm_uprn = read.csv("../data/import/ahgm_uprn.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  cat("ahgm uprn imported \n")
  benefits = read.csv("../data/import/benefits_live_2017_02_07.csv", fileEncoding="latin1", stringsAsFactors = FALSE, strip.white = TRUE)
  cat("benefits imported \n")
  finref_uprn = read.csv("../data/import/academy_uprn_lookup_2017_02_06.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  cat("finref_uprn imported \n")
  ct = read.csv("../data/import/ctax_live_2017_02_03.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  cat("ctax imported \n")
  ctr = read.csv("../data/import/ctr_2016-12-13.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  cat("ctr imported \n")
  hb_household = read.csv("../data/import/hb_2016_05_12/household.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  cat("hb_household imported \n")
  hb_income = read.csv("../data/import/hb_2016_05_12/income.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  cat("hb_income imported \n")
  hb_member = read.csv("../data/import/hb_2016_05_12/member.csv", stringsAsFactors = FALSE, strip.white = TRUE)
  cat("hb_member imported \n")
  schedule_codes = read.csv("../data/import/schedule_codes.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  owner_codes = read.csv("../data/import/owner_codes.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  cat('ct_codes imported \n')
  income_codes = read.csv("../data/import/hbct_income_codes.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  
  
  return(list(ahgm=ahgm, ahgm_uprn=ahgm_uprn, benefits = benefits, finref_uprn=finref_uprn, ct= ct, ctr=ctr, hb_income=hb_income, hb_member = hb_member, hb_household=hb_household, schedule_codes = schedule_codes, owner_codes = owner_codes, income_codes = income_codes))  
}
