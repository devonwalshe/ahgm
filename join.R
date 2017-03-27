### Join up files

join_ahgm = function(ahgm_processed){
  
  ### Join benefits and CTR
  benefits_merged = inner_join(ahgm_processed$benefits, 
                               (ahgm_processed$ctr %>% select(property_ref, ctr.in.payment, max_partial_ctr, ctax_charge, ata, dla_total, claim_id8)), 
                               by=c("claim_id" = "claim_id8")) %>% mutate(same_prop_ref = property_ref.x == property_ref.y) %>% 
                               rename(property_ref = property_ref.x) %>% 
                               left_join(., (ahgm_processed$ct %>% select(property_ref, lead_name, unoccupied_allowance, allowance, owner_code_tenure, schedule_code_tenure)), by="property_ref")
  
  ### ahgm > uprn > benefits > youngest member > income_codes > yearly_income
  ahgm_merged = full_join(ahgm_processed$ahgm, (ahgm_processed$ahgm_uprn %>% select(plot, uprn, easting, northing, house_type)), by="plot") %>% 
                ### add property_ref via uprn
                left_join(., ahgm_processed$finref_uprn, by=c("uprn" = "UPRN")) %>% rename(property_ref = FIN_REF) %>% 
                ### join with benefits via property_ref
                left_join(., (benefits_merged %>% filter(!is.na(property_ref))), by="property_ref") %>%
                ### add youngest member in the house
                left_join(., (ahgm_processed$hb_member %>% group_by(claim_id) %>% 
                                          summarize(youngest_member = min(birth_date), household_members = n()) %>% 
                                          mutate(youngest_member_age = round(as.numeric(Sys.Date()- youngest_member)/ 365.242, 1)) %>% 
                                          select(claim_id, youngest_member_age, household_members)), by="claim_id") %>%
                ### Add income descriptions and yearly income
                left_join(., (ahgm_processed$hb_income %>% 
                                          group_by(claim_id) %>% 
                                          summarize(inc_descs = paste(Description, collapse="//"), yearly_income = sum(weekly_income_calculated)*52)), by="claim_id")
  
  ### colwork
  colnames(ahgm_merged)[c(2:12, 14:16)] = paste("AHGM_", colnames(ahgm_merged)[c(2:12, 14:16)], sep="")
  colnames(ahgm_merged)[c(21:34, 36:50)] = paste("CTHB_", colnames(ahgm_merged)[c(21:34, 36:50)], sep="")
  colnames(ahgm_merged)[c(1, 13, 17:20, 35)] = paste("ID_", colnames(ahgm_merged)[c(1,13, 17:20, 35)], sep="")
  
  ### reorder
  ahgm_merged = ahgm_merged[,c(grep("ID_", names(ahgm_merged)), 
                 grep("AHGM_", names(ahgm_merged)), 
                 grep("CTHB_", names(ahgm_merged)))]
  
  ahgm_processed$ahgm_merged = ahgm_merged
  
  return(ahgm_merged)
  
}