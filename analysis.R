### Analysis

### processing pipeline
ahgm_processed = import_ahgm() %>% process_ahgm() %>% join_ahgm()

### clone
ahgm_merged = ahgm_processed$ahgm_merged

### Add conditions
ahgm_merged = ahgm_merged %>% mutate(COND_over_70 = CTHB_youngest_member_age >= 70, 
                                     COND_full_CTD = CTHB_max_partial_ctr == "max", 
                                     COND_partial_CTD = CTHB_max_partial_ctr == "partial",
                                     COND_AA = CTHB_ata > 0,
                                     COND_DLA = CTHB_dla_total > 0)
### set up report
report = list()

### totals
report$"Total AHGM records" = nrow(ahgm_processed$ahgm)
report$"AHGM records with HB case" = ahgm_merged %>% filter(!is.na(ID_claim_id)) %>% nrow()

### No. of householders 70 years and above in receipt of full Council Tax Discount – entitled to free service
report$"Over 70 full CTD" = ahgm_merged %>% filter(COND_over_70 == TRUE & COND_full_CTD == TRUE) %>% nrow()

### No. of householders 70 years and above in receipt of partial CTD but with Attendance Allowance (AA) or Disability Living Allowance (DLA) – entitled to free service
report$"Over 70 partial CTD with AA or DLA" = ahgm_merged %>% filter((COND_over_70 == TRUE & COND_partial_CTD == TRUE) & (COND_AA == TRUE | COND_DLA == TRUE)) %>% nrow()

### No. of householders 70 years and above in receipt of partial CTD – service chargeable
report$"Over 70 partial CTD" = ahgm_merged %>% filter(COND_over_70 == TRUE & COND_partial_CTD == TRUE) %>% nrow()

### No under 70 years old – with DLA or AA
report$"Under 70 with AA or DLA" = ahgm_merged %>% filter(COND_over_70 == FALSE & (COND_DLA == TRUE | COND_AA == TRUE)) %>% nrow()

### No. under 70 years  - with no DLA or AA
report$"Under 70 no AA or DLA" = ahgm_merged %>% filter(COND_over_70 == FALSE & (COND_DLA == FALSE & COND_AA == FALSE)) %>% nrow()


### make a df
write.csv(ahgm_merged, paste("../data/export/DW_AHGM_merged_", Sys.Date(), ".csv", sep=""), row.names=FALSE)
write.csv(data.frame(condition=names(report), value= unlist(unname(report))), paste("../data/export/DW_AHGM_figures_", Sys.Date(), ".csv", sep=""), row.names=FALSE)
