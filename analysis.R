### Analysis

### processing pipeline
ahgm_joined = import_ahgm() %>% process_ahgm() %>% join_ahgm()

### clone
ahgm_merged = ahgm_joined$ahgm_merged

### New conditions tag as (elligible), (not-elligible), (unsure)

### Set up conditions
ahgm_merged = ahgm_merged %>% mutate(COND_over_70 = CTHB_youngest_member_age >= 70, 
                                     COND_AA = CTHB_ata > 0,
                                     COND_DLA = CTHB_dla_total > 0,
                                     COND_surnames_match = (AHGM_lastname == CTHB_lead_lastname)) %>%
                              ### Use the above to generate one of them
                              mutate(COND_under_70_no_disability = (COND_over_70 == FALSE & COND_AA == FALSE & COND_DLA == FALSE))

### Process elligibility
ahgm_merged = ahgm_merged %>% mutate(assumed_eligibility = "UE") %>%
  mutate(assumed_eligibility = ifelse(is.na(COND_surnames_match), assumed_eligibility,
                                       ifelse(COND_surnames_match == FALSE, "QE", assumed_eligibility))) %>% 
  mutate(assumed_eligibility = ifelse(is.na(COND_under_70_no_disability), assumed_eligibility, 
                                       ifelse(COND_under_70_no_disability==TRUE, "NE", assumed_eligibility))) %>%
  mutate(assumed_eligibility = ifelse((is.na(COND_over_70) | is.na(COND_surnames_match)), assumed_eligibility,
                                      ifelse((COND_over_70 == TRUE & COND_surnames_match == TRUE), "PE", assumed_eligibility)))



                                        
### Write out - Subset for sensitive material
write.csv(ahgm_merged %>% 
            select(grep("AHGM|ID_|youngest_member_age|household_member|lead_lastname|COND|eligibility|member_names", colnames(ahgm_merged))), 
          paste("../data/export/DW_ahgm_eligibility_processed_", Sys.Date(), ".csv", sep=""), row.names=FALSE)
 