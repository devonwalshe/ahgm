### Analysis
source("init.R")
aa_raw = import_files()

### lets answer these questions
households = aa_raw$hb_household
incomes = aa_raw$hb_income
members = aa_raw$hb_member
ahgm = aa_raw$ahgm
ct = aa_raw$ct
ctr = aa_raw$ctr
# ctr = read.csv("./data/ctr_2016-12-13.csv", stringsAsFactors = FALSE, strip.white = TRUE)

### Clean here
members$birth_date = as.Date(members$birth_date, format="%Y/%m/%d")
members[,c("claim_id", "house_id", "member_id")] = lapply(members[,c("claim_id", "house_id", "member_id")], as.character)
ctr[,c("claim_ref", "ctax_ref", "property_ref")] = data.frame(lapply(ctr[,c("claim_ref", "ctax_ref", "property_ref")], as.character), stringsAsFactors = FALSE)

ctr = data.frame(lapply(ctr, tolower))
ctr = data.frame(lapply(ctr, trim_all), stringsAsFactors = FALSE)
ctr$claimant_dob = as.Date(ctr$claimant_dob, format="%d.%m.%Y")
ctr[,c("ctr_amt", "ctax_charge", "dla_high", "dla_med", "dla_low", "ata")] = as.data.frame(lapply(ctr[,c("ctr_amt", "ctax_charge", "dla_high", "dla_med", "dla_low", "ata")], as.numeric), stringsAsFactors = FALSE)

### Add dla total amount
ctr$dla_total = ctr$dla_high + ctr$dla_med + ctr$dla_low


### Chop off the last digit of the claim_id from the ctr
ctr$claim_id8 = str_match(ctr$claim_ref, "(.+)(.)$")[,2]

### Subset members over 70 
dt = as.Date('2010/03/17')
R> seq(dt, length=2, by="-2 years")[2]
age_cutoff = seq(as.Date(Sys.Date()), length=2, by="-70 years")[2]
ctr_plus = ctr[ctr$claimant_dob <= age_cutoff,]
ctr_under = ctr[ctr$claimant_dob > age_cutoff,]

### Members / incomes work
members_plus = members[members$birth_date <= age_cutoff & members$member_id == 1,]
members_under = members[members$birth_date >= age_cutoff & members$member_id == 1,]
incomes_plus = incomes[incomes$claim_id %in% members_plus$claim_id,]
incomes_plus_aa_dla = incomes_plus[incomes_plus$inc_code %in% c("dlh", "dll", "dla", "dmh", "dlm") | incomes_plus$inc_code %in% c("ata", "atl"),]
members_plus_partial_aa_dla = members_plus[members_plus$claim_id %in% incomes_plus_aa_dla$claim_id,]

### TODO - find members who are also in ctr

### Conditions from Academy ###
###############################

### No. of householders 70 years and above in receipt of full Council Tax Discount – entitled to free service
  ### Answer - total - 20641
  ### Answer - unique claim ref - 17244
  ### Answer - no ctax_record - 3526
  ### Answer - unique property ref - 17156
nrow(ctr_plus[ctr_plus$max_partial_ctr == "max",])
nrow(ctr_plus[ctr_plus$max_partial_ctr == "max" & !duplicated(ctr_plus$claim_ref),])
nrow(ctr_plus[ctr_plus$max_partial_ctr == "max" & ctr_plus$ctax_ref == "",])
nrow(ctr_plus[ctr_plus$max_partial_ctr == "max" & !duplicated(ctr_plus$property_ref),])
ctr_plus_full = c(20641,17156,3526)
  
### No. of householders 70 years and above in receipt of partial CTD but with Attendance Allowance (AA) or Disability Living Allowance (DLA) – entitled to free service

  ### Answer - total - 4211
  ### Answer - unique claim ref - 814
  ### Answer - no ctax_record - 3398
  ### Answer - unique property ref - 813
nrow(ctr_plus[(ctr_plus$max_partial_ctr == "partial" & ctr_plus$dla_total > 0) | (ctr_plus$max_partial_ctr == "partial" & ctr_plus$ata > 0),])
nrow(ctr_plus[(ctr_plus$max_partial_ctr == "partial" & ctr_plus$dla_total > 0 & !duplicated(ctr_plus$claim_ref)) | (ctr_plus$max_partial_ctr == "partial" & ctr_plus$ata > 0 & !duplicated(ctr_plus$claim_ref)),])
nrow(ctr_plus[(ctr_plus$max_partial_ctr == "partial" & ctr_plus$dla_total > 0 & ctr_plus$ctax_ref == "") | (ctr_plus$max_partial_ctr == "partial" & ctr_plus$ata > 0 & ctr_plus$ctax_ref == ""),])
nrow(ctr_plus[(ctr_plus$max_partial_ctr == "partial" & ctr_plus$dla_total > 0 & !duplicated(ctr_plus$property_ref)) | (ctr_plus$max_partial_ctr == "partial" & ctr_plus$ata > 0 & !duplicated(ctr_plus$property_ref)),])
ctr_plus_partial_dla_aa = c(4211,813,3398)

### No. of householders 70 years and above in receipt of partial CTD – service chargeable
  ### Answer - 9830
  ### Answer - unique claim ref - 6433
  ### Answer - no ctax_record - 3398
  ### Answer - unique property_ref - 6430
nrow(ctr_plus[ctr_plus$max_partial_ctr == "partial",])
nrow(ctr_plus[ctr_plus$max_partial_ctr == "partial" & !duplicated(ctr_plus$claim_ref),])
nrow(ctr_plus[ctr_plus$max_partial_ctr == "partial" & ctr_plus$ctax_ref == "",])
nrow(ctr_plus[ctr_plus$max_partial_ctr == "partial" & !duplicated(ctr_plus$property_ref),])
ctr_plus_partial = c(9830,6430,3398)

### No under 70 years old – with DLA or AA
  ### Answer - 11280
  ### Answer - unique claim ref - 7883
  ### Answer - no ctax_record - 3560
  ### Answer - unique property_ref - 7698 
nrow(ctr_under[ctr_under$dla_total >0 | ctr_under$ata >0,])
nrow(ctr_under[(ctr_under$dla_total >0 & !duplicated(ctr_under$claim_ref))| (ctr_under$ata >0 & !duplicated(ctr_under$claim_ref)),])
nrow(ctr_under[(ctr_under$dla_total >0 & ctr_under$ctax_ref=="")| (ctr_under$ata >0 & ctr_under$ctax_ref==""),])
nrow(ctr_under[(ctr_under$dla_total > 0 | ctr_under$ata > 0 ) & !duplicated(ctr_under$property_ref),])
ctr_under_dla_aa = c(11280,7698,3560)

### No. under 70 years  - with no DLA or AA
  ### Answer - total - 70263
  ### Answer - unique claim ref - 66866
  ### Answer - no ctax_record - 4807
  ### Answer - unique property_ref - 65499
nrow(ctr_under[ctr_under$dla_total == 0 & ctr_under$ata == 0,])
nrow(ctr_under[ctr_under$dla_total == 0 & ctr_under$ata == 0 & !duplicated(ctr_under$claim_ref),])
nrow(ctr_under[ctr_under$dla_total == 0 & ctr_under$ata == 0 & ctr_under$ctax_ref == "",])
nrow(ctr_under[ctr_under$dla_total == 0 & ctr_under$ata == 0 & !duplicated(ctr_under$property_ref),])
ctr_under_no_dla_aa = c(70263, 65499, 4807)
###  Data Cleansing will probably highlight –


###   DONE - No. with no CT record (not the householder)  
### Count those from above that aren't the primary member

### No. deceased (think this would show up on CT record)
### Need to look at full CT to compare this against - check property reference

### Generate a table with all the figures I need in it
ctr_plus_full
ctr_plus_partial
ctr_plus_partial_dla_aa
ctr_under_dla_aa
ctr_under_no_dla_aa

ctr_table = data.frame(condition = "", total=0, unique_prop_ref = 0, no_ctax_ref=0)
ctr_table[1,2:4] = ctr_plus_full
ctr_table[2,2:4] = ctr_plus_partial
ctr_table[3,2:4] = ctr_plus_partial_dla_aa
ctr_table[4,2:4] = ctr_under_dla_aa
ctr_table[5,2:4] = ctr_under_no_dla_aa
ctr_conditions = c("70+ full CTR", "70+ partial CTR", "70+ partial CTR with DLA or AA", "Under 70 with DLA or AA", "Under 70 no DLA or AA")
ctr_table[,1] = ctr_conditions

### Print table
ft = formattable(ctr_table, list(
  total = normalize_bar(),
  total = color_tile("white", "orange")
))
export_formattable(ft, "./plots/assignees_table.png", width=1500)

### Test some charts
ggplot(melt(ctr_table, id.vars = "condition"), aes(x=reorder(condition, value), y=value, fill=variable)) + 
  geom_bar(stat='identity') + coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black", size=.25)) +
  expand_limits(x = 0, y = 0)


### Start again with the analysis from the matched dataset
### Pull out the ctax references from ctr
ctr_matched = ctr[ctr$claim_ref %in% rl_ctr_ids,]
ctr_matched_plus = ctr_matched[ctr_matched$claimant_dob <= age_cutoff,]
ctr_matched_under = ctr_matched[ctr_matched$claimant_dob > age_cutoff,]

### No. of householders 70 years and above in receipt of full Council Tax Discount – entitled to free service

### Answer - total - 3678
nrow(ctr_matched_plus[ctr_matched_plus$max_partial_ctr == "max",])

### No. of householders 70 years and above in receipt of partial CTD but with Attendance Allowance (AA) or Disability Living Allowance (DLA) – entitled to free service

### Answer - total - 932
nrow(ctr_matched_plus[(ctr_matched_plus$max_partial_ctr == "max" & ctr_matched_plus$dla_total > 0) | (ctr_matched_plus$max_partial_ctr == "max" & ctr_matched_plus$ata > 0), ])

### No. of householders 70 years and above in receipt of partial CTD – service chargeable

### Answer - total - 1325
nrow(ctr_matched_plus[ctr_matched_plus$max_partial_ctr == "partial",])

### No under 70 years old – with DLA or AA

### Answer - total - 696
nrow(ctr_matched_under[ctr_matched_under$dla_total > 0 | ctr_matched_under$ata > 0,])

### No. under 70 years  - with no DLA or AA

### Answer - total - 1946
nrow(ctr_matched_under[ctr_matched_under$dla_total == 0 & ctr_matched_under$ata == 0,])

### Combine these figures
matched_conditions = c(3678, 932, 1325, 696, 1946)

### copy table
final_table = ctr_table

### Add it to CTR table
final_table$ctr_ahgm_matched = matched_conditions

### Add new one for totals
full_ctr_df = data.frame(condition = "Full CTR Set", total = nrow(ctr), unique_prop_ref = nrow(ctr[!duplicated(ctr$property_ref),]), no_ctax_ref=nrow(ctr[ctr$ctax_ref == "",]), ctr_ahgm_matched = nrow(ctr_matched) )

### Combine them
final_table = rbind(full_ctr_df, final_table)

### output to csv
write.csv(final_table, paste("./data/final_table_", Sys.Date(), ".csv", sep=""), row.names=FALSE)


### New analysis of ctr with live position of all members from HB

### subset all academy members by those over 70
### Compare them with the ctr set and apply the conditions above

###   Nobody in the house is under 70, unique claim ids
academy_plus_ids = unique(academy[(academy$claim_id %in% ctr$claim_id8) & (academy$birth_date <= age_cutoff),]$claim_id)

ctr_all_plus = ctr[ctr$claim_id8 %in% academy_plus_ids,]
ctr_matched_all_plus = ctr_matched[ctr_matched$claim_id8 %in% academy_plus_ids,]


nrow(academy)
nrow(ctr)
length(academy_plus_ids)
nrow(ctr_all_plus)
nrow(ctr_matched_all_plus)


  
  
  
  