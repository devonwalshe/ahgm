### Run ###
source("init.R")

aa_processed = import_files() %>% process_aa()

aa_norm = import_files() %>% process_aa() %>% normalize_aa()


### Record Linkage
ahgm_ctr_pairs = generate_pairs(ctr_norm, ahgm_norm, c("uid"))

### Save the file for recovery later
save_rl_file(ahgm_ctr_pairs$pairs, "./data/rl_data/14_12_2016.zip")

ahgm_ctr_classified = classify_pairs(ahgm_ctr_pairs$pairs, lower_threshold=0.58, upper_threshold=.70)

### Get these badboys to disk
write.csv(ahgm_ctr_classified$review, "./data/rl_data/ahgm_ctr_2016-14-12.csv", row.names=FALSE)

### Cut it off at 60
ahgm_ctr_classified$pairs = ahgm_ctr_classified$pairs[ahgm_ctr_classified$pairs$Weight > .60,]

### link with ctr dataset and break down the conditions
rl_ctr_ids = ahgm_ctr_classified$pairs$uid.1
rl_ahgm_ids = ahgm_ctr_classified$pairs$uid.2
