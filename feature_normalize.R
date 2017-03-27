### Feature normalise for data match

normalize_aa = function(aa_processed){
  ahgm_norm = aa_processed$ahgm
  ct_norm = aa_processed$ct
  ctr_norm = aa_processed$ctr
  
  
  ### Subset the datasets
  ctr_cols = c("claim_ref", "claimant_name", "addr1", "addr2", "addr3", "addr4", "post_code")
  ctr_cols_n = match(ctr_cols, names(ctr_norm))
  ctr_norm = ctr_norm %>% select(ctr_cols_n)
  
  ahgm_cols = c("forename", "surname", "house_no", "flat", "street", "postcode")
  ahgm_cols_n = match(ahgm_cols, names(ahgm_norm))
  ahgm_norm = ahgm_norm %>% select(ahgm_cols_n)
  
  
  ### Add id to ahgm 
  ahgm_norm$uid = apply(ahgm, 1, digest)
  
  ### ctr split addresses
  ctr_addresses = ctr_norm %>% select(addr1, addr2, addr3, addr4)
  ctr_addresses[is.na(ctr_addresses)] = ""
  
  ctr_norm[,c("addr1", "addr2", "addr3", "addr4")] = ctr_addresses
  ctr_norm$address_joined = trim_all(paste(ctr_norm$addr1, ctr_norm$addr2, ctr_norm$addr3, ctr_norm$addr4))
  
  ### Split the addresses
  ctr_street_address = unname(sapply(ctr_norm$address_joined, function(x) {
    ## if there is a date in front or just two sets of numbers
    if (!is.na(str_match(x, "^(?!flat)\\d+-\\w+ \\d+|^(?!flat|house|site)[/?[\\d]\\w?]+ \\d+"))){
      str_match(x, "(^.+?)( )(.+)(  [/[\\d]]+ glasgow$| glasgow$| flat [/[\\d]]+$| \\d+$| \\d+-\\w+$|$)")[4]
      ## If its just a number - move it  
    } else if (!is.na(str_match(x, "^\\d+\\w?"))) {
      str_match(x, "(^[(?)?[\\d]\\w?]+ .+?) ([/[\\d]]+ glasgow$|glasgow$|flat [/[\\d]]+$|\\d+$|\\d+-\\w+$|flat .+?$|\\w+?\\.\\d+?$|\\w\\d$|\\w\\w$|\\d/\\d$|\\w/\\w|\\w\\w$|//w+?$|(glasgow|glagsow|glsgow|glasgoq|gladgow|glagow|gasgow)$|$)")[,2]
      ## If there is a flat or house or site at the front
    } else if(!is.na(str_match(x, "^flat")) | !is.na(str_match(x, "^house")) | !is.na(str_match(x, "^site"))) {
      str_match(x, "(^\\w+ .+?)( )(.+)( [/[\\d]]+ glasgow$| glasgow$| flat [/[\\d]]+$| \\d+$| \\d+-\\w+$|( glasgow| glagsow| glsgow| glasgoq| gladgow| glagow| gasgow)$)|$")[4]
    }
  }))
  
  ctr_house_no = unname(sapply(ctr_norm$address_joined, function(x) {
    ## if it has a flat, site or house on it
    if (any(!is.na(str_match(x,  "(flat|site|house) (\\d+/\\d+|\\w+/\\d+|\\d+|\\w+\\d+|//w+|\\w+\\.\\d+)")))){
      str_match(x,  "(flat|site|house) (\\d+/\\d+|\\w+/\\d+|\\d+|//w+|\\d+\\w|\\w+/\\d+\\w+|\\w+\\d+|\\w+\\.\\d+)")[1]
      ## only the strings that match for example 19/9 or 18/a if it doesn't have flat site or house in it
      ## OR if there are a sequence of two numbers next to eachother, take the first
    } else if (any(!is.na(str_match(x, "(\\d+/\\d+|\\w+/\\d+|//w+|\\w+\\.\\d+)")))){
      str_match(x, "\\d+/\\d+|\\w+/\\d+|\\d+/\\w+|\\w+\\.\\d+|\\d+\\(.+\\)")
      ### Special case of two numbers seperated by a space - 244 21 somthing drive
    } else if (any(!is.na(str_match(x, "\\d+ \\d+")))) {
      str_match(x, "\\d+")
      ## Empty otherwise
    } else {
      ""
    }
  }))
  
  ### Ensure glasgow is off the end of the street_address
  ctr_street_address = gsub(" glasgow$", "", ctr_street_address)
  
  ### Assign addresses
  ctr_norm$house_no = ctr_house_no
  ctr_norm$street_address = as.character(ctr_street_address)

  ### split the names
  ctr_norm$claimant_name = strip_titles(ctr_norm$claimant_name)
  ctr_norm[,c("firstname", "middlenames", "lastname")] = top_tail_names(ctr_norm$claimant_name)

  ahgm_norm$name_joined = paste(ahgm_norm$forename, ahgm_norm$surname)
  ahgm_norm[,c("firstname", "middlenames", "lastname")] = top_tail_names(ahgm_norm$name_joined)

  
  ### rename the columns
  norm_cols = c("uid", "firstname", "middlenames", "lastname", "house_no", "street_address", "post_code")
  
  colnames(ctr_norm)[1] = "uid"
  colnames(ahgm_norm)[c(3,4,5,6)] = c("house_ref","house_no", "street_address", "post_code")
  
  ### Add house no to street_address ahgm
  ahgm_norm$street_address = paste(as.character(as.integer(str_match(ahgm_norm$house_ref,"\\d+"))), ahgm_norm$street_address)
  
  ctr_norm = ctr_norm[,norm_cols]
  ahgm_norm = ahgm_norm[,norm_cols] 
  
  return(list(ahgm=ahgm_norm, academy=ctr_norm))
}