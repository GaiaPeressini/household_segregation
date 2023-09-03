# --Code based on Pablo Bello's work--

trace_splits <- function(iter_splits, current_vac_splits, current_year_splits) {
  
  max_iter_splits <- 10
  vacancy_dt_splits <- list()
  try_splits <- 2017 - current_year_splits
  current_vac_splits <- current_vac_splits
  current_year_splits <- current_year_splits
  
  while (try_splits >=0) {
    
    # people moving to current thread  
    replacers_splits <- hushall_mobility_dt[to_lgh == current_vac_splits & from_ar == current_year_splits]
    
    # break if someone stayed in the house
    stayed_home_splits <- replacers_splits[from_lgh == to_lgh]
    
    if(nrow(stayed_home_splits) > 0) {
      replacers_splits[,abs := "not_empty"]
      vacancy_dt_splits[[iter_splits]] <- replacers_splits
      break}
    
    
    coming_from_splits <- unique(replacers_splits$from_lgh)
    
    inside_splits <- lgh_info[lgh_id %in% coming_from_splits & Kommun %in% kommun_vec]$lgh_id
    outside_splits <- lgh_info[lgh_id %in% coming_from_splits & !Kommun %in% kommun_vec]$lgh_id
    
    # If all coming from outside then the chain reaches absorption. Break.
    if(length(outside_splits) == length(coming_from_splits) & length(outside_splits) > 0) {  # length(outside) > 0  bc otherwise we wanna look into the future for replacements
      
      replacers_splits[,chain_step := iter_splits]
      #print(paste("Replacers", replacers[1,1]))
      print(paste("Iter splits", iter_splits))
      replacers_splits[,abs := "outside"]
      vacancy_dt_splits[[iter_splits]] <- replacers_splits
      
      break
    }
    
    # Keep only replacers coming from stockholm
    replacers_splits <- replacers_splits[from_lgh %in% inside_splits]
    
    # Identify and stop loops
    from_splits <- unique(replacers_splits$from_lgh)
    apt_ids_splits <- rbindlist(vacancy_dt_splits,use.names = T, fill = T)
    
    if(length(intersect(from_splits, apt_ids_splits$from_lgh)) > 0) {  
      
  
      replacers_splits[,abs := "loop"]
      vacancy_dt_splits[[iter_splits]] <- replacers_splits
      
      break
    }
    
    #############
    # If there are replacers coming from just one apartment then store and follow up on that one in next iter
    if (length(unique(replacers_splits$from_lgh)) == 1) {
      
      replacers_splits[,chain_step := iter_splits]
      
      vacancy_dt_splits[[iter_splits]] <- replacers_splits
      
      
      current_vac_splits <- unique(replacers_splits$from_lgh) # Apartment to be followed in next iter
      current_year_splits <- unique(replacers_splits$from_ar)
      
      iter_splits <- iter_splits + 1
      print(paste("iter", iter_splits))
    }
    
    # Replacers come from more than 1 apartment --> break  (changed from trace_chain_function)
    if (length(unique(replacers_splits$from_lgh)) > 1) {
      
      replacers_splits[,chain_step := iter_splits]
      replacers_splits[,abs := "split"]
      vacancy_dt_splits[[iter_splits]] <- replacers_splits
      break
    }
    
    
    # If the vacancy has not been occupied or absorbed then look in the next year:
    if(length(unique(replacers_splits$from_lgh)) == 0) {
      current_year_splits <- current_year_splits + 1
      #print(paste("New current year", current_year))
      
    }
    
    try_splits <- 2017 - current_year_splits
    
    # If the vacancy is empty at the end of the period then break and indicate absorption :
    if (current_year_splits == 2018) {
      replacers_splits <- data.table(abs = "empty",from_ar = 2017,to_lgh = current_vac_splits)
      vacancy_dt_splits[[iter_splits]] <- replacers_splits
      
    }
    
    
    if(iter_splits>=max_iter_splits){
      print ("Max iter_splits reached!")
      break
    }
  }
    
    
    # rbind
    vacancy_dt_splits <- rbindlist(vacancy_dt_splits, use.names = T, fill = T)
    return(vacancy_dt_splits)
}
  
