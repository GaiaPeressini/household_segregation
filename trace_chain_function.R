# While loop that traces a chain


trace_chain <- function(
  #hushall_mobility_dt,
  #lgh_info,
  iter,
  split, # To track the number of times the chain splits.
  current_vac,
  current_year
  ) {
  
  max_iter <- 10 # decresed from 50
  vacancy_dt <- list() 
  try <- 2017 - current_year # Observe if a vacancy has been filled until the end of the period 
  current_vac <- current_vac
  current_year <- current_year
  
  
  while(try >= 0) { 
    
    
    # Find people moving in to the current vacancy
    replacers <- hushall_mobility_dt[to_lgh == current_vac & from_ar == current_year]
    
    # Was there someone staying? if yes, Then it wasn't a vacancy
    stayed_home <- replacers[from_lgh == to_lgh]
    
    
    
    
    # Break if someone stayed in the apartment.
    # To do: something not to count the previous step as part of the chain
    if(nrow(stayed_home) > 0) {
      replacers[,abs := "not_empty"]
      vacancy_dt[[iter]] <- replacers
      break}
    
    # If they are really taking over a vacancy then move on to study where do they come from:
    coming_from <-  unique(replacers$from_lgh)   
    
    
    # Conditions to follow up:
    
    # (1) they came from an apartment inside Stockholm
    
    inside <- lgh_info[lgh_id %in% coming_from & Kommun %in% kommun_vec]$lgh_id
    outside <- lgh_info[lgh_id %in% coming_from & !Kommun %in% kommun_vec]$lgh_id
    
    # Did they all come from outside?
    # If all coming from outside then the chain reaches absorption. Break.
    if(length(outside) == length(coming_from) & length(outside) > 0) {  # length(outside) > 0  bc otherwise we wanna look into the future for replacements
      
      # print("Everyone from outside!")
      
      # Store their info and break
      replacers[,chain_step := iter]
      print(paste("Replacers", replacers[1,1]))
      print(paste("Iter", iter))
      replacers[,abs := "outside"]
      vacancy_dt[[iter]] <- replacers
      
      break
    }
    
    
    # Subset replacers to those that passed the checks
    # i.e. people coming from a single apartment within Stockholm
  
    # True replacers that might have left a vacancy behind.
    replacers <- replacers[from_lgh %in% inside]
    
    
    
    ###################
    # Identify and stop loops
    # there is a loop if any of the replacers come from apartments that have appeared 
    # earlier in the chain.
    from <- unique(replacers$from_lgh) 
    apt_ids <- rbindlist(vacancy_dt,use.names = T, fill = T)
    
    #print(paste("apartments in the chain:",apt_ids$from_lgh))
    #print(paste("threads", from))
    
    if(length(intersect(from,apt_ids$from_lgh)) > 0) {  
      
      #print("LOOP!")
      replacers[,abs := "loop"]
      vacancy_dt[[iter]] <- replacers
      
      break
    }
    
    
    #################
    # If there are replacers coming from just one apartment then store and follow up on that one in next iter
    if(length(unique(replacers$from_lgh)) == 1) {
    
      replacers[,chain_step := iter]
      
      vacancy_dt[[iter]] <- replacers
      
      
      current_vac <- unique(replacers$from_lgh) # Apartment to be followed in next iter
      current_year <- unique(replacers$from_ar)
      
      iter <- iter + 1
      print(paste("iter", iter))
    }
    
    
    
    
    # More than one vacancy to follow
    # Recursive function. 
    # When there is more than one vacancy to follow i call that a moment of SPLIT in the chain.
    # Each of the chains stemming from this split is a THREAD.
    
    if (length(unique(replacers$from_lgh)) > 1) {
      
      
      split <- split + 1 
      replacers[,chain_step := iter] 
      replacers[,split := split]
      print("VC Split here!")
  
    
    
      vacancy_dt[[iter]] <- replacers
      vacancy_dt1 <- rbindlist(vacancy_dt,use.names = T, fill = T) # Save previous steps here. Otherwise erased by the recursive function.
      
      
      threads <- unique(replacers$from_lgh) # Apartments to be followed
      year <- unique(replacers$from_ar) 
    
      
      
      vacancy_dt2 <- list() # to store the steps of each of the threads
      
      for (i in 1:length(threads)) { # One thread at a time
        
        #print(paste("Vacancy split", split, "Thread", threads[i]))
        
        
        # Trace the chain
        chain <- trace_splits(current_vac_splits = threads[i],
                             current_year_splits = year,
                             iter_splits = iter + 1)
        # Store the chain
        vacancy_dt2[[i]] <- chain
        
        
      }
      
      
      # Give and  ID to each thread      
      vacancy_dt2 <- lapply(1:length(threads),function(i)vacancy_dt2[[i]][,thread := i])
      
      # Bind the threads into a dt 
      vacancy_dt2 <- rbindlist(vacancy_dt2,use.names = T, fill = T)
      
      # The split they correspond to
      vacancy_dt2[,split := split]
      
      # Bind main with threads
      vacancy_dt <- rbindlist(list(vacancy_dt1,vacancy_dt2), use.names = TRUE,fill = TRUE)
      
      return(vacancy_dt)
      break
    }
    
    
    
    
    
    # If the vacancy has not been occupied or absorbed then look in the next year:
    if(length(unique(replacers$from_lgh)) == 0) {
      current_year <- current_year + 1
      #print(paste("New current year", current_year))
      
    }
    
    try <- 2017 - current_year
    
    # If the vacancy is empty at the end of the period then break and indicate absorption :
    if (current_year == 2018) {
      #print ("Empty apartment. End of observation period.")
      replacers <- data.table(abs = "empty",from_ar = 2017,to_lgh = current_vac,split = split)
      vacancy_dt[[iter]] <- replacers
      
    }
    
    
     if(iter>=max_iter){
       print ("Max iter reached!")
       break
     }
  }
  
  
  # Rbind
  vacancy_dt <- rbindlist(vacancy_dt,use.names = T, fill = T)

    return(vacancy_dt)
  
}
