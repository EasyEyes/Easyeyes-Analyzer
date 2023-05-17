get_measures <- function(data_list){
  all_measures <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% 
      select(participant, block_condition, conditionName, key_resp.keys, correctAns) %>% 
      filter(!is.na(key_resp.keys),
             !is.na(correctAns),
             key_resp.keys != "",
             correctAns != "") %>% 
      rename(x = correctAns,
             y = key_resp.keys) %>% 
      mutate(x = str_sub(x,3,-3))
  }
  return(all_measures)
}

get_counts_all <- function(all_measures){
  counts_all <- all_measures %>% 
    mutate(n = n()) %>% 
    group_by(x) %>% 
    mutate(`n(x)` = n()) %>% 
    ungroup() %>% 
    group_by(y) %>% 
    mutate(`n(y)` = n()) %>% 
    ungroup() %>% 
    group_by(x,y) %>% 
    mutate(`n(x,y)` = n()) %>% 
    ungroup()
  return(counts_all)
}

get_prob <- function(counts) {
  prob <- counts %>% 
    mutate(`p(x)` = `n(x)`/n,
           `p(y)` = `n(y)`/n,
           `p(x,y)` = `n(x,y)`/n) %>% 
    select(-c(`n(x)`,`n(y)`,`n(x,y)`, block_condition, conditionName, n)) %>% 
    distinct() %>% 
    mutate(`I(x,y)` = `p(x,y)` * log2(`p(x,y)`/(`p(x)` * `p(y)`)))
  return(prob)
}

get_bits <- function(prob){
  return(sum(prob$`I(x,y)`))
}

get_bits_each <- function(prob){
  I <- prob %>% group_by(participant) %>% summarize(`I(X|Y)` = sum(`I(x,y)`))
  return(I)
}

get_counts_each_participant <- function(all_measures){
  counts_each <- all_measures %>% 
    group_by(participant) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    group_by(participant,x) %>% 
    mutate(`n(x)` = n()) %>% 
    ungroup() %>% 
    group_by(participant,y) %>% 
    mutate(`n(y)` = n()) %>% 
    ungroup() %>% 
    group_by(participant,x,y) %>% 
    mutate(`n(x,y)` = n()) %>% 
    ungroup() %>% 
    distinct()
  return(counts_each)
}




