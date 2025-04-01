read_json <- function(f){
  json_data <- fromJSON(f, flatten = TRUE)$data %>% 
    tibble() %>% 
    select(matches('id|created_at|author_id|text|conversation_id|lang|entities.mentions|referenced_tweets|public_metrics')) %>% 
    janitor::clean_names() %>% 
    select(-matches('edit_history_tweet_ids|in_reply_to_user_id|geo_place_id')) %>% 
    mutate(sourcefile = f)
  return(json_data)
}

get_data <- function(){
  dd <- map_dfr(list.files('json', full.names = TRUE), read_json, .progress = TRUE)
  return(dd)
}

clean_source <- function(s) {
  ans <- s %>% 
    str_remove_all('_[0-9]{1,}\\.json') %>% 
    str_remove_all('json/') %>% 
    str_remove_all('"')
  return(ans)
}

preproc <- function(d) {
  ans <- d %>% 
    filter(lang == 'en') %>% # Sample only English tweets
    filter(!str_detect(text, '^RT @')) %>%  # Remove retweets
    mutate(compound_bin = ifelse(compound >= 0, 1, 0)) %>% 
    mutate(year = lubridate::year(created_at)) %>% 
    mutate(yearmonth = lubridate::floor_date(lubridate::as_date(created_at), "month")) %>% 
    mutate(clean_source = sourcefile %>% clean_source()) %>% 
    mutate(
      is_ngss = str_detect(clean_source, regex('ngss|next generation science standard|next gen science standard', ignore_case = TRUE)),
      is_ccss = str_detect(clean_source, regex('common-core|common core|ccss|commoncore', ignore_case = TRUE))
    ) 
  
  return(ans)
}

clean_and_combine <- function(d, vader) {
  d2 <- d %>% 
    left_join(vader %>% rename(id = X1, compound = X2), by = 'id')
  d2 <- d2 %>% 
    preproc()
  return(d2)
}

split_data <- function(d) {
  
  d_ngss <- d %>% 
    filter(is_ngss) %>% 
    distinct(id, .keep_all = TRUE)
  
  d_ccss <- d %>% 
    filter(is_ccss) %>% 
    distinct(id, .keep_all = TRUE)
  
  ngss_ids <- d_ngss$id %>% unique()
  ccss_ids <- d_ccss$id %>% unique()
  both_id <- base::intersect(ngss_ids, ccss_ids)
  
  d_both <- d %>% 
    filter(id %in% both_id) %>% 
    distinct(id, .keep_all = TRUE)
  
  return(
    list(
      d_ngss = d_ngss,
      d_ccss = d_ccss,
      d_both = d_both
    )
  ) 
}

recombine_splits <- function(l) {
  
  ans <- rbind(
    l$d_both %>% mutate(is_both = TRUE, is_ngss = TRUE, is_ccss = TRUE),
    l$d_ngss %>% mutate(is_both = FALSE, is_ngss = TRUE, is_ccss = FALSE),
    l$d_ccss %>% mutate(is_both = FALSE, is_ngss = FALSE, is_ccss = TRUE)
  ) %>% 
    distinct(id, .keep_all = TRUE) # Keeps is_both annotation first
  
  return(ans)
}

