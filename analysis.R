library(tidyverse)
library(lme4)

d <- targets::tar_read(d_analysis)

d$created_at_posix <- d$created_at %>% as.POSIXct()

d_bots <- d %>%
  #head(10000) %>%
  group_by(author_id) %>%
  summarize(n_days = 1+(as.numeric(difftime(max(created_at_posix), min(created_at_posix), units='days'))),
            n_tweets = n()) %>%
  ungroup() %>%
  mutate(tweets_per_day = n_tweets/n_days)

d_bots$tweets_per_day %>% quantile
remove <- d_bots %>%
 filter(n_days>3 & tweets_per_day>1) %>%
 pull(author_id)

# Descriptives
d$is_ccss %>% sum()
d$is_ngss %>% sum()

sum(d$is_ngss & d$is_ccss)

sum(d$is_both)

# Users
length(unique(d$author_id))
length(remove)/length(unique(d$author_id))

d %>%
  filter(is_ngss) %>%
  pull(author_id) %>%
  unique()-> ngss_users

length(ngss_users)

d %>%
  filter(is_ccss) %>%
  pull(author_id) %>%
  unique() -> ccss_users

length(ccss_users)

length(base::intersect(ngss_users, ccss_users))

# RQ1
# Average sentiment

d$compound[d$is_ngss] %>% mean()
d$compound[d$is_ngss] %>% sd()

d$compound[d$is_ccss] %>% mean()
d$compound[d$is_ccss] %>% sd()

# Figure 1
d_plot <- d %>%
  group_by(is_ngss, yearmonth) %>%
  summarize(
    senti_ratio = sum(compound_bin==0, na.rm=TRUE)/sum(compound_bin==1, na.rm=TRUE),
    compound_mean = mean(compound, na.rm=TRUE),
    compound_polar = mean(abs(compound), na.rm=TRUE),
    nn = n()
  ) %>%
  ungroup() %>%
  mutate(Reform = ifelse(is_ngss, 'NGSS', 'CCSS')) %>%
  select(-is_ngss)

d_plot %>%
  ggplot(aes(yearmonth, compound_mean, group = Reform)) +
  geom_point(aes(colour = Reform)) +
  geom_smooth(aes(colour = Reform)) +
  theme_bw() +
  theme(legend.position = "top",
        text = element_text(family = "Times New Roman")) +  # Setting font to Times New Roman
  labs(x = 'Month', y = 'Average Monthly Sentiment') +
  scale_color_manual(values = c("NGSS" = "#000000", "CCSS" = "#E69F00")) +
  scale_x_date(breaks = scales::breaks_pretty(20))

# Regression models (linear vs. cubic)

add_hashtags <- function(d) {
  d <- d %>%
    mutate(hashtags = text %>% tolower() %>% str_extract_all("#[[:alnum:]_]+") %>% map(unique))
  return(d)
}

d_model <- d %>%
  add_hashtags()

# Sample hashtags that were used at least 10 times, then take most popular one
selected <- d_model$hashtags %>% unlist() %>% table() %>% sort() #%>% tail(100) %>% names()
selected <- selected[selected>=100] %>% names()

# Correlation test between popularity rank and variation in sentiment (SD)
joinn <- d_model$hashtags %>% unlist() %>% table() %>% sort() %>% rev() %>% tibble::enframe(value = 'rank') %>%
  mutate(rank = 1:n())

core <- d_model %>%
  select(hashtags, compound) %>%
  unchop(hashtags) %>%
  group_by(hashtags) %>%
  summarize(sdv = sd(compound, na.rm = TRUE)) %>%
  ungroup()

analysis <- core %>% left_join(joinn, by = c('hashtags'='name')) %>%
  filter(!is.na(sdv))

cor.test(analysis$rank, analysis$sdv)

# Popularity first filtering
extract_shash <- function(v, selected=selected) {
  ans <- tail(base::intersect(v, selected),1)
  if (length(ans) == 0)
    return(NA)
  else
    return(ans)
}

d_model <- d_model %>%
  mutate(shash = map_chr(hashtags, extract_shash, selected)) %>%
  as.data.frame()

d_model %>%
  filter(shash=='#senatebill') %>%
  pull(text)

# Time variable scaling to origin
firstmonth <- d_model$yearmonth %>% min()
d_model['yearmonth_num'] <- as.numeric(d_model$yearmonth - firstmonth)/365.25 # days to years

# Time models (linear vs. quadratic vs. cubic)
d_model_ngss <- d_model %>% filter(is_ngss)
d_model_ccss <- d_model %>% filter(!is_ngss)

m_linear_ngss <- lm(compound ~ 1 + yearmonth_num, d_model_ngss, verbose=2)
m_quad_ngss <- lm(compound ~ 1 + yearmonth_num + I(yearmonth_num^2), d_model_ngss, verbose=2)
m_cubic_ngss <- lm(compound ~ 1 + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3), d_model_ngss, verbose=2)

# Table 1 metrics
AIC(m_linear_ngss)
AIC(m_quad_ngss)
AIC(m_cubic_ngss)

BIC(m_linear_ngss)
BIC(m_quad_ngss)
BIC(m_cubic_ngss)

sjPlot::tab_model(m_linear_ngss)
sjPlot::tab_model(m_quad_ngss)
sjPlot::tab_model(m_cubic_ngss)

m_linear_ccss <- lm(compound ~ 1 + yearmonth_num, d_model_ccss, verbose=2)
m_quad_ccss <- lm(compound ~ 1 + yearmonth_num + I(yearmonth_num^2), d_model_ccss, verbose=2)
m_cubic_ccss <- lm(compound ~ 1 + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3), d_model_ccss, verbose=2)

AIC(m_linear_ccss)
AIC(m_quad_ccss)
AIC(m_cubic_ccss)

BIC(m_linear_ccss)
BIC(m_quad_ccss)
BIC(m_cubic_ccss)

sjPlot::tab_model(m_linear_ccss)
sjPlot::tab_model(m_quad_ccss)
sjPlot::tab_model(m_cubic_ccss)

plot(m_cubic_ngss)
plot(m_cubic_ccss)

#saveRDS(cooks.distance(m_cubic_ngss), 'm_cubic_ngss_cooks.rds')
#saveRDS(cooks.distance(m_cubic_ccss), 'm_cubic_ccss_cooks.rds')

l <- get_model_diagnostics(m_cubic_ngss, mname='m_cubic_ngss', is_lmm=FALSE)
l$`Residuals QQ-Plot`
l$`Cook's Distance Plot`
l <- get_model_diagnostics(m_cubic_ccss, mname='m_cubic_ccss', is_lmm=FALSE)
l$`Residuals QQ-Plot`
l$`Cook's Distance Plot`

# RQ2 models, including Table 2 metrics
m0 <- lm(compound ~ 1 + is_ngss, d_model, verbose=2)
summary(m0)

m1 <- lm(compound ~ 1 + is_ngss + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3), d_model, verbose=2)
sjPlot::tab_model(m1)

m2 <- lmer(compound ~ 1 + is_ngss + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3) + (1|author_id), d_model, verbose=2)
#sjPlot::tab_model(m2)
MuMIn::r.squaredGLMM(m2)

m3 <- lmer(compound ~ 1 + is_ngss + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3) + (1|author_id) + (1|shash), d_model, verbose=2)
#sjPlot::tab_model(m2)
MuMIn::r.squaredGLMM(m3)

# RQ2 models by Reform
m1_ngss <- lm(compound ~ 1 + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3), d_model_ngss, verbose=2)
m2_ngss <- lmer(compound ~ 1 + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3) + (1|author_id), d_model_ngss, verbose=2)
m3_ngss <- lmer(compound ~ 1 + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3) + (1|author_id) + (1|shash), d_model_ngss, verbose=2)

MuMIn::r.squaredGLMM(m1_ngss)
MuMIn::r.squaredGLMM(m2_ngss)
MuMIn::r.squaredGLMM(m3_ngss)
##
m1_ccss <- lm(compound ~ 1 + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3), d_model_ccss, verbose=2)
m2_ccss <- lmer(compound ~ 1 + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3) + (1|author_id), d_model_ccss, verbose=2)
m3_ccss <- lmer(compound ~ 1 + yearmonth_num + I(yearmonth_num^2) + I(yearmonth_num^3) + (1|author_id) + (1|shash), d_model_ccss, verbose=2)

MuMIn::r.squaredGLMM(m1_ccss)
MuMIn::r.squaredGLMM(m2_ccss)
MuMIn::r.squaredGLMM(m3_ccss)

coef(m1)
fixef(m2)
fixef(m3)

#saveRDS(ranef(m2_ngss), 'm2_ngss_ranef.rds')
#saveRDS(ranef(m3_ngss), 'm3_ngss_ranef.rds')
#saveRDS(ranef(m2_ccss), 'm2_ccss_ranef.rds')
#saveRDS(ranef(m3_ccss), 'm3_ccss_ranef.rds')

#saveRDS(cooks.distance(m2_ngss), 'm2_ngss_cooks.rds')
#saveRDS(cooks.distance(m3_ngss), 'm3_ngss_cooks.rds')
#saveRDS(cooks.distance(m2_ccss), 'm2_ccss_cooks.rds')
#saveRDS(cooks.distance(m3_ccss), 'm3_ccss_cooks.rds')

library(car)
library(lattice)

get_model_diagnostics <- function(m, mname='m2_ngss', is_lmm=FALSE) {
  # Initialize list to store plots
  plots <- list()

  # Plot 1: QQ-plot for residuals
  if (is_lmm) {
    qqnorm(resid(m), main = "QQ-Plot of Residuals")
  } else {
    qqnorm(resid(m), main = "QQ-Plot of Residuals")
  }
  qqline(resid(m), col = "red")
  plots[['Residuals QQ-Plot']] <- recordPlot()

  # Plot 2: QQ-plot for random effects
  if (is_lmm) {
    ranef_dist <- readRDS(paste0(mname, '_ranef.rds'))$author_id['(Intercept)'] %>% unlist() %>% as.numeric()
    qqnorm(ranef_dist, main = "QQ-Plot of Random Effects Author")
    qqline(ranef_dist, col = "red")
    plots[['Random Effects QQ-Plot Author']] <- recordPlot()
    ranef_dist <- readRDS(paste0(mname, '_ranef.rds'))$shash['(Intercept)'] %>% unlist() %>% as.numeric()
    qqnorm(ranef_dist, main = "QQ-Plot of Random Effects Hashtag")
    qqline(ranef_dist, col = "red")
    plots[['Random Effects QQ-Plot Hashtag']] <- recordPlot()
  }

  # Plot 3: Cook's Distance
  cooks <- readRDS(paste0(mname, '_cooks.rds'))
  plot(sort(cooks), type = "h", main = "Cook's Distance", ylab = "Cook's distance", xlab = "Sorted Index")
  abline(h = 1, col = "red")  # heuristic threshold line
  plots[['Cook\'s Distance Plot']] <- recordPlot()

  return(plots)
}

ps <- get_model_diagnostics(m3_ngss, 'm3_ngss', is_lmm=TRUE)
ps$`Residuals QQ-Plot`
ps$`Random Effects QQ-Plot Author`
ps$`Random Effects QQ-Plot Hashtag`
ps$`Cook's Distance Plot`

ps <- get_model_diagnostics(m3_ccss, 'm3_ccss', is_lmm=TRUE)
ps$`Residuals QQ-Plot`
ps$`Random Effects QQ-Plot Author`
ps$`Random Effects QQ-Plot Hashtag`
ps$`Cook's Distance Plot`

# p_m_linear_ngss <- performance::check_model(m_linear_ngss)
# p_m_quad_ngss <- performance::check_model(m_quad_ngss)
# p_m_cubic_ngss <- performance::check_model(m_cubic_ngss)
#
# p_m_linear_ccss <- performance::check_model(m_linear_ccss)
# p_m_quad_ccss <- performance::check_model(m_quad_ccss)
# p_m_cubic_ccss <- performance::check_model(m_cubic_ccss)
#
# p_m1 <- performance::check_model(m1)
# p_m2 <- performance::check_model(m2)
# p_m3 <- performance::check_model(m3)
#
# p_m1_ngss <- performance::check_model(m1_ngss)
# p_m2_ngss <- performance::check_model(m2_ngss)
# p_m3_ngss <- performance::check_model(m3_ngss)
#
# p_m1_ccss <- performance::check_model(m1_ccss)
# p_m2_ccss <- performance::check_model(m2_ccss)
# p_m3_ccss <- performance::check_model(m3_ccss)
#
# l <- list(
#   p_m_linear_ngss=p_m_linear_ngss,
#   p_m_quad_ngss=p_m_quad_ngss,
#   p_m_cubic_ngss=p_m_cubic_ngss,
#   p_m_linear_ccss=p_m_linear_ccss,
#   p_m_quad_ccss=p_m_quad_ccss,
#   p_m_cubic_ccss=p_m_cubic_ccss,
#   p_m1=p_m1,
#   p_m2=p_m2,
#   p_m3=p_m3,
#   p_m1_ngss=p_m1_ngss,
#   p_m2_ngss=p_m2_ngss,
#   p_m3_ngss=p_m3_ngss,
#   p_m1_ccss=p_m1_ccss,
#   p_m2_ccss=p_m2_ccss,
#   p_m3_ccss=p_m3_ccss
# )
#
# saveRDS(l, 'performance-l.rds')

# RQ3
#effects <- ranef(m3)
#saveRDS(ranef(m3), 'raneffects-v2.rds')
effects <- readRDS('raneffects-v2.rds') # Stored as it takes long to compute

keep_this <- d_model %>% count(author_id) %>% filter(n>=100) %>% pull(author_id) # Sample authors with at least 100 original tweets

hashref_time <- d_model %>%
  group_by(shash) %>%
  summarize(
    before_2016 = median(yearmonth) < '2016-01-01'
  ) %>%
  ungroup()

userref_time <- d_model %>%
  group_by(author_id) %>%
  summarize(
    before_2016 = median(yearmonth) < '2016-01-01'
  ) %>%
  ungroup()

# Figure 2 Setup

# Year
tmp <- d_model %>%
  group_by(shash) %>%
  summarize(
    median_year = substr(median(yearmonth), 1, 4)
  ) %>%
  ungroup()

# Affiliations
affiliations <- d_model %>%
  group_by(shash) %>%
  summarize(is_ngss = mean(is_ngss)>0.5) %>%
  ungroup()

# Plot data
d_plot <- effects$shash %>%
  rownames_to_column() %>%
  tibble() %>%
  janitor::clean_names() %>%
  arrange(desc(abs(intercept))) %>%
  left_join(tmp, by=c('rowname'='shash')) %>%
  group_by(median_year) %>%
  slice_max(order_by = abs(intercept), n=3) %>%
  left_join(affiliations, by=c('rowname'='shash'))

library(ggrepel)

# Figure generation
d_plot %>%
  mutate(is_ngss = ifelse(is_ngss, 'NGSS', 'CCSS')) %>%
  ggplot(aes(median_year, intercept, color = is_ngss, label = rowname)) +
  geom_text_repel(family = "Times New Roman") +
  scale_color_manual(values = c("NGSS" = "#000000", "CCSS" = "#E69F00")) +
  theme_bw(base_size = 16) + # Change the base font size here
  theme(text = element_text(family = "Times New Roman")) +
  labs(x='Median Time of Posting', y='Baseline Sentiment Expression Model Intercept', color='Reform')

# Affilitations of most extreme instances of Users and hashtags

# Author breakout
d_model %>%
  left_join(userref_time) %>%
  count(before_2016, is_ngss)

d_model %>%
  left_join(hashref_time) %>%
  count(before_2016, is_ngss)

# Authors before 2016
keep_this <- d_model %>%
  left_join(userref_time) %>%
  filter(!before_2016) %>%
  count(author_id) %>%
  filter(n>=100) %>%
  pull(author_id)

tmp <- effects$author_id %>%
  rownames_to_column() %>%
  tibble() %>%
  janitor::clean_names() %>%
  arrange(desc(abs(intercept))) %>%
  filter(rowname %in% keep_this) %>%
  head(10) %>%
  as.data.frame()

tmp

d_model %>%
  filter(author_id %in% tmp$rowname) %>%
  group_by(author_id) %>%
  summarize(
    is_ngss = mean(is_ngss),
    is_ccss = mean(is_ccss),
    is_both = mean(is_both)
  )

# Tweet examples of extreme instances of authors
d_model %>%
  filter(author_id %in% tmp$rowname) %>%
  group_by(author_id) %>%
  summarize(
    is_ngss = mean(is_ngss),
    is_ccss = mean(is_ccss),
    is_both = mean(is_both),
    median_time = median(yearmonth),
    avg_sentiment = mean(compound),
    sd_sentiment = sd(compound),
    example_tweet = sample(text, 1)
  ) %>%
  as.data.frame()

# Hashtags before 2016
keep_this <- d_model %>%
  left_join(hashref_time) %>%
  filter(!before_2016) %>%
  pull(shash)

tmp <- effects$shash %>%
  rownames_to_column() %>%
  tibble() %>%
  janitor::clean_names() %>%
  arrange(desc(abs(intercept))) %>%
  filter(rowname %in% keep_this) %>%
  head(10) %>%
  as.data.frame()

d_model %>%
  filter(shash %in% tmp$rowname) %>%
  group_by(shash) %>%
  summarize(
    is_ngss = mean(is_ngss),
    is_ccss = mean(is_ccss),
    is_both = mean(is_both),
    median_time = median(yearmonth),
    avg_sentiment = mean(compound),
    sd_sentiment = sd(compound)
  )

d_model %>%
  filter(shash %in% tmp$rowname) %>%
  group_by(shash) %>%
  summarize(
    example_tweet = sample(text, 1)
  )

effects %>% names()
tmp <- effects$author_id %>%
  rownames_to_column() %>%
  tibble() %>%
  janitor::clean_names() %>%
  arrange(desc(abs(intercept))) %>%
  filter(rowname %in% keep_this) %>%
  head(10) %>%
  as.data.frame()

d_model %>%
  filter(author_id %in% tmp$rowname) %>%
  group_by(author_id) %>%
  summarize(
    is_ngss = mean(is_ngss),
    is_ccss = mean(is_ccss),
    is_both = mean(is_both)
  )

# Further tweet example sampling by instance of user and hashtag
lookup <- tmp$rowname

example_tweets_by_id <- function(d_model, s) {
  d_model %>%
    filter(author_id == s) %>%
    sample_n(5) %>%
    pull(text)
}
example_tweets_by_id(d_model, lookup[1])
example_tweets_by_id(d_model, lookup[2])
example_tweets_by_id(d_model, lookup[3])
example_tweets_by_id(d_model, lookup[4])
example_tweets_by_id(d_model, lookup[5])
example_tweets_by_id(d_model, lookup[6])
example_tweets_by_id(d_model, lookup[7])
example_tweets_by_id(d_model, lookup[8])
example_tweets_by_id(d_model, lookup[9])
example_tweets_by_id(d_model, lookup[10])

example_tweets_by_id <- function(d_model, s) {
  d_model %>%
    filter(author_id == s) %>%
    pull(created_at) %>%
    median()
}
example_tweets_by_id(d_model, lookup[1])
example_tweets_by_id(d_model, lookup[2])
example_tweets_by_id(d_model, lookup[3])
example_tweets_by_id(d_model, lookup[4])
example_tweets_by_id(d_model, lookup[5])
example_tweets_by_id(d_model, lookup[6])
example_tweets_by_id(d_model, lookup[7])
example_tweets_by_id(d_model, lookup[8])
example_tweets_by_id(d_model, lookup[9])
example_tweets_by_id(d_model, lookup[10])

tmp <- effects$shash %>%
  rownames_to_column() %>%
  tibble() %>%
  janitor::clean_names() %>%
  arrange(desc(abs(intercept))) %>%
  head(10) %>%
  as.data.frame()
tmp

# Affiliation
d_model %>%
  filter(shash %in% tmp$rowname) %>%
  group_by(shash) %>%
  summarize(
    is_ngss = mean(is_ngss),
    is_ccss = mean(is_ccss),
    is_both = mean(is_both)
  )
example_tweets_by_shash <- function(d_model, s) {
  d_model %>%
    filter(shash == s) %>%
    sample_n(5) %>%
    pull(text)
}
example_tweets_by_shash(d_model, tmp$rowname[1])
example_tweets_by_shash(d_model, tmp$rowname[2])
example_tweets_by_shash(d_model, tmp$rowname[3])
example_tweets_by_shash(d_model, tmp$rowname[4])
example_tweets_by_shash(d_model, tmp$rowname[5])
example_tweets_by_shash(d_model, tmp$rowname[6])
example_tweets_by_shash(d_model, tmp$rowname[7])
example_tweets_by_shash(d_model, tmp$rowname[8])
example_tweets_by_shash(d_model, tmp$rowname[9])
example_tweets_by_shash(d_model, tmp$rowname[10])
