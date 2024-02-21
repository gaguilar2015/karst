library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(stringr)
library(janitor)
library(DBI)

base <- read_sav('./KARST_FILES/tmnr.sav')
listing <- read_sav('./KARST_FILES/listingRoster.sav')
k2 <- read_sav('./KARST_FILES/k2_roster.sav')

k2

glimpse(base)

#47-88-11-61
#37-38-79-37

ignore_qs <- c('79-84-83-57',
               '30-26-25-41',
               '66-59-17-95',
               '92-56-91-59',
               '70-81-50-38',
               '17-39-72-74'
               )

base |> 
  select(interview__key, ctv, ed, building_number, household_number, dwelling_number, result) |> 
  get_dupes(ed, building_number, dwelling_number)

distinct(base, ctv)
distinct(ksample, ctv_official)


listing2 <- base |> 
  select(interview__key, ctv, ed, building_number, household_number, dwelling_number, result) |> 
  left_join(
    listing,
    by = c('interview__key')
  ) |> 
  filter(
    result == 1,
    !interview__key %in% ignore_qs
  )

ll <- listing2 |> 
  mutate(
    age_group = case_when(
      age >= 50 ~ '50+',
      age %in% 35:49 ~ '35-49',
      age %in% 18:35 ~ '18-35',
      age %in% 5:17 ~ '5-17',
      age %in% 0:15 ~ '0-4'
    )
  ) |> select(age, sex, age_group)

ll |> 
  ggplot(aes(x = age_group)) +
  geom_bar()

listing2 |> 
  filter(is.na(age)) |> select(interview__key)


gtcars |>
  dplyr::select(model, year, hp, trq) |>
  dplyr::slice(1:8) |>
  gt(rowname_col = "model") |>
  tab_row_group(
    label = "numbered",
    rows = matches("^[0-9]")
  )

anal_vars <- c('strata', 'age_group', 'ethnicity', 'education', 'workertype_group')

households |> 
  select(all_of(anal_vars), starts_with('k4')) |> 
  select(-k4_1) |> 
  pivot_longer(
    cols = starts_with('k4'),
    names_to = 'k4_item',
    values_to = 'k4_value'
  ) |> 
  filter(k4_value == 1) |> 
  tabyl(strata, k4_item) |> 
  adorn_totals(c('col'))

households |> 
  select(all_of(anal_vars), starts_with('k4')) |> 
  select(-k4_1) |> 
  pivot_longer(
    cols = starts_with('k4'),
    names_to = 'item',
    values_to = 'value'
  ) |> 
  group_by(strata, item) |> 
  summarise(
    selected = sum(value == 1, na.rm = T),
    total = sum(!is.na(value), na.rm = T),
    prop = selected/total,
    pct = paste0(round(prop * 100, 1), '%'),
    full = paste0(pct, '(', selected,')')
  ) |> 
  select(strata, item, full) |> 
  pivot_wider(
    names_from = item,
    values_from = full
  )

multi_tabulate <- function(row, col){
  
  households |> 
    select(all_of(anal_vars), starts_with(col)) |> 
    pivot_longer(
      cols = starts_with(col),
      names_to = 'item',
      values_to = 'value'
    ) |> 
    group_by(!!as.name(row), item) |> 
    summarise(
      selected = sum(value == 1, na.rm = T),
      total = sum(!is.na(value), na.rm = T),
      prop = selected/total,
      pct = paste0(round(prop * 100, 1), '%'),
      full = paste0(pct, '(', selected,')')
    ) |> 
    select(!!as.name(row), item, full) |> 
    pivot_wider(
      names_from = item,
      values_from = full
    )
  
}


multi_aggregate <- function(var){
  df_list <- purrr::map(anal_vars, ~multi_func1(.x, var))
  
  rbind(df_list[[1]] |> rename(Item = strata),
        df_list[[2]] |> rename(Item = age_group),
        df_list[[3]] |> rename(Item = ethnicity),
        df_list[[4]] |> rename(Item = education),
        df_list[[5]] |> rename(Item = workertype_group)
  )
}


households |> 
  count(strata, age_group, wt = ind_weights) |>
  mutate(total = sum(n)) |>
  mutate(
    pct = n/total
  ) |>
  select(-c(total, n)) |>
  pivot_wider(
    names_from = age_group,
    values_from = pct
  )


households |> 
  tabyl(strata, age_group, show_missing_levels = F, show_na = F) |> 
  adorn_totals(c("col")) |> 
  adorn_percentages(c('row')) |> 
  adorn_pct_formatting(digits = 1) |> 
  adorn_ns() |> 
  rename(Item = strata)
households |>
  count(strata, wt = hh_final_weight)

households |> 
  count(strata, age_group, wt = ind_weights) |>
  group_by(strata) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  mutate(
    prop = n/total,
    pct = paste0(round(prop * 100, 1), '%')
  ) |>
  mutate(
    n_pct = paste0(pct, ' (', ceiling(n), ')')
  )  |>
  select(strata, age_group, n_pct) |>
  pivot_wider(
    names_from = age_group,
    values_from = n_pct
  )

households |> 
  tabyl(strata, !!as.name(var), show_missing_levels = F, show_na = F) |> 
  adorn_totals(c("col")) |> 
  rename(Item = strata)


#----------- function

households |> 
  count(k1, wt = ind_weights) |>
  mutate(Item = 'Total') |> 
  group_by(Item) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  mutate(
    prop = n/total,
    pct = paste0(round(prop * 100, 1), '%')
  ) |>
  mutate(
    n_pct = paste0(pct, ' (', ceiling(n), ')')
  )  |>
  select(Item, k1, n_pct) |>
  pivot_wider(
    names_from = k1,
    values_from = n_pct
  ) 



ct_single <- function(row, col, format = 'pct'){
  
  households |> 
    count(!!as.name(row), !!as.name(col), wt = ind_weights) |>
    group_by(!!as.name(row)) |>
    mutate(total = sum(n)) |>
    ungroup() |>
    mutate(
      prop = n/total,
      pct = paste0(round(prop * 100, 1), '%')
    ) |>
    mutate(
      n_pct = paste0(pct, ' (', ceiling(n), ')')
    )  |>
    select(!!as.name(row), !!as.name(col), n_pct) |>
    pivot_wider(
      names_from = !!as.name(col),
      values_from = n_pct
    )
  
}

ct_single_aggregate <- function(var){
  df_list <- purrr::map(anal_vars, ~ct_single(.x, var))
  
  df <- rbind(df_list[[1]] |> rename(Item = strata),
        df_list[[2]] |> rename(Item = age_group),
        df_list[[3]] |> rename(Item = ethnicity),
        df_list[[4]] |> rename(Item = education),
        df_list[[5]] |> rename(Item = workertype_group)
  )
  
  df[is.na(df)] <- '0% (0)'
  
  df
}

ct_single_aggregate('k1')


#######----

ct_multi <- function(row, col, format = 'pct'){
  
  
  households |> 
    select(all_of(anal_vars), starts_with(col), ind_weights) |> 
    pivot_longer(
      cols = starts_with(col),
      names_to = 'item',
      values_to = 'value'
    ) |> 
    group_by(!!as.name(row), item) |> 
    summarise(
      .groups = 'drop',
      selected = sum((value == 1) * ind_weights, na.rm = T) ,
      total = sum(!is.na(value) * ind_weights, na.rm = T),
      prop = selected/total,
      pct = paste0(round(prop * 100, 1), '%'),
      full = paste0(pct, '(', selected,')')
    ) |> 
    select(!!as.name(row), item, full) |> 
    mutate(
      item = stringr::str_replace(item, paste0(col, '_'), '')
    ) |> 
    pivot_wider(
      names_from = item,
      values_from = full
    )
  
}


ct_multi_aggregate <- function(var){
  df_list <- purrr::map(anal_vars, ~ct_multi(.x, var))
  
  rbind(df_list[[1]] |> rename(Item = strata),
        df_list[[2]] |> rename(Item = age_group),
        df_list[[3]] |> rename(Item = ethnicity),
        df_list[[4]] |> rename(Item = education),
        df_list[[5]] |> rename(Item = workertype_group)
  )
}

ct_multi_aggregate('k4')


households |> 
  select(strata, starts_with('k4'), ind_weights) |> 
  pivot_longer(
    cols = starts_with('k4'),
    names_to = 'item',
    values_to = 'value'
  ) |> 
  mutate(
    value = as.numeric(value) * ind_weights,
    ind_weights = ind_weights * !is.na(value)
  ) |> 
  group_by(strata, item) |> 
  summarise(
    .groups = 'drop',
    selected = ceiling(sum(value, na.rm = T)),
    total = ceiling(sum(ind_weights, na.rm = T)),
    prop = selected/total,
    pct = paste0(round(prop * 100, 1), '%'),
    full = paste0(pct, '(', selected,')')
  ) |> 
  select(!!as.name(row), item, full) |> 
  mutate(
    item = stringr::str_replace(item, paste0(col, '_'), '')
  ) |> 
  pivot_wider(
    names_from = item,
    values_from = full
  )

k2 |> as_factor() |> 
  filter(!is.na(k2_roster__id)) |> 
  select(interview__key, k2_roster__id, k2_rate) |> 
  pivot_wider(
    names_from = k2_roster__id,
    values_from = k2_rate
  )

k2 <- k2 |> filter(!is.na(ind_weights))

w_vec <- round(k2$ind_weights)

k2 <- k2[rep(seq_len(nrow(k2)), w_vec), ]

k2 |> as_factor() |> 
  filter(!is.na(k2_roster__id)) |> 
  ggplot(aes(x = k2_roster__id, fill = k2_rate)) +
  geom_bar( position = position_stack(reverse = TRUE))  +
  scale_fill_brewer(palette = "BrBG") 

(values = MetBrewer::met.brewer(name="Hokusai2", n=5, type="discrete"))

library(HH)  

k2 |> 
  mutate(
    k2_roster__id = as_factor(k2_roster__id),
    k2_rate = as_factor(k2_rate)
  ) |> 
  filter(!is.na(k2_roster__id)) |> 
  dplyr::select(
    interview__key, k2_roster__id, k2_rate
  ) |> 
  pivot_wider(names_from = k2_rate, values_from = k2_rate) |> 
  group_by(
    k2_roster__id
  ) |> summarise(
    One = sum(!is.na(`1`), na.rm = T)
  )


b <- k2 |> 
  mutate(
    k2_roster__id = as_factor(k2_roster__id),
    k2_rate = as_factor(k2_rate)
  ) |> 
  filter(!is.na(k2_roster__id)) |> 
  dplyr::select(
    interview__key, k2_roster__id, k2_rate
  ) |> 
  group_by(
    k2_roster__id
  ) |> summarise(
    One = sum(k2_rate == 1, na.rm = T),
    Two = sum(k2_rate == 2, na.rm = T),
    Three = sum(k2_rate == 3, na.rm = T),
    Four = sum(k2_rate == 4, na.rm = T),
    Five = sum(k2_rate == 5, na.rm = T)
  ) |> 
  mutate(
    Onep = One/(One + Two + Three + Four + Five),
    Twop = Two/(One + Two + Three + Four + Five),
    Threep = Three/(One + Two + Three + Four + Five),
    Fourp = Four/(One + Two + Three + Four + Five),
    Fivep = Five/(One + Two + Three + Four + Five)
  ) |> 
  dplyr::select(k2_roster__id, Onep, Twop, Threep, Fourp, Fivep)

# use function likert() to plot likert data
HH::likert(k2_roster__id~., b, positive.order=TRUE, as.percent = TRUE,
           main="At my child's school my child is safe.",
           xlab="percentage",ylab="School Code")

b <- k2 |> 
  mutate(
    k2_roster__id = as_factor(k2_roster__id)
  ) |> 
  tabyl(k2_roster__id, k2_rate, show_missing_levels = F, show_na = F) |> 
  adorn_percentages()


HH::likert(k2_roster__id~., b, positive.order=TRUE, as.percent = TRUE,
           rightAxis = F,
           main="Knowledge",
           xlab="percentage",ylab="Item")

library(likert)

data(pisaitems) 
items28 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST24Q"] 

b2 <- k2 |> 
  filter(!is.na(k2_roster__id)) |> 
  select(interview__key, strata, k2_roster__id, k2_rate) |> 
  mutate(
    k2_roster__id = as_factor(k2_roster__id)
  ) |> 
  pivot_wider(
    names_from = k2_roster__id,
    values_from = k2_rate
  ) |> 
  mutate(
    `Protecting wildlife and nature` = as_factor(`Protecting wildlife and nature`),
    `Using Resources Wisely` = as_factor(`Using Resources Wisely`),
    `Reducing Pollution` = as_factor(`Reducing Pollution`),
    `Clean and reusable power` = as_factor(`Clean and reusable power`),
    `Habitat Restoration` = as_factor(`Habitat Restoration`),
    `Waste Reduction and Recycling` = as_factor(`Waste Reduction and Recycling`),
    `Education and Advocacy` = as_factor(`Education and Advocacy`),
  ) |> as.data.frame()

b2 <- purrr::modify_if(b2, is.factor, ~ forcats::fct_drop(.x))

plot(likert::likert(b2[,3:9], grouping = b2[,2]))

k2 |> 
  mutate(
    k2_roster__id = as_factor(k2_roster__id)
  ) 

k2 |>   
  mutate(
    strata = case_when(
      ctv.x %in% c('El Progress (7 Miles)/Upper Barton Creek', 
                   'Lower Barton Creek/New Holland/East Land') ~ 'Mennonite',
      TRUE ~ 'Non-Mennonite'
    )
  ) |> 
  count(k2_roster__id, k2_rate, wt = ind_weights)
  
  
  group_by(!!as.name(row)) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  mutate(
    prop = n/total,
    pct = paste0(round(prop * 100, 1), '%')
  ) |>
  mutate(
    n_pct = paste0(pct, ' (', ceiling(n), ')')
  )  |>
  select(!!as.name(row), !!as.name(col), n_pct) |>
  pivot_wider(
    names_from = !!as.name(col),
    values_from = n_pct
  )
  
  
  
  k2 |> 
    filter(!is.na(k2_roster__id), !is.na(ind_weights)) |> 
    group_by(k2_roster__id) |> 
    summarise(
      .groups = 'drop',
      avg = weighted.mean(k2_rate, ind_weights, na.rm = T)
    )
  
  k2 |> 
    filter(!is.na(k2_roster__id)) |> 
    select(strata, ind_weights, k2_rate) |> 
    group_by(strata) |> 
    summarise(
      weighted.mean(k2_rate, ind_weights, na.rm = T)
    )

  k2 |> 
    mutate(k2_roster__id = as_factor(k2_roster__id)) |> 
    group_by(k2_roster__id) |> 
    summarise(
      n = sum(!is.na(k2_rate)),
      total = sum(ind_weights, na.rm = T)
    )
  
  
  ## Create df
  Item <- c("Oatmeal Raisin is The Best Type of Cookie", "Chocolate Chip is The Best Type of Cookie", "Snickerdoodle is The Best Type of Cookie")
  Strata <- c(1,1,2)
  strong_disagree <- c(60, 20, 10)
  disagree <- c(7, 25, 47)
  neutral <- c(0,0,0)
  agree <- c(3, 15, 38)
  strong_agree  <- c(30, 40, 05)
  df <- data.frame(Strata, Item, strong_disagree, disagree, neutral, agree, strong_agree)
  
  ## Rename Cols (for legend)
  df <- df  %>% 
    rename("Strong Disagree" = strong_disagree,
           "Disagree" = disagree,
           "Agree" = agree,
           "Strong Agree" = strong_agree)
  
  ## Basic Plot (not image below)
  plot(likert(summary = df[,2:7], grouping = df[,1]))
  
  ## Pretty Plot (Image Below)
  plot(likert(summary = df), plot.percent.neutral=FALSE, legend.position="right")
  
  
  
  
  k3 |> 
    mutate(id = row_number()) |> 
    filter(!is.na(k2_roster__id)) |> 
    select(id, !!as.name(row), k2_roster__id, k2_rate) |> 
    mutate(
      k2_roster__id = as_factor(k2_roster__id)
    ) |> 
    pivot_wider(
      names_from = k2_roster__id,
      values_from = k2_rate
    )
  df2 <- df[,c(5,8,7,6,9,4,3)]
  df$age_group <- reverse.levels(df$age_group)
  plot(likert::likert(df2, grouping = df[,2]), legend.position = 'top')
  
  
  k2_hh <- households |> filter(!is.na(k2__1), !is.na(ind_weights))
  
  w_vec_hh <- round(k2_hh$ind_weights)
  
  k2_hh_2 <- k2_hh[rep(seq_len(nrow(k2_hh)), w_vec_hh), ]
  
  k2_3 <- k2_hh_2 |> 
    select(strata, starts_with('k2')) |> 
    filter(!is.na(k2__1)) |> 
    pivot_longer(
      starts_with('k2'),
      names_to = 'Item',
      values_to = 'Value'
    ) |> 
    mutate(
      Value = case_when(
        Value == 1 ~ 'Yes',
        TRUE ~ 'No'
      ),
      Item = case_when(
        Item == 'k2__1' ~ 'Protecting wildlife and nature',
        Item == 'k2__2' ~ 'Using Resources Wisely',
        Item == 'k2__3' ~ 'Reducing Pollution',
        Item == 'k2__4' ~ 'Clean and reusable power',
        Item == 'k2__5' ~ 'Habitat Restoration',
        Item == 'k2__6' ~ 'Waste Reduction and Recycling',
        Item == 'k2__7' ~ 'Education and Advocacy'
      )
    ) 
  
  k2_3$Item <- factor(k2_3$Item, levels = c(  'Clean and reusable power',
                                              'Education and Advocacy',
                                              'Habitat Restoration',
                                              'Protecting wildlife and nature',
                                              'Reducing Pollution',
                                              'Using Resources Wisely',
                                              'Waste Reduction and Recycling'))
  
  k2_3$Item <- fct_rev(k2_3$Item)
  
  
  k2_3 |> 
    ggplot(aes(x = Item, fill = Value)) +
    geom_bar(position = 'fill') +
    geom_text(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                  label=paste0(round((..count../tapply(..count.., ..x.. ,sum)[..x..]), 3) * 100, '%')),
    size = 4, 
    color = 'white',
    stat="count", 
    position=position_fill(0.5)
    ) + 
    coord_flip() +
    gg_blank_x + 
    labs(y = 'Proportion',
         title = 'Has heard of the term...') +
    scale_fill_manual(values = c('burlywood3', 'lightseagreen')) +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x =  element_blank()
    )
  
  
listing_head_vars |> 
  count(age_group, wt = ind_weights) |> 
  mutate(
    Count = round(n),
    total = sum(n, na.rm = T),
    Percent = paste0(round(n/total,3) * 100, '%')
  ) |> 
  select(
    Item = age_group, Count, Percent
  )
  
listing_head_vars |> 
  mutate(sex = as)
  count(sex, wt = ind_weights) |> 
  mutate(
    Count = round(n),
    total = sum(n, na.rm = T),
    Percent = paste0(round(n/total,3) * 100, '%')
  ) |> 
  select(
    Item = sex, Count, Percent
  )

  
  col <- 'k8__'
  
  households |> 
    select(all_of(anal_vars), starts_with(col), ind_weights) |> 
    pivot_longer(
      cols = starts_with(col),
      names_to = 'item',
      values_to = 'value'
    ) |> 
    mutate(
      value = as.numeric(as.character(value)) * ind_weights,
      ind_weights = ind_weights * !is.na(value)
    ) |> 
    group_by(!!as.name(row), item) |> 
    summarise(
      .groups = 'drop',
      selected = ceiling(sum(value, na.rm = T)),
      total = ceiling(sum(ind_weights, na.rm = T)),
      prop = selected/total,
      pct = paste0(round(prop * 100, 1), '%'),
      full = paste0(pct, '(', selected,')')
    ) |>
    select(!!as.name(row), item, full) |> 
    mutate(
      item = stringr::str_replace(item, paste0(col, '_'), '')
    ) |> 
    pivot_wider(
      names_from = item,
      values_from = full
    )

  
 bar <-  households |> 
    mutate(
      k3 = case_when(
        k3 == 'Yes' ~ 'Yes',
        TRUE ~ 'No or DK'
      )
    ) |> 
    ggplot(aes(x = "", fill = k3)) +
    geom_bar(position = 'fill') +
    geom_text(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                  label=paste0(round((..count../tapply(..count.., ..x.. ,sum)[..x..]), 3) * 100, '%')),
              size = 5, 
              color = 'white',
              stat="count", 
              position=position_fill(0.5)
    ) + 
    coord_flip() +
    gg_blank_x + 
    labs(y = 'Proportion',
         title = 'Proportion of respondends who have heard about TMNR') +
    scale_fill_manual(values = c('burlywood3', 'lightseagreen')) +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x =  element_blank()
    )

 bar + coord_polar("y", start=0)  
 
 
 
 households |> 
   mutate(
   k3 = case_when(
     k3 == 'Yes' ~ 'Yes',
     TRUE ~ 'No or DK'
   )) |> 
  count(k3, wt = ind_weights)
   
   
 
 households$p13__1
 