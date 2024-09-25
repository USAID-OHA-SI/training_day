# PROJECT:  C:/Users/tessam/Documents/Github/training_day
# PURPOSE:  Questions for new SI staff
# AUTHOR:   T. Essam | USAID
# REF ID:   0710af56
# LICENSE:  MIT
# DATE:     2024-09-24
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

# general
library(tidyverse)
library(glue)
# oha
library(gagglr)
library(scales, warn.conflicts = FALSE)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)
library(gt)
library(gtExtras)
library(gtayblr)


# FETCH DATA --------------------------------------------------------------

  piggyback::pb_download("MER_Structured_TRAINING_Datasets_PSNU_IM_FY59-62_20240816_v1_1.zip",
    repo = "USAID-OHA-SI/themask",
    tag = "latest",
    dest = "Data"
  )

# GLOBAL VARIABLES --------------------------------------------------------

  # Return the most recent version of the PSNU_IM file
  path_msd <- return_latest("Data", pattern = "PSNU_IM")
  
  ref_id <- "0710af56" # a reference to be places in viz captions
  
  # Store metadata in list object for use in plotting and filters
  meta <- get_metadata(path_msd) # extract MSD metadata


# IMPORT ------------------------------------------------------------------

  df_msd <- read_psd(path_msd)


# QUESTIONS -------------------------------------------------------------------

## Which partner should we focus on with lower target achievement in test pos --------

  df_hts_ptnr <- df_msd %>%
    filter(
      fiscal_year == meta$curr_fy,
      indicator == "HTS_TST_POS",
      standardizeddisaggregate == "Total Numerator"
    ) %>%
    group_by(prime_partner_name) %>%
    summarize(across(.cols = c(targets, cumulative), \(x) sum(x, na.rm = T))) %>%
    calc_achievement()
  
  # What is the median target level?
  tgt_median <- median(df_hts_ptnr$targets, na.rm = T)
  
  df_hts_ptnr <-
    df_hts_ptnr %>%
    mutate(hts_volume = ifelse(targets > tgt_median, "High", "Low")) %>%
    arrange(achievement)
  
  trbl_ptnr <- df_hts_ptnr %>%
    filter(hts_volume == "High", achievement < 0.5) %>%
    pull(prime_partner_name)
  
  df_hts_ptnr %>%
    gt(groupname_col = "hts_volume") %>%
    sub_missing(missing_text = "-") %>%
    fmt_percent(columns = achievement, decimal = 0) %>%
    gt_highlight_rows(rows = prime_partner_name == "Dash", fill = grey20k) %>%
    gtayblr::si_gt_base() %>%
    tab_header(title = glue("{trbl_ptnr} lags behind in performance")) %>%
    tab_source_note(source_note = glue("{meta$caption}"))


## What share of total and positive tests are from index testing in --------


  # What is the last quarter?
  meta$curr_qtr
  
  # Indicator list
  indic_list <- c("HTS_TST_POS", "HTS_TST")
  
  # What are total test results?
  df_index_tot <- df_msd %>%
    filter(
      indicator %in% indic_list,
      fiscal_year == meta$curr_fy,
      standardizeddisaggregate == "Total Numerator"
    ) %>%
    count(indicator, wt = qtr3) %>%
    rename(total = n)
  
  df_msd %>%
    filter(
      indicator %in% indic_list,
      standardizeddisaggregate == "Modality/Age/Sex/Result",
      str_detect(modality, "Index")
    ) %>%
    group_by(indicator, fiscal_year) %>%
    summarize(across(.cols = contains("qtr"), \(x) sum(x, na.rm = T)), .groups = "drop") %>%
    reshape_msd() %>%
    filter(period == meta$curr_pd) %>%
    left_join(df_index_tot) %>%
    mutate(share = value / total) %>%
    # Turn into a table
    gt(groupname_col = "period") %>%
    cols_hide(period_type) %>%
    fmt_percent(columns = share) %>%
    fmt_number(columns = value:total, decimals = 0) %>%
    cols_label(value = "index testing results") %>%
    si_gt_base() %>%
    tab_header(title = "Index testing results summary") %>%
    tab_source_note(source_note = glue("{meta$caption}"))


## SNU decline in testing but increase in positivity over last few qtrs. ----------------------

  df_hts_snu1 <- df_msd %>%
    filter(
      indicator %in% indic_list,
      standardizeddisaggregate == "Total Numerator"
    ) %>%
    group_by(snu1, fiscal_year, indicator) %>%
    summarize(across(.cols = contains("qtr"), \(x) sum(x, na.rm = T)), .groups = "drop") %>%
    reshape_msd() %>%
    select(-period_type) %>%
    pivot_wider(
      names_from = indicator,
      values_from = value
    ) %>%
    mutate(positivity = HTS_TST_POS / HTS_TST) %>%
    group_by(snu1) %>%
    arrange(period) %>%
    mutate(
      HTS_TST_trend = (HTS_TST - lag(HTS_TST)) / lag(HTS_TST),
      pos_trend = (positivity - lag(positivity)) / lag(positivity)
    ) %>%
    ungroup() %>%
    mutate(
      flag_up_down = (HTS_TST_trend < 0 & pos_trend > 0),
      flag_down_up = (HTS_TST_trend > 0 & pos_trend < 0)
    )
  
  
  # Any decline in testing but increase in positivity?
  # No SNUs had a decline in testing but increase in positivity
  df_hts_snu1 %>%
    filter(period > "FY59Q1") %>%
    count(snu1, period, flag_up_down) %>% 
    spread(period, n) %>% arrange(flag_up_down)
  
  df_hts_snu1 %>%
    filter(period > "FY59Q1") %>%
    count(snu1, flag_down_up, sort = T) %>%
    pivot_wider(names_from = flag_down_up, values_from = n)
  
  
  # Explore data
  df_hts_snu1 %>%
    ggplot(aes(x = period)) +
    geom_col(aes(y = HTS_TST), fill = hw_hunter, width = 0.5) +
    geom_col(aes(y = HTS_TST_POS),
      fill = tango, width = 0.5,
      position = position_nudge(x = 0.1)
    ) +
    geom_label(aes(label = percent(positivity, accuracy = 0.01), y = HTS_TST_POS),
      size = 10 / .pt,
      family = "Source Sans Pro",
      color = grey90k,
      nudge_x = 0.1,
      vjust = -1
    ) +
    facet_wrap(~snu1, scale = "free_y") +
    scale_y_continuous(labels = comma) +
    si_style_ygrid() +
    labs(
      title = "HTS_TST COMPARED TO HTS_TST_POS RESULTS ACROSS SNU1S",
      subtitle = "Midwest and Northwest have declining test volumne and increasing positivity",
      caption = glue("{meta$caption}")
    )




## What share of newly initiated patients onto treatment have a CD4 --------
# Newly initiated -- TX_NEW

  # How many clients on TX_NEW in latest quarter?
  df_tx_new <- df_msd %>%
    filter(
      indicator == "TX_NEW",
      standardizeddisaggregate == "Total Numerator",
      fiscal_year == meta$curr_fy
    ) %>%
    count(indicator, wt = cumulative, name = "tot_tx_new")
  
  # Not all facilities report CD4 data so disag may not total up
  df_msd %>%
    filter(
      indicator == "TX_NEW",
      standardizeddisaggregate == "Age/Sex/CD4/HIVStatus",
      fiscal_year == meta$curr_fy
    ) %>%
    group_by(indicator, otherdisaggregate) %>%
    summarise(cd4_totals = sum(cumulative, na.rm = T)) %>%
    left_join(df_tx_new) %>%
    mutate(share = cd4_totals / tot_tx_new) %>%
    gt() %>%
    fmt_number(columns = where(is.double), decimals = 0) %>%
    fmt_percent(columns = share) %>%
    si_gt_base() %>%
    tab_header(title = "Small share of cd4 clients below 200") %>%
    tab_source_note(source_note = glue("{meta$caption}"))


## Which age bands have had two or more quarters with declining net new--------
# TX_NET_NEW =

  df_tx_nn <- df_msd %>%
    filter(
      indicator == "TX_NET_NEW",
      use_for_age == "Y",
      age_2019 != "Unknown Age"
    ) %>%
    group_by(indicator, age_2019, fiscal_year) %>%
    summarize(across(.cols = contains("qtr"), \(x) sum(x, na.rm = T)), .groups = "drop") %>%
    reshape_msd() %>%
    group_by(age_2019) %>%
    mutate(
      tx_nn_decline = value < 0,
      tx_nn_count = sum(tx_nn_decline)
    ) %>%
    ungroup()
  
  # Label the periods on the graph
  period_labels <- df_tx_nn %>%
    distinct(period) %>%
    pull()
  period_custom_labels <- ifelse(grepl("Q1", period_labels) | period_labels == tail(period_labels, 1),
    period_labels, ""
  )
  
  # Summary plot
  df_tx_nn %>%
    ggplot(aes(x = period, y = value)) +
    geom_col(aes(fill = ifelse(value < 0, hw_orchid_bloom, hunter))) +
    facet_wrap(~ glue::glue("Ages {age_2019}: {tx_nn_count} total quarters with declines"),
      scales = "free_y"
    ) +
    scale_fill_identity() +
    si_style_ygrid(facet_space = 0.5) +
    labs(
      title = "AGES 45 AND ABOVE LOGGED FEWER THAN TWO QUARTERS OF TREATMENT LOSS",
      x = NULL, y = "TX_NET_NEW",
      caption = glue("{meta$caption}")
    ) +
    scale_x_discrete(labels = period_custom_labels)



## Which sub-national units (SNUs) had net new on treatment volumes below tx_new --------

  df_msd %>%
    filter(
      indicator %in% c("TX_NEW", "TX_NET_NEW"),
      fiscal_year == meta$curr_fy,
      standardizeddisaggregate == "Total Numerator"
    ) %>%
    group_by(snu1, indicator) %>%
    summarize(total = sum(cumulative, na.rm = T), .groups = "drop") %>%
    spread(indicator, total) %>%
    mutate(nn_below_tx_new = TX_NET_NEW < TX_NEW) %>%
    gt() %>%
    fmt_number(columns = where(is.double), decimals = 0) %>%
    si_gt_base() %>%
    tab_header(title = "Treatment volume summary by snu1") %>%
    tab_source_note(source_note = glue("{meta$caption}"))


## Are any sub-national units (SNUs) lagging behind 6 MO MMD to a majority share --------

# CHECK TX_CURR DISAGS
# Double check TX_CURR aggregegates -- there is a difference so we need to use
# T / N numbers
  df_msd %>%
    filter(
      indicator == "TX_CURR",
      fiscal_year == meta$curr_fy,
      str_detect(standardizeddisaggregate, "ARV|Total")
    ) %>%
    count(snu1, indicator, standardizeddisaggregate, wt = cumulative) %>%
    pivot_wider(names_from = standardizeddisaggregate, values_from = n) %>%
    mutate(diff = .[[3]] - .[[4]])
  
  df_tx <- df_msd %>%
    filter(
      indicator == "TX_CURR",
      fiscal_year == meta$curr_fy,
      str_detect(standardizeddisaggregate, "Total")
    ) %>%
    count(snu1, wt = cumulative, name = "tx_curr")
  
  
  df_mmd <- df_msd %>%
    filter(
      indicator == "TX_CURR",
      standardizeddisaggregate == "Age/Sex/ARVDispense/HIVStatus",
      fiscal_year == meta$curr_fy
    ) %>%
    group_by(snu1, otherdisaggregate) %>%
    summarise(tx_mmd = sum(cumulative, na.rm = T), .groups = "drop") %>%
    left_join(df_tx) %>%
    mutate(
      mmd_6mo_share = tx_mmd / tx_curr,
      mmd_order = case_when(
        str_detect(otherdisaggregate, "3 to 5") ~ "3-5 months",
        str_detect(otherdisaggregate, "Less") ~ "less than 3 months",
        TRUE ~ "6 months +"
      ),
      mmd_order = fct_relevel(mmd_order, c("less than 3 months", "3-5 months", "6 months +"))
    ) %>%
    select(-otherdisaggregate)
  
  si_palettes$orchid_bloom_t %>% show_col()
  hilght_color <- si_palettes$orchid_bloom_t[4]
  
  
  df_mmd %>%
    arrange(desc(mmd_order), mmd_6mo_share) %>%
    gt(groupname_col = "mmd_order") %>%
    fmt_number(columns = where(is.numeric), decimals = 0) %>%
    fmt_percent(columns = mmd_6mo_share, decimals = 0) %>%
    si_gt_base() %>%
    gt_highlight_rows(rows = mmd_6mo_share < 0.5 & mmd_order == "6 months +", fill = hilght_color) %>%
    tab_source_note(source_note = glue("{meta$caption}")) %>%
    tab_header(title = "Three of four regions lage behind majority 6 month mmd coverage")



# Do we see any differences in viral load coverage (VLC) by age and sex in current year
# TX_PVLS has both a total numerator and total denominator. We need the latter

  df_msd %>%
    filter(indicator %in% c("TX_PVLS", "TX_CURR")) %>%
    count(standardizeddisaggregate, numeratordenom, indicator, use_for_age, wt = cumulative) %>%
    arrange(numeratordenom, indicator)
  
  df_vlc <- df_msd %>%
    filter(
      age_2019 %ni% c("Unknown Age", "Newer Age Band"),
      indicator %in% c("TX_CURR_Lag2", "TX_PVLS"),
      standardizeddisaggregate %in% c(
        "Age/Sex/HIVStatus",
        "Age/Sex/Indication/HIVStatus"
      )
    ) %>%
    gophr::clean_indicator() %>% # Affixes an N or D to TX_PVLS stub
    group_by(indicator, fiscal_year, sex, age_2019) %>%
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    reshape_msd(include_type = FALSE) %>%
    pivot_wider(
      names_from = indicator,
      names_glue = "{tolower(indicator)}"
    ) %>%
    mutate(
      vlc = tx_pvls_d / tx_curr_lag2,
      vls = tx_pvls / tx_pvls_d,
      sex_color = ifelse(sex == "Female", hw_lavender_haze, hw_hunter)
    )
  
  # Plot as dot plot --> flipped at last minute (coord_flip at bottom)
  # Create a label for 50+ male & female for plot
  df_vlc %>%
    filter(period == meta$curr_pd) %>%
    mutate(dot_labels = case_when(
      age_2019 == "50+" & sex == "Female" ~ "Female",
      age_2019 == "50+" & sex == "Male" ~ "Male",
      TRUE ~ NA_character_
    )) %>% 
    ggplot(aes(x = age_2019, y = vlc, color = sex_color, group = sex)) +
    geom_hline(yintercept = 1, color = grey10k, size = 4) +
    geom_line(aes(group = age_2019), color = grey70k, linewidth = 1) +
    geom_point(size = 5) +
    geom_point(size = 5, stroke = 0.9, shape = 1, color = "white") +
    geom_text(aes(label = dot_labels, color = ifelse(dot_labels == "Female", hw_lavender_haze, hw_hunter)), 
              size = 11/.pt,
              family = "Source Sans Pro",
              vjust = -1) +
    scale_y_continuous(
      limits = c(0, 1.1),
      labels = percent,
      breaks = seq(0, 1, 0.25)
    ) +
    scale_color_identity() +
    si_style() +
    labs(
      title = "ADULT MEN APPEAR TO HAVE BETTER VIRAL LOAD COVERAGE THAN ADULT WOMEN",
      caption = glue("{meta$caption}"),
      x = NULL,
      y = "Viral load coverage"
    ) +
    coord_flip()



# EXTRA -------------------------------------------------------------------


# Existing function to calculate VLS / VLC
# Function to calculate viral load S and C
# Creates a wide viral load coverage data frame for use in basic plots
  create_vl_df <- function(df, ...) {
    df <- df %>%
      filter(
        indicator %in% c("TX_CURR", "TX_PVLS"),
        standardizeddisaggregate %in% c(
          "Age/Sex/HIVStatus",
          "Age/Sex/Indication/HIVStatus"
        )
      ) %>%
      gophr::clean_indicator() %>%
      group_by(indicator, fiscal_year, ...) %>%
      summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      reshape_msd(include_type = FALSE) %>%
      pivot_wider(
        names_from = indicator,
        names_glue = "{tolower(indicator)}"
      ) %>%
      group_by(...) %>%
      mutate(
        tx_curr_lag2 = lag(tx_curr, n = 2),
        vlc = tx_pvls_d / tx_curr_lag2,
        vls = tx_pvls / tx_pvls_d,
        vls_adj = tx_pvls / tx_curr_lag2
      ) %>%
      ungroup()
    return(df)
  }

