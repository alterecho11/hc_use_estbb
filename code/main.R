# users is df (can have multiple rows per individual) with column names id, yob, yod, age_join, 
  # date1 (start of HC usage period), date2 (end of HC usage period), hc_ATC, days (date2 - date1),
    # closest_BMI, gtStatusFVL (binary status for carrying FVL variant), gtStatusPTM (binary status for carrying PTM variant)
# core.js was used to create users df from purchases df

users$purchase_age <- as.integer(format(as.Date(users$date1), "%Y")) - users$yob

users <- users %>%
  arrange(id, date1)

# censor with pregnancy codes; df_diagnoses is df (can have multiple rows per individual) with column names id, icd10_code, diag_date, diag_first_date
matched_data <- users %>%
  inner_join(
    # filter diagnoses for O pregnancy codes first
    df_diagnoses %>% dplyr::filter(grepl("^O[0-9]{2}", icd10_code) & 
                                         !(substr(icd10_code, 1, 3) %in% c("O85", "O86", "O87", "O88", "O89", "O90", "O91", "O92"))),
    by = "id", relationship = "many-to-many") %>%
  # filter for dates where O diagnosis falls within range of hc usage period
  dplyr::filter(diag_date > date1 & diag_date <= date2) %>%
  group_by(id, date1, date2) %>%
  summarise(earliest_relevant_date = min(diag_date, na.rm = TRUE), .groups = "drop") %>%
  # calculate new date12
  mutate(new_date12 = earliest_relevant_date)

nrow(matched_data)

users_fixed <- users %>%
  left_join(matched_data %>% select(id, date1, date2, new_date12),
    by = c("id", "date1", "date2")) %>%
  mutate(date2 = if_else(!is.na(new_date12), new_date12, date2)) %>%
  mutate(days = round(as.numeric(difftime(date2, date1, units = "days"))), days) %>%
  select(-new_date12)  # Remove the temporary column

# IMPORTANT: Always sort, throughout the whole code!
# Some functions (e.g. summarise, join) may reorder rows,
# which can break downstream logic that sometimes assumes ordering by id and date1.

users <- users_fixed %>%
  arrange(id, date1)

# info for table 1 ----
users %>%
  group_by(id) %>%
  slice_min(date1) %>%
  ungroup() %>%
  summarise(
    mean(age_join, na.rm = T),
    sd(age_join, na.rm = T),
    median(age_join, na.rm = T),
    q25 = quantile(age_join, 0.25, na.rm = T),
    q75 = quantile(age_join, 0.75, na.rm = T),
    below_30 = mean(age_join < 30, na.rm = T))

users %>%
  group_by(id) %>%
  summarise(
    first_purchase = min(date1),
    year_of_death = first(yod)) %>%
  mutate(
    study_end = as.Date("2022-12-31"),
    death_date = if_else(!is.na(year_of_death),
                         as.Date(ISOdate(year_of_death, 01, 01)),
                         study_end),
    end_date = pmin(study_end, death_date)) %>%
  summarise(
    n_people = n(),
    total_person_years = sum(as.numeric(difftime(end_date, first_purchase, units="days"))/365.25),
    mean_follow_up = mean(as.numeric(difftime(end_date, first_purchase, units="days"))/365.25)) %>%
  ungroup()

users %>%
  group_by(id) %>%
  slice_min(date1) %>%
  ungroup() %>%
  summarise(
    mean_purchase_age = mean(purchase_age, na.rm = TRUE),
    sd_purchase_age = sd(purchase_age, na.rm = TRUE),
    median_purchase_age = median(purchase_age, na.rm = TRUE),
    q25 = quantile(purchase_age, 0.25, na.rm = T),
    q75 = quantile(purchase_age, 0.75, na.rm = T),
    mode_purchase_age = as.numeric(names(sort(table(round(purchase_age)), decreasing = TRUE)[1])),
    n_at_mode = max(table(round(purchase_age))),
    total_people = n())

users %>%
  group_by(id) %>%
  slice_min(date1) %>%
  ungroup() %>%
  summarise(
    mean_bmi = mean(closest_BMI, na.rm = TRUE),
    sd_bmi = sd(closest_BMI, na.rm = TRUE),
    median_bmi = median(closest_BMI, na.rm = TRUE),
    q25_bmi = quantile(closest_BMI, 0.25, na.rm = TRUE),
    q75_bmi = quantile(closest_BMI, 0.75, na.rm = TRUE),
    # counts
    underweight_count = sum(closest_BMI < 18.5, na.rm = TRUE),
    healthy_count = sum(closest_BMI >= 18.5 & closest_BMI < 25, na.rm = TRUE),
    overweight_count = sum(closest_BMI >= 25 & closest_BMI < 30, na.rm = TRUE),
    obese_count = sum(closest_BMI >= 30, na.rm = TRUE),
    # percentages
    underweight_pct = mean(closest_BMI < 18.5, na.rm = TRUE) * 100,
    healthy_pct = mean(closest_BMI >= 18.5 & closest_BMI < 25, na.rm = TRUE) * 100,
    overweight_pct = mean(closest_BMI >= 25 & closest_BMI < 30, na.rm = TRUE) * 100,
    obese_pct = mean(closest_BMI >= 30, na.rm = TRUE) * 100) %>%
  mutate(across(contains("_pct"), ~round(., 1)))

# info for table 2 ----
intrauterine <- c("G02BA03")
intravaginal <- c("G02BB01")
oral <- c("G03AA07", "G03AA09", "G03AA10", "G03AA11",
          "G03AA12", "G03AA14", "G03AA15", "G03AA16", 
          "G03AB03", "G03AB05", "G03AB06", "G03AB08",
          "G03AC03", "G03AC09", "G03AC10", "G03HB01")
transdermal <- c("G03AA13", "G03AA10p")
subdermal <- c("G03AC08")

chc <- c("G02BB01", "G03AA07", "G03AA09", "G03AA10", "G03AA10p", "G03AA11", 
         "G03AA12", "G03AA13", "G03AA14", "G03AA15", "G03AA16", "G03AB03", 
         "G03AB05", "G03AB06", "G03AB08", "G03HB01")
poc <- c("G02BA03", "G03AC03", "G03AC08", "G03AC09", "G03AC10")

short_acting <- c("G03AA07", "G03AA09", "G03AA10", "G03AA10p", "G03AA11", "G03AA12", 
                  "G03AA14", "G03AA15", "G03AA16", "G03AB03", "G03AB05", "G03AB06", 
                  "G03AB08", "G03AC03", "G03AC10", "G03AC09", "G03HB01", "G03AA13", 
                  "G02BB01")
long_acting <- c("G02BA03", "G03AC08")

# categorisation of oral hcs based on the pharmacological properties and progestin generations
anti_androgenic <- c("G03HB01", "G03AA15")
second_generation <- c("G03AA07", "G03AB03") 
third_generation <- c("G03AA09", "G03AA10", "G03AA11", "G03AB05", "G03AB06") # norelgestromin is a metabolite of norgestimate that is why we put it in the third gen
fourth_generation <- c("G03AA12", "G03AA16")
estradiol_preparation <- c("G03AA14", "G03AB08")
pop <- c("G03AC03", "G03AC09", "G03AC10")

# hc type
get_random_grouping <- function(code)  {
  if (code %in% subdermal) {
    return("Implant")
  } else if (code %in% anti_androgenic) {
    return("Anti-androgenic COC")
  } else if (code %in% estradiol_preparation) {
    return("Estradiol COC")
  } else if (code %in% second_generation) {
    return("COC 2nd generation")
  } else if (code %in% third_generation) {
    return("COC 3rd generation")
  } else if (code %in% fourth_generation) {
    return("COC 4th generation")
  } else if (code %in% intrauterine) {
    return("IUD with progestogen")
  } else if (code %in% pop) {
    return("Progestin-only pill")
  } else if (code %in% transdermal) {
    return("CHC patch")
  } else if (code %in% intravaginal) {
    return("CHC ring")
  } else {
    return(NA) # failsafe
  }
}

get_route_of_administration <- function(code) {
  if (code %in% oral){
    return("oral")
  } else if (code %in% subdermal) {
    return("subdermal")
  } else if (code %in% transdermal) {
    return("transdermal")
  } else if (code %in% intrauterine) {
    return("intrauterine")
  } else if (code %in% intravaginal) {
    return("intravaginal")
  } else {
    return(NA) # failsafe
  }
}

get_hormonal_composition <- function(code) {
  if (code %in% chc) {
    return("progestin + estrogen")
  } else if (code %in% poc) {
    return("progestin")
  } else {
    return(NA) # failsafe
  }
}

get_duration_of_action <- function(code) {
  if (code %in% short_acting) {
    return("short acting")
  } else if (code %in% long_acting) {
    return("long acting")
  } else {
    return(NA) # failsafe
  }
}

# purchases is df (can have multiple rows per individual) with column names id, yob, purchase_date, hc_ATC, hc_ATC_name, dosage and package_contents
purchases$purchase_age <- as.integer(format(as.Date(purchases$purchase_date), "%Y")) - purchases$yob

purchases <- purchases %>%
  mutate(
    random_grouping = sapply(hc_ATC, get_random_grouping),
    route_of_administration = sapply(get_route_of_administration, hc_ATC),
    hormonal_composition = sapply(hc_ATC, get_hormonal_composition),
    duration_of_action = sapply(hc_ATC, get_duration_of_action))

purchases <- purchases %>%
  mutate(hc_name = recode(hc_ATC,
                                "G02BA03" = "LNG IUD",  
                                "G02BB01" = "CHC RING",  
                                "G03AA07" = "LNG/EE",  
                                "G03AA09" = "DSG/EE",  
                                "G03AA10" = "GSD/EE",
                                "G03AA10p" = "GSD/EE(p)",
                                "G03AA11" = "NGM/EE",  
                                "G03AA12" = "DRSP/EE",  
                                "G03AA13" = "NGMN/EE",  
                                "G03AA14" = "NOMAC/E2",  
                                "G03AA15" = "CMA/EE",  
                                "G03AA16" = "DNG/EE",  
                                "G03AB03" = "LNG/EE(s)",  
                                "G03AB05" = "DSG/EE(s)",  
                                "G03AB06" = "GSD/EE(s)",  
                                "G03AB08" = "DNG/E2V",  
                                "G03AC03" = "LNG",  
                                "G03AC08" = "ENG",  
                                "G03AC09" = "DSG",
                                "G03AC10" = "DRSP",
                                "G03HB01" = "CPA/EE"))

purchases_table2 <- purchases %>%
  group_by(hc_ATC, hc_name, hc_ATC_name, hormonal_composition, route_of_administration, duration_of_action, random_grouping) %>%
  summarise(n_individuals = length(unique(id)), n_purchases=n()) %>%
  arrange(desc(n_individuals)) %>%
  ungroup() %>%
  rename(`HC ATC Code` = hc_ATC,
         `HC Abbreviation Name` = hc_name,
         `HC Formulation Name` = hc_ATC_name,
         `Main Hormonal Components` = hormonal_composition,
         `Route of Administration` = route_of_administration,
         `Duration of Action` = duration_of_action,
         `HC Type` = random_grouping,
         `Number of Individuals` = n_individuals,
         `Number of Purchases` = n_purchases)

formattable(purchases_table2, align = c("c", "c", "c", "c", "c"))

# figure 2 ----
{
  ## figure 2a ----
  # study_sample is df (one row per individual) with column names id, year of birth (yob), year of death (yod)
  calculate_ages <- function(df, start_year = 2004, end_year = 2022) {
    years_df <- data.frame(year = start_year:end_year)
    result <- merge(df, years_df, by = NULL)
    
    # calculate age for each person-year combination
    result$age <- result$year - result$yob
    
    # only include records where the year is less than or equal to YoD (if YoD exists)
    result <- result[is.na(result$yod) | result$year <= result$yod, ]
    
    # create age groups
    result$age_group <- cut(result$age, 
                            breaks = c(-Inf, 15, 20, 30, 40, 50, Inf),
                            labels = c("0-14", "15-19", "20-29", "30-39", "40-49", "50-55", "56+"),
                            right = FALSE)
    
    # filter results to 15-55
    result <- result %>% 
      filter(age >= 15 & age < 56)
    
    return(result)
  }
  
  ages_by_year <- calculate_ages(study_sample)
  
  # verify there is no 0-14 and 56+
  with(ages_by_year, table(year, age_group))
  
  age_group_percentages <- ages_by_year %>%
    group_by(year, age_group) %>%
    summarise(count = n(), .groups = "drop_last") %>%
    group_by(year) %>%
    mutate(percentage = count / sum(count) * 100)
  
  age_group_percentages$age_group <- factor(age_group_percentages$age_group, 
                                            levels = c("50-55", "40-49", "30-39", "20-29", "15-19"))
  
  ggplot(age_group_percentages, aes(x = factor(year), y = percentage, fill = age_group)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Year",
         y = "Population (%)",
         fill = "Age Group") +
    theme_classic() +
    theme(text = element_text(family = "Helvetica", size = 10),
          plot.title = element_text(family = "Helvetica", size = 12, face = "bold"),
          axis.title = element_text(family = "Helvetica", size = 10),
          axis.text = element_text(family = "Helvetica", size = 8),
          legend.title = element_text(family = "Helvetica", size = 10),
          legend.text = element_text(family = "Helvetica", size = 8)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_brewer(palette = "Set2")
  
  age_group_percentages_complete <- age_group_percentages %>%
    ungroup() %>%
    tidyr::complete(year, age_group, fill = list(count = 0, percentage = 0))
  
  age_group_percentages_complete$age_group <- factor(
    age_group_percentages_complete$age_group, 
    levels = c("50-55", "40-49", "30-39", "20-29", "15-19"))
  
  plot1 <- ggplot(age_group_percentages_complete, aes(x = year, y = percentage, fill = age_group, group = age_group)) +
    geom_area(position = "stack") +
    labs(x = "Year",
         y = "Population (%)",
         fill = "Age Group") +
    scale_fill_brewer(palette = "Set2", labels = c("50–55", "40–49", "30–39", "20–29", "15–19")) +
    theme_classic() +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(
      text = element_text(family = "Helvetica"),
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100.1)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2004, 2022, by = 2))
  
  ## figure 2b ----
  users <- users %>%
    mutate(
      random_grouping = sapply(hc_ATC, get_random_grouping),
      route_of_administration = sapply(get_route_of_administration, hc_ATC),
      hormonal_composition = sapply(hc_ATC, get_hormonal_composition),
      duration_of_action = sapply(hc_ATC, get_duration_of_action))
  
  years_expanded <- users %>%
    rowwise() %>%
    mutate(years = list(lubridate::year(date1):lubridate::year(date2))) %>%
    ungroup() %>%
    unnest(years)
  
  years_expanded <- years_expanded %>%
    mutate(adjusted_age = years - YoB) %>%
    mutate(age_group = case_when(
      adjusted_age <= 19 ~ "15-19",
      adjusted_age >= 20 & adjusted_age <= 29 ~ "20-29",
      adjusted_age >= 30 & adjusted_age <= 39 ~ "30-39",
      adjusted_age >= 40 & adjusted_age <= 49 ~ "40-49",
      adjusted_age >= 50 ~ "50-55"))
  
  distribution_of_hc_users_per_year <- years_expanded %>%
    distinct(id, age_group, hormonal_composition, years) %>%
    group_by(hormonal_composition, age_group, years) %>%
    summarise(n = n()) %>%
    rename(year = years) %>%
    ungroup()
  
  estbb_rates <- distribution_of_hc_users_per_year %>%
    left_join(age_group_percentages, by = c("year", "age_group")) %>%
    select(-percentage) %>%
    mutate(prevalence_rate = (n / count)*100) %>%
    select(-n, -count)
  
  # Kurvits et al (2021) (PMID: 34160334; DOI: 10.1080/13625187.2021.1931839)
  custom_df_A <- tribble(
    ~year, ~age_group, ~value, ~type,
    # 2005
    2005,  "15-19",    0.4,       "progestin",
    2005,  "20-29",    3.6,       "progestin",
    2005,  "30-39",    3.2,       "progestin",
    2005,  "40-49",    1.5,       "progestin",
    
    # 2006 
    2006,  "15-19",    0.5,       "progestin",
    2006,  "20-29",    4.4,       "progestin",
    2006,  "30-39",    4.7,       "progestin",
    2006,  "40-49",    2.2,       "progestin",
    
    # 2007 
    2007,  "15-19",    0.4,       "progestin",
    2007,  "20-29",    4.4,       "progestin",
    2007,  "30-39",    5.6,       "progestin",
    2007,  "40-49",    2.9,       "progestin",
    
    # 2008
    2008,  "15-19",    0.5,       "progestin",
    2008,  "20-29",    5.1,       "progestin",
    2008,  "30-39",    7.2,       "progestin",
    2008,  "40-49",    4.3,       "progestin",
    
    # 2009
    2009,  "15-19",    0.5,       "progestin",
    2009,  "20-29",    5.2,       "progestin",
    2009,  "30-39",    8.2,       "progestin",
    2009,  "40-49",    5.4,       "progestin",
    
    # 2010
    2010,  "15-19",    0.4,       "progestin",
    2010,  "20-29",    5.2,       "progestin",
    2010,  "30-39",    8.8,       "progestin",
    2010,  "40-49",    6.3,       "progestin",
    
    # 2011
    2011,  "15-19",    0.4,       "progestin",
    2011,  "20-29",    4.9,       "progestin",
    2011,  "30-39",    9.4,       "progestin",
    2011,  "40-49",    7.2,       "progestin",
    
    # 2012
    2012,  "15-19",    0.5,       "progestin",
    2012,  "20-29",    4.7,       "progestin",
    2012,  "30-39",    9.7,       "progestin",
    2012,  "40-49",    7.6,       "progestin",
    
    # 2013
    2013,  "15-19",    0.6,       "progestin",
    2013,  "20-29",    5.0,       "progestin",
    2013,  "30-39",    10.5,      "progestin",
    2013,  "40-49",    8.8,       "progestin",
    
    # 2014
    2014,  "15-19",    0.8,       "progestin",
    2014,  "20-29",    5.7,       "progestin",
    2014,  "30-39",    11.6,      "progestin",
    2014,  "40-49",    10.3,      "progestin",
    
    # 2015
    2015,  "15-19",    1.4,       "progestin",
    2015,  "20-29",    7.1,       "progestin",
    2015,  "30-39",    13.4,      "progestin",
    2015,  "40-49",    12.2,      "progestin",
    
    # 2016
    2016,  "15-19",    1.7,       "progestin",
    2016,  "20-29",    8.1,       "progestin",
    2016,  "30-39",    14.9,      "progestin",
    2016,  "40-49",    14.0,      "progestin",
    
    # 2017
    2017,  "15-19",    1.8,       "progestin",
    2017,  "20-29",    8.7,       "progestin",
    2017,  "30-39",    15.7,      "progestin",
    2017,  "40-49",    15.4,      "progestin",
    
    # 2018
    2018,  "15-19",    2.0,       "progestin",
    2018,  "20-29",    9.3,       "progestin",
    2018,  "30-39",    15.8,      "progestin",
    2018,  "40-49",    16.2,      "progestin",
    
    # 2019
    2019,  "15-19",    2.5,       "progestin",
    2019,  "20-29",    9.9,       "progestin",
    2019,  "30-39",    16.2,      "progestin",
    2019,  "40-49",    17.3,      "progestin")
  
  custom_df_B <- tribble(
    ~year, ~age_group, ~value, ~type,
    # 2005
    2005,  "15-19",    13.7,      "progestin + estrogen",
    2005,  "20-29",    31.1,      "progestin + estrogen",
    2005,  "30-39",    16.5,      "progestin + estrogen",
    2005,  "40-49",    6.3,       "progestin + estrogen",
    
    # 2006
    2006,  "15-19",    15,        "progestin + estrogen",
    2006,  "20-29",    34.3,      "progestin + estrogen",
    2006,  "30-39",    18.9,      "progestin + estrogen",
    2006,  "40-49",    7.5,       "progestin + estrogen",
    
    # 2007
    2007,  "15-19",    16.2,      "progestin + estrogen",
    2007,  "20-29",    35.3,      "progestin + estrogen",
    2007,  "30-39",    20.1,      "progestin + estrogen",
    2007,  "40-49",    8.1,       "progestin + estrogen",
    
    # 2008
    2008,  "15-19",    16.6,      "progestin + estrogen",
    2008,  "20-29",    35.7,      "progestin + estrogen",
    2008,  "30-39",    20.6,      "progestin + estrogen",
    2008,  "40-49",    8.5,       "progestin + estrogen",
    
    # 2009
    2009,  "15-19",    15.3,      "progestin + estrogen",
    2009,  "20-29",    34.6,      "progestin + estrogen",
    2009,  "30-39",    19.9,      "progestin + estrogen",
    2009,  "40-49",    8.4,       "progestin + estrogen",
    
    # 2010
    2010,  "15-19",    14.2,      "progestin + estrogen",
    2010,  "20-29",    33.2,      "progestin + estrogen",
    2010,  "30-39",    19.3,      "progestin + estrogen",
    2010,  "40-49",    8.4,       "progestin + estrogen",
    
    # 2011
    2011,  "15-19",    15.3,      "progestin + estrogen",
    2011,  "20-29",    34.0,      "progestin + estrogen",
    2011,  "30-39",    20.3,      "progestin + estrogen",
    2011,  "40-49",    8.8,       "progestin + estrogen",
    
    # 2012
    2012,  "15-19",    15.7,      "progestin + estrogen",
    2012,  "20-29",    34.3,      "progestin + estrogen",
    2012,  "30-39",    20.7,      "progestin + estrogen",
    2012,  "40-49",    9.2,       "progestin + estrogen",
    
    # 2013
    2013,  "15-19",    16.1,      "progestin + estrogen",
    2013,  "20-29",    33.1,      "progestin + estrogen",
    2013,  "30-39",    20.2,      "progestin + estrogen",
    2013,  "40-49",    9.1,       "progestin + estrogen",
    
    # 2014
    2014,  "15-19",    16.0,      "progestin + estrogen",
    2014,  "20-29",    31.3,      "progestin + estrogen",
    2014,  "30-39",    19.1,      "progestin + estrogen",
    2014,  "40-49",    8.5,       "progestin + estrogen",
    
    # 2015
    2015,  "15-19",    15.7,      "progestin + estrogen",
    2015,  "20-29",    30.7,      "progestin + estrogen",
    2015,  "30-39",    18.2,      "progestin + estrogen",
    2015,  "40-49",    8.3,       "progestin + estrogen",
    
    # 2016
    2016,  "15-19",    15.2,      "progestin + estrogen",
    2016,  "20-29",    29.3,      "progestin + estrogen",
    2016,  "30-39",    17.6,      "progestin + estrogen",
    2016,  "40-49",    8.4,       "progestin + estrogen",
    
    # 2017
    2017,  "15-19",    15.1,      "progestin + estrogen",
    2017,  "20-29",    27.7,      "progestin + estrogen",
    2017,  "30-39",    17.3,      "progestin + estrogen",
    2017,  "40-49",    8.5,       "progestin + estrogen",
    
    # 2018
    2018,  "15-19",    15.4,      "progestin + estrogen",
    2018,  "20-29",    26.3,      "progestin + estrogen",
    2018,  "30-39",    16.8,      "progestin + estrogen",
    2018,  "40-49",    8.5,       "progestin + estrogen",
    
    # 2019
    2019,  "15-19",    15.2,      "progestin + estrogen",
    2019,  "20-29",    25.5,      "progestin + estrogen",
    2019,  "30-39",    16.3,      "progestin + estrogen",
    2019,  "40-49",    8.5,       "progestin + estrogen")
  
  kurvits_rates <- rbind(custom_df_A, custom_df_B)
  
  kurvits_rates <- kurvits_rates %>%
    rename(prevalence_rate = value,
           hormonal_composition = type)
  
  estbb_rates$source <- "estbb"
  kurvits_rates$source <- "kurvits"
  
  combo <- rbind(estbb_rates, kurvits_rates)
  
  combo <- combo %>%
    select(hormonal_composition, age_group, year, source, prevalence_rate) %>%
    arrange(hormonal_composition, age_group, year, source)
  
  plot2 <- ggplot(combo, aes(x = as.numeric(year), 
                             y = prevalence_rate, 
                             color = age_group,
                             alpha = source,
                             shape = source)) +
    geom_point(size = 3) +
    geom_line() +
    scale_x_continuous(breaks = seq(2004, 2022, by = 2)) +
    scale_color_manual(
      values = c(
        "15-19" = "#A6D75B", 
        "20-29" = "#E68BBC",
        "30-39" = "#8DA9D6",
        "40-49" = "#F4A582",
        "50-55" = "#8DD3C7"), labels = c("15–19", "20–29", "30–39", "40–49", "50–55")) +
    scale_alpha_manual(values = c("estbb" = 1.0, "kurvits" = 0.5)) +
    scale_shape_manual(name = "Source", values = c("estbb" = 20, "kurvits" = 18), labels = c("estbb" = "EstBB", "kurvits" = "Kurvits et al")) +
    facet_wrap(~hormonal_composition, scales = "free", 
               labeller = as_labeller(c("progestin" = "Progestin-only Contraceptives", "progestin + estrogen" = "Combined Hormonal Contraceptives"))) +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.position = "right",
          panel.spacing = unit(1, "lines"),
          strip.background = element_rect(fill = "lightgray"),
          strip.text = element_text(face = "bold")) +
    labs(x = "Year", y = "Annual Prevalence Rate (%)") +
    guides(alpha = "none",
           color = guide_legend(override.aes = list(
             alpha = 1, linetype = 0, shape = 15, size = 5), title = "Age Group"),
           linetype = guide_legend(override.aes = list(shape = NA)))
  
  ## figure 2c ----
  custom_palette_1 <- c(
    "#1565C0",
    "#74B9FF",
    "#6C3483",
    "#D98EF0",
    "#F2BAC9",
    "#F76F8E",
    "#FABC2A",
    "#FFCAB1",
    "#F38B68",
    "#7FD8BE")
  
  prop_fig <- users %>%
    mutate(date1 = as.Date(date1), date2 = as.Date(date2)) %>%
    rowwise() %>%
    mutate(year_seq = list(seq(year(date1), year(date2)))) %>%
    unnest(year_seq) %>%
    rename(years = year_seq) %>%
    mutate(
      seg_start  = pmax(date1, as.Date(paste0(years, "-01-01"))),
      seg_end    = pmin(date2, as.Date(paste0(years, "-12-31"))),
      seg_days   = as.numeric(seg_end - seg_start),
      age_at_seg = year(seg_start) - YoB,
      age_group  = case_when(
        age_at_seg >= 15 & age_at_seg < 20 ~ "15–19",
        age_at_seg >= 20 & age_at_seg < 30 ~ "20–29",
        age_at_seg >= 30 & age_at_seg < 40 ~ "30–39",
        age_at_seg >= 40 & age_at_seg < 50 ~ "40–49",
        age_at_seg >= 50 & age_at_seg <= 55 ~ "50–55",
        TRUE ~ NA_character_)) %>%
    ungroup() %>%
    filter(!is.na(age_group))
  
  prop_fig_summary <- prop_fig %>%
    group_by(years, age_group) %>%
    mutate(total_days_all_hc = sum(seg_days, na.rm = TRUE)) %>%
    group_by(years, age_group, random_grouping) %>%
    summarise(
      total_days_hc = sum(seg_days, na.rm = TRUE),
      total_days_all_hc = first(total_days_all_hc),
      pct = ifelse(total_days_all_hc == 0, NA, total_days_hc / total_days_all_hc * 100),
      .groups = "drop")
  
  prop_fig_summary <- prop_fig_summary %>%
    mutate(random_grouping = factor(random_grouping, levels = c("IUD with progestogen", "Progestin-only pill",
                                                                "Anti-androgenic COC", "COC 2nd generation",
                                                                "COC 3rd generation", "COC 4th generation",
                                                                "Estradiol COC", "CHC ring", "CHC patch",
                                                                "Implant")))
  
  prop_fig_summary <- prop_fig_summary %>%
    mutate(age_group = factor(age_group, levels = c("15–19", "20–29", "30–39", "40–49", "50–55")))
  
  plot3 <- ggplot(prop_fig_summary, aes(x = years, y = pct, fill = random_grouping)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = custom_palette_1) +
    scale_x_continuous(breaks = seq(2004, 2022, by = 2)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_wrap(~age_group, scales = "free_x", nrow = 1, strip.position = "top") +
    labs(fill = "HC Type", x = "Year", y = "Total Covered Days (%)") +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          strip.background = element_rect(fill = "lightgray"),
          strip.text = element_text(face = "bold"))
  
  ## figure 2 final ----
  plot1 <- plot1 + theme(aspect.ratio = 1)
  
  combined_plot <- ((plot1 + plot2)/(plot3)) + plot_layout(widths = c(1, 2)) + plot_annotation(
    tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', family = "Helvetica"))
  
}

# initiators ----
{
  first_purchases <- users %>%
    group_by(id) %>%
    slice_min(date1) %>%
    mutate(bmi_category = case_when(
      closest_BMI < 18.5 ~ "Underweight",
      closest_BMI >= 18.5 & closest_BMI < 25 ~ "Healthy Weight",
      closest_BMI >= 25 & closest_BMI < 30 ~ "Overweight",
      closest_BMI >= 30 ~ "Obesity",
      TRUE ~ NA_character_))
  
  first_purchases$purchase_year <- as.numeric(format(as.Date(first_purchases$date1), "%Y"))
  
  ## figure 3 ----
  first_purchases <- first_purchases %>%
    mutate(hc_ATC_name = recode(hc_ATC,
                                "G02BA03" = "LNG IUD",  
                                "G02BB01" = "CHC RING",  
                                "G03AA07" = "LNG/EE",  
                                "G03AA09" = "DSG/EE",  
                                "G03AA10" = "GSD/EE",
                                "G03AA10p" = "GSD/EE(p)",
                                "G03AA11" = "NGM/EE",  
                                "G03AA12" = "DRSP/EE",  
                                "G03AA13" = "NGMN/EE",  
                                "G03AA14" = "NOMAC/E2",  
                                "G03AA15" = "CMA/EE",  
                                "G03AA16" = "DNG/EE",  
                                "G03AB03" = "LNG/EE(s)",  
                                "G03AB05" = "DSG/EE(s)",  
                                "G03AB06" = "GSD/EE(s)",  
                                "G03AB08" = "DNG/E2V",  
                                "G03AC03" = "LNG",  
                                "G03AC08" = "ENG",  
                                "G03AC09" = "DSG",
                                "G03AC10" = "DRSP",
                                "G03HB01" = "CPA/EE"))
  
  drug_colors_names <- c(
    "LNG IUD" = "#1565C0",
    
    "LNG" = "#74B9FF",  
    "DSG" = "#74B9FF",
    "DRSP" = "#74B9FF",
    
    "CPA/EE" = "#6C3483",
    "CMA/EE" = "#6C3483",
    
    "LNG/EE" = "#D98EF0",
    "LNG/EE(s)" = "#D98EF0",
    
    "DSG/EE" = "#F2BAC9",
    "DSG/EE(s)" = "#F2BAC9", 
    "GSD/EE" = "#F2BAC9",
    "GSD/EE(s)" = "#F2BAC9", 
    "NGM/EE" = "#F2BAC9",
    
    "DRSP/EE" = "#F76F8E",  
    "DNG/EE" = "#F76F8E",  
    
    "DNG/E2V" = "#FABC2A",
    "NOMAC/E2" = "#FABC2A",
    
    "CHC RING" = "#FFCAB1",  
    
    "NGMN/EE" = "#F38B68",
    "GSD/EE(p)" = "#F38B68",
    
    "ENG" = "#7FD8BE")
  
  drug_colors_ordered <- drug_colors_names[match(levels(first_purchases$hc_ATC_name), names(drug_colors_names))]
  
  first_purchases$hc_ATC_name <- factor(first_purchases$hc_ATC_name, levels = rev(c(
    "LNG IUD",      # IUD
    "LNG",          # Progestin-only
    "DSG",
    "DRSP",
    "CPA/EE",       # Anti-androgenic
    "CMA/EE",
    "LNG/EE",       # Second generation
    "LNG/EE(s)",
    "DSG/EE",       # Third generation
    "DSG/EE(s)",
    "GSD/EE", 
    "GSD/EE(s)",
    "NGM/EE",
    "DRSP/EE",      # Fourth generation
    "DNG/EE",
    "NOMAC/E2",     # Estradiol COC
    "DNG/E2V",
    "CHC RING",     # Ring
    "GSD/EE(p)",
    "NGMN/EE",      # Patch
    "ENG"           # Implant
  )))
  
  plot4 <- ggplot(first_purchases, aes(x = days, y = hc_ATC_name, fill = hc_ATC_name)) +
    geom_violin(width = 3, linewidth = 0.2) +
    geom_boxplot(width = 0.1, linewidth = 0.2, outlier.size = 0.2) +
    stat_summary(
      geom = "point", 
      fun = median,
      shape = 23, size = 1, color = "black", stroke = 0.4) +
    scale_fill_manual(values = drug_colors_names) +
    scale_x_log10() +
    guides(fill="none") +
    labs(x = "HC usage period in days (log scale)", y = "") +
    theme(text = element_text(family = "Helvetica", size = 10))+
    theme_classic()
  
  main_legend <- get_legend(plot3)
  plot_with_legend <- plot_grid(plot4, main_legend, rel_widths = c(3, 1))
  
  ## general stat ----
  prop.table(table(first_purchases$hormonal_composition, useNA="always"))*100
  prop.table(table(first_purchases$random_grouping, useNA="always"))*100
  
  first_purchases <- first_purchases %>%
    mutate(
      age_group = case_when(
        Person_purchaseAge >= 15 & Person_purchaseAge <= 19 ~ "15-19",
        Person_purchaseAge >= 20 & Person_purchaseAge <= 29 ~ "20-29",
        Person_purchaseAge >= 30 & Person_purchaseAge <= 39 ~ "30-39",
        Person_purchaseAge >= 40 & Person_purchaseAge <= 49 ~ "40-49",
        Person_purchaseAge >= 50 & Person_purchaseAge <= 55 ~ "50-55",
        TRUE ~ "Other"))
  
  prop.table(table(first_purchases$bmi_category, first_purchases$random_grouping, useNA = "always"), margin = 1) * 100
  first_per_year <- prop.table(table(first_purchases$purchase_year, first_purchases$random_grouping, useNA = "always"), margin = 1) * 100
  prop.table(table(first_purchases$age_group, first_purchases$random_grouping, useNA = "always"), margin = 1) * 100
  
  ## s4 appendix ----
  plot_initiators <- first_purchases %>%
    group_by(age_group, purchase_year, random_grouping) %>%
    summarise(people_count = n_distinct(id), .groups = "drop") %>%
    group_by(age_group, purchase_year) %>%
    mutate(total_people = sum(people_count),
           percentage = people_count / total_people * 100) %>%
    ungroup()
  
  plot_initiators <- plot_initiators %>%
    mutate(random_grouping = factor(random_grouping, levels = c("IUD with progestogen", "Progestin-only pill",
                                                                "Anti-androgenic COC", "COC 2nd generation",
                                                                "COC 3rd generation", "COC 4th generation",
                                                                "Estradiol COC", "CHC ring", "CHC patch",
                                                                "Implant")))
  
  plot_initiators <- plot_initiators[plot_initiators$age_group != "Other", ]
  
  supp_s4 <- ggplot(plot_initiators, aes(x = purchase_year, y = percentage, fill = random_grouping)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = custom_palette_1) +
    scale_x_continuous(breaks = seq(2004, 2022, by = 2)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_wrap(~age_group, scales = "free", ncol = 3) +
    labs(x = "Year",
         y = "First-ever initiators (%)",
         fill = "HC Type") +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          strip.background = element_rect(fill = "lightgray"),
          strip.text = element_text(face = "bold"))
  
  ## diagnoses 3m before ----
  # three_months_before_first is df (can have multiple rows per individual) with column names id, date1, hc_ATC, diagnosis_code (e.g. Axx, Axx.x, Diagnosis not found)
  three_months_before_first <- three_months_before_first %>%
    mutate(icd10_main = ifelse(
      startsWith(diagnosis_code, "E28"),
      diagnosis_code,
      sub("\\..*", "", diagnosis_code)))

  # step 1: get unique person-diagnosis combinations
  # (because we want to count each person only once per diagnosis)
  unique_person_diagnosis <- three_months_before_first %>%
    distinct(id, icd10_main)
  
  # step 2: count how many unique people have each diagnosis
  diagnosis_counts <- unique_person_diagnosis %>%
    group_by(icd10_main) %>%
    summarise(people_count = n()) 
  
  # step 3: calculate the total number of unique people
  total_people <- n_distinct(three_months_before_first$id)
  
  # step 4: calculate percentages and sort from highest to lowest
  diagnosis_percentages <- diagnosis_counts %>%
    mutate(percentage = (people_count / total_people) * 100) %>%
    arrange(desc(percentage))
  
  length(unique(unique_person_diagnosis$icd10_main))
  print(diagnosis_percentages, n=100)
  
}

# covid/s5 appendix ----
# a is df with column names year (2005-2021), month (1-12), month_name (Jan-Dec), purchases_count and total_packages
n=length(a$total_packages)
yearaverage=as.vector(by(a$total_packages, a$year, mean))
change=a$total_packages/rep(yearaverage, each=12)

plot_a <- wrap_elements(~ {
  plot(a$month, change, xaxt = "n", type = "n", xlab = "Month", ylab = "Change")
  ind=a$year!=2020
  meanvec=as.vector(by(change[ind], a$month[ind], mean))
  sdvec=as.vector(by(change[ind], a$month[ind], sd))
  abline(v = unique(a$month), col = "lightgrey", lwd = 1.5)
  lines(1:12, meanvec-3*sdvec, lwd=2, col="gray80")
  lines(1:12, meanvec+3*sdvec, lwd=2, col="gray80")
  lines(1:12, change[a$year==2020], col="red")
  lines(1:12, meanvec, lwd=3)
  points(a$month, change, col=c("gray80", "red")[1+1*(a$year=="2020")] , pch=20, cex=2)
  axis(1, at = a$month, labels = month.abb[a$month])
})

# b is df with column names year (2019-2021), month, month_name, purchases_count and total_packages
plot_b <- ggplot(b, aes(x = month_name, y = total_packages, group = factor(year), color = factor(year))) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "2019" = "lightblue",
    "2020" = "red",
    "2021" = "darkblue"), name = "Year") +
  labs(
    x = "Month",
    y = "Total monthly number of HC purchases") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        panel.grid.major.x = element_line(color = "lightgray", linewidth = 0.5, linetype = "solid"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

# c is df with column names year (2019-2021), day (1-31), purchases_count and total_packages
plot_c <- ggplot(c, aes(x = day, y = total_packages, group = factor(year), color = factor(year))) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "2019" = "lightblue",
    "2020" = "red",
    "2021" = "darkblue"), name = "Year") +
  scale_x_continuous(breaks = 1:31) +
  labs(
    x = "Day",
    y = "Total daily number of HC purchases in March") +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(color = "darkgray", linewidth = 0.3), axis.line = element_blank(),
    legend.position = "null",panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

# d is df with column names random_grouping, year (2019-2021), month_name (Jan-Dec), purchases_count, total_packages
plot_d <- ggplot(d, aes(x = month_name, y = total_packages, group = factor(year), color = factor(year))) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "2019" = "lightblue", 
    "2020" = "red",
    "2021" = "darkblue"), name = "Year") +
  labs(x = "Month",
       y = "Total number of HC purchases") +
  facet_wrap(~random_grouping, scales = "free", ncol = 2) +
  labs(
    x = "Month",
    y = "Total monthly number of HC purchases") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), axis.line = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "lightgray", linewidth = 0.5, linetype = "solid"),
        legend.position = "null", 
        strip.text = element_text(face = "bold"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

supp_figure <- plot_a/(plot_b+plot_d)/(plot_c)+
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', family = "Helvetica"))


# hc switching ----
{
  ## figure 4a ----
  variety <- users %>%
    group_by(id) %>%
    summarise(unique_types=n_distinct(hc_ATC)) %>%
    ungroup() %>%
    count(unique_types) %>%
    mutate(percentages = (n / 73071)*100)
  
  variety$pct <- round(variety$n / sum(variety$n)*100, 2)
  variety$types_factor <- factor(variety$unique_types)
  
  total_switches <- sum((as.numeric(variety$unique_types) - 1) * variety$n)
  total_people <- sum(variety$n)
  total_switches / total_people
  
  plot5 <- ggplot(variety, aes(x = reorder(types_factor, -n), y = n)) +
    geom_segment(aes(xend = reorder(types_factor, n), yend = 0, color = types_factor)) +
    geom_point(aes(color = types_factor), size = 4) +
    coord_flip() +
    scale_color_viridis_d(option = "viridis", begin = 0.3, end = 0.9) + 
    scale_y_continuous(limits=c(0,40000), labels = c(0,10000,20000,30000), breaks = c(0,10000,20000,30000)) + 
    theme_classic() +
    theme(legend.position = "none", text = element_text(family = "Helvetica", size = 10)) +
    geom_text(aes(label = paste0(comma(n), " (", round(n/sum(n)*100, 1), "%)")), 
              hjust = -0.2) +
    labs(x = "Number of different HC formulations",
         y = "Number of HC users")
  
  ## classify switchers
  classify_switches <- function(df) {
    switch_types <- df %>%
      arrange(id, date1) %>%
      group_by(id) %>%
      mutate(
        prev_drug         = lag(hc_ATC),
        prev_end_date     = lag(date2),
        prev_covered_days = lag(days),
        gap_days          = round(as.numeric(difftime(date1, prev_end_date, units = "days"))),
        is_switch         = !is.na(prev_drug) & hc_ATC != prev_drug,
        is_rapid_switch   = is_switch & gap_days <= 90 & prev_covered_days < 90
      ) %>%
      summarise(
        unique_drugs     = n_distinct(hc_ATC),
        total_rows       = n(),
        any_long_period  = any(days > 180, na.rm = TRUE),
        any_rapid_switch = isTRUE(any(is_rapid_switch, na.rm = TRUE)),
        only_short       = all(days < 90, na.rm = TRUE),
        switch_type = case_when(
          unique_drugs == 1 & any_long_period                ~ 0L,
          unique_drugs == 1 & total_rows == 1 & only_short   ~ 3L,
          unique_drugs > 1  & any_rapid_switch               ~ 2L,
          unique_drugs > 1                                   ~ 1L,
          TRUE                                               ~ NA_integer_
        )
      )
    
    # Then, get drug name for rapid switches
    rapid_switches <- df %>%
      arrange(id, date1) %>%
      group_by(id) %>%
      mutate(
        prev_drug = lag(hc_ATC),
        prev_start_date = lag(date1),
        prev_end_date = lag(date2),
        prev_covered_days = lag(days),
        gap_days = round(as.numeric(difftime(date1, prev_end_date, units = "days"))),
        new_covered_days = days,
        is_rapid_switch = !is.na(prev_drug) & 
          hc_ATC != prev_drug & 
          gap_days <= 90 & 
          prev_covered_days < 90) %>%
      filter(is_rapid_switch) %>%
      select(id, Switch_From = prev_drug, Switch_To = hc_ATC, prev_start_date, 
             switch_date = date1, prev_covered_days, gap_days, new_covered_days)
    
    # Return both datasets as a list
    return(list(
      patient_classifications = switch_types,
      rapid_switches = rapid_switches
    ))
  }
  
  switch_sorting_results <- classify_switches(users)
  patient_types <- switch_sorting_results$patient_classifications
  
  # 0 - non-switchers | 1 - broad switchers | 2 - rapid switchers | 3 - rapid discontinuers
  round(prop.table(table(patient_types$switch_type, useNA="always"))*100,1)
  
  non_switchers <- patient_types[patient_types$switch_type == 0 & !is.na(patient_types$switch_type),]
  rapid_switchers <- patient_types[patient_types$switch_type == 2 & !is.na(patient_types$switch_type),]
  rapid_discontinuers <- patient_types[patient_types$switch_type == 3 & !is.na(patient_types$switch_type), ]
  
  ## rapid switchers/figure 4b ----
  rapid_switches_detail <- switch_sorting_results$rapid_switches
  
  rapid_switches_detail <- rapid_switches_detail %>%
    mutate(random_grouping_from = sapply(Switch_From, get_random_grouping),
           random_grouping_to = sapply(Switch_To, get_random_grouping))
  
  rapid_switches_detail <- rapid_switches_detail %>%
    mutate(Switch_From = recode(Switch_From,
                                "G02BA03" = "LNG IUD",  
                                "G02BB01" = "CHC RING",  
                                "G03AA07" = "LNG/EE",  
                                "G03AA09" = "DSG/EE",  
                                "G03AA10" = "GSD/EE",
                                "G03AA10p" = "GSD/EE(p)",
                                "G03AA11" = "NGM/EE",  
                                "G03AA12" = "DRSP/EE",  
                                "G03AA13" = "NGMN/EE",  
                                "G03AA14" = "NOMAC/E2",  
                                "G03AA15" = "CMA/EE",  
                                "G03AA16" = "DNG/EE",  
                                "G03AB03" = "LNG/EE(s)",  
                                "G03AB05" = "DSG/EE(s)",  
                                "G03AB06" = "GSD/EE(s)",  
                                "G03AB08" = "DNG/E2V",  
                                "G03AC03" = "LNG",  
                                "G03AC08" = "ENG",  
                                "G03AC09" = "DSG",
                                "G03AC10" = "DRSP",
                                "G03HB01" = "CPA/EE"),
           Switch_To = recode(Switch_To,
                              "G02BA03" = "LNG IUD",  
                              "G02BB01" = "CHC RING",  
                              "G03AA07" = "LNG/EE",  
                              "G03AA09" = "DSG/EE",  
                              "G03AA10" = "GSD/EE",
                              "G03AA10p" = "GSD/EE(p)",
                              "G03AA11" = "NGM/EE",  
                              "G03AA12" = "DRSP/EE",  
                              "G03AA13" = "NGMN/EE",  
                              "G03AA14" = "NOMAC/E2",  
                              "G03AA15" = "CMA/EE",  
                              "G03AA16" = "DNG/EE",  
                              "G03AB03" = "LNG/EE(s)",  
                              "G03AB05" = "DSG/EE(s)",  
                              "G03AB06" = "GSD/EE(s)",  
                              "G03AB08" = "DNG/E2V",  
                              "G03AC03" = "LNG",  
                              "G03AC08" = "ENG",  
                              "G03AC09" = "DSG",
                              "G03AC10" = "DRSP",
                              "G03HB01" = "CPA/EE"))
  
  river_data <- rapid_switches_detail %>%
    group_by(Switch_From, Switch_To) %>%
    summarise(value = n(), .groups = 'drop')
  
  library(ggalluvial)
  
  river_lodes <- to_lodes_form(river_data, key = "axis", value = "stratum", axes = c("Switch_From", "Switch_To"))
  
  plot6 <- ggplot(river_lodes, aes(x = axis, stratum = stratum, alluvium = alluvium, y = value)) +
    geom_alluvium(aes(fill = stratum), alpha = 0.8) +
    geom_stratum(aes(fill = stratum)) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), min.y=150) +
    scale_x_discrete(expand = c(.1, .1), name = "", labels = c("Switch_From" = "Pre-Switch Formulation", "Switch_To" = "New Formulation")) +
    scale_y_continuous(name = "Number of rapid switch events") +
    scale_fill_manual(values = drug_colors_names, name = "HC ATC Code") +
    theme_classic() +
    theme(text = element_text(family = "Helvetica", size = 10)) +
    guides(fill = "none")
  
  plot_with_legend <- plot_grid(plot6, main_legend, rel_widths = c(3, 1))
  
  ## switches from/to text/figure 4c ----
  get_stuff <- function(code) {
    if (code %in% oral && code %in% chc) {
      return("COC")
    } else if (code %in% oral && code %in% pop){
      return("oral POC")
    } else if (code %in% subdermal) {
      return("subdermal")
    } else if (code %in% transdermal) {
      return("CHC patch")
    } else if (code %in% intrauterine) {
      return("IUD")
    } else if (code %in% intravaginal) {
      return("CHC ring")
    } else {
      return(NA)
    }
  }
  
  rapid_switches_detail <- rapid_switches_detail %>%
    mutate(switch_from_group = sapply(Switch_From, get_stuff),
           switch_to_group = sapply(Switch_To, get_stuff))
  
  cocs <- rapid_switches_detail %>%
    filter(switch_from_group == "COC" & switch_to_group == "COC")
  
  rings <- rapid_switches_detail %>%
    filter(switch_from_group == "COC" & switch_to_group == "CHC ring")
  
  patches <- rapid_switches_detail %>%
    filter(switch_from_group == "COC" & switch_to_group == "CHC patch")
  
  ## rapid switchers diagnoses/figure 4c
  rapid_switches_from <- rapid_switches_detail %>%
    select(id, Switch_From, prev_start_date, switch_date)
  
  rapid_switches_from_with_diagnoses <- rapid_switches_from %>%
    inner_join(df_diagnoses, by = "id", relationship = "many-to-many") %>%
    filter(diag_first_date > prev_start_date,
           diag_first_date <= switch_date) %>%
    group_by(id, Switch_From, prev_start_date, switch_date) %>%
    summarise(matching_codes = paste(icd10_code, collapse = ", "), .groups = "drop")
  
  # select diagnoses of interest and create df with column names random_grouping, icd10_code_main, count (number of individuals with diagnosis record per hc type), 
    # total_count (total number of individuals with diagnosis record across all hcs)
  top_diagnoses <- top_diagnoses %>%
    mutate(random_grouping = factor(random_grouping, levels = c("IUD with progestogen", 
                                                                "Progestin-only pill", 
                                                                "Anti-androgenic COC", 
                                                                "COC 2nd generation", 
                                                                "COC 3rd generation", 
                                                                "COC 4th generation",
                                                                "Estradiol COC",
                                                                "CHC ring", 
                                                                "CHC patch",
                                                                "Implant")))
  
  # these were our diagnoses of interest; these are NOT all hc associated potential side effects !!!
  diagnosis_names <- c(
    # O codes
    "O03" = "Spontaneous abortion",
    "O04" = "Medical abortion",
    "O05" = "Other abortion",
    
    # N codes
    "N80" = "Endometriosis",
    "N83" = "Ovarian cyst",
    "N84" = "Polyp of female genital tract",
    "N87" = "Cervical dysplasia",
    "N91" = "Absent/scanty menstruation",
    "N92" = "Excessive/frequent menstruation",
    "N93" = "Other abnormal uterine bleeding",
    "N94" = "Pain associated with menstrual cycle",
    
    # D codes
    "D25" = "Uterine leiomyoma",
    "D27" = "Benign ovarian tumor",
    "B37" = "Candidiasis",
    # E code
    "E28" = "Ovarian dysfunction",
    
    # F codes (mental health)
    "F32" = "Depressive episode",
    "F41" = "Other anxiety disorders",
    "F52" = "Sexual dysfunction",
    
    # G codes (neurological)
    "G43" = "Migraine",
    "G44" = "Other headache syndrome",
    
    # L code
    "L70" = "Acne",
    "L68" = "Hirsutism",
    
    # K code
    "K80" = "Cholelithiasis",
    
    # additional N codes
    "N30" = "Cystitis",
    "N64" = "Mastodynia",
    "N72" = "Cervicitis",
    "N76" = "Vaginal/vulvar inflammation",
    
    # R codes (symptoms related)
    "R10" = "Abdominal/pelvic pain",
    "R51" = "Headaches",
    "R53" = "Fatigue/malaise")
  
  plot7 <- ggplot(top_diagnoses, aes(x = reorder(icd10_code_main, total_count), y = count, fill = random_grouping)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "",
         y = "Number of HC users",
         fill = "HC Type") +
    scale_fill_manual(values = custom_palette_1) +
    scale_x_discrete(labels = diagnosis_names) +
    scale_y_continuous(expand = c(0,0)) +
    theme_classic() +
    theme(text = element_text(family = "Helvetica", size = 10)) +
    coord_flip()
  
  ## figure 4 final ----
  combined_plot2 <- (plot5|plot6)/free(plot7)+
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', family = "Helvetica"))
}

# venous and arterial thromboembolism (vte and ate) risk factors ----
{
  df_2022 <- subset(purchases, purchase_year == 2022 & purchase_age >= 15 & purchase_age <= 55)
  df_2022$purchase_date <- as.Date(df_2022$purchase_date)
  
  # add dose_category to df_2022 based on your available hcs
  # "contemporary chc may be classified by estrogen dose into ‘high-dose’ (50 µg or more), ‘moderate-dose’ (30–35 µg), and ‘low-dose’ (15–20 µg)" (https://www.ncbi.nlm.nih.gov/books/NBK304327/)

  main_life_table <- df_2022 %>%
    select(id, hormonal_composition, purchase_date, purchase_age, hc_ATC_name, dose_category) %>%
    group_by(id) %>%
    slice(which.min(purchase_date)) %>%
    ungroup()
  
  prop.table(table(main_life_table$hormonal_composition, useNA="always"))*100
  sort(prop.table(table(main_life_table$hc_ATC_name, useNA="always"))*100, decreasing = T)
  
  mean(main_life_table$purchase_age[main_life_table$hormonal_composition == "progestin + estrogen"], na.rm = TRUE)
  sd(main_life_table$purchase_age[main_life_table$hormonal_composition == "progestin + estrogen"], na.rm = TRUE)
  median(main_life_table$purchase_age[main_life_table$hormonal_composition == "progestin + estrogen"], na.rm = TRUE)
  quantile(main_life_table$purchase_age[main_life_table$hormonal_composition == "progestin + estrogen"], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  
  mean(main_life_table$purchase_age[main_life_table$hormonal_composition == "progestin"], na.rm = TRUE)
  sd(main_life_table$purchase_age[main_life_table$hormonal_composition == "progestin"], na.rm = TRUE)
  median(main_life_table$purchase_age[main_life_table$hormonal_composition == "progestin"], na.rm = TRUE)
  quantile(main_life_table$purchase_age[main_life_table$hormonal_composition == "progestin"], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  
  vte_ate <- c(
    # Venous thromboembolism (VTE)
    "I26",
    "I80",  # This will match I80, but pattern needs modification (no I80.0)
    "I81",
    "I82",
    "O22.3",
    "O87.1",
    "O88.2",
    # Arterial thromboembolism (ATE)
    "I21",
    "I24.0",
    "I63",  # This will match I63, but pattern needs modification (no I63.6)
    "I74")
  
  pattern2 <- paste0("^(?!(I80\\.0|I63\\.6)$)(", paste(vte_ate, collapse="|"), ")(\\.\\d+)?$")
  
  vte_ate_for_2022_users <- diag_for_2022_users[grepl(pattern2, diag_for_2022_users$icd10_code, perl=TRUE), ]
  
  vte_ate_after_purchase <- vte_ate_for_2022_users %>%
    left_join(main_life_table, by = "id", relationship = "many-to-many") %>%
    dplyr::filter(diag_first_date > purchase_date | diag_last_date > purchase_date)
  
  vte_ate_after_purchase %>%
    summarise(mean_age = round(mean(diag_age, na.rm=T),1), sd_age = round(sd(diag_age, na.rm=T),1),
              median_age = median(diag_age, na.rm=T), q25_age = quantile(diag_age, probs = 0.25, na.rm = TRUE),
              q75_age = quantile(diag_age, probs = 0.75, na.rm = TRUE))
  
  length(unique(vte_ate_after_purchase$id))
  table(vte_ate_after_purchase$icd10_code) # I21.1, I26.0, I26.9, I74.3, I80, I80.2, I80.3, I80.8, I80.9, I81, I82.8, I82.9
  
  ## create main_life_table_with_conditions df by adding binary status (0/1) of relevant thromboembolism risk conditions ----
  
  risk_factors <- c("eitherFVLorPTM", "Previous VTE/ATE (recent)", "Previous VTE/ATE (ever)", "ageOver35", "Purchase of Risk-Increasing Drugs", 
                    "Family History of VTE/ATE", "Obesity", "Migraine", "Other Cardiovascular Conditions", "Hyperlipidaemia", "Pneumonia", 
                    "Hypertension", "Diabetes mellitus, type 1 and type 2", "Other Thrombosis Risk Conditions", "Recent Labour")
  
  ## derive clean group variables ----
  main_life_table_with_conditions <- main_life_table_with_conditions %>%
    mutate(
      # primary group: A = POC, B = CHC
      group_AB = case_when(
        hormonal_composition == "progestin"            ~ "POC",
        hormonal_composition == "progestin + estrogen" ~ "CHC"
      ),
      # subgroup: only meaningful for CHC users
      group_B = case_when(
        dose_category == "low-ee"      ~ "low-ee",
        dose_category == "moderate-ee" ~ "moderate-ee",
        dose_category == "estradiol-multiphase" ~ "e2",
        dose_category == "e2"          ~ "e2",
        TRUE                           ~ NA_character_))
  
  ## data qc ----
  cat(sprintf("Total women: %d\n", nrow(main_life_table_with_conditions)))
  cat(sprintf("POC users: %d\n",
              sum(main_life_table_with_conditions$group_AB == "POC")))
  cat(sprintf("CHC users: %d\n",
              sum(main_life_table_with_conditions$group_AB == "CHC")))
  cat(sprintf("Missing hormonal_composition: %d\n",
              sum(is.na(main_life_table_with_conditions$group_AB))))
  
  # chc users must have a valid dose_category !!!
  n_chc_missing_dose <- main_life_table_with_conditions %>%
    filter(group_AB == "CHC", is.na(group_B)) %>%
    nrow()
  cat(sprintf("CHC users with missing dose_category:     %d (should be 0)\n",
              n_chc_missing_dose))
  if (n_chc_missing_dose > 0) {
    warning("Some CHC users have missing dose_category. Check data.")
  }
  
  cat("\nDose category distribution:\n")
  print(table(main_life_table_with_conditions$dose_category, useNA = "always"))
  
  ## helper functions ----
  
  run_comparison_2 <- function(data, group_var, group_labels,
                               n_comparisons = 1) {
    map_dfr(risk_factors, function(rf) {
      
      na_counts <- data %>%
        group_by(.data[[group_var]]) %>%
        summarise(
          n_na = sum(is.na(.data[[rf]])),
          .groups = "drop"
        )
      
      data_rf <- data %>% filter(!is.na(.data[[rf]]))
      
      tbl <- table(data_rf[[group_var]], data_rf[[rf]])
      
      counts <- data_rf %>%
        group_by(.data[[group_var]]) %>%
        summarise(
          n_total = n(),
          n_rf    = sum(.data[[rf]], na.rm = TRUE),
          pct_rf  = round(mean(.data[[rf]], na.rm = TRUE) * 100, 1),
          .groups = "drop"
        )
      
      p <- fisher.test(tbl, simulate.p.value = TRUE)$p.value
      
      get_val <- function(col, grp) {
        val <- counts[[col]][counts[[group_var]] == grp]
        if (length(val) == 0) return(NA_real_)
        val
      }
      
      get_na <- function(grp) {
        val <- na_counts$n_na[na_counts[[group_var]] == grp]
        if (length(val) == 0) return(0L)
        val
      }
      
      pct_1 <- get_val("pct_rf", group_labels[1]) / 100
      pct_2 <- get_val("pct_rf", group_labels[2]) / 100
      
      a <- get_val("n_rf",    group_labels[1])
      b <- get_val("n_total", group_labels[1]) - a
      c <- get_val("n_rf",    group_labels[2])
      d <- get_val("n_total", group_labels[2]) - c
      
      measure_recommended <- case_when(
        is.na(pct_1) | is.na(pct_2)  ~ NA_character_,
        pct_1 > 0.10 | pct_2 > 0.10  ~ "PR",
        TRUE                          ~ "OR or PR"
      )
      
      epi_result <- tryCatch({
        if (any(is.na(c(a, b, c, d))) || a == 0 || c == 0) {
          list(
            or     = NA_real_, or_low = NA_real_, or_upp = NA_real_,
            pr     = NA_real_, pr_low = NA_real_, pr_upp = NA_real_
          )
        } else {
          mat <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
          
          or_result <- oddsratio.fisher(mat)
          
          pr_est <- pct_1 / pct_2
          se_log <- sqrt((1 - pct_1) / a + (1 - pct_2) / c)
          pr_low <- exp(log(pr_est) - 1.96 * se_log)
          pr_upp <- exp(log(pr_est) + 1.96 * se_log)
          
          list(
            or     = round(or_result$measure[2, 1], 2),
            or_low = round(or_result$measure[2, 2], 2),
            or_upp = round(or_result$measure[2, 3], 2),
            pr     = round(pr_est, 2),
            pr_low = round(pr_low, 2),
            pr_upp = round(pr_upp, 2)
          )
        }
      }, error = function(e) {
        list(
          or     = NA_real_, or_low = NA_real_, or_upp = NA_real_,
          pr     = NA_real_, pr_low = NA_real_, pr_upp = NA_real_
        )
      })
      
      or     <- epi_result$or
      or_low <- epi_result$or_low
      or_upp <- epi_result$or_upp
      pr     <- epi_result$pr
      pr_low <- epi_result$pr_low
      pr_upp <- epi_result$pr_upp
      
      primary_estimate <- case_when(
        measure_recommended == "PR"       ~ paste0(pr,  " (", pr_low,  ", ", pr_upp,  ")"),
        measure_recommended == "OR or PR" ~ paste0(or,  " (", or_low,  ", ", or_upp,  ")"),
        TRUE                              ~ NA_character_
      )
      
      tibble(
        risk_factor                            = rf,
        !!paste0("n_", group_labels[1])       := get_val("n_total", group_labels[1]),
        !!paste0("n_rf_", group_labels[1])    := get_val("n_rf",    group_labels[1]),
        !!paste0("pct_", group_labels[1])     := get_val("pct_rf",  group_labels[1]),
        !!paste0("n_na_", group_labels[1])    := get_na(group_labels[1]),
        !!paste0("n_", group_labels[2])       := get_val("n_total", group_labels[2]),
        !!paste0("n_rf_", group_labels[2])    := get_val("n_rf",    group_labels[2]),
        !!paste0("pct_", group_labels[2])     := get_val("pct_rf",  group_labels[2]),
        !!paste0("n_na_", group_labels[2])    := get_na(group_labels[2]),
        prev_diff           = round((pct_1 - pct_2) * 100, 1),
        pr                  = pr,
        pr_95ci             = case_when(
          !is.na(pr) ~ paste0(pr, " (", pr_low, ", ", pr_upp, ")"),
          TRUE       ~ NA_character_
        ),
        or                  = or,
        or_95ci             = case_when(
          !is.na(or) ~ paste0(or, " (", or_low, ", ", or_upp, ")"),
          TRUE       ~ NA_character_
        ),
        measure_recommended = measure_recommended,
        primary_estimate    = primary_estimate,
        test_used           = "Fisher",
        p_value_raw         = round(p, 4),
        p_bonferroni        = p_value_raw*15
      )
    })
  }
  
  
  ## comparison 1: poc vs chc overall/figure 5 ----
  cat(sprintf("n_POC = %d, n_CHC = %d\n",
              sum(main_life_table_with_conditions$group_AB == "POC"),
              sum(main_life_table_with_conditions$group_AB == "CHC")))
  
  comp1 <- run_comparison_2(main_life_table_with_conditions, "group_AB", c("CHC", "POC"),
                            n_comparisons = 1)
  print(comp1)
  comp1df <- as.data.frame(comp1) # CHC is group 1 (exposed group) and POC is group 2 (reference group)
  
  plot_data <- comp1df %>%
    mutate(
      pr_low = as.numeric(sub(".*\\((.+),.*", "\\1", pr_95ci)),
      pr_upp = as.numeric(sub(".*,\\s*(.+)\\)", "\\1", pr_95ci)),
      label_text = paste0(pct_POC, "% vs ", pct_CHC, "%"),
      sig = case_when(
        p_bonferroni < 0.001 ~ "***",
        p_bonferroni < 0.01  ~ "**",
        p_bonferroni < 0.05  ~ "*",
        TRUE                 ~ "ns"),
      risk_factor = recode(risk_factor,
                           "ageOver35" = "Age Above 35",
                           "eitherFVLorPTM" = "FVL or PTM Carrier"),
      risk_factor = reorder(risk_factor, pr))
  
  ggplot(plot_data, aes(y = risk_factor, x = pr, xmin = pr_low, xmax = pr_upp)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "black", linewidth = 0.6) +
    geom_linerange(aes(colour = sig), linewidth = 1, alpha = 0.8) +
    geom_point(aes(colour = sig), size = 2.5, shape = 18) +
    geom_text(aes(x = 2.2, label = label_text),
              size = 3.5, hjust = 0, lineheight = 0.9) +
    scale_colour_manual(values = c("***" = "#D55E00", "**" = "#E69F00", "ns" = "grey60"),
                        labels = c("***" = "p < 0.001", "**" = "p < 0.01", "ns" = "p >= 0.05"),
                        breaks = c("ns", "**", "***"),
                        name = "Bonferroni-corrected p-value") +
    labs(x = "Prevalence Ratio, 95% CI (reference: POC users)", y = NULL) +
    scale_x_continuous(
      breaks = c(0.5, 1, 1.5),
      limits = c(0, 3.5)) +
    theme_minimal(base_size = 12) +
    theme(panel.background   = element_rect(fill = "white", colour = NA),
          plot.background    = element_rect(fill = "white", colour = NA),
          panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_line(colour = "grey90"),
          panel.grid.major.y = element_line(colour = "grey90"),
          axis.text.y = element_text(colour = "black"),
          legend.position = "bottom",
          plot.margin = margin(20, 150, 10, 10)) +
    annotate("text", x = 1.65, y = nrow(plot_data) + 1,
             label = "bold(POC~(n[users]~'= 4,770')~vs~CHC~(n[users]~'= 8,840'))",
             parse = TRUE, size = 3.5, hjust = 0) +
    coord_cartesian(clip = "off", xlim = c(NA, 2))
  
  ## comparison 2-4: poc vs each chc dose subgroup/s6 appendix ----
  # Bonferroni correction across 3 comparisons; note: poc women appear in all three comparisons
  bx_labels <- c("low-ee", "moderate-ee", "e2")
  
  comp_bx <- map(bx_labels, function(bx) {
    df_pair <- main_life_table_with_conditions %>%
      filter(group_AB == "POC" | group_B == bx) %>%
      mutate(group_pair = if_else(group_AB == "POC", "POC", bx))
    cat(sprintf("POC vs %s: n_POC = %d, n_%s = %d\n",
                bx,
                sum(df_pair$group_pair == "POC"),
                bx,
                sum(df_pair$group_pair == bx)))
    result <- run_comparison_2(df_pair, "group_pair", c(bx, "POC"),
                               n_comparisons = 45)
    cat(sprintf("\n--- Results: POC vs %s ---\n", bx))
    print(result)
    result
  })
  names(comp_bx) <- bx_labels
  comp_bxdf <- as.data.frame(comp_bx)
  
  # extract each comparison
  comp_low <- comp_bxdf %>%
    select(risk_factor = low.ee.risk_factor,
           pr = low.ee.pr,
           pr_95ci = low.ee.pr_95ci,
           p_bonferroni = low.ee.p_bonferroni) %>%
    mutate(comparison = "POC vs Low-EE")
  
  comp_mod <- comp_bxdf %>%
    select(risk_factor = moderate.ee.risk_factor,
           pr = moderate.ee.pr,
           pr_95ci = moderate.ee.pr_95ci,
           p_bonferroni = moderate.ee.p_bonferroni) %>%
    mutate(comparison = "POC vs Moderate-EE")
  
  comp_e2 <- comp_bxdf %>%
    select(risk_factor = e2.risk_factor,
           pr = e2.pr,
           pr_95ci = e2.pr_95ci,
           p_bonferroni = e2.p_bonferroni) %>%
    mutate(comparison = "POC vs E2")
  
  # overall
  comp_overall <- comp1df %>%
    select(risk_factor, pr, pr_95ci, p_bonferroni) %>%
    mutate(comparison = "POC vs all CHC")
  
  # combine
  plot_data_all <- bind_rows(comp_overall, comp_low, comp_mod, comp_e2) %>%
    mutate(
      pr_low = as.numeric(sub(".*\\((.+),.*", "\\1", pr_95ci)),
      pr_upp = as.numeric(sub(".*,\\s*(.+)\\)", "\\1", pr_95ci)),
      sig = case_when(
        p_bonferroni < 0.001 ~ "***",
        p_bonferroni < 0.01  ~ "**",
        p_bonferroni < 0.05  ~ "*",
        TRUE                 ~ "ns"),
      sig = factor(sig, levels = c("***", "**", "*", "ns")),
      comparison = factor(comparison, levels = rev(c("POC vs all CHC", "POC vs Low-EE", 
                                                     "POC vs Moderate-EE", "POC vs E2")))) %>%
    group_by(risk_factor) %>%
    mutate(pr_overall = pr[comparison == "POC vs all CHC"]) %>%
    ungroup() %>%
    mutate(risk_factor = recode(risk_factor,
                                "ageOver35" = "Age Above 35",
                                "eitherFVLorPTM" = "FVL or PTM Carrier"),
           risk_factor = reorder(risk_factor, pr_overall))
  
  ggplot(plot_data_all,
         aes(y = risk_factor, x = pr, xmin = pr_low, xmax = pr_upp, colour = comparison)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.6) +
    geom_linerange(position = position_dodge(width = 0.7), linewidth = 1, alpha = 0.8) +
    geom_point(position = position_dodge(width = 0.7), shape = 18, size = 2.5) +
    scale_colour_manual(
      values = c("POC vs all CHC" = "black", "POC vs Low-EE" = "#E69F00", 
                 "POC vs Moderate-EE" = "#56B4E9", "POC vs E2" = "#009E73")) +
    labs(x = "Prevalence Ratio, 95% CI (reference: POC users)", y = NULL, colour = "Comparison") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA),
          legend.position = "right") +
    guides(colour = guide_legend(reverse = TRUE)) +
    coord_cartesian(clip = "off", xlim = c(NA, 3))
  
  
  ## carriers vs non-carriers ----
  # percentage of hc users who carry at least one thrombosis-associated variant
  users %>%
    group_by(id) %>%
    slice_min(date1) %>%
    ungroup() %>%
    summarise(
      total_individuals = n_distinct(id),
      total_carriers = n_distinct(id[gtStatusFVL == 1 | gtStatusPTM == 1]),
      proportion = (total_carriers / total_individuals)*100)
  
  # percentage of hc users who carry FVL-only
  users %>%
    group_by(id) %>%
    slice_min(date1) %>%
    ungroup() %>%
    summarise(
      total_individuals = n_distinct(id),
      total_carriers = n_distinct(id[gtStatusFVL == 1 & gtStatusPTM == 0]),
      proportion = (total_carriers / total_individuals)*100)
  
  # percentage of hc users who carry PTM-only
  users %>%
    group_by(id) %>%
    slice_min(date1) %>%
    ungroup() %>%
    summarise(
      total_individuals = n_distinct(id),
      total_carriers = n_distinct(id[gtStatusPTM == 1 & gtStatusFVL == 0]),
      proportion = (total_carriers / total_individuals)*100)
  
  # percentage of hc users who carry both variants
  users %>%
    group_by(id) %>%
    slice_min(date1) %>%
    ungroup() %>%
    summarise(
      total_individuals = n_distinct(id),
      total_carriers = n_distinct(id[gtStatusFVL == 1 & gtStatusPTM == 1]),
      proportion = (total_carriers / total_individuals)*100)
  
  #vte_ate_for_all_users is df (can have multiple rows per individual) with column names id, icd10_code, diag_first_date
  earliest_vte_ate <- vte_ate_for_all_users %>%
    group_by(id) %>%
    slice_min(diag_first_date, with_ties = FALSE) %>%
    ungroup()
  
  # hc users who eventually had VTE/ATE during inferred hc usage period
  overlapping_vte_ate <- vte_ate_for_all_users %>%
    inner_join(users, by = "id", relationship = "many-to-many") %>%
    filter(diag_first_date > date1 & diag_first_date <= date2) %>%
    arrange(desc(id))
  
  length(unique(overlapping_vte_ate$id))
  sort(table(overlapping_vte_ate$icd10_code))
  
  # percentage of carriers in those who had VTE during treatment coverage period
  overlapping_vte_ate %>%
    group_by(id) %>%
    slice_min(date1) %>%
    ungroup() %>%
    summarise(
      total_with_ever_VTE_during_hc_use1 = n_distinct(id),
      total_carriers = n_distinct(id[gtStatusFVL == 1 | gtStatusPTM == 1]),
      total_with_ever_VTE_during_hc_use2 = n_distinct(id[id %in% overlapping_vte_ate$id]),
      proportion = (total_carriers / total_with_ever_VTE_during_hc_use1)*100)
  
  sort(table(overlapping_vte_ate$hc_ATC))
  
  overlapping_vte_ate$relevant_date <- NA
  overlapping_vte_ate$date1 <- as.Date(overlapping_vte_ate$date1)
  overlapping_vte_ate$date2 <- as.Date(overlapping_vte_ate$date2)
  overlapping_vte_ate$diag_first_date <- as.Date(overlapping_vte_ate$diag_first_date)
  
  overlapping_vte_ate <- overlapping_vte_ate %>%
    mutate(relevant_date = case_when(
      !is.na(diag_first_date) & diag_first_date >= date1 & diag_first_date <= date2 ~ diag_first_date,
      TRUE ~ as.Date(NA)))
  
  # check for purchase of antithrombotic_drugs; antithrombotic_drugs is df (can have multiple rows per individual) with column names id, purchase_date (only purchases in 2022), drug_ATC
  result <- overlapping_vte_ate %>%
    left_join(antithrombotic_drugs, by = "id", relationship = "many-to-many") %>%
    mutate(valid_purchase = !is.na(purchase_date) & 
             !is.na(relevant_date) &
             purchase_date >= relevant_date & 
             purchase_date <= date2) %>%
    group_by(id) %>%
    mutate(has_purchases = any(valid_purchase, na.rm = TRUE)) %>%
    ungroup() %>%
    distinct(id, .keep_all = TRUE)
  
  prop.table(table(result$valid_purchase))*100

  df_carriers <- users %>%
    group_by(id) %>%
    slice_min(date1) %>%
    ungroup() %>%
    filter(gtStatusFVL == 1 | gtStatusPTM == 1)
  
  tib1 <- df_carriers %>%
    summarise(total_carriers = n_distinct(id),
              total_carriers_with_ever_VTE_ATE = n_distinct(id[id %in% earliest_vte_ate$id]),
              total_carriers_with_ever_VTE_ATE_during_hc_use = n_distinct(id[id %in% overlapping_vte_ate$id]),
              proportion1 = (total_carriers_with_ever_VTE_ATE / total_carriers)*100,
              proportion2 = (total_carriers_with_ever_VTE_ATE_during_hc_use / total_carriers)*100)
  
  df_non_carriers <- users %>%
    group_by(id) %>%
    slice_min(date1) %>%
    ungroup() %>%
    filter(gtStatusFVL == 0 & gtStatusPTM == 0)
  
  tib2 <- df_non_carriers %>%
    summarise(total_non_carriers = n_distinct(id),
              total_non_carriers_with_ever_VTE_ATE = n_distinct(id[id %in% earliest_vte_ate$id]),
              total_non_carriers_with_ever_VTE_ATE_during_hc_use = n_distinct(id[id %in% overlapping_vte_ate$id]),
              proportion1 = (total_non_carriers_with_ever_VTE_ATE / total_non_carriers)*100,
              proportion2 = (total_non_carriers_with_ever_VTE_ATE_during_hc_use / total_non_carriers)*100)
  
  contingency_table1 <- matrix(c(
    tib1$total_carriers_with_ever_VTE_ATE, tib1$total_carriers - tib1$total_carriers_with_ever_VTE_ATE,
    tib2$total_non_carriers_with_ever_VTE_ATE, tib2$total_non_carriers - tib2$total_non_carriers_with_ever_VTE_ATE), nrow = 2, byrow = TRUE)
  
  rownames(contingency_table1) <- c("Carriers", "Non-carriers")
  colnames(contingency_table1) <- c("With thrombosis", "Without thrombosis")
  
  fisher_result <- fisher.test(contingency_table1, conf.level = 0.95)
  print(fisher_result)
  oddsratio(contingency_table1, method = "fisher")
  
  chisq_result <- chisq.test(contingency_table1)
  print(chisq_result)
  
  contingency_table2 <- matrix(c(
    tib1$total_carriers_with_ever_VTE_ATE_during_hc_use, tib1$total_carriers - tib1$total_carriers_with_ever_VTE_ATE_during_hc_use,
    tib2$total_non_carriers_with_ever_VTE_ATE_during_hc_use, tib2$total_non_carriers - tib2$total_non_carriers_with_ever_VTE_ATE_during_hc_use),
    nrow = 2, byrow = TRUE)
  
  rownames(contingency_table2) <- c("Carriers", "Non-carriers")
  colnames(contingency_table2) <- c("With thrombosis", "Without thrombosis")
  
  fisher_result <- fisher.test(contingency_table2, conf.level = 0.95)
  print(fisher_result)
  oddsratio(contingency_table2, method = "fisher")
  
  # df_relatives is df (one row per individual) with column names id, carrier_status (binary status 0/1 for carrying either FVL or PTM), 
    # and relative_thrombosis (binary status 0/1 for relative having VTE/ATE)
  num_carriers_with_relatives_with_throm <- sum(df_relatives$carrier_status == 1 & df_relatives$relative_thrombosis == 1)
  
  num_non_carriers_with_relatives_with_throm <- sum(df_relatives$carrier_status == 0 & df_relatives$relative_thrombosis == 1)
  
  num_carriers_with_relatives <- sum(df_relatives$carrier_status == 1)
  
  num_non_carriers_with_relatives <- sum(df_relatives$carrier_status == 0)
  
  num_carriers_with_relatives_with_throm <- 76
  num_non_carriers_with_relatives_with_throm <- 946
  num_carriers_with_relatives <- 403
  num_non_carriers_with_relatives <- 7994
  
  num_carriers_without_relatives_with_throm <- num_carriers_with_relatives - num_carriers_with_relatives_with_throm
  num_non_carriers_without_relatives_with_throm <- num_non_carriers_with_relatives - num_non_carriers_with_relatives_with_throm
  
  data <- as.table(matrix(c(
    num_carriers_with_relatives_with_throm, num_non_carriers_with_relatives_with_throm,
    num_carriers_without_relatives_with_throm, num_non_carriers_without_relatives_with_throm), 
    nrow = 2, byrow = TRUE))
  
  rownames(data) <- c("Relative with thrombosis +", "Relative with thrombosis -")
  colnames(data) <- c("Carrier +", "Carrier -")
  
  fisher.test(data, conf.level = 0.95)
  
  sens <- data[1,1] / sum(data[,1])
  spec <- data[2,2] / sum(data[,2])
  sens_se <- sqrt((sens * (1 - sens)) / sum(data[,1]))
  sens_ci_lower <- max(0, sens - 1.96 * sens_se)
  sens_ci_upper <- min(1, sens + 1.96 * sens_se)
  
  spec_se <- sqrt((spec * (1 - spec)) / sum(data[,2]))
  spec_ci_lower <- max(0, spec - 1.96 * spec_se)
  spec_ci_upper <- min(1, spec + 1.96 * spec_se)
  
}