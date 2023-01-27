library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(lubridate)

source("R_snippets.R")
source("questions_sections.R")

WRITE <- FALSE
dir_data <- "data"

flow_chart_df <- read_csv("data/to_use_for_stats/flow_chart.csv",
                          col_types = cols(included = "l",
                                           full_article = "l",
                                           third = "l",
                                           during_extraction = "l",
                                           .default = "c"))

# DM adjudication first part ----------------------------------------------

first_part_ASL <- read_csv(file.path(dir_data, "extraction/adjudicated/first_part/ASL.csv"),
                           col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                           col_types = c("cccccc"))
first_part_BZ <- read_csv(file.path(dir_data, "extraction/adjudicated/first_part/BZ.csv"),
                          col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                          col_types = c("cccccc"))
first_part <- bind_rows(first_part_ASL, first_part_BZ) %>%
  filter(decision != "XXXX") %>%
  filter(tolower(`Ind studies num`) != "xx" | is.na(`Ind studies num`))


stopifnot(
  # checking that no other unknown left in any other questions than CT country (after DM)
  first_part %>%
    filter(decision == "unknown") %>%
    pull(questions) %>%
    unique() == "Country where the clinical trial/observational study was conducted (international if more than one)"
)
stopifnot(!any(is.na(first_part$decision)))

#  DM adjudication: second_part -------------------------------------------

second_part_ASL <- read_csv(file.path(dir_data, "extraction/adjudicated/second_part/ASL.csv"),
                            col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                            col_types = c("ccccccc"))
second_part_BZ <- read_csv(file.path(dir_data, "extraction/adjudicated/second_part/BZ_raw.csv"),
                           col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                           col_types = c("ccccccc"))
second_part_DH <- read_csv(file.path(dir_data, "extraction/adjudicated/second_part/DH_raw.csv"),
                           col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                           col_types = c("ccccccc"))



second_part <- bind_rows(second_part_ASL, second_part_BZ, second_part_DH) %>%
  filter(tolower(`Ind studies num`) != "xx" | is.na(`Ind studies num`)) %>%
  filter(`ITC num` != "xx" | is.na(`ITC num`)) %>%
  # keeping the NAs in the decision column for now,
  # to check afterward that there are none left in the methods/results sections
  # (there could be some left in the rows of the first_part)
  # A corriger à la source ce 'Xx'
  filter(!decision %in% c("XXXX", "Xx") | is.na(decision))


# Bring together questions of first and second part -----------

second_part_subset <- anti_join(second_part, first_part, by = c("doi", "section", "ITC num", "Ind studies num", "questions"))

# On 02/12/2022: manual correction of the different files --> replacing NA by XXXX into the individual files, before generation of the 'raw file'. asl 02/12/2022, DH and BZ done on 05/12/2022.
stopifnot(!any(is.na(second_part_subset$decision)))
stopifnot(!any(is.na(second_part_subset %>% filter(section %in% c("methodology", "results")) %>% pull(`ITC num`))))
second_part_subset <- second_part_subset %>%
  # no NAs there
  # no NAs there
  filter(`ITC num` != "xx")

# DM steps:
# - Retrieve supplementary adjudicated columns (total sample size, included covariates)

sup_columns <- read_csv(file.path(dir_data,
                                  "extraction/adjudicated/second_part/sup_columns_adjudicated.csv"),
                        col_select = all_of(c("doi", "section", "ITC num", "Ind studies num", "questions", "decision")),
                        col_types = c("cccccc"))
stopifnot(!any(is.na(sup_columns$decision)))
# Removing rows discarded with`ITC num` == "xx"
subset_sup_columns <- semi_join(sup_columns, second_part_subset, by = c("doi", "ITC num"))


# answers sanity checking --------------------------------------------------------


long_results <- bind_rows(first_part, second_part_subset, subset_sup_columns) %>%
  rename(answer = "decision",
         n_itc = "ITC num",
         study_number = "Ind studies num") %>%
  left_join(order_sections) %>%
  arrange(doi, order_sections, n_itc, study_number) %>%
  select(-order_sections) %>%
# type checking
  mutate(
    n_itc = as.integer(n_itc),
    study_number = as.integer(study_number)
  )

# Types checking: detecting non numerical values within supposedly numerical columns
test <- long_results %>%
  pivot_wider(id_cols = c("doi", "n_itc", "study_number"), names_from = "questions", values_from = "answer") %>%
  select(all_of(numerical_questions)) %>%
  # Warning expected here
  mutate(across(.fns = function(x) ifelse(x %>% as.numeric() %>% is.na(), x, NA))) %>%
  select(where(~ any(!is.na(.x)))) %>%
  filter(if_any(.cols = everything(), .fns = ~ !is.na(.x)))
# Correcting non numeric answers in numerical columns
long_results[grepl("%", long_results$answer), "answer"] <- long_results[grepl("%", long_results$answer), "answer", drop = TRUE] %>%
  sub(pattern = "%", replacement = "", x = .)
# Maybe assign "XXXX" instead of NA
long_results[long_results$answer == "Non anchored comparison" & grepl("^If anchored comparison,\\s", long_results$questions), "answer"] <- NA


# Renaming questions ------------------------------------------------------
source("questions_sections.R")
renaming_questions_list <- flatten_list(questions_sections)
long_results$questions <- replace_in_vec(long_results$questions, renaming_questions_list)


# DM outcomes -------------------------------------------------------------
# Loading the manually edited file about treatment contrast effect
outcomes_datamanaged <- read_tsv("data/extraction/adjudicated/second_part/outcome_benefits_datamanaged.tsv") %>%
  select(primary_outcome_name, outcome_short_name, contrast, direction_benefit) %>%
  # outcome_name and primary_outcome_name, and contrast_effect_type and contrast differ,
  # differ because outcomes_benefits_datamanaged file manually edited before renaming change
  rename(outcome_name = primary_outcome_name,
         contrast_effect_type = contrast)

wide_methodo_results_df <- long_results %>%
  filter(section %in% c("methodology", "results")) %>%
  pivot_wider(id_cols = c(doi, n_itc), names_from = questions, values_from = answer) %>%
  left_join(distinct(outcomes_datamanaged, outcome_name, outcome_short_name)) %>%
  left_join(distinct(outcomes_datamanaged, outcome_short_name, contrast_effect_type, direction_benefit)) %>%
  # harmonizing the outcome_types and contrasts
  mutate(
    contrast_effect_type = case_when(
      outcome_short_name == "ACR Response Rate" & contrast_effect_type == "Means difference" ~ "Proportion difference",
      contrast_effect_type == "None" ~ NA_character_,
      TRUE ~ contrast_effect_type),
    outcome_variable_type = case_when(
      # because Mean annualized bleeding rate varies between 0 and +\inf
      outcome_short_name == "Mean Annualized Bleeding Rate" ~ "Continuous (count, mean, ...)",
      TRUE ~ outcome_variable_type
    ),
    effect_cutoff = case_when(
      # "1" and "0" as characters to be able to pivot into longer column at next step
      contrast_effect_type %in% c("HR", "RR", "OR", "Means Ratio", "Incidence Rate Ratio", "Rate ratio") ~ "1",
      contrast_effect_type %in% c("Means difference", "Median difference", "Rate difference", "Risk difference", "Proportions difference", "Proportion difference") ~ "0",
      TRUE ~ NA_character_),
    contrast_effect_unadjusted_value = case_when(
      #TODO: to be checked if new data is eventually added, works so far because
      # for the first extraction, all proportion differences are actually percentage differences
      contrast_effect_type %in% c("Proportions difference", "Proportion difference") ~ as.character(as.numeric(contrast_effect_unadjusted_value) / 100),
      TRUE ~ contrast_effect_unadjusted_value
    ),
    contrast_effect_adjusted_value = case_when(
      #TODO: to be checked if new data is eventually added, works so far because
      # for the first extraction, all proportion differences are actually percentage differences
      contrast_effect_type %in% c("Proportions difference", "Proportion difference") ~ as.character(as.numeric(contrast_effect_adjusted_value) / 100),
      TRUE ~ contrast_effect_adjusted_value
    )
  ) %>%
  select(outcome_name,
         outcome_short_name,
         outcome_variable_type,
         contrast_effect_type,
         direction_benefit,
         everything())

# Creating the file to manually enter which directions makes a given treatment contrast effect beneficial
if (FALSE) {
  wide_methodo_results_df %>%
    select(outcome_name,
           outcome_variable_type,
           contrast,
           effect_cutoff) %>%
    distinct() %>%
    write_tsv("data/extraction/adjudicated/second_part/outcome_benefits_raw.tsv")
}


if (wide_methodo_results_df %>%
   filter(is.na(direction_benefit) & !is.na(contrast_effect_type)) %>%
   nrow() != 0) {
  # Reviewing cases with missing direction benefit
  View(
    wide_methodo_results_df %>%
      filter(is.na(direction_benefit) & !is.na(contrast_effect_type)) %>%
      select(doi, outcome_name, outcome_short_name, outcome_variable_type, contrast_effect_type, direction_benefit)
  )
}

long_methodo_results_df <- wide_methodo_results_df %>%
  pivot_longer(cols = !c(doi, n_itc),
               names_to = "questions",
               values_to = "answer") %>%
  # to add back information about the section
  left_join(distinct(long_results, questions, section), by = "questions") %>%
  mutate(section = case_when(
    questions == "outcome_short_name" ~ "methodology",
    questions == "direction_benefit" ~ "results",
    questions == "effect_cutoff" ~ "results",
    TRUE ~ section
  ))

stopifnot(all(!is.na(long_methodo_results_df$section)))

# DM pvalues --------------------------------------------------------------

pvalues_df <- long_results %>%
  filter(questions %in% c("contrast_effect_unadjusted_pvalci", "contrast_effect_adjusted_pvalci")) %>%
  mutate(lb_ci = str_extract(answer, "(?<=\\[).+(?=;)"),
         ub_ci = str_extract(answer, "(?<=;).+(?=\\])"),
         # pattern compliqué pour prendre en compte les cas ou l'IC et la pvalues sont simultanément reportés
         pval = str_extract(answer, "(^[<>\\=\\.\\-\\d]+)|((?<=[p\\=])[<>\\=\\.\\-\\d]+)")
  ) %>%
  mutate(across(.cols = c(lb_ci, ub_ci, pval), .fns = str_trim)) %>%
  mutate(across(.cols = c(lb_ci, ub_ci), .fns = as.numeric),
         # a warning is expected here
         num_pval = as.numeric(pval))
tryCatch(
  stopifnot(pvalues_df %>%
              filter(if_all(.cols = c(lb_ci, ub_ci, pval), .fns = is.na)) %>%
              nrow() == 0
  ), error = function(e) {
    View(pvalues_df %>%
           filter(if_all(.cols = c(lb_ci, ub_ci, pval), .fns = is.na))
    )
  }
)

contrasts_df <- wide_methodo_results_df %>%
  select(doi, n_itc, contrast_effect_type, effect_cutoff)

significance_df <- left_join(pvalues_df, contrasts_df, by = c("doi", "n_itc")) %>%
  mutate(significant = case_when(
    grepl("^<", pval) ~ TRUE,
    num_pval < 0.05 ~ TRUE,
    num_pval >= 0.05 ~ FALSE,
    lb_ci <= effect_cutoff & ub_ci >= effect_cutoff ~ FALSE,
    !is.na(lb_ci) & !is.na(ub_ci) ~ TRUE,
    TRUE ~ NA
  )) %>%
  # mutate(z_value = qnorm(num_pval/2, lower.tail = FALSE),
  #        se_effect = (abs(ub_ci - lb_ci)) / 2 * qnorm(0.975),
  #        z_value = effect / se_effect) %>%
  mutate(adjustment = ifelse(grepl("_adjusted_", questions), "adjusted",
                             ifelse(grepl("_unadjusted_", questions), "unadjusted", NA))) %>%
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  pivot_longer(cols = c("lb_ci", "ub_ci", "pval", "num_pval", "significant"),
               names_to = "indicator",
               values_to = "values") %>%
  pivot_wider(names_from = c("adjustment", "indicator"),
              values_from = "values",
              names_sep = "_") %>%
  select(-questions, -answer, -contrast_effect_type, -effect_cutoff) %>%
  pivot_longer(cols = !c(doi, section, n_itc, study_number),
               names_to = "questions",
               values_to = "answer") %>%
  drop_na(answer)


long_results_final <- filter(long_results, section %in% c("general_information", "study_information")) %>%
  bind_rows(long_methodo_results_df) %>%
  mutate(across(everything(), .fns = as.character)) %>%
  bind_rows(significance_df) %>%
  arrange(doi, section, n_itc, study_number) %>%
  # to discard article removed at the very end
  semi_join(filter(flow_chart_df, included), by = "doi")

# Writing final table of results to use for stats -------------------------
long_results_final %>% write_tsv("data/to_use_for_stats/long_results_final.tsv")


# Subsetting articles information df on articles included in the results ----
all_articles_info <- read_csv("data/articles_selection/all_articles_info.csv",
                              col_types = cols(.default = "c"))
all_articles_id <- read_csv("data/articles_selection/articles_id.csv",
                            col_types = cols(.default = "c")) %>%
  mutate(DOI = tolower(DOI)) %>%
  rename(doi = DOI)

included_articles_info <- all_articles_id %>%
  # Subsetting articles information for only the ones included in the review
  semi_join(filter(flow_chart_df, included),
            by = "PMID") %>%
# Joining information from 2 files exported from Pubmed: id and information
  left_join(all_articles_info,
            by = "PMID") %>%
  rename_with(tolower) %>%
  select(doi, pmid, everything())

# Testing whether included_articles only contains information for the included articles
stopifnot(identical(
  unique(included_articles_info$doi)[order(unique(included_articles_info$doi))],
  unique(long_results_final$doi)[order(unique(long_results_final$doi))]
))

write_csv(included_articles_info, "data/included_articles_info.csv")




# Treatments and ATC ------------------------------------------------------
ttt_df <- read_tsv("data/mapping/treatment_mapping.tsv") %>%
  mutate("CAR-T cells" = ifelse(!is.na(`CAR-T cells`), TRUE, FALSE)) %>%
  select(ttt_name, `ATC code`, others, `CAR-T cells`, others) %>%
  distinct()

## Handling multiple ATC code per lines ----------------------------------
handling_multiple_codes <- ttt_df[, "ATC code"] %>%
  mutate("ATC code parsed" = str_split(`ATC code`, ";")) %>%
  unnest_longer(`ATC code parsed`) %>%
  distinct()
ttt_df <- ttt_df %>% left_join(handling_multiple_codes, by = "ATC code") %>%
  select(-`ATC code`) %>%
  rename(`ATC code` = `ATC code parsed`)

atc_ancestors <- read_tsv("data/mapping/classification_ATC/CONCEPT_ANCESTOR.csv")
atc_classification <- read_tsv("data/mapping/classification_ATC/CONCEPT.csv") %>%
  select(all_of(c("concept_id", "concept_code", "concept_class_id", "concept_name")))
ttt_mapping <- ttt_df %>%
  select(`ATC code`) %>%
  drop_na(`ATC code`) %>%
  distinct() %>%
  left_join(atc_classification,
                     by = c("ATC code" = "concept_code")) %>%
  left_join(atc_ancestors[c("ancestor_concept_id", "descendant_concept_id")],
            by = c("concept_id" = "descendant_concept_id")) %>%
  left_join(atc_classification,
            by = c("ancestor_concept_id" = "concept_id"),
            suffix = c("", "_ancestor")) %>%
  # Last ancestor level seems useless, but allows to keep the codes without ancestor
  # filter(concept_class_id_ancestor != "ATC 5th") %>%
  rename(concept_code_ancestor = concept_code) %>%
  select(-ancestor_concept_id, concept_name_ancestor) %>%
  distinct() %>%
  pivot_wider(names_from = "concept_class_id_ancestor",
              values_from = c("concept_code_ancestor", "concept_name_ancestor")
  ) %>%
  select(!ends_with(c("5th", "NA"))) %>%
  select(-concept_id)


## manually inserting 2023 incoming atc codes ------------------------------
L01EX <- ttt_mapping %>%
  filter(str_starts(`ATC code`, "L01EX")) %>%
  mutate(concept_name = ifelse(`ATC code` == "L01EX25", "umbralisib: oral", `ATC code`)) %>%
  fill(everything(), .direction = "down")

N06AX29 <- ttt_mapping %>%
  filter(str_starts(`ATC code`, "N06AX29")) %>%
  mutate(
    `concept_code_ancestor_ATC 1st` = "N",
    `concept_code_ancestor_ATC 2nd` = "N06",
    `concept_code_ancestor_ATC 3rd` = "N06A",
    `concept_code_ancestor_ATC 4th` = "N06AX",
    `concept_name_ancestor_ATC 1st` = "NERVOUS SYSTEM",
    `concept_name_ancestor_ATC 2nd` = "PSYCHOANALEPTICS",
    `concept_name_ancestor_ATC 3nd` = "ANTIDEPRESSANTS",
    `concept_name_ancestor_ATC 4nd` = "OTHER ANTIDEPRESSANTS"
  )

L04AA54 <- ttt_mapping %>%
  filter(str_starts(`ATC code`, "L04AA54")) %>%
  mutate(
    `concept_code_ancestor_ATC 1st` = "L",
    `concept_code_ancestor_ATC 2nd` = "L04",
    `concept_code_ancestor_ATC 3rd` = "L04A",
    `concept_code_ancestor_ATC 4th` = "L04AA",
    `concept_name_ancestor_ATC 1st` = "NERVOUS ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS",
    `concept_name_ancestor_ATC 2nd` = "IMMUNOSUPPRESSANTS",
    `concept_name_ancestor_ATC 3nd` = "IMMUNOSUPPRESSANTS",
    `concept_name_ancestor_ATC 4nd` = "Selective immunosuppressants"
  )


car_t_cells <- ttt_mapping %>%
  semi_join(filter(ttt_df, `CAR-T cells` == TRUE)) %>%
  filter(is.na(concept_class_id)) %>%
  mutate(
    `concept_code_ancestor_ATC 1st` = "L",
    `concept_code_ancestor_ATC 2nd` = "L01",
    `concept_code_ancestor_ATC 3rd` = "L01X",
    `concept_code_ancestor_ATC 4th` = "L01XL",
    `concept_name_ancestor_ATC 1st` = "ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS",
    `concept_name_ancestor_ATC 2nd` = "ANTINEOPLASTIC AGENTS",
    `concept_name_ancestor_ATC 3nd` = "OTHER ANTINEOPLASTIC AGENTS",
    `concept_name_ancestor_ATC 4nd` = "ANTINEOPLASTIC CELL AND GENE THERAPY"
  )

curated <- bind_rows(L01EX, N06AX29, L04AA54, car_t_cells)
atc_mapping_final <- ttt_mapping %>%
  anti_join(curated, by = "ATC code") %>%
  bind_rows(curated)
ttt_df_mapped <- ttt_df %>% left_join(atc_mapping_final, by = "ATC code") %>%
  distinct()

# Checking that all ttt names found in extraction have either ATC ancestors mapped or something written in 'others' column
stopifnot(!is.na(ttt_df_mapped$others) | !is.na(ttt_df_mapped$`concept_code_ancestor_ATC 2nd`))

ttt_df_mapped_final <- ttt_df_mapped %>%
  mutate(final_classification_code = case_when(
    !is.na(`concept_code_ancestor_ATC 4th`) ~ `concept_code_ancestor_ATC 4th`,
    !is.na(`concept_code_ancestor_ATC 3rd`) ~ `concept_code_ancestor_ATC 3rd`,
    !is.na(`concept_code_ancestor_ATC 2nd`) ~ `concept_code_ancestor_ATC 2nd`,
    TRUE ~ NA_character_
    ),
    final_classification_name = case_when(
      !is.na(`concept_name_ancestor_ATC 4th`) ~ `concept_name_ancestor_ATC 4th`,
      !is.na(`concept_name_ancestor_ATC 3rd`) ~ `concept_name_ancestor_ATC 3rd`,
      !is.na(`concept_name_ancestor_ATC 2nd`) ~ `concept_name_ancestor_ATC 2nd`,
      !is.na(others) ~ others,
      TRUE ~ NA_character_
    )) %>%
  distinct(
    ttt_name,
    final_classification_code,
    final_classification_name,
    `CAR-T cells`
  )

list_replacing_ttt_manual <- c(
  "etoposide-ifosfamide" = "etoposide - ifosfamide",
  "bendamustine- brentuximab" = "bendamustine - brentuximab",
  "vinblastine-procarbazine" = "vinblastine - procarbazine",
  "brentuximab-bendamustine" = "brentuximab - bendamustine",
  "vinblastine-procarbazine" = "vinblastine - procarbazine",
  "methotrexate + pl mtx-hydrocortancyl" = "mtx - hydrocortancyl",
  "mogamulizumab + pl infiltree triple (aracytine, mtx, hydrocortancyl)" = "mogamulizumab - aracytine - mtx - hydrocortancyl",
  "rituximab-gemcitabine-oxaliplatine" = "rituximab - gemcitabine - oxaliplatine"
)
str_transformation <- function(x) {
# Function modified as compared to the one used in dm_extraction to simplify ttt names from extraction
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>%
    str_squish() %>%
    replace_in_vec(named_list = list_replacing_ttt_manual) %>%
    str_split("(\\+)|(\\s-\\s)|(\\set\\s)|(\\sor\\s)|(\\sand\\s)|(/)|(followed\\sby)")
}

wide_ttt_parsed <- long_results_final  %>%
  filter(questions %in% c("treatment_name_ipd", "treatment_name_nonipd")) %>%
  distinct(answer) %>%
  rename(ttt_name = answer) %>%
  mutate(ttt_parsed = str_transformation(ttt_name)) %>%
  unnest_wider(ttt_parsed) %>%
  rename("first" = "...1", "second" = "...2", "third" = "...3") %>%
  mutate(across(.cols = all_of(c("first", "second", "third")),
                .fns = str_squish)
  ) %>%
  distinct()

long_ttt_parsed <- wide_ttt_parsed %>%
  pivot_longer(cols = c("first", "second", "third"),
               names_to = "instance",
               values_to = "ttt_parsed") %>%
  filter(!is.na(ttt_parsed))

ttt_atc_mapped <- long_ttt_parsed %>%
  left_join(ttt_df_mapped_final, by = c("ttt_parsed" = "ttt_name")) %>%
  filter(!is.na(final_classification_name)) %>%
  pivot_wider(id_cols = "ttt_name",
              names_from = "instance",
              values_from = c("final_classification_name", "CAR-T cells")) %>%
  rowwise() %>%
  mutate(car_t_cells = any(`CAR-T cells_first`, `CAR-T cells_second`, `CAR-T cells_third`, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(first = final_classification_name_first,
         second = final_classification_name_second,
         third = final_classification_name_third) %>%
  select(ttt_name, first, second, third, car_t_cells)

decision_atc_mapped <- long_results_final  %>%
  filter(questions %in% c("treatment_name_ipd", "treatment_name_nonipd")) %>%
  left_join(ttt_atc_mapped, by = c("answer" = "ttt_name"))

decision_atc_mapped %>% write_tsv(file.path(dir_data, "to_use_for_stats/decision_atc_mapped.tsv"))


# ICD mapping -------------------------------------------------------------

icd <- read_tsv("data/mapping/classification_ICDWHO/CONCEPT.csv") %>%
  select(all_of(c("concept_id", "concept_code", "concept_class_id", "concept_name")))
concept_relationship <- read_tsv("data/mapping/classification_ICDWHO/CONCEPT_RELATIONSHIP.csv")
conditions_mapping <- read_tsv("data/mapping/conditions_mapping.tsv")

icd <- icd %>%
  filter(str_starts(concept_class_id, "ICD10") & !str_starts(concept_name, "Invalid ICD10"))

# Not possible to use CONCEPT_ANCESTOR.csv because for some reason concept.csv doesn't map to it --> use of concept_relationship somehow
conditions_mapped <- conditions_mapping %>%
  left_join(icd[, c("concept_code", "concept_id")], by = c("ICD-10 code" = "concept_code")) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("concept_id"  = "concept_id_2")) %>%
  rename(ancestor_concept_id_1 = concept_id_1) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("ancestor_concept_id_1" = "concept_id_2")) %>%
  rename(ancestor_concept_id_2 = concept_id_1) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("ancestor_concept_id_2" = "concept_id_2")) %>%
  rename(ancestor_concept_id_3 = concept_id_1) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("ancestor_concept_id_3" = "concept_id_2")) %>%
  rename(ancestor_concept_id_4 = concept_id_1) %>%
  left_join(filter(concept_relationship, relationship_id == "Subsumes"), by = c("ancestor_concept_id_4" = "concept_id_2")) %>%
  rename(ancestor_concept_id_5 = concept_id_1) %>%
  select(condition_name,
         `ICD-10 code`,
         concept_id,
         ancestor_concept_id_1,
         ancestor_concept_id_2,
         ancestor_concept_id_3,
         ancestor_concept_id_4,
         ancestor_concept_id_5) %>%
  pivot_longer(cols = contains("concept_id"), values_to = "concept_id", names_to = "level") %>%
  filter(!is.na(concept_id)) %>%
  left_join(icd[, c("concept_id", "concept_class_id", "concept_code", "concept_name")], by = c("concept_id")) %>%
  select(-level, -concept_id, -`ICD-10 code`) %>%
  distinct() %>%
  pivot_wider(names_from = c("concept_class_id"),
              values_from = c("concept_code", "concept_name"),
              values_fn = function(...) paste(..., collapse = "; "))

conditions_mapped %>% write_tsv("data/to_use_for_stats/decision_icd_mapped.tsv")
