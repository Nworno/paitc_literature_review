library("readr")
library("tidyr")
library("dplyr")
library("stringr")
library("purrr")
library("data.table")

source("R_snippets.R")

folder_input_rayyan <- "data/articles_selection"

csv_pubmed <- read_csv(file.path(folder_input_rayyan, "csv-indirecttr-set.csv"))
# Data Managing Rayyan extractions ---------------------------------

parsing_rayyan_export <- function(rayyan_export_df) {
  rayyan_export_df <- rayyan_export_df[!is.na(rayyan_export_df$notes), ]
  list_strings <- rayyan_export_df$notes %>%
    str_split("\\|") %>%
    map(str_trim) %>%
    map(str_split, pattern = ":", n = 2) %>%
    map_depth(2, .f = function(x) {
      values <- c(x[[2]])
      names(values) <- x[[1]]
      return(values)
    }
    ) %>%
    map(.f = unlist) %>%
    tibble() %>%
    unnest_wider(col = ".")
  if (!"USER-NOTES" %in% colnames(list_strings)) list_strings[["USER-NOTES"]] <- NA
  list_strings_dm <- list_strings %>%
    rename(inclusion = "RAYYAN-INCLUSION",
           labels = "RAYYAN-LABELS",
           exclusion_reasons = "RAYYAN-EXCLUSION-REASONS",
           user_notes = "USER-NOTES") %>%
    mutate(across(starts_with("inclusion"),
                  .fns = str_extract,
                  pattern = "(Maybe)|(Included)|(Excluded)")) %>%
    mutate(user_notes = sub('{"Max"=>[', "", user_notes, fixed = TRUE) %>%
             sub(']}', "", x = ., fixed = TRUE) %>%
             sub('", "', "\\n", x = ., fixed = TRUE)
    ) %>%
    mutate(across(.cols = starts_with(c("labels", "exclusion_reasons", "user_notes")),
                  .fns = str_trim)) %>%
    mutate(across( all_of(c("exclusion_reasons", "labels")),
                   .fns = str_split,
                   pattern = ","))
  stopifnot(nrow(list_strings_dm) == nrow(rayyan_export_df))
  return(bind_cols(rayyan_export_df[, "pubmed_id"], list_strings_dm))
}

# Separating export in two files because no real way to differentiate notes and exclusion reasons between reviewers
# Arnaud
rayyan_output_arnaud <- "export_arnaud_final"
arnaud_output <- read_csv(file.path(folder_input_rayyan, rayyan_output_arnaud, "articles.csv"),
                          col_types = cols_only(pubmed_id = "c",
                                                key = "c",
                                                notes = "c")
) %>%
  rename(rayyan_key = key) %>%
  parsing_rayyan_export()
labels_df_arnaud <- arnaud_output %>%
  select(pubmed_id, labels) %>%
  unnest(labels) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = labels,
              values_from = value,
              values_fill = FALSE) %>%
  select(- `NA`)
exclusion_reasons_df_arnaud = arnaud_output %>%
  select(pubmed_id, exclusion_reasons) %>%
  unnest(exclusion_reasons) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = exclusion_reasons, values_from = value, values_fill = FALSE) %>%
  select(- `NA`)
inclusion_df_arnaud <- arnaud_output %>%
  select(pubmed_id, inclusion) %>%
  left_join(labels_df_arnaud[, c("pubmed_id", "FULL_ARTICLE")] %>%
              rename_with(str_to_lower),
            by = "pubmed_id") %>%
  left_join(exclusion_reasons_df_arnaud,
            by = "pubmed_id")


## Belkacem
rayyan_output_belkacem <- "export_belkacem_final"
belkacem_output <- read_csv(file.path(folder_input_rayyan, rayyan_output_belkacem, "articles.csv"),
                            col_types = cols_only(pubmed_id = "c",
                                                  key = "c",
                                                  notes = "c")
) %>%
  rename(rayyan_key = key) %>%
  parsing_rayyan_export()
labels_df_belkacem <- belkacem_output %>%
  select(pubmed_id, labels) %>%
  unnest(labels) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = labels,
              values_from = value,
              values_fill = FALSE) %>%
  select(- `NA`)
exclusion_reasons_df_belkacem <- belkacem_output %>%
  select(pubmed_id, exclusion_reasons) %>%
  unnest(exclusion_reasons) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = exclusion_reasons, values_from = value, values_fill = FALSE) %>%
  select(- `NA`) %>%
  rename_w_named_list(alignement_reasons)

inclusion_df_belkacem <- belkacem_output %>%
  select(pubmed_id, inclusion) %>%
  left_join(labels_df_belkacem[, c("pubmed_id", "FULL_ARTICLE")] %>%
              rename_with(str_to_lower),
            by = "pubmed_id") %>%
  left_join(exclusion_reasons_df_belkacem,
            by = "pubmed_id")

# Consensus
consensus_df <- arnaud_output[, c("pubmed_id", "inclusion")] %>%
  full_join(belkacem_output[, c("pubmed_id", "inclusion")],
            by = "pubmed_id", suffix = c("_1", "_2")) %>%
  mutate(consensus = inclusion_1 == inclusion_2,
         decision = if_else(inclusion_1 == inclusion_2, inclusion_1, "disagreement")) %>%
  select(pubmed_id, consensus, decision)

# Reasons
## Aligning labels
exclusion_reasons_df_arnaud %>% names() %>% clipr::write_clip()
exclusion_reasons_df_belkacem %>% names() %>% clipr::write_clip()

alignement_reasons <- c(
  "Methodological" = "methodological",
  "Not population adjusted" = "no_paitc",
  "no_comparison" = "no_comparison",
  "PAITC but reusing data from another PAITC" = "other_paitc",
  "Wrong topic" = "wrong_topic",
  "Wrong article type" =  "not_original_article",
  "Not human study" = "no_human",
  "result_not_described" =  "result_not_described",
  "Not indirect comparison" = "no_itc",
  "Review" =  "review",
  "protocol" =  "protocol",
  "feasibility" = "feasibility",
  "duplicated" =  "duplicated"
)

hierarchy_reasons <- c(
  "not_original_article",
  "protocol",
  "methodological",
  "review",
  "feasibility",
  "result_not_described",
  "other_paitc",
  "no_paitc",
  "no_itc",
  "no_comparison",
  "no_human",
  "wrong_topic",
  "duplicated"
)


inclusion_df_belkacem <- inclusion_df_belkacem %>% rename_w_named_list(alignement_reasons)

exclusion_reasons_df <- inclusion_df_arnaud %>%
  select(pubmed_id, all_of(hierarchy_reasons)) %>%
  pivot_longer(cols = !pubmed_id,
               names_to = "exclusion_reasons",
               values_to = "reviewer") %>%
  full_join(inclusion_df_belkacem %>%
              select(pubmed_id, any_of(hierarchy_reasons)) %>%
              pivot_longer(cols = !pubmed_id,
                           names_to = "exclusion_reasons",
                           values_to = "reviewer"),
            by = c("pubmed_id", "exclusion_reasons"),
            suffix = c("_1", "_2")
  ) %>%
  mutate(reviewer_2 = replace_na(reviewer_2, FALSE)) %>%
  select(pubmed_id, exclusion_reasons, everything())

## Checks because next step relies on the order of the exclusion_reasons column
stopifnot(
  "Reasons of exclusion are not ordered according to the exclusion reasons hierarchy" =
    exclusion_reasons_df %>%
    group_by(pubmed_id) %>%
    group_map(~ (.x$exclusion_reasons == hierarchy_reasons) %>% unique()) %>%
    unlist() %>%
    unique()
)
# Taking identical exclusion reasons
first_identical_reason <- exclusion_reasons_df %>%
  filter(reviewer_1 + reviewer_2 != 0) %>%
  group_by(pubmed_id) %>%
  filter(reviewer_1 + reviewer_2 == 2) %>%
  slice_head()

# Taking first cited reasons (most specific)
first_cited_reason <- exclusion_reasons_df %>%
  filter(reviewer_1 + reviewer_2 != 0) %>%
  group_by(pubmed_id) %>%
  slice_head() %>%
  filter(!pubmed_id %in% first_identical_reason$pubmed_id)

final_reason <- bind_rows(first_identical_reason, first_cited_reason)

final_inclusion_df <- consensus_df %>%
  left_join(final_reason, by = "pubmed_id") %>%
  arrange(consensus)

final_df <- csv_pubmed %>%
  mutate(PMID = as.character(PMID)) %>%
  left_join(final_inclusion_df,
            by = c("PMID" = "pubmed_id")) %>%
  left_join(rename(inclusion_df_arnaud, full_article_arnaud = full_article) %>% select(full_article_arnaud, pubmed_id),
            by = c("PMID" = "pubmed_id")) %>%
  left_join(rename(inclusion_df_belkacem, full_article_belkacem = full_article) %>% select(full_article_belkacem, pubmed_id),
            by = c("PMID" = "pubmed_id")) %>%
  mutate(full_article = full_article_arnaud | full_article_belkacem) %>%
  select(-full_article_arnaud, -full_article_belkacem) %>%
  select(PMID, DOI, Title, Authors, consensus, decision, everything(), exclusion_reasons, reviewer_1, reviewer_2)

included_articles <- final_df %>% filter(consensus == TRUE & decision == "Included")
to_review_articles <- final_df %>% filter(consensus == FALSE)


included_articles %>%
  select(all_of(names(csv_pubmed))) %>%
  write_csv(file.path(folder_input_rayyan, "included_articles.csv"))
to_review_articles %>%
  select(all_of(names(csv_pubmed))) %>%
  write_csv(file.path(folder_input_rayyan, "to_review_articles.csv"))


# Adjudication -------------------------------------------------------

adjudication <- read_csv(file.path(folder_input_rayyan, "to_review/Decision_DHE.csv"))
adjudication$RAISON <- replace_in_vec(adjudication$RAISON,
                                      c("Mention d'une MAIC dans le résumé" = NA,
                                        "Mention d'une MAIC dans le résumé" = NA,
                                        "Mention d'une comparaison indirecte dans le résumé / utilisation MAIC + STC dans le full text" = NA,
                                        "Bucher" = "no_paitc",
                                        "Mention d'une STC dans le résumé et le full text" = NA,
                                        "Mention d'une MAIC dans le résumé" = NA,
                                        "Objectif principal = methodo (illustration avec un case study)" = "methodological",
                                        "Mention d'une STC dans le résumé et le full text" = NA,
                                        "Il s'agit d'un erratum" = "not_original_article",
                                        "MAIC dans le résumé, mais très douteux dans le full text" = "no_paitc",
                                        "Objectif principal = methodo" = "methodological",
                                        "Bucher (mais avec des rafinements)" = "no_paitc",
                                        "Objectif principa = method / ce n'est qu'un abstract sans article complet" = "methodological",
                                        "Je comprends qu'il n'y a pas de données individuelles (juste des simus)" = "no_paitc",
                                        "Objectif principal = methodo (illustration avec un case study)" = "methodological",
                                        "Objectif principal = methodo (illustration avec un case study)" = "methodological"
                                      )
)
adjudication <- adjudication %>%
  mutate(`FULL TEXT` = ifelse(`FULL TEXT` == "NON", FALSE, TRUE),
         INCLUSION = ifelse(INCLUSION == "NON", FALSE, TRUE),
         PMID = as.character(PMID)) %>%
  rename(third_reviewer_inclusion = "INCLUSION",
         reason_third_reviewer = "RAISON",
         full_article_third_reviewer = "FULL TEXT") %>%
  select(PMID, third_reviewer_inclusion, reason_third_reviewer, full_article_third_reviewer)


# articles_discarded_afterwards -------------------------------------------

#TODO: rajouter logique avec articles éliminés lors de la revue
# read_csv
# filter(included == "XX")
# full_article_review = TRUE
# reason --> manual
# included = FALSE

# flow chart df -----------------------------------------------------------

flow_chart_df <- final_df %>%
  left_join(adjudication, by = "PMID") %>%
  mutate(final_included = ifelse(decision == "Included",
                           TRUE,
                           ifelse(third_reviewer_inclusion,
                                  TRUE,
                                  FALSE)),
         final_exclusion_reasons = ifelse(!is.na(reason_third_reviewer), reason_third_reviewer, exclusion_reasons),
         final_full_text = full_article | full_article_third_reviewer
  )

stopifnot(sum(is.na(flow_chart_df$final_included)) != 0)
stopifnot(sum(is.na(flow_chart_df$final_exclusion_reasons)) != 0)
stopifnot(sum(is.na(flow_chart_df$final_full_text)) != 0)
##############
## Exploration des désaccords

# Merging outputs
inclusion_df_comparative <- left_join(inclusion_df_arnaud, inclusion_df_belkacem,
                                      suffix = c("_arnaud", "_belkacem"),
                                      by = "pubmed_id") %>%
  mutate(agreed = inclusion_arnaud == inclusion_belkacem) %>%
  select(pubmed_id, agreed, inclusion_arnaud, inclusion_belkacem, full_article_arnaud, full_article_belkacem, everything()) %>%
  arrange(agreed)

disagreements_df <- inclusion_df_comparative %>% filter(agreed == FALSE)
disagreements_df %>% write_csv(file = file.path(folder_input_rayyan, "disagreements_final.csv"))
# Displaying agreements
library(formattable)
bool_formatter <- formattable::formatter("span", style = x ~ style(color = ifelse(x, "green", "red")))
inclusion_formatter <- formattable::formatter("span", style = x ~ style(color = ifelse(x == "Included", "blue", "red")))
inclusion_df_comparative %>% formattable(list(
  agreed = bool_formatter,
  inclusion_arnaud = inclusion_formatter,
  inclusion_belkacem = inclusion_formatter,
  full_article_arnaud = bool_formatter,
  full_article_belkacem = bool_formatter
))
# library(DT)
# inclusion_df_comparative %>% DT::datatable(
#   options = list(
#     pageLength = 100
#   ))
# )

# Statistics about disagreements
library(printr)
with(inclusion_df_comparative, table(inclusion_arnaud, inclusion_belkacem)) %>%
  knitr::kable() %>%
  kableExtra::kable_minimal()

with(inclusion_df_comparative, table(inclusion_arnaud, inclusion_belkacem)) %>%
  prop.table() %>%
  round(3) %>%
  knitr::kable() %>%
  kableExtra::kable_minimal()

inclusion_df_comparative %>%
  filter(inclusion_arnaud != inclusion_belkacem,
         !is.na(inclusion_belkacem),
         inclusion_belkacem != "Maybe") %>%
  select(pubmed_id) %>%
  simplify() %>%
  unname() %>%
  clipr::write_clip()

# List agreements
inclusion_df_comparative %>%
  filter(inclusion_arnaud == inclusion_belkacem,
         !is.na(inclusion_belkacem)) %>%
  select(pubmed_id) %>%
  simplify() %>%
  unname() %>%
  clipr::write_clip()

