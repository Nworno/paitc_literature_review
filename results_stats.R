library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(lubridate)
library(ggthemr)
library(arsenal)
library(flextable)
library(ggthemr)

source("R_snippets.R")
# ggtheme setting ---------------------------------------------------------
ggthemr::ggthemr("fresh")

all_articles_info <- read_csv("data/articles_selection/all_articles_info.csv", col_types = cols(.default = "c"))
all_articles_id <- read_csv(file.path(data_dir, "articles_selection/articles_id.csv"),
                            col_types = cols(.default = "c"))  %>%
  mutate(DOI = tolower(DOI)) %>%
  rename(doi = DOI)
# extraction_list <- read_csv("data/articles_selection/included_articles_after_review.csv", col_types = cols(.default = "c)))
flow_chart_df <- read_csv("data/to_use_for_stats/flow_chart.csv", col_types = cols(included = "l",
                                                                                   full_article = "l",
                                                                                   third = "l",
                                                                                   during_extraction = "l",
                                                                                   .default = "c"))
results_df <- read_tsv("data/to_use_for_stats/long_results_final.tsv", col_types = cols(.default = "c"))
ttt_mapped <- read_tsv("data/to_use_for_stats/decision_atc_mapped.tsv", col_types = cols(.default = "c", car_t_cells = "l"))
conditions_mapped <- read_tsv("data/to_use_for_stats/decision_icd_mapped.tsv", col_types = cols(.default = "c"))

# list_questions ----------------------------------------------------------
source("questions_sections.R")
methodology_questions <- questions_sections$methodology %>% unname()

# selection articles ---------------------------------------------------------
included_articles <- filter(flow_chart_df, included)
included_articles_info <- semi_join(all_articles_info, included_articles, by = "PMID")

results_df <- left_join(results_df, all_articles_id[, c("doi", "PMID")], by = "doi")

# articles metadata -------------------------------------------------------

## Authors specific affiliations would need to be parsed and retrieved through pubv1_info.txt --> regex looking at ". " preceded by a ", " or "^"
included_articles_info %>%
  mutate("journal_title" = JT,
         "journal_title_short" = TA) %>%
  group_by(journal_title_short) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(journal_title_short, count), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.y = element_text(size = 7)) +
  scale_x_discrete() +
  coord_flip() +
  labs(title = "Publication journals",
       x = "Journals",
       y = "Articles count")

## Publication dates
publication_date_df <- included_articles_info %>%
  select(EDAT) %>%
  mutate(publication_date = as_date(EDAT, format = "%Y/%m/%d %H:%M"),
         publication_year = year(publication_date),
         publication_date_q = lubridate::quarter(publication_date, with_year = TRUE),
         publication_date_s = lubridate::semester(publication_date, with_year = TRUE),
         publication_date_s = ifelse(round(publication_date_s %% 0.2, 1) == 0.1,
                                      trunc(publication_date_s),
                                      trunc(publication_date_s) + 0.5))


publication_date_df %>%
  filter(publication_year < 2022) %>%
  group_by(publication_year) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = publication_year, y = count)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = count), nudge_y = 2.5, nudge_x = -0.05) +
  scale_x_continuous(breaks = 2010:2022) +
  labs(title = "MAIC/STC implementation: publications by years",
       x = "Year",
       y = "Publication counts")

## Results

general_information_df <- results_df[results_df$section == "general_information",]
individual_study_df <- results_df[results_df$section == "individual_study",]
paitc_results <- results_df[results_df$section %in% c("methodology", "results"), ]


results_wide <- paitc_results %>%
  filter(section == "results") %>%
  pivot_wider(id_cols = c(doi, PMID, n_itc), names_from = questions, values_from = answer) %>%
  mutate(across(where(is.character), .fns = as.factor))

with(results_wide, table(n_itc))
distinct(results_wide, doi, n_itc) %>% dim()


results_wide %>%
  names()
## Checks
results_wide %>%
  summarise(across(.fns = ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = everything()) %>%
  View()

results_wide %>%
  select(all_of(methodology_questions)) %>%
  tableby(as.formula("`Type of population-adjusted indirect comparisons performed` ~ ."),
          data = .) %>%
  summary() %>%
  as.data.frame() %>%
  flextable() %>%
  print(preview = "docx")
  save_as_html(path = "test.html")





