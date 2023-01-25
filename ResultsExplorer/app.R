library(shiny)
library(dplyr)
library(tidyr)
library(psych)
library(arsenal)
library(readr)

options(shiny.autoreload = TRUE)

data_dir <- "../data/"


# Sample data frame
all_articles_info <- read_csv(file.path(data_dir, "articles_selection/all_articles_info.csv"), col_types = cols(.default = "c"))
all_articles_id <- read_csv(file.path(data_dir, "articles_selection/articles_id.csv"),
                            col_types = cols(.default = "c"))  %>%
    mutate(DOI = tolower(DOI)) %>%
    rename(doi = DOI)
extraction_list <- read_csv(file.path(data_dir, "articles_selection/included_articles_after_review.csv"), col_types = cols(.default = "c"))

flow_chart_df <- read_csv(file.path(data_dir, "to_use_for_stats/flow_chart.csv"), col_types = cols(
    included = "l",
    full_article = "l",
    third = "l",
    during_extraction = "l",
    .default = "c"
))
results_df <- read_tsv(file.path(data_dir, "to_use_for_stats/long_results_final.tsv"), col_types = cols(.default = "c"))
ttt_mapped <- read_tsv(file.path(data_dir, "to_use_for_stats/decision_atc_mapped.tsv"), col_types = cols(.default = "c", car_t_cells = "l"))
conditions_mapped <- read_tsv(file.path(data_dir, "to_use_for_stats/decision_icd_mapped.tsv"), col_types = cols(.default = "c"))


source("../questions_sections.R")


# selection articles ---------------------------------------------------------
all_articles_id <- all_articles_id
included_articles <- filter(flow_chart_df, included)

results_df <- left_join(results_df, all_articles_id[, c("doi", "PMID")], by = "doi")

stopifnot(identical(
    unique(included_articles$PMID)[order(unique(included_articles$PMID))],
    unique(results_df$PMID)[order(unique(results_df$PMID))]
))

included_articles_info <- semi_join(all_articles_info, included_articles, by = "PMID")

general_information_df <- results_df[results_df$section == "general_information", ]
individual_study_df <- results_df[results_df$section == "study_information", ]
paitc_results <- results_df[results_df$section %in% c("methodology", "results"), ]

source("../../code_snippets/R_snippets/R_snippets.R")
source("../questions_sections.R")
# TODO: reprendre d'ici, renommer à partir des noms de questions_sections.R
questions_names <- flatten_list(questions_sections)

results_df$questions <- results_df$questions %>% replace_in_vec(questions_names)

wide_articles_df <- results_df %>%
    filter(section == "general_information") %>%
    pivot_wider(id_cols = c(doi, PMID), names_from = questions, values_from = answer) %>%
    left_join(
        conditions_mapped,
        by = c("condition_name" = "condition_name")
    )

wide_comparisons_df <- results_df %>%
    filter(section %in% c("methodology", "results")) %>%
    pivot_wider(
        id_cols = c(doi, PMID, n_itc),
        names_from = questions,
        values_from = answer
    ) %>%
    left_join(
        ttt_mapped[, c("decision", "first", "second", "third", "car_t_cells")],
        by = c("treatment_name_ipd" = "decision")
    )

#TODO updater l'app pour pouvoir afficher à la fois les résultats pour les deux DF
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("dataset",
                        "Select data frame to summarize:",
                        c("wide_articles_df", "wide_comparisons_df"),
                        # selected = "outcome_type",
                        multiple = FALSE
            ),
            selectInput("var",
                "Select a variable to summarize:",
                names(output$dataset),
                selected = "outcome_type",
                multiple = FALSE
            ),
            selectInput("group_var",
                "Select a variable to group by:",
                names(output$dataset),
                selected = "adjusted_pval_char",
                multiple = FALSE
            ),
            textInput("filter", "Filter the data (e.g. hp > 100):", value = "hp > 100"),
            # checkboxGroupInput("functions", "Select some aggregation functions:",
            #                    c("Mean" = "mean", "Median" = "median", "Standard Deviation" = "sd"),
            #                    selected = c("mean", "median"))
        ),
        # mainPanel(verbatimTextOutput("summary"))
        mainPanel(tableOutput("summary"))
    )
)

server <- function(input, output) {

    df <- reactive(get(input$dataset))
    message("class of data is:", class(df))
    message(head(df()))
    message(str(df()))
    output$df <- df()

    tableby_formula <- reactive({
        formulize(
        y = input$var,
        x = input$group_var
    )
    })

    output$summary <- renderTable({
        # browser()
        # mtcars
        df() %>%
            #    select(-doi, -PMID)
            select(-treatment_name_ipd, -treatment_name_nonipd) %>%
            # filter(eval(parse(text = input$filter))) %>%
            tableby(,
            data = .
            ) %>%
            summary(text = "html") %>%
            as.data.frame()
        # }, sanitize.text.function = function(x) x)
    })
}


# unique(wide_comparisons_df$treatment_name_ipd)
# unique(wide_comparisons_df$treatment_name_ipd)

shinyApp(ui, server)
