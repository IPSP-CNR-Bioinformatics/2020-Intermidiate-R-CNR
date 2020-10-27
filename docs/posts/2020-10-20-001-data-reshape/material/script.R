################################################################################
# Scritp for R intermidiate course
# Data reshaping lesson
################################################################################

# LOAD LIBRARIES
library(tidyverse)
library(gt)

# READ DATA IN
raw  <- read_csv("raw.csv") %>%
            select(Sample, Cq) %>%
            mutate(Cq = str_replace(.$Cq, ",", ".") %>% as.numeric(),
                   Sample = str_c("T", Sample)) %>%
            separate(Sample, sep = "-", extra = "merge", c("Sample", "Primer")) 


df <- raw %>%
    group_by(Sample, Primer) %>%
    summarise_at(vars(Cq), list(Max = max, Min = min), na.rm = FALSE) %>%
    mutate(Exp = case_when(
                          Max < 30 & Min < 30 ~ 1,
                          TRUE ~ 0),
           Max = replace(Max, is.infinite(Max), 40),
           Min = replace(Min, is.infinite(Min), 40))

df2 <- df %>%
    pivot_wider(id_cols = -(Max:Min),
                names_from = Primer,
                values_from = Exp) %>%
    mutate(order = as.numeric(str_remove(Sample, "T"))) %>%
    arrange(order) %>%
    select(-order) %>%
    ungroup

df2 %>%
    mutate(pippo = rowSums(select(., -Sample))) %>%
    gt() %>%
    tab_header(title = "Virus detection in qRT-PCR") %>%
    tab_spanner(
    label = "Primers",
    columns = vars(`T41-5`, `T41-6`, `T41-7`, `T41-8`)
    ) %>%
    tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
    ) %>%
    tab_footnote(
    footnote = "Not present in any sample",
    locations = cells_column_labels(columns = vars(`T41-6`))
    ) %>%
    opt_footnote_marks(marks = "letters") %>%
    tab_style(
    style = list(
      cell_fill(color = "#F9E3D6")
      ),
    locations = cells_body(
      rows = pippo > 0)
    ) %>% 
    cols_hide(
              columns = vars(pippo)
    )
