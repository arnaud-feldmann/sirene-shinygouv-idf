library(readr)
library(here)
library(tidyr)
library(dplyr)

df_a88_a17 <- 
  read_csv(here("input", "a17_a88t.csv"),
           col_types = cols_only(A17 = col_character(),
                                 A88 = col_character(),
                                 A88_lbl = col_character())) %>%
  as.data.frame()
list_a88_a17 <-
  df_a88_a17 %>%
  nest(data = c(A88, A88_lbl)) %>%
  mutate(data = lapply(data,\(tbl) tbl %>% pull(A88) %>% setNames(tbl %>% pull(A88_lbl)))) %>%
  (\(tbl) tbl %>% pull(data) %>% setNames(tbl %>% pull(A17)))

list_tranches <-
  c(
    `1 ou 2 salariés` = "01",
    `3 à 5 salariés` = "02",
    `6 à 9 salariés` = "03",
    `10 à 19 salariés` = "11",
    `20 à 49 salariés` = "12",
    `50 à 99 salariés` = "21",
    `100 à 199 salariés` = "22",
    `200 à 249 salariés` = "31",
    `250 à 499 salariés` = "32",
    `500 à 999 salariés` = "41",
    `1 000 à 1 999 salariés` = "42",
    `2 000 à 4 999 salariés` = "51",
    `5 000 à 9 999 salariés` = "52",
    `5 000 à 9 999 salariés` = "53"
  )
df_tranches <-
  tibble(tranche_lbl = names(list_tranches),
         tranche = list_tranches) %>%
  as.data.frame()

dir.create(here("..", "app", "tables"), showWarnings = FALSE)
saveRDS(df_a88_a17, here("..", "app", "tables", "df_a88_a17.Rds"))
saveRDS(list_a88_a17, here("..", "app", "tables", "list_a88_a17.Rds"))
saveRDS(df_tranches, here("..", "app", "tables", "df_tranches.Rds"))
saveRDS(list_tranches, here("..", "app", "tables", "list_tranches.Rds"))
