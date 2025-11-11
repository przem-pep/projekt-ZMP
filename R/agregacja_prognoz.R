library(tidyverse)


prognozy_zmiennych <- tibble(
  date = seq.Date(as.Date("2023-01-01"), as.Date("2024-12-01"), "1 month"),
  hdd = prog_hdd$mean,
  import_gazu = prog_ig$mean,
  import_ropy = prog_ir$mean,
  zuzycie_energii = prog_ze$mean,
  zuzycie_gazu = prog_zg$mean,
  dostawy_prod_naft = prog_dpn$mean,
  dostawy_wegla = prog_wd$mean,
  cena_akcji_orlen = prog_co$mean,
  cena_akcji_energa = prog_ce$mean,
  cena_gazu = prog_cg$mean,
  cena_ropy = prog_cr$mean
)


dane <- read_csv("data/dane.csv")
dane_bez_dat_pos <- dane %>%
  filter(date < "2023-01-01") %>%
  bind_rows(prognozy_zmiennych) %>%
  select(-date)


# Standaryzacja

dane_std_pos <- dane_bez_dat_pos %>%
  sapply(scale) %>%
  as_tibble()


# Budowanie wskaźnika EWM

zmienne <- list(hdd = "destymulanta",
                import_gazu = "stymulanta",
                import_ropy = "stymulanta",
                zuzycie_energii = "destymulanta",
                zuzycie_gazu = "destymulanta",
                dostawy_prod_naft = "stymulanta",
                dostawy_wegla = "stymulanta",
                cena_akcji_orlen = "stymulanta",
                cena_akcji_energa = "stymulanta",
                cena_gazu = "destymulanta",
                cena_ropy = "destymulanta")

variables <- names(zmienne)

dane_unit_pos <- dane_bez_dat_pos %>%
  mutate(
    across(
      .cols = all_of(names(zmienne)),
      .fns = ~ minmax(., type = zmienne[[cur_column()]])
    )
  )

sumy_kolumn_pos <- colSums(dane_bez_dat_pos)
p_ij_pos <- sweep(dane_bez_dat_pos, 2, sumy_kolumn_pos, FUN = "/")
p_ij_pos[p_ij_pos == 0] <- 1e-10
E_j_pos <- - (1 / log(nrow(dane_bez_dat_pos))) * colSums(p_ij_pos * log(p_ij_pos))
d_j_pos <- 1 - E_j_pos
w_j_pos <- d_j_pos / sum(d_j_pos)
names(w_j_pos) <- colnames(dane_bez_dat_pos)

dane_ewm_pos <- dane_unit_pos

for(i in 1:ncol(dane_unit_pos)) {
  dane_ewm_pos[, i] <- dane_unit_pos[, i] * w_j_pos[i]
}

indeks_ewm_pos <- rowSums(dane_ewm_pos)


# Analiza głównych składowych

pca_result_pos <- dane_std_pos %>%
  select(all_of(variables)) %>%
  prcomp(center = FALSE, scale. = FALSE)


# Stworzenie mierników

prognozy_agregowane <- dane_std_pos %>%
  mutate(miernik_ew = rowMeans(pick(all_of(variables))),
         miernik_pca = pca_result_pos$x[, 1],
         miernik_ewm = indeks_ewm_pos,
         date = dane$date) %>%
  mutate(prognoza_ew_pos = miernik_ew,
         prognoza_pca_pos = miernik_pca,
         prognoza_ewm_pos = miernik_ewm) %>%
  select(date, prognoza_ew_pos, prognoza_pca_pos, prognoza_ewm_pos) %>%
  filter(date >= "2023-01-01")


dane_wizualizacje_posrednie <- dane_std %>%
  select(date, miernik_ew, miernik_pca, miernik_ewm) %>%
  full_join(prognozy_agregowane, by = "date") %>%
  pivot_longer(cols = c(-date))

write_csv(dane_wizualizacje_posrednie, file = "data/dane_wizualizacje_posrednie.csv")

met_ew_pos <- round(metrics(test$miernik_ew, prognozy_agregowane$prognoza_ew_pos), 3)
met_pca_pos <- round(metrics(test$miernik_pca, prognozy_agregowane$prognoza_pca_pos), 3)
met_ewm_pos <- round(metrics(test$miernik_ewm, prognozy_agregowane$prognoza_ewm_pos), 3)



podsumowanie_prognoz <- bind_rows(met_ew_hw, met_ew_arima,
                                  met_pca_hw, met_pca_arima,
                                  met_ewm_hw, met_ewm_arima,
                                  met_ew_pos, met_pca_pos, met_ewm_pos) %>%
  mutate(metoda = c("Bezpośredni EW Holt Winters", "Bezpośredni EW ARIMA",
                    "Bezpośredni PCA Holt Winters", "Bezpośredni PCA ARIMA",
                    "Bezpośredni EWM Holt Winters", "Bezpośredni EWM ARIMA",
                    "Pośredni EW", "Pośredni PCA", "Pośredni EWM")) %>%
  select(metoda, everything())


podsumowanie_prognoz %>%
  gt() %>%
  tab_style(style = cell_text(color = "black"), locations = cells_body()) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "black", style = "solid", weight = px(2)),
    locations = cells_body()) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "black", style = "solid", weight = px(2)),
    locations = cells_column_labels()) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "black", style = "solid", weight = px(2)),
    locations = cells_row_groups()) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_column_labels())
