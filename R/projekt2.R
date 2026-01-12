library(tidyverse)
library(corrplot)

dane <- read_csv("data/dane.csv")

dane_bez_dat <- dane %>%
  select(-date)

dane_14_22 <- dane %>%
  filter(date < "2023-01-01")

dane_23_24 <- dane %>%
  filter(date >= "2023-01-01")

correlations <- cor(dane_bez_dat, use = "complete.obs", method = "pearson")

png("figures/macierz_korelacji.png", width = 800, height = 800, res = 150)
cor_matrix <- corrplot(
  corr = correlations,
  method = "color",
  type = "full",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.75,
  number.cex = 0.7
)
dev.off()


scen_opt <- dane_23_24 %>%
  mutate(hdd = hdd * 0.9,
         import_gazu = import_gazu * 1.1,
         import_ropy = import_ropy * 1.1,
         zuzycie_energii = zuzycie_energii * 0.9,
         zuzycie_gazu = zuzycie_gazu * 0.9,
         dostawy_prod_naft = dostawy_prod_naft * 1.1,
         dostawy_wegla = dostawy_wegla * 1.1,
         cena_akcji_orlen = cena_akcji_orlen * 1.1,
         cena_akcji_energa = cena_akcji_energa * 1.1,
         cena_gazu = cena_gazu * 0.9,
         cena_ropy = cena_ropy * 0.9)

scen_pes <- dane_23_24 %>%
  mutate(hdd = hdd * 1.1,
         import_gazu = import_gazu * 0.9,
         import_ropy = import_ropy * 0.9,
         zuzycie_energii = zuzycie_energii * 1.1,
         zuzycie_gazu = zuzycie_gazu * 1.1,
         dostawy_prod_naft = dostawy_prod_naft * 0.9,
         dostawy_wegla = dostawy_wegla * 0.9,
         cena_akcji_orlen = cena_akcji_orlen * 0.9,
         cena_akcji_energa = cena_akcji_energa * 0.9,
         cena_gazu = cena_gazu * 1.1,
         cena_ropy = cena_ropy * 1.1)

dane_bez_dat_opt <- dane_14_22 %>%
  bind_rows(scen_opt) %>%
  select(-date)

dane_bez_dat_pes <- dane_14_22 %>%
  bind_rows(scen_pes) %>%
  select(-date)


# Standaryzacja

dane_std_opt <- dane_bez_dat_opt %>%
  sapply(scale) %>%
  as_tibble()

dane_std_pes <- dane_bez_dat_pes %>%
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

dane_unit_opt <- dane_bez_dat_opt %>%
  mutate(
    across(
      .cols = all_of(names(zmienne)),
      .fns = ~ minmax(., type = zmienne[[cur_column()]])
    )
  )

dane_unit_pes <- dane_bez_dat_pes %>%
  mutate(
    across(
      .cols = all_of(names(zmienne)),
      .fns = ~ minmax(., type = zmienne[[cur_column()]])
    )
  )

sumy_kolumn_opt <- colSums(dane_bez_dat_opt)
p_ij_opt <- sweep(dane_bez_dat_opt, 2, sumy_kolumn_opt, FUN = "/")
p_ij_opt[p_ij_opt == 0] <- 1e-10
E_j_opt <- - (1 / log(nrow(dane_bez_dat_opt))) * colSums(p_ij_opt * log(p_ij_opt))
d_j_opt <- 1 - E_j_opt
w_j_opt <- d_j_opt / sum(d_j_opt)
names(w_j_opt) <- colnames(dane_bez_dat_opt)


sumy_kolumn_pes <- colSums(dane_bez_dat_pes)
p_ij_pes <- sweep(dane_bez_dat_pes, 2, sumy_kolumn_pes, FUN = "/")
p_ij_pes[p_ij_pes == 0] <- 1e-10
E_j_pes <- - (1 / log(nrow(dane_bez_dat_pes))) * colSums(p_ij_pes * log(p_ij_pes))
d_j_pes <- 1 - E_j_pes
w_j_pes <- d_j_pes / sum(d_j_pes)
names(w_j_pes) <- colnames(dane_bez_dat_pes)

dane_ewm_opt <- dane_unit_opt

dane_ewm_pes <- dane_unit_pes

for(i in 1:ncol(dane_unit_opt)) {
  dane_ewm_opt[, i] <- dane_unit_opt[, i] * w_j_opt[i]
}

for(i in 1:ncol(dane_unit_pes)) {
  dane_ewm_pes[, i] <- dane_unit_pes[, i] * w_j_pes[i]
}

indeks_ewm_opt <- rowSums(dane_ewm_opt)

indeks_ewm_pes <- rowSums(dane_ewm_pes)


# Analiza głównych składowych

pca_result_opt <- dane_std_opt %>%
  select(all_of(variables)) %>%
  prcomp(center = FALSE, scale. = FALSE)

pca_result_pes <- dane_std_pes %>%
  select(all_of(variables)) %>%
  prcomp(center = FALSE, scale. = FALSE)


# Stworzenie mierników

dane_std_opt <- dane_std_opt %>%
  mutate(miernik_ew = rowMeans(pick(all_of(variables))),
         miernik_pca = pca_result_opt$x[, 1],
         miernik_ewm = indeks_ewm_opt,
         date = dane$date)

dane_scen_opt <- dane_std_opt %>%
  mutate(prognoza_ew_opt = miernik_ew,
         prognoza_pca_opt = miernik_pca,
         prognoza_ewm_opt = miernik_ewm) %>%
  select(date, prognoza_ew_opt, prognoza_pca_opt, prognoza_ewm_opt) %>%
  filter(date >= "2023-01-01")


dane_std_pes <- dane_std_pes %>%
  mutate(miernik_ew = rowMeans(pick(all_of(variables))),
         miernik_pca = pca_result_pes$x[, 1],
         miernik_ewm = indeks_ewm_pes,
         date = dane$date)

dane_scen_pes <- dane_std_pes %>%
  mutate(prognoza_ew_pes = miernik_ew,
         prognoza_pca_pes = miernik_pca,
         prognoza_ewm_pes = miernik_ewm) %>%
  select(date, prognoza_ew_pes, prognoza_pca_pes, prognoza_ewm_pes) %>%
  filter(date >= "2023-01-01")


dane_wizualizacje_opt <- dane_std %>%
  select(date, miernik_ew, miernik_pca, miernik_ewm) %>%
  full_join(dane_scen_opt, by = "date") %>%
  pivot_longer(cols = c(-date))


dane_wizualizacje_pes <- dane_std %>%
  select(date, miernik_ew, miernik_pca, miernik_ewm) %>%
  full_join(dane_scen_pes, by = "date") %>%
  pivot_longer(cols = c(-date))



# Prognoza optymistyczna indeksu EWM

wykres_ewm_opt <- dane_wizualizacje_opt %>%
  filter(name %in% c("miernik_ewm", "prognoza_ewm_opt")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_ewm" = "grey15",
               "prognoza_ewm_opt" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_ewm" = "solid",
               "prognoza_ewm_opt" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza indeksu EWM w wariancie optymistycznym",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_ewm_opt.png", wykres_ewm_opt)



# Prognoza pesymistyczna indeksu EWM

wykres_ewm_pes <- dane_wizualizacje_pes %>%
  filter(name %in% c("miernik_ewm", "prognoza_ewm_pes")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_ewm" = "grey15",
               "prognoza_ewm_pes" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_ewm" = "solid",
               "prognoza_ewm_pes" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza indeksu EWM w wariancie pesymistycznym",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_ewm_pes.png", wykres_ewm_pes)

