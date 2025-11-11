library(tidyverse)

dane_wizualizacje_zmiennych <- read_csv("data/dane.csv") %>%
  pivot_longer(cols = c(-date))
dane_wizualizacje_bezposrednie <- read_csv("data/dane_wizualizacje_bezposrednie.csv")
dane_wizualizacje_posrednie <- read_csv("data/dane_wizualizacje_posrednie.csv")


# Wykresy pierwszych pięciu zmiennych

wykres_zmiennych_1 <- dane_wizualizacje_zmiennych %>%
  filter(name %in% c("hdd", "import_gazu", "import_ropy", "zuzycie_energii", "zuzycie_gazu")) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(linewidth = 0.75, colour = "grey15") +
  facet_wrap(~ name, scales = "free", ncol = 2,
             labeller = labeller(
               name = c("hdd" = "Liczba stopniodni grzania (hdd)",
                        "import_gazu" = "Import gazu w mln m^3 (import_gazu)",
                        "import_ropy" = "Import ropy w tys. ton (import_ropy)",
                        "zuzycie_energii" = "Zużycie energii elektrycznej w GWh (zuzycie_energii)",
                        "zuzycie_gazu" = "Zużycie gazu w mln m^3 (zuzycie_gazu)"))) +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%Y") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_zmiennych_1.png", wykres_zmiennych_1)


# Wykresy pozostałych sześciu zmiennych

wykres_zmiennych_2 <- dane_wizualizacje_zmiennych %>%
  filter(name %in% c("dostawy_prod_naft", "dostawy_wegla", "cena_akcji_orlen", "cena_akcji_energa", "cena_gazu", "cena_ropy")) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(linewidth = 0.75, colour = "grey15") +
  facet_wrap(~ name, scales = "free", ncol = 2,
             labeller = labeller(
               name = c("dostawy_prod_naft" = "Dostawy produktów naftowych\nw tys. ton (dostawy_prod_naft)",
                        "dostawy_wegla" = "Dostawy węgla kamiennego w tys. ton (dostawy_wegla)",
                        "cena_akcji_orlen" = "Cena akcji ORLEN w zł (cena_akcji_orlen)",
                        "cena_akcji_energa" = "Cena akcji ENERGA SA w zł (cena_akcji_energa)",
                        "cena_gazu" = "Cena gazu w USD za mln brytyjskich\njednostek cieplnych (cena_gazu)",
                        "cena_ropy" = "Cena ropy w USD za baryłkę (cena_ropy)"))) +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%Y") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_zmiennych_2.png", wykres_zmiennych_2)


# Prognoza bezpośrednia indeksu EW (Holt_winters)

wykres_ew_hw_bezp <- dane_wizualizacje_bezposrednie %>%
  filter(name %in% c("miernik_ew", "prognoza_ew_hw")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_ew" = "grey15",
               "prognoza_ew_hw" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_ew" = "solid",
               "prognoza_ew_hw" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza bezpośrednia indeksu EW (Holt-Winters)",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_ew_hw_bezp.png", wykres_ew_hw_bezp)


# Prognoza bezpośrednia indeksu EW (ARIMA)

wykres_ew_arima_bezp <- dane_wizualizacje_bezposrednie %>%
  filter(name %in% c("miernik_ew", "prognoza_ew_arima")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_ew" = "grey15",
               "prognoza_ew_arima" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_ew" = "solid",
               "prognoza_ew_arima" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza bezpośrednia indeksu EW (ARIMA)",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_ew_arima_bezp.png", wykres_ew_arima_bezp)


# Prognoza bezpośrednia indeksu PCA (Holt-Winters)

wykres_pca_hw_bezp <- dane_wizualizacje_bezposrednie %>%
  filter(name %in% c("miernik_pca", "prognoza_pca_hw")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_pca" = "grey15",
               "prognoza_pca_hw" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_pca" = "solid",
               "prognoza_pca_hw" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza bezpośrednia indeksu PCA (Holt-Winters)",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_pca_hw_bezp.png", wykres_pca_hw_bezp)


# Prognoza bezpośrednia indeksu PCA (ARIMA)

wykres_pca_arima_bezp <- dane_wizualizacje_bezposrednie %>%
  filter(name %in% c("miernik_pca", "prognoza_pca_arima")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_pca" = "grey15",
               "prognoza_pca_arima" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_pca" = "solid",
               "prognoza_pca_arima" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza bezpośrednia indeksu PCA (ARIMA)",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_pca_arima_bezp.png", wykres_pca_arima_bezp)


# Prognoza bezpośrednia indeksu EWM (Holt-Winters)

wykres_ewm_hw_bezp <- dane_wizualizacje_bezposrednie %>%
  filter(name %in% c("miernik_ewm", "prognoza_ewm_hw")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_ewm" = "grey15",
               "prognoza_ewm_hw" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_ewm" = "solid",
               "prognoza_ewm_hw" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza bezpośrednia indeksu EWM (Holt-Winters)",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_ewm_hw_bezp.png", wykres_ewm_hw_bezp)


# Prognoza bezpośrednia indeksu EWM (ARIMA)

wykres_ewm_arima_bezp <- dane_wizualizacje_bezposrednie %>%
  filter(name %in% c("miernik_ewm", "prognoza_ewm_arima")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_ewm" = "grey15",
               "prognoza_ewm_arima" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_ewm" = "solid",
               "prognoza_ewm_arima" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza bezpośrednia indeksu EWM (ARIMA)",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_ewm_arima_bezp.png", wykres_ewm_arima_bezp)


# Prognoza pośrednia indeksu EW

wykres_ew_pos <- dane_wizualizacje_posrednie %>%
  filter(name %in% c("miernik_ew", "prognoza_ew_pos")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_ew" = "grey15",
               "prognoza_ew_pos" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_ew" = "solid",
               "prognoza_ew_pos" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza pośrednia indeksu EW",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_ew_pos.png", wykres_ew_pos)


# Prognoza pośrednia indeksu PCA

wykres_pca_pos <- dane_wizualizacje_posrednie %>%
  filter(name %in% c("miernik_pca", "prognoza_pca_pos")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_pca" = "grey15",
               "prognoza_pca_pos" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_pca" = "solid",
               "prognoza_pca_pos" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza pośrednia indeksu PCA",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_pca_pos.png", wykres_pca_pos)


# Prognoza pośrednia indeksu EWM

wykres_ewm_pos <- dane_wizualizacje_posrednie %>%
  filter(name %in% c("miernik_ewm", "prognoza_ewm_pos")) %>%
  ggplot(aes(x = date, y = value, colour = name, linetype = name)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(
    name = NULL,
    values = c("miernik_ewm" = "grey15",
               "prognoza_ewm_pos" = "blue"),
    labels = c("Obserwacje", "Prognoza")) +
  scale_linetype_manual(
    name = NULL,
    values = c("miernik_ewm" = "solid",
               "prognoza_ewm_pos" = "dotted"),
    labels = c("Obserwacje", "Prognoza")
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(title = "Prognoza pośrednia indeksu EWM",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "black"))

ggsave("figures/wykres_ewm_pos.png", wykres_ewm_pos)
