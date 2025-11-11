library(tidyverse)
library(forecast)
library(tseries)
library(gt)


# Deklaracja funkcji

# Normalizacja min-max
minmax <- function(x, type = "stymulanta") {
  min_val = min(x)
  max_val = max(x)
  if(type == "stymulanta") {
    return((x - min_val) / (max_val - min_val))
  } else if(type == "destymulanta") {
    return((max_val - x) / (max_val - min_val))
  }
}

# Błędy prognoz wygasłych
metrics <- function(actual, pred) {
  me = mean(pred - actual)
  mpe = 100 * mean((pred - actual) / actual)
  mae = mean(abs(pred - actual))
  mape = 100 * mean(abs((pred - actual) / actual))
  rmse = sqrt(mean((pred - actual) ^ 2))
  rmspe = 100 * sqrt(mean(((pred - actual) / actual) ^ 2))
  result = data.frame(ME = me, MPE = mpe,
                      MAE = mae, MAPE = mape,
                      RMSE = rmse, RMSPE = rmspe)
  return(result)
}

# Optymalizowanie modeli ARIMA i SARIMA za pomocą RMSPE
create_arima_models <- function(x, actual) {
  parametre_range <- 0:2
  n <- length(parametre_range) ^ 4 - 1
  result <- tibble(p = rep(NA, n),
                   q = rep(NA, n),
                   P = rep(NA, n),
                   Q = rep(NA, n),
                   RMSPE = rep(NA, n))
  i <- 0
  cat("Rozpoczynam", n, "iteracji\n")
  for(p in parametre_range) {
    for(q in parametre_range) {
      for(P in parametre_range) {
        for(Q in parametre_range) {
          if(p + q + P + Q == 0) {
            next
          }
          i <- i + 1
          current_model <- try(Arima(x, order = c(p, 0, q), seasonal = c(P, 0, Q)), silent = TRUE)
          if(inherits(current_model, "try-error")) {
            result[i, "p"] <- p
            result[i, "q"] <- q
            result[i, "P"] <- P
            result[i, "Q"] <- Q
            cat("Iteracja", i, "nie powiodła się.\n")
            next
          } else {
            current_forecast <- forecast(current_model, h = 24)
            current_metrics <- metrics(actual, current_forecast$mean)
            result[i, "p"] <- p
            result[i, "q"] <- q
            result[i, "P"] <- P
            result[i, "Q"] <- Q
            result[i, "RMSPE"] <- current_metrics[, "RMSPE"]
            cat("Iteracja", i, "powiodła się.\n")
          }
        }
      }
    }
  }
  return(result)
}


# Wczytanie danych

dane <- read_csv("data/dane.csv")
dane_bez_dat <- dane %>%
  select(-date)


# Standaryzacja

dane_std <- dane_bez_dat %>%
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

dane_unit <- dane_bez_dat %>%
  mutate(
    across(
      .cols = all_of(names(zmienne)),
      .fns = ~ minmax(., type = zmienne[[cur_column()]])
      )
    )

sumy_kolumn <- colSums(dane_bez_dat)
p_ij <- sweep(dane_bez_dat, 2, sumy_kolumn, FUN = "/")
p_ij[p_ij == 0] <- 1e-10
E_j <- - (1 / log(nrow(dane_bez_dat))) * colSums(p_ij * log(p_ij))
d_j <- 1 - E_j
w_j <- d_j / sum(d_j)
names(w_j) <- colnames(dane_bez_dat)

dane_ewm <- dane_unit

for(i in 1:ncol(dane_unit)) {
  dane_ewm[, i] <- dane_unit[, i] * w_j[i]
}

indeks_ewm <- rowSums(dane_ewm)


# Analiza głównych składowych

pca_result <- dane_std %>%
  select(all_of(variables)) %>%
  prcomp(center = FALSE, scale. = FALSE)


# Stworzenie mierników

dane_std <- dane_std %>%
  mutate(miernik_ew = rowMeans(pick(all_of(variables))),
         miernik_pca = pca_result$x[, 1],
         miernik_ewm = indeks_ewm,
         date = dane$date)


# Train test split

train <- slice_head(dane_std, n = 108)
test <- slice_tail(dane_std, n = 24)


# Mierniki jako szeregi czasowe

miernik_ew <- ts(train$miernik_ew, start = c(2014, 1), frequency = 12)
miernik_pca <- ts(train$miernik_pca, start = c(2014, 1), frequency = 12)
miernik_ewm <- ts(train$miernik_ewm, start = c(2014, 1), frequency = 12)


# Testy Ljung-Boxa dla mierników

Box.test(miernik_ew, type = "Ljung-Box")
Box.test(miernik_pca, type = "Ljung-Box")
Box.test(miernik_ewm, type = "Ljung-Box")


# Testy stacjonarności dla mierników

adf.test(miernik_ew)
adf.test(miernik_pca)
adf.test(miernik_ewm)


# Budowanie prognoz metodą Holta-Wintersa

fc_ew_hw <- forecast(HoltWinters(miernik_ew), h = 24)
fc_pca_hw <- forecast(HoltWinters(miernik_pca), h = 24)
fc_ewm_hw <- forecast(HoltWinters(miernik_ewm), h = 24)


# Znalezienie najlepszego modelu z modeli ARIMA i SARIMA na podstawie RMSPE

models_ew <- create_arima_models(miernik_ew, test$miernik_ew)
models_pca <- create_arima_models(miernik_pca, test$miernik_pca)
models_ewm <- create_arima_models(miernik_ewm, test$miernik_ewm)


# Najlepsze modele ARIMA/SARIMA dla każdego miernika

models_ew %>%
  slice_min(RMSPE)

models_pca %>%
  slice_min(RMSPE)

models_ewm %>%
  slice_min(RMSPE)


# Zbudowanie prognoz dla najlepszych modeli ARIMA/SARIMA

fc_ew_arima <- forecast(Arima(miernik_ew, order = c(0, 0, 1), seasonal = c(0, 0, 4)), h = 24) # 001 000 / 001 004
fc_pca_arima <- forecast(Arima(miernik_pca, order = c(0, 0, 1), seasonal = c(2, 0, 4)), h = 24) # 002 200 / 001 204
fc_ewm_arima <- forecast(Arima(miernik_ewm, order = c(0, 0, 4), seasonal = c(1, 0, 1)), h = 24) # 002 101 / 004 101


# Błędy prognoz dla wszystkich metod

met_ew_hw <- round(metrics(test$miernik_ew, fc_ew_hw$mean), 3)
met_ew_arima <- round(metrics(test$miernik_ew, fc_ew_arima$mean), 3)

met_pca_hw <- round(metrics(test$miernik_pca, fc_pca_hw$mean), 3)
met_pca_arima <- round(metrics(test$miernik_pca, fc_pca_arima$mean), 3)

met_ewm_hw <- round(metrics(test$miernik_ewm, fc_ewm_hw$mean), 3)
met_ewm_arima <- round(metrics(test$miernik_ewm, fc_ewm_arima$mean), 3)


# Podsumowanie prognoz

podsumowanie_prognoz <- bind_rows(met_ew_hw, met_ew_arima,
                                  met_pca_hw, met_pca_arima,
                                  met_ewm_hw, met_ewm_arima) %>%
  mutate(metoda = c("EW Holt Winters", "EW ARIMA",
                    "PCA Holt Winters", "PCA ARIMA",
                    "EWM Holt Winters", "EWM ARIMA")) %>%
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


# Dane do wizualizacji

prognozy <- tibble(
  date = test$date,
  prognoza_ew_hw = fc_ew_hw$mean,
  prognoza_ew_arima = fc_ew_arima$mean,
  prognoza_pca_hw = fc_pca_hw$mean,
  prognoza_pca_arima = fc_pca_arima$mean,
  prognoza_ewm_hw = fc_ewm_hw$mean,
  prognoza_ewm_arima = fc_ewm_arima$mean
)

dane_wizualizacje_bezposrednie <- dane_std %>%
  select(date, miernik_ew, miernik_pca, miernik_ewm) %>%
  full_join(prognozy, by = "date") %>%
  pivot_longer(cols = c(-date))


write_csv(dane_wizualizacje_bezposrednie, file = "data/dane_wizualizacje_bezposrednie.csv")

