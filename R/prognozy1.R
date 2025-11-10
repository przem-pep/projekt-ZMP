library(tidyverse)
library(forecast)
library(ggplot2)

dane <- read_csv("data/dane.csv")[, 1:6]

# standaryzacja
dane_scale <- dane %>%
  select(-date) %>%
  sapply(scale) %>%
  as_tibble()

dane_scale <- bind_cols(date = dane$date, dane_scale)

# szereg czasowy i podzial train / test
dane_mts <- ts(dane_scale[, 2:6], start = c(2014, 1), frequency = 12)
train <- window(dane_mts, end = c(2022, 12))
test <- window(dane_mts, start = c(2023, 1))

# auto (s)arima
model_auto <- auto.arima(train[, 1])
summary(model_auto)
prog_auto <- forecast(model_auto, h = 24)
print(prog_auto)
autoplot(prog_auto)

# auto (s)arima - lista modeli
modele_arima <- lapply(colnames(train), function(zmienna) {
  auto.arima(train[, zmienna])
})

names(modele_arima) <- colnames(train)
summary(modele_arima$hdd)

# auto (s)arima - lista prognoz
prognozy_arima <- lapply(modele_arima, function(model) {
  forecast(model, h = 24)
})

print(prognozy_arima)
autoplot(prognozy_arima$hdd)

# auto holt winters
model_hw <- ets(train[, 1], allow.multiplicative.trend = TRUE)
summary(model_hw)
prog_hw <- forecast(model_hw, h = 24)
print(prog_hw)
autoplot(prog_hw)

# auto holt winters - lista modeli
modele_holt <- lapply(colnames(train), function(zmienna) {
  ets(train[, zmienna], allow.multiplicative.trend = TRUE)
})

names(modele_holt) <- colnames(train)
summary(modele_holt$hdd)

# auto holt winters - lista prognoz
prognozy_holt <- lapply(modele_holt, function(model) {
  forecast(model, h = 24)
})

print(prognozy_holt)
autoplot(prognozy_holt$hdd)

# dopasowanie (s)arima wg RMSE ex post
best_arima <- function(train_data, test_data, variable) {
  train_set <- train_data[, variable]
  test_set <- test_data[, variable]
  forecast_horizon <- length(test_set)
  param_range <- 0:2
  results_list <- list()
  total_models <- length(param_range)^6
  count <- 0
  cat(paste("Rozpoczynam testowanie", total_models, "kombinacji modeli...\n"))
  for (p in param_range) {
    for (d in param_range) {
      for (q in param_range) {
        for (P in param_range) {
          for (D in param_range) {
            for (Q in param_range) {
              count <- count + 1
              if (count %% 50 == 0) {
                cat(paste("... Testuję model", count, "z", total_models, "\n"))
              }
              model_rmse <- tryCatch({
                fit <- Arima(train_set, 
                             order = c(p, d, q), 
                             seasonal = c(P, D, Q))
                fc <- forecast(fit, h = forecast_horizon)
                accuracy(fc, test_set)["Test set", "RMSE"]
              }, error = function(e) {
                NA 
              })
              if (!is.na(model_rmse)) {
                result_row <- data.frame(p, d, q, P, D, Q, RMSE = model_rmse)
                results_list[[length(results_list) + 1]] <- result_row
              }
            }
          }
        }
      }
    }
  }
  cat("Zakończono testowanie. Przetwarzanie wyników...\n")
  if (length(results_list) == 0) {
    warning("Nie udało się dopasować ani jednego modelu.")
    return(NULL)
  }
  all_results <- do.call(rbind, results_list)
  sorted_results <- all_results[order(all_results$RMSE), ]
  return(sorted_results)
}

top_arima <- best_arima(train, test, "hdd")

head(top_arima, 10)
head(filter(top_arima, P == 0 & D == 0 & Q == 0), 10)

# dopasowanie holta wintersa wg RMSE ex post
best_ets <- function(train_data, test_data, variable) {
  train_set <- train_data[, variable]
  test_set <- test_data[, variable]
  forecast_horizon <- length(test_set)
  error_types <- c("A", "M")
  trend_types <- c("N", "A", "M")
  season_types <- c("N", "A", "M")
  damped_types <- c(TRUE, FALSE)
  results_list <- list()
  count <- 0
  cat("Rozpoczynam testowanie kombinacji modeli ETS...\n")
  for (E in error_types) {
    for (T in trend_types) {
      for (S in season_types) {
        for (D_damped in damped_types) {
          if (T == "N" && D_damped == TRUE) {
            next
          }
          count <- count + 1
          model_code <- paste0(E, T, S)
          model_desc <- paste0(model_code, " (Damped: ", D_damped, ")")
          cat(paste0("... Testuję model ", count, ": ", model_desc, "\n"))
          model_rmse <- tryCatch({
            fit <- ets(train_set, 
                       model = model_code, 
                       damped = D_damped,
                       allow.multiplicative.trend = TRUE, 
                       restrict = FALSE)
            fc <- forecast(fit, h = forecast_horizon)
            accuracy(fc, test_set)["Test set", "RMSE"]
          }, error = function(e) {
            cat(paste0("     Model ", model_desc, " pominięty (błąd).\n"))
            NA 
          })
          if (!is.na(model_rmse)) {
            T_output <- T
            if (D_damped == TRUE) { 
              T_output <- paste0(T, "d") 
            }
            result_row <- data.frame(E = E, T = T_output, S = S, RMSE = model_rmse)
            results_list[[length(results_list) + 1]] <- result_row
          }
        }
      }
    }
  }
  cat("Zakończono testowanie. Przetwarzanie wyników...\n")
  if (length(results_list) == 0) {
    warning("Nie udało się dopasować ani jednego modelu.")
    return(NULL)
  }
  all_results <- do.call(rbind, results_list)
  sorted_results <- all_results[order(all_results$RMSE), ]
  return(sorted_results)
}

top_ets <- best_ets(train, test, "hdd")

print(top_ets)

# hdd - najlepsza (s)arima
hdd_arima <- best_arima(train, test, "hdd")

head(hdd_arima, 10)
head(filter(hdd_arima, P == 0 & D == 0 & Q == 0), 10)

# hdd - najlepszy holt winters
hdd_hw <- best_ets(train, test, "hdd")

print(hdd_hw)

# import_gazu - najlepsza (s)arima
ig_arima <- best_arima(train, test, "import_gazu")

head(ig_arima, 10)
head(filter(ig_arima, P == 0 & D == 0 & Q == 0), 10)

# import_gazu - najlepszy holt winters
ig_hw <- best_ets(train, test, "import_gazu")

print(ig_hw)

# import_ropy - najlepsza (s)arima
ir_arima <- best_arima(train, test, "import_ropy")

head(ir_arima, 10)
head(filter(ir_arima, P == 0 & D == 0 & Q == 0), 10)

# import_ropy - najlepszy holt winters
ir_hw <- best_ets(train, test, "import_ropy")

print(ir_hw)

# zuzycie_energii - najlepsza (s)arima
ze_arima <- best_arima(train, test, "zuzycie_energii")

head(ze_arima, 10)
head(filter(ze_arima, P == 0 & D == 0 & Q == 0), 10)

# zuzycie_energii - najlepszy holt winters
ze_hw <- best_ets(train, test, "zuzycie_energii")

print(ze_hw)

# zuzycie_gazu - najlepsza (s)arima
zg_arima <- best_arima(train, test, "zuzycie_gazu")

head(zg_arima, 10)
head(filter(zg_arima, P == 0 & D == 0 & Q == 0), 10)

# zuzycie_gazu - najlepszy holt winters
zg_hw <- best_ets(train, test, "zuzycie_gazu")

print(zg_hw)

# optymalne modele - tabela
optymalne_modele <- data.frame(
  model = c("sarima", "arima", "holt_winters"),
  hdd = c("(1,0,2)(0,1,1)*", "(2,0,2)(0,0,0)", "(A,N,A)"),
  import_gazu = c("(2,0,0)(1,0,1)", "(1,0,1)(0,0,0)*", "(A,Ad,N)"),
  import_ropy = c("(0,1,1)(2,0,0)", "(0,1,2)(0,0,0)*", "(A,N,N)"),
  zuzycie_energii = c("(2,2,1)(2,1,0)*", "(2,0,2)(0,0,0)", "(A,A,A)"),
  zuzycie_gazu = c("(2,2,0)(1,1,0)*", "(2,1,2)(0,0,0)", "(A,M,A)")
)

print(optymalne_modele)

# hdd - prognoza
model_hdd <- Arima(train[, "hdd"], order=c(1,0,2), seasonal=c(0,1,1))
summary(model_hdd)

prog_hdd <- forecast(model_hdd, h = 24)
print(prog_hdd)

autoplot(prog_hdd) +
  autolayer(test[, "hdd"], series = "rzeczywistosc") +
  autolayer(prog_hdd$mean, series = "prognoza")

bledy_hdd <- accuracy(prog_hdd, test[, "hdd"])
RMSPE_hdd <- bledy_hdd[2,2] / mean(abs(test[, "hdd"])) * 100
bledy_hdd <- cbind(bledy_hdd, RMSPE = c(NA, RMSPE_hdd))
print((bledy_hdd)[2,])

# import_gazu - prognoza
model_ig <- Arima(train[, "import_gazu"], order=c(1,0,1), seasonal=c(0,0,0))
summary(model_ig)

prog_ig <- forecast(model_ig, h = 24)
print(prog_ig)

autoplot(prog_ig) +
  autolayer(test[, "import_gazu"], series = "rzeczywistosc") +
  autolayer(prog_ig$mean, series = "prognoza")

bledy_ig <- accuracy(prog_ig, test[, "import_gazu"])
RMSPE_ig <- bledy_ig[2,2] / mean(abs(test[, "import_gazu"])) * 100
bledy_ig <- cbind(bledy_ig, RMSPE = c(NA, RMSPE_ig))
print((bledy_ig)[2,])

# import_ropy - prognoza
model_ir <- Arima(train[, "import_ropy"], order=c(0,1,2), seasonal=c(0,0,0))
summary(model_ir)

prog_ir <- forecast(model_ir, h = 24)
print(prog_ir)

autoplot(prog_ir) +
  autolayer(test[, "import_ropy"], series = "rzeczywistosc") +
  autolayer(prog_ir$mean, series = "prognoza")

bledy_ir <- accuracy(prog_ir, test[, "import_ropy"])
RMSPE_ir <- bledy_ir[2,2] / mean(abs(test[, "import_ropy"])) * 100
bledy_ir <- cbind(bledy_ir, RMSPE = c(NA, RMSPE_ir))
print((bledy_ir)[2,])

# zuzycie_energii - prognoza
model_ze <- Arima(train[, "zuzycie_energii"], order=c(2,2,1), seasonal=c(2,1,0))
summary(model_ze)

prog_ze <- forecast(model_ze, h = 24)
print(prog_ze)

autoplot(prog_ze) +
  autolayer(test[, "zuzycie_energii"], series = "rzeczywistosc") +
  autolayer(prog_ze$mean, series = "prognoza")

bledy_ze <- accuracy(prog_ze, test[, "zuzycie_energii"])
RMSPE_ze <- bledy_ze[2,2] / mean(abs(test[, "zuzycie_energii"])) * 100
bledy_ze <- cbind(bledy_ze, RMSPE = c(NA, RMSPE_ze))
print((bledy_ze)[2,])

# zuzycie gazu - prognoza
model_zg <- Arima(train[, "zuzycie_gazu"], order=c(2,2,0), seasonal=c(1,1,0))
summary(model_zg)

prog_zg <- forecast(model_zg, h = 24)
print(prog_zg)

autoplot(prog_zg) +
  autolayer(test[, "zuzycie_gazu"], series = "rzeczywistosc") +
  autolayer(prog_zg$mean, series = "prognoza")

bledy_zg <- accuracy(prog_zg, test[, "zuzycie_gazu"])
RMSPE_zg <- bledy_zg[2,2] / mean(abs(test[, "zuzycie_gazu"])) * 100
bledy_zg <- cbind(bledy_zg, RMSPE = c(NA, RMSPE_zg))
print((bledy_zg)[2,])



