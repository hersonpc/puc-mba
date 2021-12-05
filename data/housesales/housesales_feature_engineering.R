#' =============================================================================
#' Author.: Herson Melo
#' Date...: 2021-12-04
#' Subject: Engenharia de características para o dataset de venda de imóveis
#' =============================================================================
library(tidyverse)

#' Feature Engineering
#' -------------------
#' Criando novas propriedades para o modelo
feature_engineering <- function(data) {
    tmp <-
        data %>% 
        mutate(
            built_percent = sqft_living / sqft_lot,
            idade_imovel = lubridate::year(lubridate::now()) - yr_built,
            idade_ultima_reforma = if_else(yr_renovated > 0, lubridate::year(lubridate::now()) - yr_renovated, 0)
        )
    
    estatisticas_zipcode <- 
        tmp %>% 
        group_by(zipcode) %>% 
        summarise(zip_count = n(),
                  zip_avg_price = mean(price, na.rm = TRUE),
                  zip_median_price = median(price, na.rm = TRUE),
                  zip_min_price = min(price, na.rm = TRUE),
                  zip_max_price = max(price, na.rm = TRUE),
                  zip_avg_sqft_living = mean(sqft_living, na.rm = TRUE),
                  zip_avg_built_percent = mean(built_percent, na.rm = TRUE)
        )
    
    output <-
        tmp %>% 
        merge(estatisticas_zipcode)

    return(output)
}

# Tratando os dados de treino
url_train <- "https://raw.githubusercontent.com/jpvmm/Fasam_Analytics/master/train.csv"
df_train <- readr::read_csv(url_train)
feature_engineering(df_train) %>% 
    write.csv(file = "./housesales_train.csv")

# Tratando os dados de teste
url_valid <- "https://raw.githubusercontent.com/jpvmm/Fasam_Analytics/master/valid.csv"
df_valid <- readr::read_csv(url_valid)
feature_engineering(df_valid) %>%
    write.csv(file = "./housesales_valid.csv")

