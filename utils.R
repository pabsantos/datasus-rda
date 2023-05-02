read_datasus_preliminar <- function(url) {
    read_csv2(
        url,
        col_select = c(
            "CAUSABAS", "CODMUNOCOR", "DTOBITO", "IDADE",
            "SEXO", "RACACOR", "ESC", "OCUP"
        ),
        col_types = cols(.default = "f")
        
    )
}

join_datasus_db <- function() {
    bind_rows(datasus_causas_externas, datasus_preliminar)
}

arrange_datasus_sim <- function() {
    datasus_total |> 
        as_tibble() |> 
        mutate(cod_modal = str_sub(CAUSABAS, 1, 2)) |> 
        filter(cod_modal %in% paste0("V", seq(0, 8, 1))) |> 
        mutate(
            modal_vitima = case_match(
                cod_modal,
                "V0" ~ "Pedestre",
                "V1" ~ "Bicicleta",
                c("V2", "V3") ~ "Motocicleta",
                c("V4", "V5") ~ "Automóvel",
                "V6" ~ "Caminhão",
                "V7" ~ "Ônibus",
                "V8" ~ "Outros"
            ),
            data_ocorrencia = dmy(DTOBITO),
            ano_ocorrencia = year(data_ocorrencia),
            idade_vitima = as.numeric(str_sub(IDADE, 2, 3)),
            faixa_etaria_vitima = cut(
                idade_vitima,
                breaks = c(
                    0, 5, 10, 15, 20, 25, 30, 35, 40,
                    45, 50, 55, 60, 65, 70, 75, 80, 100
                ),
                labels = c(
                    "0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
                    "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", 
                    "35 a 39 anos", "40 a 44 anos", "45 a 49 anos", 
                    "50 a 54 anos", "55 a 59 anos", "60 a 64 anos", 
                    "65 a 69 anos", "70 a 74 anos", "75 a 79 anos",
                    "Mais de 80 anos"
                ),
                include.lowest = TRUE,
                right = FALSE
            ),
            sexo_vitima = case_match(
                SEXO,
                "1" ~ "Masculino",
                "2" ~ "Feminino"
            ),
            raca_vitima = case_match(
                RACACOR,
                "1" ~ "Branca",
                "2" ~ "Preta",
                "3" ~ "Amarela",
                "4" ~ "Parda",
                "5" ~ "Indígena"
            ),
            escolaridade_vitima = case_match(
                ESC,
                "1" ~ "Nenhuma",
                "2" ~ "de 1 a 3 anos",
                "3" ~ "de 4 a 7 anos",
                "4" ~ "de 8 a 11 anos",
                "5" ~ "12 anos ou mais"
            ),
            cod_municipio = str_sub(as.character(CODMUNOCOR), 1, 6),
            ocup_cbo_vitima = as.character(OCUP)
        ) |> 
        select(
            modal_vitima, data_ocorrencia, ano_ocorrencia, idade_vitima, 
            faixa_etaria_vitima, sexo_vitima, escolaridade_vitima, raca_vitima,
            ocup_cbo_vitima, cod_municipio
        )
}

create_lista_municipios <- function() {
    ibge_municipios |> 
        clean_names() |> 
        as_tibble() |> 
        mutate(
            cod_regiao = str_sub(uf, 1, 1),
            nome_regiao = case_match(
                cod_regiao,
                "1" ~ "Norte",
                "2" ~ "Nordeste",
                "3" ~ "Sudeste",
                "4" ~ "Sul",
                "5" ~ "Centro-Oeste"
            ),
            cod_municipio = str_sub(
                as.character(codigo_municipio_completo), 1, 6
            )
        ) |> 
        select(cod_municipio, nome_municipio, nome_uf, nome_regiao)
}

join_datasus_municipios <- function() {
    datasus_transito_sim |> left_join(lista_municipios, by = "cod_municipio")
}