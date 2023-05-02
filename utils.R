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
            idade_vitima = as.numeric(str_sub(IDADE, 2, 3)),
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
            modal_vitima, data_ocorrencia, idade_vitima, sexo_vitima,
            escolaridade_vitima, raca_vitima, ocup_cbo_vitima, cod_municipio
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