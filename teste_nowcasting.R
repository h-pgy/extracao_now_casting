library(readxl)
library(ggplot2)
library(NobBS)
library(dplyr)
library(zoo)

raw.2020 <- as.data.frame(read_xlsx("dados_teste_nowcasting.xlsx")) %>%
    mutate(dt_falecimento = as.Date(dt_falecimento),
           dt_cadastro = as.Date(dt_cadastro, "%Y-%m-%d %H:%M:%S"),
           delay = dt_cadastro - dt_falecimento) %>%
    filter(dt_falecimento >= as.Date("2020-01-01") & dt_falecimento <= Sys.Date())

## Alguns registros com data de óbito posterior à do registro
## Numero destes registros
raw.2020 %>% filter(delay<0) %>%
    select(id_dados_obito:dt_cadastro) %>%
    as.data.frame()
## N de registros eliminados
teste <- filter(raw.2020, delay>=0)
nrow(raw.2020) - nrow(teste)
## Distribuição de atrasos
table(as.integer(teste$delay))
## N de obitos por data de óbito
n.obitos <- teste %>%
    group_by(dt_falecimento) %>%
    summarise(N = n()) %>%
    as.data.frame()
## Nowcasting
teste.now <- NobBS( data = teste,
                   now = max(teste$dt_falecimento),
                   units = "1 day",
                   onset_date = "dt_falecimento",
                   report_date="dt_cadastro",
                   moving_window = 40)
## Plot
ggplot(teste.now$estimates, aes(onset_date, estimate)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha = 0.25) +
    geom_line(aes(dt_falecimento, N), data = n.obitos, col="blue") +
    xlab("Data do óbito") +
    ylab("N de óbitos") +
    theme_bw()

