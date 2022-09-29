p_load(tidyverse, combinat)

# Lista nomeada por país das linhas da tabela de seleções
lista_selecoes <- map(1:32, ~selecoes[.x,])
names(lista_selecoes) = selecoes$PAIS

obter_pares_selecoes <- function(df_selecoes){
  # Pares de seleções
  pares_selecoes <- combn(df_selecoes$PAIS, 2)
  
  selecoes_A <- bind_rows(lista_selecoes[pares_selecoes[1,]])
  colnames(selecoes_A) <- str_c(colnames(selecoes_A), "_A")
  
  selecoes_B <- bind_rows(lista_selecoes[pares_selecoes[2,]])
  colnames(selecoes_B) <- str_c(colnames(selecoes_B), "_B")
  
  # Pares de seleções e respectivas estatísticas
  bind_cols(selecoes_A, selecoes_B) %>%
    select(PAIS_A, PAIS_B, everything())
}

# Filtra apenas os jogos entre equipes do mesmo grupo
filtrar_dentre_grupos <- function(x){
  x %>% filter(GRUPO_A == GRUPO_B)
}

calcular_lambdas <- function(x) {
  x %>%
    mutate(
      LAMBDA_AB = (GOLS_FEITOS_A/JOGOS_A + GOLS_SOFRIDOS_B/JOGOS_B)/2,
      LAMBDA_BA = (GOLS_FEITOS_B/JOGOS_B + GOLS_SOFRIDOS_A/JOGOS_A)/2) %>%
    select(PAIS_A, PAIS_B, GRUPO_A, GRUPO_B, LAMBDA_AB, LAMBDA_BA)
}

simular_placares <- function(x, seed=NULL){
  set.seed(seed)
  x %>%
    mutate(GOLS_A = rpois(n(), LAMBDA_AB),
           GOLS_B = rpois(n(), LAMBDA_BA)) %>%
    select(-LAMBDA_AB, -LAMBDA_BA)
}

calcular_pontuacoes <- function(x) {
  x %>%
    mutate(PONTUACAO_A = case_when(GOLS_A > GOLS_B ~ 3,
                                   GOLS_A == GOLS_B ~ 1,
                                   TRUE ~ 0),
           PONTUACAO_B = case_when(GOLS_B > GOLS_A ~ 3,
                                   GOLS_B == GOLS_A ~ 1,
                                   TRUE ~ 0)) %>%
    select(PAIS_A, PAIS_B, GOLS_A, GOLS_B, PONTUACAO_A, PONTUACAO_B) %>%
    pivot_longer(all_of(c("PAIS_A", "PAIS_B")),
                 names_to="tipo_gol",
                 values_to="PAIS") %>%
    group_by(PAIS) %>%
    summarise(
      GOLS_FEITOS=sum(if_else(tipo_gol == "PAIS_A", GOLS_A, GOLS_B)),
      GOLS_SOFRIDOS=sum(if_else(tipo_gol == "PAIS_A", GOLS_B, GOLS_A)),
      PONTUACAO=sum(if_else(tipo_gol == "PAIS_A", PONTUACAO_A, PONTUACAO_B)),
      SALDO_GOLS=GOLS_FEITOS - GOLS_SOFRIDOS)
}

# Duas melhores selecoes segundo pontuação e saldo de gols
selecionar_selecoes <- function(x) {
  x %>%
    arrange(desc(PONTUACAO), desc(SALDO_GOLS)) %>%
    slice(1:2)
}

brasil_venceu_pais <- function(df_placares, pais){
  df_placares %>%
    filter((PAIS_A == "Brazil" & PAIS_B == pais & GOLS_A > GOLS_B) | 
           (PAIS_B == "Brazil" & PAIS_A == pais & GOLS_B > GOLS_A)) %>%
    nrow() > 0
}

brasil_foi_selecionado <- function(df_selecionados){
  "Brazil" %in% pull(df_selecionados, PAIS)
}

simular_fase_grupos <- function(x){
  placares <- x %>%
    group_by(GRUPO_A) %>%
    group_modify(~simular_placares(.x))
  
  pontuacoes <- placares %>%
    group_by(GRUPO_A) %>%
    group_modify(~calcular_pontuacoes(.x))
  
  paises_selecionados <- pontuacoes %>%
    group_by(GRUPO_A) %>%
    group_modify(~selecionar_selecoes(.x))

  return(list(placares=placares,
              pontuacoes=pontuacoes,
              paises_selecionados=paises_selecionados,
              resultados=resultados))
}

fase_grupos <- selecoes %>%
  obter_pares_selecoes() %>%
  filtrar_dentre_grupos() %>%
  calcular_lambdas()

resultados <- tibble(BRASIL_VENCEU_CAMEROON=logical(),
                     BRASIL_VENCEU_SERBIA=logical(),
                     BRASIL_VENCEU_SWITZERLAND=logical(),
                     BRASIL_SELECIONADO=logical())

# 100 simulações da fase de grupos
for(i in 1:100){
  dfs <- simular_fase_grupos(fase_grupos)
  
  paises <- c("Cameroon", "Serbia", "Switzerland")
  brasil_venceu_paises <- map(paises, ~brasil_venceu_pais(dfs$placares, .x))
  names(brasil_venceu_paises) <- str_c(
    "BRASIL_VENCEU_", str_to_upper(str_replace_all(paises, " ", "_")))
  
  brasil_selecionado <- brasil_foi_selecionado(dfs$paises_selecionados)
  names(brasil_selecionado) <- "BRASIL_SELECIONADO"
  
  if(!brasil_selecionado)
    print(dfs$pontuacoes %>% filter(GRUPO_A == "G"))
  
  resultados <- resultados %>%
    add_row(!!!append(brasil_venceu_paises, brasil_selecionado))
}

# 300 ms por simulação
# microbenchmark::microbenchmark(simular_fase_grupos(fase_grupos), times=25)

# Porcentagem de vezes que o Brasil foi selecionado para a próxima fase
# quando venceu um determinado jogo (contra Camarões, Sérvia e Suíça)
resultados %>%
  group_by(BRASIL_VENCEU_CAMEROON) %>%
  summarise(BRASIL_SELECIONADO = mean(BRASIL_SELECIONADO))
resultados %>%
  group_by(BRASIL_VENCEU_SERBIA) %>%
  summarise(BRASIL_SELECIONADO = mean(BRASIL_SELECIONADO))
resultados %>%
  group_by(BRASIL_VENCEU_SWITZERLAND) %>%
  summarise(BRASIL_SELECIONADO = mean(BRASIL_SELECIONADO))
