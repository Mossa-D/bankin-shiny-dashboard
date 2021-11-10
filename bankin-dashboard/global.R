# global.R
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(plotly)
library(lubridate)
library(data.table)
library(DT)
library(dplyr)

library(reactlog)

# tell shiny to log all reactivity
reactlog_enable()

# 1 - LOAD DATA ----

## TRANSFORM DATA ----

Preparing_Columns <- function(df) {
  # Renomme les colonnes, transforme les types et ajoute la variable AnneeMois
  
  df <- df %>% 
    rename(Categorie = Catégorie, SousCategorie = `Sous-Catégorie`, Pointee = Pointée) %>% 
    mutate(
      Date = dmy(Date),
      Compte = factor(Compte),
      Categorie = factor(Categorie),
      SousCategorie = factor(SousCategorie),
      Type = case_when(
        SousCategorie %in% c("Virements internes") ~ "TRANSFERTS",
        Categorie %in% "Entrées d'argent" ~ "CREDIT",
        TRUE ~ "DEBIT"),
      Montant = if_else(Type == "DEBIT", -Montant, Montant),
      AnneeMois = substr(Date, 1, 7))
  
  return(df)
}

df <- read_excel("bankin-dashboard/data/export_banques_2020-01-01_2021-10-31.xls", sheet = 1) %>% Preparing_Columns()

Monthly_Groupby <- function(df) {
  # Monthly group by
  
  df <- df %>% 
    group_by(AnneeMois, Type) %>%
    arrange(AnneeMois) %>% 
    summarise(Montant = sum(Montant, na.rm = TRUE)) %>% 
    pivot_wider(names_from = Type, values_from = Montant) %>%
    ungroup() %>% 
    rename(Credit = CREDIT, Debit = DEBIT, Transferts = TRANSFERTS) %>% 
    mutate(
      Balance = Credit - Debit,
      RunningBalance = cumsum(Balance),
      TauxEpargne = Balance / Credit
    )
}

monthly_summary <- Monthly_Groupby(df)

monthly_summary
  
ggplot(Monthly_Groupby(df)) + 
  geom_col(aes(x = AnneeMois, y = Transferts, group = 1)) + 
  geom_line(aes(x = AnneeMois, y = RunningBalance, group=1))
