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
library(RColorBrewer)

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
        SousCategorie %in% "Virements internes" ~ "TRANSFERTS",
        Categorie %in% "Investissement Epargne" ~ "TRANSFERTS",
        Categorie %in% "Entrées d'argent" ~ "CREDIT",
        TRUE ~ "DEBIT"),
      Montant = if_else(Montant < 0, -Montant, Montant),
      AnneeMois = substr(Date, 1, 7))
  
  return(df)
}

df <- read_excel("bankin-dashboard/data/export_banques_2020-01-01_2021-10-31.xls", sheet = 1) %>% 
  Preparing_Columns()

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
      across(everything(), ~replace_na(.x, 0)),
      Balance = Credit - Debit,
      RunningBalance = cumsum(Balance),
      TauxEpargne = Balance / Credit,
      EvolCredit = (Credit - lag(Credit)) / Credit,
      EvolDebit = (Debit - lag(Debit)) / Debit,
      EvolTransferts = (Transferts - lag(Transferts)) / Transferts
    )
}

monthly_summary <- Monthly_Groupby(df)
monthly_summary

  
# Revenus / Dépenses totales du mois en cours
Current_Month <- function(df) {
  
  
}

# Dépenses totales par catégorie sur l'année en cours

df %>% 
  group_by(Annee = year(Date), Categorie, Type) %>% 
  summarise(Montant = sum(Montant)) %>% 
  filter(Annee == 2020 & Type == "DEBIT") %>% 
  
  ggplot() + 
  geom_col(aes(x = reorder(Categorie, -Montant), y = Montant, fill = Categorie)) + 
  guides(fill = "none") +
  labs(x="", title = "Repartition des dépenses") + 
  theme(axis.text.x = element_text(angle = 90))


# Revenus totaux par catégorie sur l'année en cours

df %>% 
  group_by(Annee = year(Date), SousCategorie, Type) %>% 
  summarise(Montant = sum(Montant)) %>% 
  filter(Annee == 2020 & Type == "CREDIT") %>% 
  
  ggplot() + 
  geom_col(aes(x = reorder(SousCategorie, -Montant), y = Montant, fill = SousCategorie)) + 
  guides(fill = "none") +
  labs(x="", title = "Repartition des revenus") + 
  scale_y_continuous(labels = scales::dollar_format()) + 
  theme(axis.text.x = element_text(angle = 90))


# Evolution des niveaux de dépenses / revenus mois par mois

df %>% 
  group_by(AnneeMois, Categorie, Type) %>% 
  summarise(Montant = sum(Montant)) %>% 
  filter(Type == "DEBIT") %>%
  ggplot() +
  geom_line(aes(x = AnneeMois, y = Montant, group = Categorie, color = Categorie)) +
  labs(x="", title = "Evolution des dépenses par catégories") + 
  theme(axis.text.x = element_text(angle = 90))

df %>% 
  group_by(AnneeMois, SousCategorie, Type) %>% 
  summarise(Montant = sum(Montant)) %>% 
  filter(Type == "CREDIT") %>%
  ggplot() +
  geom_col(aes(x = AnneeMois, y = Montant, group = SousCategorie, fill = SousCategorie)) +
  scale_fill_brewer(palette = "Set1") +
  labs(x="", title = "Evolution des revenus par sous-catégories") + 
  theme(axis.text.x = element_text(angle = 90))
