# global.R
library(shiny)
library(readxl)
library(tidyverse)
library(plotly)
library(lubridate)
library(DT)
library(dplyr)
library(reactlog)
library(RColorBrewer)
library(shinythemes)

# tell shiny to log all reactivity
reactlog_enable()
theme_set(theme_bw())

# 1 - LOAD DATA ----

## TRANSFORM DATA ----

Preparing_Columns <- function(df) {
  # Renomme les colonnes, transforme les types et ajoute la variable AnneeMois
  
  df <- df %>% 
    rename(Categorie = Catégorie, SousCategorie = `Sous-Catégorie`, Pointee = Pointée) %>% 
    mutate(
      Date = dmy(Date),
      Compte = factor(Compte),
      Categorie = replace(Categorie, SousCategorie == "Virements internes", "Virements internes"),
      Categorie = factor(Categorie),
      SousCategorie = factor(SousCategorie),
      Type = case_when(
        SousCategorie %in% "Virements internes" ~ "TRANSFERTS",
        Categorie %in% "Investissement Epargne" ~ "TRANSFERTS",
        Categorie %in% "Entrées d'argent" ~ "CREDIT",
        TRUE ~ "DEBIT"),
      Montant = if_else(Montant < 0, -Montant, Montant),
      AnneeMois = substr(Date, 1, 7),
      Trimestre = zoo::as.yearqtr(Date, format="%Y-%m-%d"))
  
  return(df)
}

df <- read_excel("export_banques_2020-01-01_2021-10-30.xls", sheet = 1) %>% 
  Preparing_Columns()



Categories_Type <- function(df) {
  # Separe les categorie selon leur types
  
  dep = as.character(pull(unique(df[df$Type == "DEBIT", "Categorie"])))
  rev = as.character(pull(unique(df[df$Type == "CREDIT", "Categorie"])))
  
  categories = list("rev" = rev, "dep" = dep)
  
  return(categories)
}

categories <- Categories_Type(df)

categories$dep


Monthly_Overview <- function(df) {
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

monthly_summary <- Monthly_Overview(df)

# VISUALISATIONS ----


Revenus_Depenses_Plot <- function(df_monthly) {
  # Area plot des depenses/revenus mensuels
  
  p = ggplot(monthly_summary, aes(x = AnneeMois)) +
    geom_ribbon(aes(ymin = 0, ymax = Credit, group = 1), fill = "green", alpha=0.5) +
    geom_point(aes(y = Credit, group = 1), color = "darkgreen") +
    geom_ribbon(aes(ymin = 0, ymax = Debit, group = 1), fill = "red", alpha=0.45) +
    geom_point(aes(y = Debit, group = 1), color = "red") +
    labs(title = "Rev, Dep", x = "", y = "")  
  
  return(ggplotly(p, tooltip = "y"))
}

# Revenus_Depenses_Plot(monthly_summary)

Running_Balance_Plot <- function(df_monthly){
  # Lineplot de la running balance
  
  p = ggplot(df_monthly) +
    geom_line(aes(x = AnneeMois, y = RunningBalance, group = 1)) +
    labs(title = "Balance cumulée", x = "", y = "")
  
  return(ggplotly(p, tooltip = "y"))
}

# Running_Balance_Plot(monthly_summary)

SousCategorie_Monthly <- function(df) {
  # Somme mensuelle des montants par sous categorie
  
  return(left_join(
    df %>%  
      group_by(AnneeMois, SousCategorie) %>% 
      summarise(TotalMois = sum(Montant)) %>% 
      ungroup(),
    df %>% 
      distinct(SousCategorie, Categorie, Type),
    by = c("SousCategorie" = "SousCategorie")
  ))
}

Categorie_Monthly <- function(df) {
  # Somme mensuelle des montants par categorie
  
  return(
    left_join(
      df %>%  
        group_by(AnneeMois, Categorie) %>% 
        summarise(TotalMois = sum(Montant)) %>% 
        ungroup(),
      df %>% 
        distinct(Categorie, Type),
      by = c("Categorie" = "Categorie")
    ))
}

Revenus_Breakdown_Plot <- function(df) {
  # Detail des sources de revenus par mois
  
  p = SousCategorie_Monthly(df) %>% 
    filter(Type == "CREDIT") %>% 
    ggplot() +
    geom_col(aes(x = AnneeMois, y = TotalMois, group = SousCategorie, fill = SousCategorie)) +
    scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')) +
    labs(title = "Répartition des revenus", x = "") +
    guides(fill = "none")
  
  return(ggplotly(p, tooltip = c("y", "fill")))
}

Revenus_Breakdown_Plot(df)


Depenses_Breakdown_Monthly_Plot <- function(df, cat_filter) {
  # Detail des depenses par mois
  
  p = SousCategorie_Monthly(df) %>% 
    filter(Type == "DEBIT" & Categorie %in% cat_filter) %>% 
  
    ggplot() +
    geom_col(aes(x = AnneeMois, y = TotalMois, group = SousCategorie, fill = SousCategorie)) +
    labs(title = "Détail des sous-catégories", x = "") +
    guides(fill = "none")
  
  return(ggplotly(p, tooltip = c("y", "fill")))
}


Depenses_Breakdown_Monthly_Plot(df, c("Achats & Shopping", "Logement"))


Depenses_Monthly_Plot <- function(df) {
  # Detail des depenses par mois
  
  p = Categorie_Monthly(df) %>% 
    filter(Type == "DEBIT") %>%
    
    ggplot() +
    geom_col(aes(x = AnneeMois, y = TotalMois, group = Categorie, fill = Categorie), color = "gray") +
    labs(title = "Répartition des dépenses", x = "", y = "Montant") +
    guides(fill = "none")
  
  return(ggplotly(p, tooltip = c("y", "fill")))
}

Depenses_Monthly_Plot(df)

Part_Depenses_Annee <- function(df, selected_year) {
  # Part des dépenses
  
  df %>% 
    group_by(Annee = year(Date), Categorie, Type) %>% 
    summarise(Montant = sum(Montant)) %>% 
    filter(Annee == selected_year & Type == "DEBIT") %>%
    ungroup() %>% 
    mutate(PartMontant = Montant / sum(Montant)) %>% 
    
    ggplot() + 
    geom_col(aes(x = reorder(Categorie, PartMontant), y = PartMontant, fill = Categorie)) + 
    coord_flip() +
    guides(fill = "none") +
    scale_y_continuous(labels = scales::percent) +
    labs(x="", title = "Repartition des dépenses", subtitle = paste0("Année ", selected_year))
  
}

Part_Depenses_Annee(df, 2020)

df %>% 
  filter(Type == "DEBIT") %>% 
  group_by(Trimestre, Categorie) %>% 
  summarise(TotalTrimestre = sum(Montant)) %>% 
  mutate(Trimestre = as.character(Trimestre),
         Trimestre = str_replace(Trimestre, " ", "_")) %>% 
  # pivot_wider(names_from = Trimestre, values_from = TotalTrimestre, values_fill = 0) %>% 
  ggplot() +
  geom_col(aes(x = Categorie, y = TotalTrimestre, group = Trimestre, fill = Trimestre), position = position_dodge()) + 
  coord_flip()



# Dépenses totales par catégorie sur l'année en cours ----




# Revenus totaux par catégorie sur l'année en cours ----

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


# Treemap annee en cours

library(treemapify)
library(ggplotify)


