# ============================================================
# PROJET PONANT - Application Shiny
# Groupe 10
# ============================================================

library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(DT)
library(bslib)

# ---------- 1. PRÉPARATION DES DONNÉES ----------

# On lit votre fichier (avec le 2 et l'espace)
donnees_brutes <- read_excel("Data_Ponant 2.xlsx", sheet = "DATA_principal")

# On force les noms de colonnes (pour éviter les problèmes d'accents entre Windows et Linux)
names(donnees_brutes) <- c(
  "Ile", "Nom_commune",
  "Nb_d_habitants_2019", "Nb_d_habitants_2013", "Nb_d_habitants_2008", "Nb_d_habitants_1999",
  "Nb_de_logements_2019", "Nb_de_logements_2013", "Nb_de_logements_2008", "Nb_de_logements_1999",
  "Taux_de_residendes_secondaires_2019", "Taux_de_residendes_secondaires_2013", "Taux_de_residendes_secondaires_2008", "Taux_de_residendes_secondaires_1999",
  "Taux_de_logements_vacants_2019", "Taux_de_logements_vacants_2013", "Taux_de_logements_vacants_2008", "Taux_de_logements_vacants_1999",
  "Prix_median_du_bati_au_m2_2019", "Prix_median_du_bati_au_m2_2020", "Prix_median_du_bati_au_m2_2021", "Prix_median_du_bati_au_m2_2022", "Prix_median_du_bati_au_m2_2023"
)

# On nettoie les données
iles <- donnees_brutes %>%
  mutate(across(starts_with("Prix_median"), ~ suppressWarnings(as.numeric(.x)))) %>%
  filter(!is.na(Ile)) %>%
  filter(!Nom_commune %in% c("Bangor", "Locmaria", "Le Palais", "Sauzon"))

# Format long - population
pop_long <- iles %>%
  select(Ile, starts_with("Nb_d_habitants")) %>%
  pivot_longer(cols = -Ile, names_to = "annee", values_to = "valeur") %>%
  mutate(annee = as.numeric(str_extract(annee, "\\d{4}")),
         indicateur = "Nombre d'habitants")

# Format long - résidences secondaires
rs_long <- iles %>%
  select(Ile, starts_with("Taux_de_residendes_secondaires")) %>%
  pivot_longer(cols = -Ile, names_to = "annee", values_to = "valeur") %>%
  mutate(annee = as.numeric(str_extract(annee, "\\d{4}")),
         valeur = valeur * 100,
         indicateur = "Taux de résidences secondaires (%)")

# Format long - prix m²
prix_long <- iles %>%
  select(Ile, starts_with("Prix_median_du_bati_au_m2")) %>%
  pivot_longer(cols = -Ile, names_to = "annee", values_to = "valeur") %>%
  mutate(annee = as.numeric(str_extract(annee, "\\d{4}")),
         indicateur = "Prix médian au m² (€)")

# Fusion des trois
donnees_long <- bind_rows(pop_long, rs_long, prix_long)

# Comparaison (année la plus récente par indicateur)
comparaison <- donnees_long %>%
  group_by(indicateur) %>%
  filter(annee == max(annee)) %>%
  ungroup()

# ---------- 2. UI ----------

ui <- page_navbar(
  title = "🌊 Les îles du Ponant",
  theme = bs_theme(bootswatch = "flatly"),
  
  nav_panel(
    title = "📈 Évolution",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        radioButtons("indicateur_evol", "Choisir un indicateur :",
                     choices = c("Nombre d'habitants",
                                 "Taux de résidences secondaires (%)",
                                 "Prix médian au m² (€)"),
                     selected = "Nombre d'habitants"),
        checkboxGroupInput("iles_evol", "Choisir une ou plusieurs îles :",
                           choices = sort(unique(iles$Ile)),
                           selected = c("Groix", "Yeu", "Belle-île")),
        helpText("Décochez tout pour voir toutes les îles.")
      ),
      card(
        card_header("Évolution de l'indicateur sélectionné"),
        plotlyOutput("graph_evol", height = "500px")
      )
    )
  ),
  
  nav_panel(
    title = "📊 Comparaison",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        radioButtons("indicateur_comp", "Choisir un indicateur :",
                     choices = c("Nombre d'habitants",
                                 "Taux de résidences secondaires (%)",
                                 "Prix médian au m² (€)"),
                     selected = "Prix médian au m² (€)")
      ),
      card(
        card_header("Comparaison entre îles (année la plus récente)"),
        plotlyOutput("graph_comp", height = "500px")
      )
    )
  ),
  
  nav_panel(
    title = "📋 Données",
    card(
      card_header("Tableau complet"),
      DTOutput("tableau")
    )
  ),
  
  nav_panel(
    title = "ℹ️ À propos",
    card(
      card_header("Contexte"),
      p("Application réalisée dans le cadre du projet R Shiny (groupe 10)."),
      p("Données INSEE sur le logement dans les îles du Ponant."),
      p("Les communes de Belle-île (Bangor, Locmaria, Le Palais, Sauzon) sont regroupées dans le total Belle-île.")
    )
  )
)

# ---------- 3. SERVER ----------

server <- function(input, output, session) {
  
  output$graph_evol <- renderPlotly({
    d <- donnees_long %>% filter(indicateur == input$indicateur_evol)
    if (length(input$iles_evol) > 0) {
      d <- d %>% filter(Ile %in% input$iles_evol)
    }
    
    p <- ggplot(d, aes(x = annee, y = valeur, color = Ile, group = Ile,
                       text = paste0("Île : ", Ile,
                                     "<br>Année : ", annee,
                                     "<br>Valeur : ", round(valeur, 1)))) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(x = "Année", y = input$indicateur_evol, color = "Île") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$graph_comp <- renderPlotly({
    d <- comparaison %>%
      filter(indicateur == input$indicateur_comp) %>%
      arrange(desc(valeur))
    
    p <- ggplot(d, aes(x = reorder(Ile, valeur), y = valeur, fill = Ile,
                       text = paste0("Île : ", Ile,
                                     "<br>Année : ", annee,
                                     "<br>Valeur : ", round(valeur, 1)))) +
      geom_col() +
      coord_flip() +
      labs(x = "", y = input$indicateur_comp,
           title = paste0("Année ", unique(d$annee))) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$tableau <- renderDT({
    datatable(iles,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })
}

# ---------- 4. LANCEMENT ----------
shinyApp(ui, server)