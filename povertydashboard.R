library(shiny)
library(leaflet)
library(geodata) 
library(sf)      
library(dplyr)   
library(stringi)
library(shinythemes)
library(shinyWidgets) 
library(bslib)
library(ggplot2) 
library(tidyr) 

# ==========================================
# 1. Préparation des Données
# ==========================================

# Chargement des données
data_pauvrete <- read.csv("pauvrete.csv", stringsAsFactors = FALSE)

# Définition des nouveaux Districts
data_pauvrete <- data_pauvrete %>%
  mutate(District = case_when(
    Gouvernorat %in% c("BIZERTE","BEJA", "JENDOUBA","LE KEF") ~ "District 1 : Madjerda",
    Gouvernorat %in% c("TUNIS","ARIANA","BEN AROUS","ZAGHOUAN","MANOUBA","NABEUL") ~ "District 2 : Carthage",
    Gouvernorat %in% c("SELIANA","SOUSSE","KASSERINE", "MONASTIR", "MAHDIA", "KAIROUAN") ~ "District 3 : Sahel",
    Gouvernorat %in% c("SIDI BOUZID","TOZEUR", "SFAX", "GAFSA") ~ "District 4 : L'Atlas tunisien",
    Gouvernorat %in% c("KEBILI", "GABES", "MEDNINE", "TATAOUINE") ~ "District 5 : Ksour",
    TRUE ~ "Autre"
  ))

# Standardisation pour la jointure cartographique
standardize_name <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII") %>% toupper() %>% gsub("[^A-Z]", "", .)
  case_when(
    x %in% c("LEKEF", "ALKAF", "KEF") ~ "KEF_KEY",
    x %in% c("MANOUBA", "MANUBAH") ~ "MANOUBA_KEY",
    x %in% c("BENAROUS", "BENAROUSTUNISSUD") ~ "BENAROUS_KEY",
    x %in% c("SELIANA", "SILIANA") ~ "SILIANA_KEY",
    x %in% c("MEDNINE", "MEDENINE") ~ "MEDENINE_KEY",
    TRUE ~ x
  )
}

# Préparation de la carte GADM
tunisia_map <- gadm(country = "TUN", level = 1, path = tempdir()) %>% 
  st_as_sf() %>% 
  mutate(Join_Key = standardize_name(NAME_1))

data_pauvrete <- data_pauvrete %>% mutate(Join_Key = standardize_name(Gouvernorat))
full_map_data <- left_join(tunisia_map, data_pauvrete, by = "Join_Key")

# ==========================================
# 2. Interface Utilisateur (UI)
# ==========================================

ui <- navbarPage(
  title = span(icon("chart-area"), " Observatoire de la Pauvreté", style="font-weight:900;"),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # --- ONGLET 1 : CARTE ---
  tabPanel("Carte & Focus",
           tags$head(tags$style(HTML("
             .sidebar { background: white; border-radius: 15px; padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); }
             .value-box { padding: 15px; border-radius: 12px; color: white; margin-bottom: 10px; display: flex; align-items: center; justify-content: space-between;}
             .bg-blue { background: linear-gradient(135deg, #3498db, #2980b9); }
             .bg-red { background: linear-gradient(135deg, #e74c3c, #c0392b); }
           "))),
           
           fluidRow(
             column(width = 3,
                    div(class = "sidebar",
                        pickerInput("select_district", "District :", 
                                    choices = c("Tout le pays", sort(unique(data_pauvrete$District)))),
                        pickerInput("select_gouv", "Gouvernorat :", choices = "Tout le pays"),
                        hr(),
                        uiOutput("stat_panel")
                    )
             ),
             column(width = 9, leafletOutput("map", height = "80vh"))
           )
  ),
  
  # --- ONGLET 2 : ANALYSES AVANCÉES ---
  tabPanel("Analyses Avancées",
           fluidRow(
             column(width = 6,
                    div(style="background:white; padding:20px; border-radius:15px; box-shadow: 0 4px 12px rgba(0,0,0,0.05);",
                        h4(icon("compress-arrows-alt"), " Inégalités par District (Boxplot)"),
                        plotOutput("boxplot_pauvrete", height = "400px")
                    )
             ),
             column(width = 6,
                    div(style="background:white; padding:20px; border-radius:15px; box-shadow: 0 4px 12px rgba(0,0,0,0.05);",
                        h4(icon("history"), " Évolution 2015 vs Récent"),
                        plotOutput("comparison_plot", height = "400px")
                    )
             )
           ),
           fluidRow(
             column(width = 12,
                    div(style="background:white; padding:20px; border-radius:15px; margin-top:20px; box-shadow: 0 4px 12px rgba(0,0,0,0.05);",
                        h4(icon("table"), " Données Comparatives Détaillées"),
                        tableOutput("comp_table")
                    )
             )
           )
  )
)

# ==========================================
# 3. Logique Serveur
# ==========================================

server <- function(input, output, session) {
  
  # Palette pour la carte
  pal <- colorNumeric(palette = "YlOrRd", domain = full_map_data$Taux_pauvrete_recensement)
  
  # Mise à jour des menus déroulants
  observeEvent(input$select_district, {
    choices <- if(input$select_district == "Tout le pays") "Tout le pays" 
    else c("Vue d'ensemble", sort(data_pauvrete$Gouvernorat[data_pauvrete$District == input$select_district]))
    updatePickerInput(session, "select_gouv", choices = choices)
  })
  
  # --- BOXPLOT CORRIGÉ ---
  output$boxplot_pauvrete <- renderPlot({
    # Préparation des labels pour éviter les chevauchements
    df_plot <- data_pauvrete %>%
      mutate(District_Label = gsub(" : ", ":\n", District))
    
    ggplot(df_plot, aes(x = reorder(District_Label, Taux_pauvrete_recensement, FUN = median), 
                        y = Taux_pauvrete_recensement, fill = District)) +
      geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 18, width = 0.6) +
      geom_jitter(width = 0.1, alpha = 0.4, color = "#2c3e50") +
      theme_minimal() +
      labs(y = "Taux de pauvreté (%)", x = "") +
      theme(axis.text.x = element_text(size = 9, face = "bold"), legend.position = "none") +
      scale_fill_brewer(palette = "Pastel1")
  })
  
  # --- GRAPHIQUE DE COMPARAISON 2015 vs RÉCENT ---
  output$comparison_plot <- renderPlot({
    df_comp <- if(input$select_district == "Tout le pays") data_pauvrete 
    else data_pauvrete %>% filter(District == input$select_district)
    
    df_long <- df_comp %>%
      select(Gouvernorat, Taux_pauvrete_2015, Taux_pauvrete_recensement) %>%
      pivot_longer(cols = starts_with("Taux"), names_to = "Annee", values_to = "Value") %>%
      mutate(Annee = ifelse(Annee == "Taux_pauvrete_2015", "Année 2015", "Recensement"))
    
    ggplot(df_long, aes(x = reorder(Gouvernorat, Value), y = Value, fill = Annee)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      scale_fill_manual(values = c("Année 2015" = "#bdc3c7", "Recensement" = "#e74c3c")) +
      theme_minimal() +
      labs(x = "", y = "Taux (%)", fill = "Période")
  })
  
  # --- TABLEAU COMPARATIF ---
  output$comp_table <- renderTable({
    data_pauvrete %>%
      select(District, Gouvernorat, Taux_pauvrete_2015, Taux_pauvrete_recensement) %>%
      mutate(Evolution = Taux_pauvrete_recensement - Taux_pauvrete_2015) %>%
      arrange(desc(Evolution))
  }, digits = 2)
  
  # --- CARTE INTERACTIVE ---
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(9.5, 34, 6) %>% 
      addLegend(pal = pal, values = full_map_data$Taux_pauvrete_recensement, 
                title = "Pauvreté (%)", position = "bottomright")
  })
  
  observe({
    data_map <- if(input$select_district == "Tout le pays") full_map_data 
    else if(input$select_gouv == "Vue d'ensemble") full_map_data %>% filter(District == input$select_district)
    else full_map_data %>% filter(Gouvernorat == input$select_gouv)
    
    proxy <- leafletProxy("map", data = data_map)
    proxy %>% clearShapes() %>%
      addPolygons(fillColor = ~pal(Taux_pauvrete_recensement), weight = 1.5, color = "white", fillOpacity = 0.7,
                  label = ~paste(Gouvernorat, ":", Taux_pauvrete_recensement, "%"))
    
    if (input$select_district != "Tout le pays") {
      bbox <- st_bbox(data_map)
      proxy %>% flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    } else {
      proxy %>% flyTo(9.5, 34, 6)
    }
  })
  
  # --- VALUE BOXES ---
  output$stat_panel <- renderUI({
    df <- if(input$select_district == "Tout le pays") data_pauvrete 
    else data_pauvrete %>% filter(District == input$select_district)
    avg <- round(mean(df$Taux_pauvrete_recensement, na.rm=T), 2)
    
    tagList(
      div(class="value-box bg-blue", div(h6("MOYENNE ZONE"), h3(paste0(avg, "%"))), icon("chart-line")),
      div(class="value-box bg-red", div(h6("MAXIMUM"), h3(paste0(max(df$Taux_pauvrete_recensement), "%"))), icon("arrow-up"))
    )
  })
}

shinyApp(ui, server)