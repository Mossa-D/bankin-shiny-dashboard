## ui.R ##


fluidPage(
    
    # App title ----
    titlePanel("Bankin Dashboard"),
    
    # Sidebar layout with ouput definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            width = 3,
            
            # Input: Import the data file ----
            fileInput(
                inputId = "file_bankin",
                label = "Import fichier bankin",
                multiple = FALSE,
                accept = ".xls",
                buttonLabel = "Importer",
                placeholder = "..."
            ),
            
            # Input: Select the date range to analyze ----
                     
            # Selection date range
            # Filter year
            # uiOutput(
            #     outputId = "date_range"
            # )
            
            dateRangeInput(
                inputId = "daterange",
                label = "Période à analyser",
                start = "2020-03-02",
                end = "2021-12-31",
                weekstart = 1
            ), 
            
                     
            # Input: Select categories to focus on ----
            uiOutput(outputId = "categorie")
        ),
                     
        
        # Main panel for displaying outputs ----
        mainPanel(
        
            # Output: Tabsets with summary plots
            tabsetPanel(
                type = "tabs",
                        
                # RESUME tab ----
                # dataTableOutput(outputId = "show_bankin"),
                tabPanel(
                    "RESUME",
                    
                    # Revnus depenses area plot
                    plotlyOutput(outputId = "show_dep_rev_area_monthy"),
                            
                    # Running balance plot
                    plotlyOutput(outputId = "show_runbal_monthly")
                ),
                
                # REVENUS tab ----
                tabPanel(
                    "REVENUS",
                    plotlyOutput(outputId = "show_rev_monthly")
                ),
                
                # DEPENSES tab ----
                tabPanel(
                    "DEPENSES",
                    plotlyOutput(outputId = "show_dep_monthly")
                )
            )
        )
    )
)
