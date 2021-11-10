# User interface for shiny application



# Define UI for application that draws a histogram
dashboardPage(
    # Titre
    dashboardHeader(title = "Bankin"),
    
    # Sidebar
    dashboardSidebar(
        
        # Data input
        fileInput(
            inputId = "file_bankin",
            label = "Import fichier bankin",
            multiple = FALSE,
            accept = ".xls",
            buttonLabel = "Importer",
            placeholder = "..."
        ),
        
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
        
        uiOutput(
            outputId = "categorie"
        )
    ),
    
    # Body
    dashboardBody(
        dataTableOutput(outputId = "show_bankin")
    )
)
