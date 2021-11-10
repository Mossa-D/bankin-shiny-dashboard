#
# Server
#


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    # Load data
    # bankin <- reactive({
    #     
    #     # Make sure code waits for file to be uploaded
    #     req(input$file_bankin)
    #     
    #     # Extension validation
    #     ext <- tools::file_ext(input$file_bankin$name)
    #     switch(ext,
    #            xls = Preparing_Columns(read_excel(input$file_bankin$datapath, sheet = 1)),
    #            validate("Format de fichier non valide. Doit être .xls"))
    # })
    
    # Load data
    bankin <- reactive({
        donnees = read_excel("data/export_banques_2020-01-01_2021-10-31.xls", sheet = 1) %>% 
            Preparing_Columns()
    })
    
    
    # UI ouptut  ----
    
    ## date range
    output$date_range <- renderUI({
        req(bankin)
        
        dateRangeInput(
            inputId = "input_daterange",
            label = "Période à analyser",
            start = min(bankin()$Date),
            end = max(bankin()$Date),
            weekstart = 1
        )
    })
    
    ## Categories
    output$categorie <- renderUI({
        
        selectInput(
            inputId = "rdr_categorie",
            label = "Selection categories",
            choices = levels(bankin()$Categorie),
            multiple = TRUE,
            selected = levels(bankin()$Categorie)
        )
    })
    
    # Subset based on date
    bankin_sub <- reactive({
        req(input$daterange)
        
        bankin() %>% dplyr::filter(Date >= input$daterange[1] & Date <= input$daterange[2])
    })
    
    
    # Display subset table  
    output$show_bankin <- renderDataTable({
        datatable(bankin_sub(), options = list("pageLength" = 10))
    })

})
