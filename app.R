# app to create conditional probability map given a reference location
# Author: tnybny

# load required libraries
require(shiny)
require(rworldmap)

ui <- fluidPage(titlePanel(paste("Probability that there were extreme ",
                                 "temperatures in NAm. region given that ",
                                 "the reference location experienced extreme ", 
                                 "temperature")),
                numericInput(inputId = "lon",
                             label = "Choose a longitude index in [6, 54]",
                             min = 6, max = 54, value = 41),
                numericInput(inputId = "lat",
                             label = "Choose a latitude index in [6, 26]",
                             min = 6, max = 26, value = 22),
                actionButton(inputId = "go", label = "Update"),
                plotOutput(outputId = "warmEx"),
                plotOutput(outputId = "coldEx")
                )

server <- function(input, output){
    # set reference location
    #loc = c(41, 22) # Raleigh, NC
    
    cutMin <- 5
    cutMax <- 95
    path_to_data <- './'
    STemp_folder_name <- paste(path_to_data, 'NA_STempfilteredBlobCSVs_',
                               cutMin, '_', 
                               cutMax,'_200k/' ,sep = '')
    
    p <- eventReactive(
        input$go, {
        loc = c(input$lon, input$lat)
    
    # create placeholder matrices for spatial lag plots
    warmtoplot <- matrix(0, 144, 73)
    coldtoplot <- matrix(0, 144, 73)
    
    warmCount = 0
    coldCount = 0
    
    withProgress(message = 'Calculating...', value = 0, {
        # Number of times we'll go through the loop
        n <- 34
        
    for(year in 1979:(1979 + n))
    {
        incProgress(1, detail = paste("Doing year", year))
        for(day in 1:361)
        {
            current_file <- paste(STemp_folder_name, 'STemp_blobs_200k_day_', day, 
                                  '_year_', year, '.csv', sep = '')
            STemp.df <- try(read.table(current_file, header = F,
                                       sep = ','), silent = T)
            if(class(STemp.df) == 'try-error')
                next
            
            # if today was warm at loc, update warmtoplot
            occ <- which(STemp.df[, 2] == loc[1] & STemp.df[, 3] == loc[2])
            if(length(occ) == 0)
                next
            
            # update appropriate spatial lag plot (warm or cold)
            if(STemp.df[occ, 1] > 0)
            {
                # increase number of days reference location had anomaly
                warmCount = warmCount + 1
                for(i in 1:nrow(STemp.df))
                {
                    warmtoplot[STemp.df[i, 2], STemp.df[i, 3]] =
                        warmtoplot[STemp.df[i, 2], STemp.df[i, 3]] + 1
                }
            }
            else
            {
                coldCount = coldCount + 1
                for(i in 1:nrow(STemp.df))
                {
                    coldtoplot[STemp.df[i, 2], STemp.df[i, 3]] =
                        coldtoplot[STemp.df[i, 2], STemp.df[i, 3]] + 1
                }
            }
        }
    }
    
    # plot the blob mask for NAm region
    warmtoplot = warmtoplot / warmCount
    coldtoplot = coldtoplot / coldCount
    
    warmtoplot[loc[1], loc[2]] = 0
    coldtoplot[loc[1], loc[2]] = 0
    list(wa = warmtoplot, co = coldtoplot)
    })
    })
    
    output$warmEx = renderPlot({
        mapGriddedData(p()$wa, borderCol = "black", xlim = c(-165, -45),
                   ylim = c(25, 75),
                   catMethod = seq(0, 1, length.out = 10))
        title('Warm spatial lag map for reference location in North Am.')
        }, width = 600, height = 400)
    
    output$coldEx = renderPlot({
        mapGriddedData(p()$co, borderCol = "black", xlim = c(-165, -45),
                   ylim = c(25, 75), 
                   catMethod = seq(0, 1, length.out = 10))
        title('Cold spatial lag map for reference location in North Am.')
        }, width = 600, height = 400)
}

shinyApp(ui = ui, server = server)