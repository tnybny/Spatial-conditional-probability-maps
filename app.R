# app to create conditional probability map given a reference location
# Author: tnybny

# load required libraries
require(shiny)
require(rworldmap)

ui <- fluidPage(
    titlePanel(paste("Probability that there were extreme ",
                     "values in NAm. region given that ",
                     "the reference location experienced extreme ", 
                     "values")),
    sidebarPanel(
        numericInput(inputId = "lon",
                     label = "Choose a longitude index in [6, 54]",
                     min = 6, max = 54, value = 41),
        numericInput(inputId = "lat",
                     label = "Choose a latitude index in [6, 26]",
                     min = 6, max = 26, value = 22),
        selectInput(inputId = "variable",
                    label = "Choose a reference climate variable",
                    choices = c("Tsfc", "Z500")),
        selectInput(inputId = "timeframe",
                    label = "Choose a timeframe",
                    choices = c("Period of record" = "por",
                                "Specific season" = "seas",
                                "Specific month" = "mth")),
        selectInput(inputId = "crossref",
                    label = "Cross referencing between variables",
                    choices = c("No cross referencing" = "nocr",
                                "Cross referencing" = "cr")),
        htmlOutput("timeUI"),
        actionButton(inputId = "go", label = "Update")
    ),
    mainPanel(
        plotOutput(outputId = "warmEx"),
        plotOutput(outputId = "coldEx")
    )
)

server <- function(input, output){
    # set reference location
    # loc = c(41, 22) # Raleigh, NC
    output$timeUI <- renderUI({ 
        if(input$timeframe == "seas")
        {
            selectInput(inputId = "time", label = "Select your choice",
                        choices = c("Winter", "Spring", "Summer", "Fall"))
        }else if(input$timeframe == "mth")
        {
            selectInput(inputId = "time", label = "Select your choice",
                        choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
        }
    })
    cutMin <- 5
    cutMax <- 95
    path_to_data <- "./"
    months <- cumsum(c(0, 29, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 29))
    
    p <- eventReactive(
        input$go, {
            loc <- c(input$lon, input$lat)
            var <- ifelse(input$variable == "Tsfc", "STemp", "Z500")
            filterArea <- ifelse(input$variable == "Tsfc", "200k", "0k")
            folderName <- paste(path_to_data, "NA_", var, "filteredBlobCSVs_",
                                cutMin, "_", 
                                cutMax, "_", filterArea, "/", sep = "")
            # create placeholder matrices for spatial lag plots
            warmtoplot <- matrix(0, 144, 73)
            coldtoplot <- matrix(0, 144, 73)
            
            warmCount = 0
            coldCount = 0
            
            if(input$crossref == "cr")
            {
                var2 <- ifelse(var == "Stemp", "Z500", "STemp")
                filterArea2 <- ifelse(var == "Tsfc", "0k", "200k")
                folderName2 <- paste(path_to_data, "NA_", var2, "filteredBlobCSVs_",
                                     cutMin, "_", 
                                     cutMax, "_", filterArea2, "/", sep = "")
            }
            if(input$timeframe == "por")
            {
                withProgress(message = "Calculating...", value = 0, {
                    # Number of times we'll go through the loop
                    n <- 34
                    
                    for(year in 1979:(1979 + n))
                    {
                        incProgress(1, detail = paste("Doing year", year))
                        for(day in 1:361)
                        {
                            currentFile <- paste(folderName, var,
                                                 "_blobs_", filterArea, 
                                                 "_day_", day, 
                                                 "_year_", year, ".csv",
                                                 sep = "")
                            df <- try(read.table(currentFile, header = F,
                                                 sep = ","), silent = T)
                            if(class(df) == "try-error")
                                next
                            
                            # if today was warm at loc, update warmtoplot
                            occ <- which(df[, 2] == loc[1] &
                                             df[, 3] == loc[2])
                            if(length(occ) == 0)
                                next
                            
                            # update appropriate spatial lag plot (warm or cold)
                            if(df[occ, 1] > 0)
                            {
                                # increase number of days reference location had anomaly
                                warmCount <- warmCount + 1
                                if(input$crossref == "cr")
                                {
                                    # account for cross referencing
                                    currentFile2 <- paste(folderName2, var2,
                                                          "_blobs_",
                                                          filterArea2,
                                                          "_day_", day, 
                                                          "_year_", year,
                                                          ".csv",
                                                          sep = "")
                                    df2 <- try(read.table(currentFile2, header = F,
                                                          sep = ","), silent = T)
                                    if(class(df2) == "try-error")
                                        next
                                } else {
                                    df2 = df
                                }
                                for(i in 1:nrow(df2))
                                {
                                    warmtoplot[df2[i, 2], df2[i, 3]] <-
                                        warmtoplot[df2[i, 2],
                                                   df2[i, 3]] + 1
                                }
                            }
                            else
                            {
                                coldCount <- coldCount + 1
                                if(input$crossref == "cr")
                                {
                                    # account for cross referencing
                                    currentFile2 <- paste(folderName2, var2,
                                                          "_blobs_",
                                                          filterArea2,
                                                          "_day_", day, 
                                                          "_year_", year,
                                                          ".csv",
                                                          sep = "")
                                    df2 <- try(read.table(currentFile2, header = F,
                                                          sep = ","), silent = T)
                                    if(class(df2) == "try-error")
                                        next
                                } else {
                                    df2 = df
                                }
                                for(i in 1:nrow(df2))
                                {
                                    coldtoplot[df2[i, 2], df2[i, 3]] <-
                                        coldtoplot[df2[i, 2],
                                                   df2[i, 3]] + 1
                                }
                            }
                        }
                    }
                })
            }else if(input$timeframe == "seas")
            {
                withProgress(message = "Calculating...", value = 0, {
                    # Number of times we'll go through the loop
                    n <- 34
                    
                    # the season we'll go through
                    if(input$time == "Winter"){
                        days <- c(seq(1, 58, 1), seq(332, 361, 1))
                    }else if(input$time == "Spring"){
                        days <- seq(59, 149, 1)
                    }
                    else if(input$time == "Summer"){
                        days <- seq(150, 240, 1)
                    }
                    else if(input$time == "Fall"){
                        days <- seq(241, 331, 1)
                    }
                    
                    for(year in 1979:(1979 + n))
                    {
                        incProgress(1, detail = paste("Doing year", year))
                        for(day in days)
                        {
                            currentFile <- paste(folderName, var,
                                                 "_blobs_", filterArea,
                                                 "_day_", day, 
                                                 "_year_", year, ".csv",
                                                 sep = "")
                            df <- try(read.table(currentFile, header = F,
                                                 sep = ","), silent = T)
                            if(class(df) == "try-error")
                                next
                            
                            # if today was warm at loc, update warmtoplot
                            occ <- which(df[, 2] == loc[1] &
                                             df[, 3] == loc[2])
                            if(length(occ) == 0)
                                next
                            
                            # update appropriate spatial lag plot (warm or cold)
                            if(df[occ, 1] > 0)
                            {
                                # increase number of days reference location had anomaly
                                warmCount <- warmCount + 1
                                if(input$crossref == "cr")
                                {
                                    # account for cross referencing
                                    currentFile2 <- paste(folderName2, var2,
                                                          "_blobs_",
                                                          filterArea2,
                                                          "_day_", day, 
                                                          "_year_", year,
                                                          ".csv",
                                                          sep = "")
                                    df2 <- try(read.table(currentFile2,
                                                          header = F,
                                                          sep = ","),
                                               silent = T)
                                    if(class(df2) == "try-error")
                                        next
                                } else {
                                    df2 = df
                                }
                                for(i in 1:nrow(df2))
                                {
                                    warmtoplot[df2[i, 2], df2[i, 3]] <-
                                        warmtoplot[df2[i, 2],
                                                   df2[i, 3]] + 1
                                }
                            }
                            else
                            {
                                coldCount <- coldCount + 1
                                if(input$crossref == "cr")
                                {
                                    # account for cross referencing
                                    currentFile2 <- paste(folderName2, var2,
                                                          "_blobs_",
                                                          filterArea2,
                                                          "_day_",day, 
                                                          "_year_", year,
                                                          ".csv",
                                                          sep = "")
                                    df2 <- try(read.table(currentFile2,
                                                          header = F,
                                                          sep = ","),
                                               silent = T)
                                    if(class(df2) == "try-error")
                                        next
                                } else {
                                    df2 = df
                                }
                                for(i in 1:nrow(df2))
                                {
                                    coldtoplot[df2[i, 2], df2[i, 3]] <-
                                        coldtoplot[df2[i, 2],
                                                   df2[i, 3]] + 1
                                }
                            }
                        }
                    }
                })
                
            }else if(input$timeframe == "mth")
            {
                withProgress(message = "Calculating...", value = 0, {
                    # Number of times we'll go through the loop
                    n <- 34
                    
                    # find the month we choose
                    month_time <- match(input$time, month.abb)
                    days <- seq(months[month_time] + 1,
                               months[month_time + 1], 1)
                    
                    for(year in 1979:(1979 + n))
                    {
                        incProgress(1, detail = paste("Doing year", year))
                        
                        for(day in days)
                        {
                            currentFile <- paste(folderName, var,
                                                 "_blobs_", filterArea,
                                                 "_day_", day, 
                                                 "_year_", year, ".csv",
                                                 sep = "")
                            df <- try(read.table(currentFile, header = F,
                                                 sep = ","), silent = T)
                            if(class(df) == "try-error")
                                next
                            
                            # if today was warm at loc, update warmtoplot
                            occ <- which(df[, 2] == loc[1] &
                                             df[, 3] == loc[2])
                            if(length(occ) == 0)
                                next
                            
                            # update appropriate spatial lag plot (warm or cold)
                            if(df[occ, 1] > 0)
                            {
                                # increase number of days reference location had anomaly
                                warmCount <- warmCount + 1
                                if(input$crossref == "cr")
                                {
                                    # account for cross referencing
                                    currentFile2 <- paste(folderName2, var2,
                                                          "_blobs_",
                                                          filterArea2,
                                                          "_day_", day, 
                                                          "_year_", year,
                                                          ".csv",
                                                          sep = "")
                                    df2 <- try(read.table(currentFile2,
                                                          header = F,
                                                          sep = ","),
                                               silent = T)
                                    if(class(df2) == "try-error")
                                        next
                                } else {
                                    df2 = df
                                }
                                for(i in 1:nrow(df2))
                                {
                                    warmtoplot[df2[i, 2], df2[i, 3]] <-
                                        warmtoplot[df2[i, 2],
                                                   df2[i, 3]] + 1
                                }
                            }
                            else
                            {
                                coldCount <- coldCount + 1
                                if(input$crossref == "cr")
                                {
                                    # account for cross referencing
                                    currentFile2 <- paste(folderName2, var2,
                                                          "_blobs_",
                                                          filterArea2,
                                                          "_day_", day, 
                                                          "_year_", year,
                                                          ".csv",
                                                          sep = "")
                                    df2 <- try(read.table(currentFile2,
                                                          header = F,
                                                          sep = ","),
                                               silent = T)
                                    if(class(df2) == "try-error")
                                        next
                                } else {
                                    df2 = df
                                }
                                for(i in 1:nrow(df2))
                                {
                                    coldtoplot[df2[i, 2], df2[i, 3]] <-
                                        coldtoplot[df2[i, 2],
                                                   df2[i, 3]] + 1
                                }
                            }
                        }
                    }
                })
            }
            # plot the blob mask for NAm region
            warmtoplot <- warmtoplot / warmCount
            coldtoplot <- coldtoplot / coldCount
            
            warmtoplot[loc[1], loc[2]] <- -1
            coldtoplot[loc[1], loc[2]] <- -1
            list(wa = warmtoplot, co = coldtoplot)
        })
    
    output$warmEx <- renderPlot({
        mapGriddedData(p()$wa, borderCol = "black", xlim = c(-165, -45),
                       ylim = c(25, 75),
                       catMethod = seq(0, 1, length.out = 10))
        title("Warm spatial lag map for reference location in North Am.")
    }, width = 600, height = 400)
    
    output$coldEx <- renderPlot({
        mapGriddedData(p()$co, borderCol = "black", xlim = c(-165, -45),
                       ylim = c(25, 75), 
                       catMethod = seq(0, 1, length.out = 10))
        title("Cold spatial lag map for reference location in North Am.")
    }, width = 600, height = 400)
}

shinyApp(ui = ui, server = server)