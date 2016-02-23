
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(dplyr)


jasper <- read.csv("jasper.csv", sep=",", header = TRUE)                                #Read in database file




## RENAME COLUMNS ##

colnames(jasper)[1] <- "AC_REG"
colnames(jasper)[2] <- "PN_IN"
colnames(jasper)[3] <- "SN_IN"
colnames(jasper)[4] <- "PN_OUT"
colnames(jasper)[5] <- "SN_OUT"
colnames(jasper)[6] <- "RMVL_DATE"
colnames(jasper)[7] <- "RMVL_REASON"
colnames(jasper)[8] <- "SCHEDULED"
colnames(jasper)[9] <- "JIC_CODE"
colnames(jasper)[10] <- "FAULT_CODE"
colnames(jasper)[11] <- "DAYS_SINCE_INSTALL"
colnames(jasper)[12] <- "CSI"
colnames(jasper)[13] <- "TSI"
colnames(jasper)[14] <- "MANU_DATE"

###########

jasper <- distinct(jasper, PN_OUT, RMVL_DATE, SN_OUT)                          #Remove rows with the same value in these columns

jasper <- distinct(jasper, PN_IN, RMVL_DATE, SN_IN)                            #Remove rows with the same value in these columns

jasper <- filter(jasper, CSI > 1)                                              #Filter out values with CSI < 1 

jasper$TSI <- round(as.numeric(gsub(",", ".", jasper$TSI)))                    #Replace comma with a dot
jasper$CSI <- round(as.numeric(gsub(",", ".", jasper$CSI)))                    #Replace comma with a dot

jasper$MANU_DATE <- as.Date(jasper$MANU_DATE, "%Y.%m.%d %H:%M:%S")
jasper$RMVL_DATE <- as.Date(jasper$RMVL_DATE, "%Y.%m.%d %H:%M:%S")

jasper <- mutate(jasper, DAYS = round(difftime(RMVL_DATE,MANU_DATE, units = "days")))  #Lifetime of part calculated

jasper$DAYS <- as.numeric(jasper$DAYS)                                         #Make sure DAYS is numeric

shinyServer(function(input, output, session) {                                 #Start of shinyServer reactive calculations
  
  
  
  ############### ALL DATA ################
  
  jasperALL <- select(jasper, AC_REG, PN_IN, SN_IN, PN_OUT, SN_OUT, RMVL_DATE, MANU_DATE,  #Select columns in ALL DATA
                      RMVL_REASON, SCHEDULED, JIC_CODE, FAULT_CODE, 
                      DAYS_SINCE_INSTALL, CSI, TSI, DAYS)
  
  
  tableInput <- reactive({                                                    #Reactive function for list selection in all data
    switch (input$select,                                                       #Switch for user input from dropdown list
            "2015"={
              
              jasperALL <- filter(jasperALL, as.Date(RMVL_DATE) > as.Date(Sys.Date() -    #Filter from user choice
                                                                            365))
              jasperALL
            },
            "2014"={
              
              jasperALL <- filter(jasperALL, as.Date(RMVL_DATE) > as.Date(Sys.Date() -
                                                                            730))
              jasperALL
            },
            "2013"={
              
              jasperALL <- filter(jasperALL, as.Date(RMVL_DATE) > as.Date(Sys.Date() -
                                                                            1095))
              jasperALL
            },
            "2010"={
              
              jasperALL <- filter(jasperALL, as.Date(RMVL_DATE) > as.Date(Sys.Date() -
                                                                            1825))
              jasperALL
            },
            "2005"={
              
              jasperALL <- filter(jasperALL, as.Date(RMVL_DATE) > as.Date(Sys.Date() -
                                                                            3650))
              jasperALL
            },
            "2000"={
              
              jasperALL <- filter(jasperALL, as.Date(RMVL_DATE) > as.Date(Sys.Date() -
                                                                            5475))
              jasperALL
            },
            "1995"={
              
              jasperALL <- filter(jasperALL, as.Date(RMVL_DATE) > as.Date(Sys.Date() -
                                                                            7300))
              jasperALL
            },
            "ALLDATA"={
              jasperALL                                                                #Show all data without any filtering
            }
    )
  })
  
  
  output$rawtbl = DT::renderDataTable(                                                                 #DT datatable ALL DATA
    
    tableInput(), extensions = 'ColVis', filter = 'top', options = list(dom = 'C<"clear">lfrtip', #Table data from tableInput function
                                                                        lengthMenu = list(c(5, 10, 15, 100, -1), c('5', '10', '15', '100', 'All')),                  #Table properties
                                                                        pageLength = 10, activate = 'mouseover')
  )
  
  output$downloadDataALL <- downloadHandler(                                  #Download button
    
    filename = function() {paste("AllData", "_",Sys.Date(),".csv",sep="")},   #File name 
    content = function(file){
      write.csv(jasperALL, file)                                              #Make .csv file
    }
  )
  
  ##################  ALL DATA  ######################
  
  
  
  #### CALCULATE DATA ####
  observeEvent(input$go, {                                                   #If action button pressed, do:
    
    #### PARTNUMBER CHOICE #####    
    if(input$PNChoice == "exact"){                                            #Radio buttons PNChoice
      jasper <- filter(jasper, PN_OUT == input$partNumber)                    #Filter out partnumbers from user choice
    }
    else{
      jasper <- filter(jasper, grepl(input$partNumber, PN_OUT))               #Filter out partnumbers if it contains a string (grepl())
    }
    
    MoreThanOne <- jasper                                                     #Make a temp database as jasper is now
    
    MoreThanOne <- group_by(MoreThanOne, PN_OUT)                              #Find out how many PN are in the table
    MoreThanOne <- summarise(MoreThanOne, N = n())                            #Find out how many parts are for each PN
    
    if(nrow(MoreThanOne) == 1){                                               #Make a temp variable if there are more than one PN in table
      one <- TRUE
    }
    else{
      one <- FALSE
    }
    
    #############################
    
    ######## DATE CHOICE #######
    
    
    
    jasper <- filter(jasper, as.character(RMVL_DATE) > as.character(input$dates[1]) & as.character(RMVL_DATE) < as.character(input$dates[2]))        #Filter from user date input
    
    
    ######## DATE CHOICE #######
    
    ################### PARTNUMBER FILTER ##########################
    
    groupby <- jasper%>%                                                       #Dplyr find mean, median, std and #PN for every PN CSI
      group_by(PN_OUT)%>%
      summarise(MEAN = mean(CSI), MEDIAN = median(CSI)*1.0, 
                StandardDev = sd(CSI), N = n())
    
    groupbyTSI <- jasper%>%                                                    #Dplyr find mean, median, std and #PN for every PN TSI
      group_by(PN_OUT)%>%
      summarise(MEAN_TSI = mean(TSI), MEDIAN_TSI = median(TSI)*1.0, 
                StandardDev_TSI = sd(TSI))
    
    
    
    jasper <- left_join(jasper, groupby, by = "PN_OUT")                      #Add calculated calculated CSI to jasper
    
    jasper <- left_join(jasper, groupbyTSI, by = "PN_OUT")                   #Add calculated calculated TSI to jasper
    
    count <- jasper                                                          #Make a new table as jasper is right here
    
    
    if(nrow(jasper) == 0){                                                #If there is no data in jasper, print error
      output$error <- renderText({
        paste("This partnumber has no removals")
      })
    }
    
    else{                                                                #Else print empty
      output$error <- renderText({
        paste(" ")
      })
    }
    
    if(one){                                                            #If there is only one partnumber, print how many removals it has
      output$count <- renderText({
        paste("This partnumber has" , nrow(count), "removals")
      })
    }
    else{
      output$count <- renderText({                                      #Else print empty string
        paste(" ")
      })
    }
    ################### PARTNUMBER FILTER ##########################
    
    ################## SELECTED DATA ################################
    jasperTBL <- select(jasper, AC_REG, PN_IN, SN_IN, PN_OUT, SN_OUT, RMVL_DATE, MANU_DATE,           #Select colomns for SELECTED DATA
                        RMVL_REASON, SCHEDULED, JIC_CODE, FAULT_CODE, 
                        DAYS_SINCE_INSTALL, CSI, TSI, DAYS)
    
    output$tbl = DT::renderDataTable(                                   #DT datatable in SELECTED DATA
      
      jasperTBL, extensions = 'ColVis', filter = 'top', options = list(dom = 'C<"clear">lfrtip',
                                                                       lengthMenu = list(c(5, 10, 15, 100, -1), c('5', '10', '15', '100', 'All')),
                                                                       pageLength = 10, activate = 'mouseover')
    )
    
    output$downloadDataTBL <- downloadHandler(                          #Download button for SELECTED DATA
      
      filename = function() {paste("SelectedData", "_",Sys.Date(),".csv",sep="")},
      content = function(file){
        write.csv(jasperTBL, file)
      }
    )
    
    ################## SELECTED DATA ################################
    
    ################## EXTREME VALUES ################################
    
    #Make a table with extreme data for both CSI/TSI. Values are in table if they are 2 std above or below median.
    
    extremeValuesData <- filter(jasper, CSI > (MEDIAN + 2*StandardDev) | CSI < (MEDIAN - 2*StandardDev)) 
    
    extremeValuesDataTSI <- filter(jasper, TSI > (MEDIAN_TSI + 2*StandardDev_TSI) | TSI < (MEDIAN_TSI - 2*StandardDev_TSI))
    
    
    if(one){                                                  #If only one partnumber
      if(nrow(extremeValuesData) > 0){                        #If there are any data marked as extreme value
        
        output$extremeCount <- renderText({                     #Show how many extreme values are in table
          
          paste("There are", nrow(extremeValuesData), "extreme values")     #Text showing extreme values
        })
        
        output$extreme <- renderUI({                                        #Checkbox showing where extreme values are and option to hide them
          checkboxInput("extreme", label = paste("Remove extreme values 
                                                 (values outside of: ", round(jasper$MEDIAN[1]), "+/-", 
                                                 round(2*(jasper$StandardDev[1])),")"), value = FALSE)
        })
      }
      
      else{                                                            #If there are more than one PN show empty string
        output$extremeCount <- renderText({
          
          paste(" ")
        })
      }
      }
    
    
    ################## EXTREME VALUES ################################
    
    ################## JITTER #################################
    
    output$jitter <- renderUI({                                      #Jitter output
      checkboxInput("jitter", label = "Jitter", value = TRUE)        #Checkbox for jitter
    })
    output$jitterTSI <- renderUI({                                   #Jitter output for TSI
      checkboxInput("jitterTSI", label = "Jitter", value = TRUE)     #Checkbox for jitter
    })
    
    ################## JITTER #################################
    
    
    ##################### LESS THAN 10 ###########################
    jasperLess <- filter(jasper, N <= 10)                            #If there are any partnumbers with less than 10 removals
    
    if(nrow(jasperLess) > 0){                                        #Check if there are any partnumbers in jasperLess
      output$lessThan10 <- renderUI({                                #UI output for partnumbers than have less than 10 removals
        checkboxInput("less10", label =                              #Checkbox for less than 10 removals (CSI)
                        "Remove partnumbers with less than 10 removals", value = FALSE)
      })
      
      output$lessThan10TSI <- renderUI({                             #(TSI)
        checkboxInput("less10TSI", label =
                        "Remove partnumbers with less than 10 removals", value = FALSE)
      })
      
      output$lessThan10RR <- renderUI({                              #(REMOVAL REASON)
        checkboxInput("less10RR", label = 
                        "Remove partnumbers with less than 10 removals", value = FALSE)
      })
      
      output$lessThan10S <- renderUI({                               #(SCHEDUAL)
        checkboxInput("less10S", label = 
                        "Remove partnumbers with less than 10 removals", value = FALSE)
      })
      
      output$lessThan10VIO <- renderUI({                             #(VIOLIN)
        checkboxInput("less10VIO", label = 
                        "Remove partnumbers with less than 10 removals", value = FALSE)
      })
      
      output$lessThan10L <- renderUI({                               #(LIFE)
        checkboxInput("less10L", label = 
                        "Remove partnumbers with less than 10 removals", value = FALSE)
      })
      
      less <<- TRUE                          #Temporary variables that help with if-loops
      hide <- FALSE
      
    }
    else{
      less <<- FALSE
      hide <- TRUE
    }
    
    ##################### LESS THAN 10 ###########################
    
    ##################### ABSOLUTE SLIDER UI ########################
    
    output$absoluteSlider <- renderUI({                                    #Slider for UI if absolute value chosen           
      
      jasper <- filter(jasper, CSI > 0)                                    #Filter out CSI that are larger than zero
      
      if(nrow(extremeValuesData) > 0){                                     #If there are any extreme values
        if(input$extreme){                                                 #If user wants to remove extreme values
          jasper1 <- filter(jasper,  CSI < (MEDIAN + 2*StandardDev)        #Filter out extreme values
                            & CSI > (MEDIAN - 2*StandardDev))
          minCSI <- min(jasper1$CSI[!is.na(jasper1$CSI)])                  #Find minimum value of CSI
          maxCSI <- max(jasper1$CSI[!is.na(jasper1$CSI)])                  #Find maximum value of CSI
        }
        else{                                                              #If there are extreme values, but user doesn't want to remove them
          minCSI <- min(jasper$CSI[!is.na(jasper$CSI)])
          maxCSI <- max(jasper$CSI[!is.na(jasper$CSI)])
        }
      }
      else{                                                                #If there aren't any extreme values
        minCSI <- min(jasper$CSI[!is.na(jasper$CSI)])
        maxCSI <- max(jasper$CSI[!is.na(jasper$CSI)])
      }
      
      sliderInput("absoluteSlider", label = h3("Choose absolute value:"),  #Slider UI, value are at both ends
                  min = minCSI, max = maxCSI, value = c(minCSI, maxCSI))
    }) 
    
    ##################### ABSOLUTE SLIDER UI ########################
    
    ################OUTPUT######################
    
    ############ CSI #################
    
    
    output$plot1 <- renderPlot({                                    #Make CSI plot                           
      
      if(less){                                                     #If less than 10 
        if(input$less10){                                           #If less than 10 checkbox is checked
          jasper <- filter(jasper, N > 10)                          #Filter out all PN with less than 10 values
          
          groupbyMORE <<- group_by(jasper, PN_OUT)                #How many PN are left after removing PN with less than 10
          groupbyMORE <- summarise(groupbyMORE, N = n())          #Gather PN together
          
          if(nrow(groupbyMORE) == 1){                             #If there is onlu one PN left, handle rest as "one" is true
            one <<- TRUE
          }
        }
      }
      
      
      plot1 <- ggplot(jasper, aes_string("PN_OUT", "CSI"))+                               #Plot PN_OUT vs. CSI from jasper
        geom_boxplot(outlier.colour = "red", outlier.size = 3)+                           #Make boxplot layer with outlier as red
        ggtitle("CSI per Part Number")+                                                   #Title of graph
        xlab("Part Number")+                                                              #X-label
        ylab("CSI")                                                                       #Y-label
      
      if(is.null(input$extreme) || is.na(input$extreme))                                  #If there are any errors from extreme
      {
        plot1 <- ggplot(jasper, aes_string("PN_OUT", "CSI"))+ 
          geom_boxplot(outlier.colour = "red", outlier.size = 3)+
          ggtitle("CSI per Part Number")+
          xlab("Part Number")+
          ylab("CSI") 
      }
      else{                                                                               #Else if there are no errors
        if(nrow(extremeValuesData) > 0){                                                    #If there are any extreme values
          if(input$extreme){                                                                #If user wants to hide extreme values
            jasper <- filter(jasper,  CSI < (MEDIAN + 2*StandardDev)                        #Filter out extreme values
                             & CSI > (MEDIAN - 2*StandardDev))
            
            plot1 <- ggplot(jasper, aes_string("PN_OUT", "CSI"))+ 
              geom_boxplot(outlier.colour = "red", outlier.size = 3)+
              ggtitle("CSI per Part Number")+
              xlab("Part Number")+
              ylab("CSI") 
            
          }
        }
      }
      if(one){                                                                          #If there is only one PN
        if(input$rogue == "interval"){                                                   #If user chooses interval
          if(input$dataManipulation == "std"){                                           #If the user chooses Standard Deviation for interval
            groupby <- jasper%>%
              group_by(PN_OUT)%>%
              summarise(MEAN = mean(CSI), MEDIANE = median(CSI)*1.0, 
                        StandardDevE = sd(CSI), N = n())                                   #Calculate new values
            jasper <- left_join(jasper, groupby, by = "PN_OUT")                            #Add new values to jasper
            plot1 <- plot1 +                                                              #Add layer to CSI plot
              geom_hline(yintercept = (jasper$MEDIANE + 
                                         jasper$StandardDevE*(input$scrollUp)), color = 'red')+                  #Layer: red line for upper std limit
              geom_hline(yintercept = (jasper$MEDIANE - 
                                         jasper$StandardDevE*(input$scrollLow)), color = 'red')                  #Layer: red line for lower std limit
          }
          if(input$dataManipulation == "absolute"){                                       #If user chooses Absolute value                      
            
            jasper <- filter(jasper, CSI > input$absoluteSlider[1]                        #Filter out from user preference of slider
                             & CSI < input$absoluteSlider[2])
            
            plot1 <- ggplot(jasper, aes_string("PN_OUT", "CSI"))+
              geom_boxplot(outlier.colour = "red", outlier.size = 3)+
              ggtitle("CSI per Part Number")+
              xlab("Part Number")+
              ylab("CSI")
          }
          
        }
      }
      else{                                                                            #Else if there are more than one
        plot1 <- plot1 + facet_wrap( ~ PN_OUT, scales="free")                          #Then split graphs by PN
      }
      
      if(input$jitter){                                                                #If user checks with jitter checkbox
        plot1 <- plot1 + geom_jitter()                                                 #Add layer, layer: jitter
      }
      plot1                                                                            #Return plot1
    })
    
    
    ############ CSI #################
    
    #######################################
    
    ################### EXTREME TSI VALUES ######################
    
    if(one){                                                                          #If just one PN
      if(nrow(extremeValuesData) > 0){                                                #If any extreme values
        
        output$extremeCount <- renderText({                                           #Make a UI textbox 
          
          paste("There are", nrow(extremeValuesData), "extreme values")               #Print how many extreme values there are
        })
        
        output$extremeTSI <- renderUI({                                                  #Make a UI checkbox
          checkboxInput("extreme", label = 
                          paste("Remove extreme values (values outside of: ",                         #Print what interval extreme values are outside of
                                round(jasper$MEDIAN[1]), "+/-", round((2*jasper$StandardDev[1])),")"), value = FALSE)
        })
      }
      
      else{                                                                           #Else if there are no extreme values 
        output$extremeCount <- renderText({                                           #Make a UI textbox                            
          
          paste(" ")                                                                  #Print empty string
        })
      }
    }
    
    ######################     SEE COMMENTS ABOVE / SAME CODE, JUST FOR TSI
    
    if(one){ 
      if(nrow(extremeValuesDataTSI) > 0){ 
        
        output$extremeCountTSI <- renderText({ 
          
          paste("There are", nrow(extremeValuesDataTSI), "extreme values") 
          
          
        })
        output$extremeTSI <- renderUI({
          checkboxInput("extremeTSI", label = paste("Remove extreme values 
                                                    (values outside of: ", round(jasper$MEDIAN_TSI[1]), "+/-", round(2*(jasper$StandardDev_TSI[1])),")"), value = FALSE)
        })
        }
      else{
        output$extremeCountTSI <- renderText({
          
          paste(" ")
        })
      }
      }
    
    ################### EXTREME TSI VALUES ######################
    
    ################### ABSOLUTE SLIDER TSI #########################
    
    ############### COMMENTS ARE THE SAME AS FOR CSI ABOVE
    
    output$absoluteSliderTSI <- renderUI({                                                                                         
      
      jasper <- filter(jasper, CSI > 0)
      
      if(nrow(extremeValuesData) > 0){
        if(input$extremeTSI){
          jasper1 <- filter(jasper,  TSI < (MEDIAN_TSI + 
                                              2*StandardDev_TSI) & TSI > (MEDIAN_TSI - 2*StandardDev_TSI))
          minTSI<- min(jasper1$TSI[!is.na(jasper1$TSI)])
          maxTSI<- max(jasper1$TSI[!is.na(jasper1$TSI)])
        }
        else{
          minTSI<- min(jasper$TSI[!is.na(jasper$TSI)])
          maxTSI<- max(jasper$TSI[!is.na(jasper$TSI)])
        }
      }
      else{
        minTSI<- min(jasper$TSI[!is.na(jasper$TSI)])
        maxTSI<- max(jasper$TSI[!is.na(jasper$TSI)])
      }
      
      sliderInput("absoluteSliderTSI", label = h3("Choose absolute value:"), 
                  min = minTSI, max = maxTSI, value = c(minTSI, maxTSI))
    }) 
    
    ################### ABSOLUTE SLIDER TSI #########################
    
    
    ############### TSI ###########################
    
    ############### COMMENTS ARE THE SAME AS FOR CSI ABOVE
    
    output$plot4 <- renderPlot({
      
      if(less){
        if(input$less10TSI){
          jasper <- filter(jasper, N > 10)
          
          groupbyMORE <<- group_by(jasper, PN_OUT)
          groupbyMORE <- summarise(groupbyMORE, N = n())
          
          if(nrow(groupbyMORE) == 1){
            one <<- TRUE
          }
        }
      }
      
      plot4 <- ggplot(jasper, aes_string("PN_OUT", "TSI"))+ 
        geom_boxplot(outlier.colour = "red", outlier.size = 3)+
        ggtitle("TSI per Part Number")+
        xlab("Part Number")+
        ylab("TSI")
      
      
      if(is.null(input$extremeTSI) || is.na(input$extremeTSI))
      {
        plot1 <- ggplot(jasper, aes_string("PN_OUT", "TSI"))+ 
          geom_boxplot(outlier.colour = "red", outlier.size = 3)+
          ggtitle("TSI per Part Number")+
          xlab("Part Number")+
          ylab("TSI") 
      }
      else{
        if(nrow(extremeValuesData) > 0){
          if(input$extremeTSI){
            jasper <- filter(jasper,  TSI < (MEDIAN_TSI + 2*StandardDev_TSI) & TSI > (MEDIAN_TSI - 2*StandardDev_TSI))
            
            plot4 <- ggplot(jasper, aes_string("PN_OUT", "TSI"))+ 
              geom_boxplot(outlier.colour = "red", outlier.size = 3)+
              ggtitle("TSI per Part Number")+
              xlab("Part Number")+
              ylab("TSI") 
            
          }
        }
      }
      if(one){
        if(input$rogue == "interval"){
          if(input$dataManipulation == "std"){
            groupby <- jasper%>%
              group_by(PN_OUT)%>%
              summarise(MEAN = mean(TSI), MEDIANE = median(TSI)*1.0, 
                        StandardDevE = sd(TSI), N = n())
            jasper <- left_join(jasper, groupby, by = "PN_OUT")
            plot4 <- plot4 +
              geom_hline(yintercept = (jasper$MEDIANE + jasper$StandardDevE*(input$scrollUpTSI)), color = 'red')+
              geom_hline(yintercept = (jasper$MEDIANE - jasper$StandardDevE*(input$scrollLowTSI)), color = 'red')
          }
          
          if(input$dataManipulation == "absolute"){
            
            jasper <- filter(jasper, TSI > input$absoluteSliderTSI[1] & TSI < input$absoluteSliderTSI[2])
            
            plot4 <- ggplot(jasper, aes_string("PN_OUT", "TSI"))+ 
              geom_boxplot(outlier.colour = "red", outlier.size = 3)+
              ggtitle("TSI per Part Number")+
              xlab("Part Number")+
              ylab("TSI")
          }
          
        }
      }
      else{
        plot4 <- plot4 + facet_wrap( ~ PN_OUT, scales="free")
      }
      
      if(input$jitterTSI){
        plot4 <- plot4 + geom_jitter()
      }
      plot4
    })
    
    
    ############### TSI ###########################
    
    
    ############ BAR PLOT ######################
    
    
    ###################### REMOVAL REASON ######################### 
    output$plot2 <- renderPlot({
      
      if(less){                                                                   #If there are PN with less than 10 removals
        if(input$less10){                                                         #If user checked less10 checkbox
          jasper <- filter(jasper, N > 10)                                        #Filter out PN with less than 10 removals
          
          groupbyMORE <<- group_by(jasper, PN_OUT)                                #How many PN are left after removing PN with less than 10
          groupbyMORE <- summarise(groupbyMORE, N = n())                          #Gather PN together
          
          if(nrow(groupbyMORE) == 1){                                             #If there is onlu one PN left, handle rest as "one" is true
            one <<- TRUE
          }
        }
      }
      
      if(one){                                                                   #If there is only one PN
        
        plot2 <- ggplot(jasper, aes(PN_OUT,fill=RMVL_REASON))+                   #PN_OUT bar with RMVL_REASON as fill
          geom_bar()+                                                            #Make barplot
          ggtitle("Removal Reason") +
          xlab("Part Number")+
          ylab("Number of parts")
        plot2
      }
      else{                                                                        #Else if there are more than one PN
        plot2 <- ggplot(jasper, aes(PN_OUT,fill=RMVL_REASON))+
          geom_bar()+
          ggtitle("Removal Reason") +
          facet_wrap( ~ PN_OUT, scales="free")+                                    #Split graph up by PN
          xlab("Part Number")+
          ylab("Number of parts")
        plot2
        
      }
      
    })
    
    
    ###################### REMOVAL REASON #########################
    
    ####################### SCHEDULED ###############################
    
    ##################### SEE COMMENTS ABOVE / SAME CODE AS FOR REMOVAL REASON
    
    output$plot3 <- renderPlot({
      
      if(less){
        if(input$less10){
          jasper <- filter(jasper, N > 10)
          
          groupbyMORE <<- group_by(jasper, PN_OUT)
          groupbyMORE <- summarise(groupbyMORE, N = n())
          
          if(nrow(groupbyMORE) == 1){
            one <<- TRUE
          }
        }
      }
      
      if(one){
        
        plot2 <- ggplot(jasper, aes(PN_OUT,fill=SCHEDULED))+
          geom_bar()+
          xlab("Part Number")+
          ylab("Number of parts")
        plot2
      }
      else{
        plot2 <- ggplot(jasper, aes(PN_OUT,fill=SCHEDULED))+
          geom_bar()+
          facet_wrap( ~ PN_OUT, scales="free")+
          xlab("Part Number")+
          ylab("Number of parts")
        plot2
        
      }
      
    })
    
    ####################### SCHEDULED ###############################
    
    ####################### BAR PLOTS ###############################
    
    ################### EXTREME VALUES VIOLIN ######################
    
    if(one){                                                              #If there is only one PN
      if(nrow(extremeValuesData) > 0){                                    #If there are any extreme values
        
        output$extremeCountVIO <- renderText({                            #Make a UI textbox
          
          paste("There are", nrow(extremeValuesData), "extreme values")   #Print how many extreme values there are
          
          
        })
        output$extremeVIO <- renderUI({                                   #Make a UI checkbox
          checkboxInput("extremeTSI", label = paste("Remove extreme values 
                                                    (values outside of: ", round(jasper$MEDIAN_TSI[1]), "+/-", 
                                                    round(jasper$StandardDev_TSI[1]),")"), value = FALSE)         #Checkbox showing where extreme values are and option to hide them
        })
      }
      else{                                                               #If more than one PN
        output$extremeCountVIO <- renderText({                            #Make a UI textbox
          
          paste(" ")                                                      #Print empty string
        })
      }
      }
    
    ################### EXTREME VALUES VIOLIN ######################
    
    ################# Violin plot ##############
    output$plot5 <- renderPlot({                                                       #Violin Plot
      
      if(less){
        if(input$less10){
          jasper <- filter(jasper, N > 10)
          
          groupbyMORE <<- group_by(jasper, PN_OUT)
          groupbyMORE <- summarise(groupbyMORE, N = n())
          
          if(nrow(groupbyMORE) == 1){
            one <<- TRUE
          }
        }
      }
      
      if(one){                                                                  #If one PN
        if(nrow(extremeValuesData) > 0){
          if(input$extremeVIO){
            jasper <- filter(jasper, CSI < (MEDIAN + 2*StandardDev) & CSI > (MEDIAN - 2*StandardDev)) 
          }
        }
        
        plot5 <- ggplot(jasper, aes(PN_OUT,CSI))+
          geom_violin()+
          geom_jitter()+
          xlab("Part Number")+
          ylab("Number of parts")
        plot5
      }
      
      else{                                                                    #Else if more than one PN
        if(nrow(extremeValuesData) > 0){
          if(input$extremeVIO){
            jasper <- filter(jasper, CSI < (MEDIAN + 2*StandardDev) & CSI > (MEDIAN - 2*StandardDev)) 
          }
        }
        
        plot5 <- ggplot(jasper, aes(PN_OUT,CSI))+
          geom_violin()+
          geom_jitter()+
          facet_wrap( ~ PN_OUT, scales="free")+
          xlab("Part Number")+
          ylab("Number of parts")
        plot5
        
      }
      
    })
    
    
    ################# Violin plot ##############
    
    ################## ROGUE TABLE ######################
    
    output$rogueTable <- renderTable({                                                              #Make a table for UI
      if(nrow(extremeValuesData) > 0){                                                              #If there are extreme values
        if(input$extreme){                                                                          #If extreme checkbox checked
          jasper <- filter(jasper, CSI < (MEDIAN + 2*StandardDev) & CSI > (MEDIAN - 2*StandardDev)) #Filter out extreme values
        }
      }
      if(input$rogue == "interval"){                                                                #If Interval chosen
        if(input$dataManipulation == "std"){                                                        #If Std interval chosen
          
          rogueTable <- jasper                                                                          #Make a new table for more filtering
          
          if(input$outsideCSI == 'Outside'){                                                            #If outside chosen
            rogueTable <- filter(rogueTable, CSI > (MEDIAN + StandardDev*(input$scrollUp)) |            #Filter from user slider value
                                   CSI < (MEDIAN - StandardDev*(input$scrollLow)))
          }
          else{                                                                                         #Else if inside
            rogueTable <- filter(rogueTable, CSI < (MEDIAN + StandardDev*(input$scrollUp)) &            #Filter from user slider value
                                   CSI > (MEDIAN - StandardDev*(input$scrollLow)))
          }
          rogueTable <- select(rogueTable, AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)                 #Select What to display in table
          rogueTable <- arrange(rogueTable, desc(CSI))                                                  #Arrange by descending CSI value
        }
        if(input$dataManipulation == "absolute"){                                                     #If absolute interval chosen
          
          rogueTable <- jasper                                                                      #Make a new table for more filtering
          
          
          if(input$outsideCSI == 'Outside'){                                                        #If outside chosen
            rogueTable <- filter(rogueTable, CSI < input$absoluteSlider[1]                          #Filter from user slider value
                                 | CSI > input$absoluteSlider[2])
          }
          else{                                                                                     #Else if inside
            rogueTable <- filter(rogueTable, CSI > input$absoluteSlider[1]                          #Filter from user slider value
                                 & CSI < input$absoluteSlider[2])
          }
          
          
          rogueTable <- select(rogueTable, AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)             #Select what to display in table
          rogueTable <- arrange(rogueTable, desc(CSI))                                              #Arrange by descending CSI value
          
        }
        rogueTable <<- rogueTable                                                                     #Return rogueTable
        
        
      }
    })
    
    
    
    output$downloadData <- downloadHandler(                                                       #Download rogueTable as a .csv file
      
      filename = function() {paste("RogueValues", "_",Sys.Date(),".csv",sep="")},                   #Name the file
      content = function(file){
        write.csv(rogueTable, file)
      }
    )
    
    
    ##################### SEE COMMENTS ABOVE / SAME CODE AS FOR rogueTable  
    output$rogueTableTSI <- renderTable({
      if(nrow(extremeValuesData) > 0){
        if(input$extremeTSI){
          jasper <- filter(jasper, TSI < (MEDIAN_TSI + 2*StandardDev_TSI) 
                           & TSI > (MEDIAN_TSI - 2*StandardDev_TSI)) 
        }
      }
      if(input$rogue == "interval"){
        if(input$dataManipulation == "std"){
          
          rogueTableTSI <- jasper
          
          if(input$outsideTSI == 'Outside'){
            rogueTableTSI <- filter(rogueTableTSI, TSI > (MEDIAN_TSI + StandardDev_TSI*(input$scrollUp)) | 
                                      TSI < (MEDIAN_TSI - StandardDev_TSI*(input$scrollLow)))
          }
          else{
            rogueTableTSI <- filter(rogueTableTSI, TSI < (MEDIAN_TSI + StandardDev_TSI*(input$scrollUp)) & 
                                      TSI > (MEDIAN_TSI - StandardDev_TSI*(input$scrollLow)))
          }
          rogueTableTSI <- select(rogueTableTSI, AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)
          rogueTableTSI <- arrange(rogueTableTSI, desc(TSI))
        }
        if(input$dataManipulation == "absolute"){
          
          rogueTableTSI <- jasper
          
          
          if(input$outsideTSI == 'Outside'){
            rogueTableTSI <- filter(rogueTableTSI, TSI < input$absoluteSliderTSI[1] | TSI > input$absoluteSliderTSI[2])
          }
          else{
            rogueTableTSI <- filter(rogueTableTSI, TSI > input$absoluteSliderTSI[1] & TSI < input$absoluteSliderTSI[2])
          }
          
          
          rogueTableTSI <- select(rogueTableTSI, AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)
          rogueTableTSI <- arrange(rogueTableTSI, desc(TSI))
          
        }
        rogueTableTSI <<- rogueTableTSI
        
        
      }
    })
    
    output$downloadDataTSI <- downloadHandler(
      
      filename = function() {paste("RogueValues", "_",Sys.Date(),".csv",sep="")},
      content = function(file){
        write.csv(rogueTableTSI, file)
      }
    )
    
    ################## ROGUE TABLE ######################
    
    ########## Per airplane, PLOTS BELOW ##########
    
    output$plot6 <- renderPlot({                                                  #Make plot "Rogue Parts" below main plot
      
      
      
      if(one){                                                                    #If there is only one PN
        if(input$rogue == "interval"){                                            #If Interval is chosen
          if(input$dataManipulation == "std"){                                    #If Standard Deviation is chosen
            rogueTableP <<- jasper %>%                                            #Make new table, rogueTableP       
              filter(CSI > (MEDIAN + StandardDev*(input$scrollUp)) |              #Filter from scroll input
                       CSI < (MEDIAN - StandardDev*(input$scrollLow)))%>%
              select(AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)%>%              #Select what to have in table
              arrange(desc(CSI))%>%                                               #Arrange by descending CSI
              group_by(AC_REG)%>%                                                 #Group by airplanes
              summarise(AVERAGE_CSI = mean(CSI), N = n())                         #Summarise mean CSI
          }
          
          if(input$dataManipulation == "absolute"){                               #If Absolute is chosen
            
            rogueTableP <<- jasper %>%                                                
              filter(CSI < input$absoluteSlider[1] | CSI > input$absoluteSlider[2])%>%  #Filter by absolute slider input
              select(AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)%>%
              arrange(desc(CSI))%>%
              group_by(AC_REG)%>%
              summarise(AVERAGE_CSI = mean(CSI), N = n())
          }
          
        }
        else{                                                                    #Else if there are more than one PN
          rogueTableP <<- jasper %>%
            select(AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)%>%
            arrange(desc(CSI))%>%
            group_by(AC_REG)%>%
            summarise(AVERAGE_CSI = mean(CSI), N = n())
        }
      }
      
      
      
      
      if(nrow(rogueTableP) > 0){
        plot6 <- ggplot(rogueTableP, aes(x=1, y=N, fill= AVERAGE_CSI)) +              #Plot rogueTableP 
          ggtitle("Total parts removed per Airplane") +                               #Title
          coord_polar(theta='y')                                                      #Polar Plot
        
        y.breaks <- cumsum(rogueTableP$N) - rogueTableP$N/2                           #Split up
        
        
        plot6 <- plot6 +
          geom_bar(stat="identity", color='black') +                                  
          guides(fill=guide_legend(override.aes=list(colour=NA)))+                    #Remove black diagonal line from legend
          theme(axis.ticks=element_blank(),                                           #The axis ticks
                axis.title=element_blank(),                                           #The axis labels
                axis.text.y=element_blank())+                                         #The 0.75, 1.00, 1.25 labels.
          theme(axis.text.x=element_text(color='black'))+
          scale_y_continuous(
            breaks=y.breaks,                                                          #Where to place the labels
            labels=rogueTableP$AC_REG                                                 #The labels
          )
        
        plot6
      }
    })
    
    
    output$plot7 <- renderPlot({
      
      ############# SAME CODA AS IN PLOT 6
      
      if(one){
        if(input$rogue == "interval"){
          if(input$dataManipulation == "std"){
            rogueTableP <<- jasper %>%                                                
              filter(CSI < (MEDIAN + StandardDev*(input$scrollUp)) & 
                       CSI > (MEDIAN - StandardDev*(input$scrollLow)))%>%
              select(AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)%>%
              arrange(desc(CSI))%>%
              group_by(AC_REG)%>%
              summarise(AVERAGE_CSI = mean(CSI), N = n())
          }
          
          if(input$dataManipulation == "absolute"){
            
            rogueTableP <<- jasper %>%                                               
              filter(CSI > input$absoluteSlider[1] & CSI < input$absoluteSlider[2])%>%
              select(AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)%>%
              arrange(desc(CSI))%>%
              group_by(AC_REG)%>%
              summarise(AVERAGE_CSI = mean(CSI), N = n())
          }
          
        }
        else{
          rogueTableP <<- jasper %>%
            select(AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)%>%
            arrange(desc(CSI))%>%
            group_by(AC_REG)%>%
            summarise(AVERAGE_CSI = mean(CSI), N = n())
        }
      }
      if(nrow(rogueTableP) > 0){
        plot7 <- ggplot(rogueTableP, aes(x=1, y=N, fill= AVERAGE_CSI)) +
          ggtitle("Total parts removed per Airplane") +
          coord_polar(theta='y')
        
        y.breaks <- cumsum(rogueTableP$N) - rogueTableP$N/2
        
        
        plot7 <- plot7 +
          geom_bar(stat="identity", color='black') +                                          
          guides(fill=guide_legend(override.aes=list(colour=NA)))+                            #Remove black diagonal line from legend
          theme(axis.ticks=element_blank(),                                                   #The axis ticks
                axis.title=element_blank(),                                                   #The axis labels
                axis.text.y=element_blank())+                                                 #The 0.75, 1.00, 1.25 labels.
          theme(axis.text.x=element_text(color='black')) +
          scale_y_continuous(
            breaks=y.breaks,                                                                  #Where to place the labels
            labels=rogueTableP$AC_REG                                                         #The labels
          )
        plot7
      }
    })
    
    
    output$plot8 <- renderPlot({
      
      ########################### SAME CODE AS IN PLOT 6
      if(one){
        rogueTableP <<- jasper %>%
          select(AC_REG, PN_OUT, CSI, TSI, SN_OUT, RMVL_DATE)%>%
          arrange(desc(CSI))%>%
          group_by(AC_REG)%>%
          summarise(AVERAGE_CSI = mean(CSI), N = n())
      }
      
      plot8 <- ggplot(rogueTableP, aes(x=1, y=N, fill= AVERAGE_CSI)) +
        ggtitle("Total parts removed per Airplane") +
        coord_polar(theta='y')
      
      y.breaks <- cumsum(rogueTableP$N) - rogueTableP$N/2
      
      
      plot8 <- plot8 +
        geom_bar(stat="identity", color='black') +
        guides(fill=guide_legend(override.aes=list(colour=NA)))+                           #Remove black diagonal line from legend
        theme(axis.ticks=element_blank(),                                                  #The axis ticks
              axis.title=element_blank(),                                                  #The axis labels
              axis.text.y=element_blank())+                                                #The 0.75, 1.00, 1.25 labels.
        theme(axis.text.x=element_text(color='black')) +
        scale_y_continuous(
          breaks=y.breaks,                                                                 #Where to place the labels
          labels=rogueTableP$AC_REG                                                        #The labels
        )
      plot8
      
    })
    
    ########## Per airplane, PLOTS BELOW ##########
    
    #################### LIFE ###########################
    
    counter <- filter(count, DAYS >= 0)                                           #Filter out days with value less than 0
    
    countDAYS <- counter$DAYS                                                     #Make array
    
    minYear <- min(counter$DAYS[!is.na(counter$DAYS)])                            #Find minumum value of DAYS
    
    maxYear <- max(counter$DAYS[!is.na(counter$DAYS)])                            #Find maximum value of DAYS
    
    output$dateSlider <- renderUI({                                               #Date slider UI
      sliderInput("dateSlider", label = h5("Choose Value [years]"),               #Slider
                  min = 0, max = round(maxYear/365), value = c(0, round(maxYear/365)))      #Minum value 0, maximum value maxYear
    })
    
    output$yearTableOut <- renderTable({                                          #Year outside of picked interval
      
      if(less){                                                                   #If there are PN with less than 10 removals
        if(input$less10L){                                                        #If user checked checkbox to remove less than 10
          jasper <- filter(counter, N > 10)                                       #Filter out PN with less than 10 removals
        }
      }
      
      yearTable <- filter(counter, DAYS < (input$dateSlider[1]*365) | DAYS > (input$dateSlider[2]*365)) #Filter out from date slider
      
      yearTable <- mutate(yearTable, YEAR = DAYS/365)                                                   #Make new column with year
      
      yearTable <- select(yearTable, AC_REG, SN_OUT, DAYS, RMVL_DATE, MANU_DATE, YEAR)                  #Select what to display
      
      yearTable <- arrange(yearTable, desc(DAYS))                                                       #Arrange by descending days
      
      yearTableOut <<- yearTable                                                                        #Make yearTableOut
      
      yearTable
      
    })
    
    output$yearTableIn <- renderTable({
      
      #SAME CODE AS IN yearTableIn
      
      if(less){
        if(input$less10L){
          jasper <- filter(counter, N > 10)
          
        }
      }
      
      yearTable <- filter(counter, DAYS > (input$dateSlider[1]*365) & DAYS < (input$dateSlider[2]*365))
      
      yearTable <- mutate(yearTable, YEAR = DAYS/365)
      
      yearTable <- select(yearTable, AC_REG, SN_OUT, DAYS, RMVL_DATE, MANU_DATE, YEAR)
      
      yearTable <- arrange(yearTable, desc(DAYS))
      
      yearTableIn <<- yearTable
      
      yearTable
      
      
    })
    
    
    output$downloadYearIn <- downloadHandler(                                            #Download yearTableIn
      
      filename = function() {paste("LifeTimeOfParts", "_",Sys.Date(),".csv",sep="")},
      content = function(file){
        write.csv(yearTableIn, file)
      }
    )
    
    output$downloadYearOut <- downloadHandler(                                           #Download yearTableOut
      
      filename = function() {paste("LifeTimeOfParts", "_",Sys.Date(),".csv",sep="")},
      content = function(file){
        write.csv(yearTableOut, file)
      }
    )
    
    output$plot9 <- renderPlot({                                                        #Life plot
      counter <- jasper                                                                 #Make new table counter
      
      if(less){                                                                   #If less than 10
        if(input$less10L){                                                        #If less than 10 checkbox is checked
          
          jasper <- filter(jasper, N > 10)                                        #Filter out all PN with less than 10 values
          
          groupbyMORE <<- group_by(jasper, PN_OUT)                                #How many PN are left after removing PN with less than 10
          groupbyMORE <- summarise(groupbyMORE, N = n())                          #Gather PN together
          
          if(nrow(groupbyMORE) == 1){                                             #If there is onlu one PN left, handle rest as "one" is true
            one <<- TRUE
          }
        }
      }
      
      if(one){                                                                    #If one
        
        if(input$years){                                                          #If input years checked
          jasper <- filter(jasper, DAYS > (input$dateSlider[1]*365)                 #Filter by slider input
                           & DAYS < (input$dateSlider[2]*365)) 
        }
        
        jasper <- mutate(jasper, YEAR = DAYS/365)                                 #Make new column named YEAR
        
        plot9 <- ggplot(jasper, aes(PN_OUT, YEAR))+                               #Life plot
          geom_boxplot(outlier.colour = "red", outlier.size = 3)+                 #Make boxplot layer
          ggtitle("Removal Reason")+
          geom_jitter()+
          xlab("Part Number")+
          ylab("Lifetime of parts [years]")
        plot9
        
      }
      else{
        jasper <- mutate(jasper, YEAR = DAYS/365)
        
        plot9 <- ggplot(jasper, aes(PN_OUT, YEAR))+
          geom_boxplot(outlier.colour = "red", outlier.size = 3)+
          ggtitle("Removal Reason")+
          geom_jitter()+
          facet_wrap( ~ PN_OUT, scales="free")+
          xlab("Part Number")+
          ylab("Lifetime of parts [years]")
        plot9
        
      }
      
    })
    
    #################### LIFE ########################### 
    
    ###################### INFORMATION TEXT ABOVE PLOTS #####################
    
    if(one){                                                                              #If one                         
      output$information <- renderText({                                                    #UI text output
        if(nrow(extremeValuesData) > 0){                                                    #If there are any extreme values
          if(input$extreme){                                                                #If user wants to remove extreme values
            jasper <- filter(jasper,  CSI < (MEDIAN + 2*StandardDev)                        #Filter out extreme values
                             & CSI > (MEDIAN - 2*StandardDev))
            
            groupby1 <- jasper%>%                                                           #Make new table groupby1
              summarise(MEAN = mean(CSI), MEDIAN = median(CSI)*1.0,                         #Calculate some values
                        StandardDev = sd(CSI))
            
            paste("Mean:", round(groupby1$MEAN[1]), "Median:", round(groupby1$MEDIAN[1]),     #Paste calculated values to text output
                  "Standard Deviation:", round(groupby1$StandardDev[1]))
            
          }
          else{                                                                             #Else if user doesn't want to hide extreme values
            paste("Mean:", round(jasper$MEAN[1]), "Median:", round(jasper$MEDIAN[1]), 
                  "Standard Deviation:", round(jasper$StandardDev[1]))
          }
        }
        else{                                                                               #No extreme values 
          paste("Mean:", round(jasper$MEAN[1]), "Median:", round(jasper$MEDIAN[1]), 
                "Standard Deviation:", round(jasper$StandardDev[1]))
        }
      })
    }
    
    
    ############# SAME CODE AS FOR CSI ABOVE
    if(one){                          
      output$informationTSI <- renderText({
        if(nrow(extremeValuesData) > 0){ 
          if(input$extremeTSI){
            jasper <- filter(jasper,  TSI < (MEDIAN_TSI + 2*StandardDev_TSI) 
                             & TSI > (MEDIAN_TSI - 2*StandardDev_TSI))
            
            groupby1 <- jasper%>%
              summarise(MEAN_TSI = mean(TSI), MEDIAN_TSI = median(TSI)*1.0, 
                        StandardDev_TSI = sd(TSI))
            
            paste("Mean:", round(groupby1$MEAN_TSI[1]), "Median:", 
                  round(groupby1$MEDIAN_TSI[1]), 
                  "Standard Deviation:", round(groupby1$StandardDev_TSI[1]))
            
          }
          else{
            paste("Mean:", round(jasper$MEAN_TSI[1]), "Median:", round(jasper$MEDIAN_TSI[1]), 
                  "Standard Deviation:", round(jasper$StandardDev_TSI[1]))
          }
        }
        else{
          paste("Mean:", round(jasper$MEAN_TSI[1]), "Median:", round(jasper$MEDIAN_TSI[1]), 
                "Standard Deviation:", round(jasper$StandardDev_TSI[1]))
        }
      })
    }
    
    ###################### INFORMATION TEXT ABOVE PLOTS #####################
    
    
    #################### PER AIRPLANE, PLOTS BELOW UI ########################
    
    if(nrow(MoreThanOne) == 1){                                                           #If there is only one PN
      
      output$oneData <- renderUI({                                                        #UI output
        tabsetPanel(                                                                      #Tabpanel
          type = "tab",                                                               #Tabs
          tabPanel("Rogue Parts", plotOutput("plot6")),                               #Tab - Rogue Parts - Plot 6 displayed
          tabPanel("Good Parts", plotOutput("plot7")),                                #Tab - Good Parts - Plot 7 displayed
          tabPanel("All Parts", plotOutput("plot8"))                                  #Tab - All Parts - Plot 8 displayed
        )
      })
    }
    else{                                                                                 #Else if there are more than one PN
      output$oneData <- renderUI({                    
        #Display nothing
      })
    }
    
    #################### PER AIRPLANE, PLOTS BELOW UI ########################
    
    })
  })

