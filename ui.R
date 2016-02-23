

library(shiny)
library(ggplot2)
library(DT)
library(shinydashboard)


jasper <- read.csv("jasper.csv", header = TRUE)


############# SIDEBAR ######################

sidebar <- dashboardSidebar(             #Tabs in sidebar
  sidebarMenu(
    menuItem("Plots", tabName = "plots", icon = icon("dashboard")),
    menuItem("All Data", tabName = "raw", icon = icon("th")),
    menuItem("Slected Data", tabName = "data", icon = icon("th"))
  ),
  
  ############PART NUMBER######################
  
  textInput("partNumber", label = "Enter Partnumber/s", value = ""),         #Text input for partnumber
  
  
  
  textOutput("error"),                   #Error if partnumber is not in database
  tags$head(
    tags$style("#error{color: red;       #Text format
               font-size: 12px;
               font-style: bold;
               }")
  ),
  
  radioButtons(                          #Radiobuttons of containing or exact value
    "PNChoice", label = " ",
    choices = list("Exact Value" = "exact",
                   "Contains" = "contain"), selected = "exact"
  ),
  ###############################################
  
  ############DATE RANGE########################
  
  dateRangeInput(                                                                        #Date input, 30 days from system date
    "dates", label = h5("Data from:"), format = 'yyyy-mm-dd', start = Sys.Date() - 30
  ),
  
  
  ############DATE RANGE########################
  
  
  
  #################### CALCULATE #########################
  
  actionButton("go", "Calculate Data"),           #Action button
  
  textOutput("count"),                            #Display how many parts are shown
  
  tags$head(
    tags$style("#count{color: white;              #Text format for count
               font-size: 12px;
               font-style: italic;
               }")
  ),
  
  #################### CALCULATE #########################
  
  #################### OPTIONS ##########################
  radioButtons(                                     #Radiobuttons for data or interval
    "rogue", "I want to see:",
    c("All data" = "Data",
      "Interval" = "interval")
  ),
  
  conditionalPanel(                                 #Condition - if interval - Absolute/Std
    condition = "input.rogue == 'interval'",
    radioButtons(
      "dataManipulation", "Choose:",
      c("Absolute value" = "absolute",
        "Standard Deviation" = "std")
    )
  )
  
  #################### OPTIONS ##########################
  
)

####################### SIDEBAR ########################


##################### BODY ###########################

body <- dashboardBody(tabItems(                  #Begin body
  ########################## ALL DATA ########################
  tabItem(                          #Begin tab
    tabName = "raw",                #Tab name
    
    box(                            #Begin box
      radioButtons(                 #Radio buttons
        "downl", "I want to:",      #Name
        c(                          #List choices
          "Download Data" = "download",
          "Show data here (takes few seconds to load)" = "show"
        )
      ),
      conditionalPanel(             #Condition - if show - Show dropdown list
        condition = "input.downl == 'show'",
        
        selectInput("select", label = h3("Select:"),                              #Dropdown list
                    choices = list("1 year   - Parts removed within the last year          ---- Load time about 5 sec" = 2015, 
                                   "2 years  - Parts removed within the last two years     ---- Load time about 15 sec" = 2014,
                                   "3 years  - Parts removed within the last three years   ---- Load time about 30 sec" = 2013,
                                   "5 years  - Parts removed within the last five years    ---- Load time about 2 min" = 2010,
                                   "10 years - Parts removed within the last ten years     ---- Load time about 5 min" = 2005,
                                   "15 years - Parts removed within the last fifteen years ---- Load time about 10 min" = 2000,
                                   "20 years - Parts removed within the last twenty years  ---- Load time about 20 min" = 1995,
                                   "All data" = "ALLDATA"
                    ), 
                    selected = 1),
        
        title = "All data", status = "warning", DT::dataTableOutput('rawtbl') #Make table from choice in dropdown list
      ),
      conditionalPanel(condition = "input.downl == 'download'",              #Condition - if download - show download button
                       downloadButton('downloadDataALL', 'Download')),
      width = 25
    )
  ),
  
  ########################## ALL DATA ########################
  
  ########################## PLOTS #########################
  tabItem(                        #Begin tab
    tabName = "plots",            #Tab name - Plots
    fluidRow(                 #Make boxes in a row, side by side
      box(                      #Make a box
        title = "Plot", status = "info", solidHeader = TRUE, #Box properties
        tabsetPanel(              #Make tabs inside of box
          type = "tab",           #Types - tab / Can be "pills"
          
          ####################### CSI #################################
          tabPanel("CSI",                         #First tab
                   uiOutput("jitter"),                   #Jitter checkbox
                   textOutput("extremeCount"),           #How many extreme values are
                   uiOutput("extreme"),                  #Checkbox if extreme values are any
                   uiOutput("lessThan10"),               #Checkbox if less than 10 values
                   uiOutput("information"),              #Show mean, median and standard deviation
                   conditionalPanel(                     #Condition - if interval - show slider
                     condition = "input.rogue == 'interval'",
                     conditionalPanel(                   #Condition - if std - show standard deviation slider 
                       condition = "input.dataManipulation == 'std'",
                       sliderInput(                      #Slider for upper limit
                         "scrollUp",
                         "Standard deviation from median: Upper-limit",
                         min = 0.1,                      #Minimum value
                         max = 2,                        #Maximum value
                         value = 1                       #Starting value
                       )
                     ),
                     conditionalPanel(                     #Condition - if absolute - show absolute slider
                       condition = "input.dataManipulation == 'absolute'",
                       uiOutput("absoluteSlider")
                     )),
                   plotOutput("plot1", height = 500),                #Show CSI plot
                   conditionalPanel(
                     condition = "input.rogue == 'interval'",            #Condition - if interval - show slider
                     conditionalPanel(
                       condition = "input.dataManipulation == 'std'",      #Condition - if Std - show standar deviation slider
                       sliderInput(                                        #Slider for lower limit
                         "scrollLow",                                      #Slider ID
                         "Standard deviation from median: Lower-limit",    #Name
                         min = 0.1,                                        #Minimum value
                         max = 2,                                          #Maximum value
                         value = 1                                         #Starting value
                       )))),
          
          ####################### CSI #################################
          
          
          ####################### TSI #################################
          
          ############## SEE COMMENTS FOR CSI, SAME CODE ########################
          tabPanel( 
            "TSI",
            uiOutput("jitterTSI"),
            textOutput("extremeCountTSI"),
            uiOutput("extremeTSI"),
            uiOutput("lessThan10TSI"),
            uiOutput("informationTSI"),
            conditionalPanel(
              condition = "input.rogue == 'interval'",
              conditionalPanel(
                condition = "input.dataManipulation == 'std'",
                sliderInput(
                  "scrollUpTSI",
                  "Standard deviation from median: Upper-limit",
                  min = 0.1,
                  max = 2,
                  value = 1
                )
              ),
              conditionalPanel(
                condition = "input.dataManipulation == 'absolute'",
                uiOutput("absoluteSliderTSI")
                
              )),
            plotOutput("plot4", height = 500),
            
            conditionalPanel(
              condition = "input.rogue == 'interval'",
              conditionalPanel(
                condition = "input.dataManipulation == 'std'",
                sliderInput(
                  "scrollLowTSI",
                  "Standard deviation from median: Lower-limit",
                  min = 0.1,
                  max = 2,
                  value = 1
                )
              )
            )
          ),
          
          ####################### TSI #################################
          
          ###################### REMOVAL REASON #########################
          tabPanel("Removal Reason",                             #Tabpanel - Removal Reason
                   uiOutput("lessThan10RR"),                     #Checkbox less than 10 
                   plotOutput("plot2", height = 500)),           #Removal reason plot
          ###################### REMOVAL REASON #########################
          
          ####################### SCHEDULED ###############################
          tabPanel("Scheduled",                                  #Tabpanel - Scheduled
                   uiOutput("lessThan10S"),                      #Checkbox less than 10 removals
                   plotOutput("plot3", height = 500)),           #Scheduled plot
          ####################### SCHEDULED ###############################        
          
          ####################### VIOLIN ###############################
          tabPanel(                                                      #Tabpanel - Violin
            "Violin - CSI",
            checkboxInput("extremeVIO", label = "Remove extreme values", value = FALSE), #Checkbox if extreme values
            textOutput("extremeCountVIO"),                       #Display how many extreme values
            uiOutput("lessThan10VIO"),                           #Checkbox less than 10 removals
            plotOutput("plot5", height = 500)                    #Violin plot
          ),
          
          ####################### VIOLIN ###############################
          
          ####################### LIFE ###############################
          tabPanel(                                        #Tabpanel - Life
            "Life",
            checkboxInput("years", label = "Remove parts with specific lifetime", value = FALSE), #Checkbox for time interval
            uiOutput("lessThan10L"),                      #Checkbox if less than 10 removals
            conditionalPanel(condition = "input.years == true",   #Condition - if time interval true - show date slider
                             uiOutput("dateSlider")),             #Date slider
            plotOutput("plot9", height = 500)                     #Life plot
            
          ), id = "tabs"                                         #Id for tabs
        ),
        
        ####################### PER AIRPLANE, BELOW PLOTS ###############################
        conditionalPanel(                                               #Condition - if CSI - show plot below main plot
          condition = "input.rogue == 'interval'",
            conditionalPanel(
              condition = "input.tabs == 'CSI'",
        uiOutput("oneData"))),                                             #Plots below main plots
        ####################### PER AIRPLANE, BELOW PLOTS ###############################
        width = 8),                                                     #Width of main box
      
      ####################### LIFE ###############################
      
      ####################### ROGUE PARTS CSI ###############################
      conditionalPanel(                                               #Condition - if CSI - show table for CSI rogue parts
        condition = "input.tabs == 'CSI'",
        conditionalPanel(
          condition = "input.rogue == 'interval'",                    #Condition - if interval - show rogue parts
          fluidRow(                                                   #Box are in a row
            box(                                                      #Make box for rogue parts
              title = "Rogue Parts - CSI", status = "warning", solidHeader = TRUE, collapsible = TRUE, #Box properties
              radioButtons(                                           #Radiobuttons for Outside/Inside
                "outsideCSI", "Parts in table are:",
                c("Outside interval" = "Outside",
                  "Inside interval" = "Inside")),
              tableOutput("rogueTable"),                              #Rogue Table output
              downloadButton('downloadData', 'Download'),             #Download button for table
              width = 4))                                             #Rogue box width
        )
      ),
      
      ####################### ROGUE PARTS CSI ###############################
      
      ####################### ROGUE PARTS TSI ###############################
      
      ##SEE ROGUE PARTS CSI, SAME CODE
      
      conditionalPanel( 
        condition = "input.tabs == 'TSI'",
        conditionalPanel(
          condition = "input.rogue == 'interval'",
          fluidRow(  
            box(
              title = "Rogue Parts - TSI", status = "warning", solidHeader = TRUE, collapsible = TRUE,
              radioButtons(
                "outsideTSI", "Parts in table are:",
                c("Outside interval" = "Outside",
                  "Inside interval" = "Inside")),
              tableOutput("rogueTableTSI"),
              downloadButton('downloadDataTSI', 'Download'),
              width = 4))
        )
      ),
      
      ####################### ROGUE PARTS TSI ###############################
      
      
      ####################### ROGUE PARTS LIFE ############################### 
      conditionalPanel(                              #Condition - tab is Life - show life options
        condition = "input.tabs == 'Life'",
        conditionalPanel(                              #Condition - if years checkbox checked - show box with life list
          condition = "input.years == true",
          
          box(                                         #Box for list of life
            title = "Rogue Parts - Life", status = "warning", solidHeader = TRUE, collapsible = TRUE, #box properties
            radioButtons(                                             #Radio buttons with list options
              "outside", "Parts in table are:",
              c("Outside interval" = "Outside",                       #Choices
                "Inside interval" = "Inside")),
            conditionalPanel(condition = "input.outside == 'Outside'", #Condition - if outside - Show list of parts outside of slider interval
                             tableOutput("yearTableOut"),
                             downloadButton('downloadYearOut', 'Download')),
            conditionalPanel(condition = "input.outside == 'Inside'",  #Condition - if inside - Show list of parts inside of slider interval
                             tableOutput("yearTableIn"),
                             downloadButton('downloadYearIn', 'Download')), width = 4 #Box width
          )
        )
      )
    )
  ),
  
  ####################### ROGUE PARTS LIFE ###############################
  
  ####################### SELECTED DATA ###############################
  tabItem(                                                                #Make a tab
    tabName = "data",                                                     #Tab name Data
    
    box(                                                                  #Make a box
      title = "DATA", status = "warning", DT::dataTableOutput('tbl'),     #Box properties
      downloadButton('downloadDataTBL', 'Download'),                      #Download button made
      width = 25                                                          #Box width
    )
  )
)
)

####################### SELECTED DATA ###############################

####################### DASHBOAD PAGE ###############################
dashboardPage(skin = "blue",                                             #Page layout
              dashboardHeader(title = "PARTNUMBERS"),                    #Title
              sidebar,                                                   #Sidebar
              body)                                                      #Body

####################### DASHBOAD PAGE ###############################