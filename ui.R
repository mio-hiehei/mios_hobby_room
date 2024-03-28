
library(shiny)

## I want three pages:
# first an introductory page with my name, contact details, cv for pdf download and a funny app
# second an analysis of how people in different Bundesländer view XYZ-
# third an analysis of patterns regarding 

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("lumen"),
                  tags$script("source/scrollback_function.js"),
                  #theme = "css_layout.css",
                  # https://rstudio.github.io/shinythemes/
                  withMathJax(),
    
    

    # Application title
    titlePanel("Mio's Dashboard hobby room"),
    
    tabsetPanel(
        
        
        tabPanel(title = "Overview",
                 
                 br(),
                 
                     div("Hi, there"),
                     div("you have found your way to my Dashboard hobby room, a small and hopefully convenient way to give you a small demonstration of my interests and competences in data analytics and data visualisation."),
                     br(),
                 div(""),
                 
                 
                 
                 sidebarPanel(width = 3,
                              style = "position:fixed;width:22%;",

                     #tags$img(scr = "www/mio_bild_farbe.jpg", height = 300, width = 200),
                     
                     HTML('<img src="mio_bild_farbe.jpg", height="150px" width="150px", style="align:left"</img>'),
                     
                     
                     div("Mio Hienstorfer-Heitmann"),
                     
                     div("1992/01/13"),
                     
                     div("Based in Rhineland, Germany"),
                     
                     div("mio.hiehei@gmail.com"),
                     
                     br(),
                     
                     downloadButton(outputId = "downloadcv", label ="Download CV"),
                     
                     
                     
                 ),
                 
                 column(width = 8, 
                     
                     
                     div("On this page, check out this very carefully conducted analysis on surname length and producvitity in the workplace. The data was collected from personal and very objective opinion."),
                     br(),
                     div("Hover over the points in the plot to find out more."),
                     
                     br(),
                     
                     br(),
                            
                            plotlyOutput("plotly_surname"),
                            
                     br(),
                     
                     div("You can see that there is a very strong visual relationship between the number of letters a person has in their surname and their workplace productivity."),
                     
                     br(),
                     
                     actionButton("button_surname_detail", "Find out more"),
                     
                     br(),
                 
                 br(),
                 
                 conditionalPanel(
                     condition = "input.button_surname_detail != 0",
                     
                     br(),
                     
                     tags$iframe("www/surname_details_html.html"),
                     
                     br(),
                     
                     tableOutput("surname_var_desc_table"),
                     
                     
                     tags$iframe("www/surname_details_regression.html"),
                     
                     br(),
                     
                     
                     h4("Simulate Regression output"),
                     
                     tableOutput("surname_reg_table"),
                     

                         selectInput("var1_ols",
                                     "Variable 1",
                                     choices = unique(colnames(X[,-1]))),
                         
                         conditionalPanel(condition = "var1_ols != 0",
                         
                             uiOutput("var1_sel") 
                             
                             
                             ),
                     
                     br(),
                     
                     textOutput("sim_para_text"),
                     
                     br(),
                     
                     checkboxInput("check_see_data", "See the collected data to compare", value = FALSE),
                     
                     plotOutput("sim_plot_surname")
                     
                 )
                 
                 )
                 ),
    
        tabPanel(title = "Polarisation of German Volksparteien",
                 
                 br(),
                 
                 tags$iframe("www/ca_text.html"),
                 
                 br(),
                 br(),
                 
                 
                 
                 
                 
                 ## Filter for time zone
                 
                 sliderInput(inputId = "ca_slider",
                             label = "Choose an area of time",
                             min = min(df_merged$date),
                             max = max(df_merged$date),
                             value = c(min(df_merged$date), max(df_merged$date))
                             ),
                 
                 
                 br(),
                 
                 br(),
                 
                 dygraphOutput("plot_ca"),

                 
                 ),
    
        tabPanel(title = "Schools in North-Rhine-Westphalia (still under construction)",
                 

                 column(width = 3,
                     sliderInput(inputId = "no_pupils",
                                 label = "Number of Pupils",
                                 min = min(schools_df_omitted$SCHUELER),
                                 max = max(schools_df_omitted$SCHUELER),
                                 value = c(quantile(schools_df_omitted$SCHUELER, 0.25), quantile(schools_df_omitted$SCHUELER, 0.75))),
                     
                     br(),
                     
                     pickerInput(inputId = "ort_school",
                                 label = "City of School",
                                 choices = sort(unique(schools_df_omitted$ORT)),
                                 selected = sort(unique(schools_df_omitted$ORT)),
                                 options = pickerOptions(actionsBox = TRUE),
                                 multiple = TRUE),
                     br(),
                     checkboxInput("na",
                                             "Select NA Values for inhabtitants",
                                             value = FALSE),
                     bsTooltip(id = "na",
                               title = "Some data on inhabitants of a town is not present. Do you want to filter that data out?", 
                               placement = "right",
                               trigger = "hover"),
                     
                     conditionalPanel(
                         condition = "input.na == false",
                         sliderInput(inputId = "size_munip",
                                     label = "No of inhabitants in school's town",
                                     min = min(schools_df_omitted$einwohner, na.rm = TRUE),
                                     max = max(schools_df_omitted$einwohner, na.rm = TRUE),
                                     value = c(min(schools_df_omitted$einwohner , na.rm = TRUE), max(schools_df_omitted$einwohner, na.rm = TRUE)))
                         
                         ),
                     
                     br(),
                     
                     pickerInput(inputId = "type_school",
                                 label = "School Type",
                                 choices = sort(unique(schools_df_omitted$SCHULFORM)),
                                 selected = sort(unique(schools_df_omitted$SCHULFORM)),
                                 options = pickerOptions(actionsBox = TRUE),
                                 multiple = TRUE)),

                 column(width = 7,
                        
                        
                        leafletOutput("schoolmap")
                 )
        )

    )
    

))


