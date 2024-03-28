#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Define server logic required to draw a histogram


shinyServer(function(input, output, session) {
    
    
    output$plotly_surname <- renderPlotly({
    
        fig <- plot_ly(type = 'scatter',
                       mode = 'markers',
                       opacity = 0.5,
                       data = plot_data,
                       y = ~ `Productivity Index`,
                       x  = ~`Surname Length`
                       ) %>%
            add_trace(
                
                x = plot_data$`Surname Length`, 
                
                y = plot_data$`Productivity Index`,
                
                text = paste0(paste0(str_extract(plot_data$Name, "(?<=,).+"), " " ,str_extract(plot_data$Name, "^.+?(?=,)")), ifelse(plot_data$`Av. Coffees per day` == 0, " does not drink cofee " ,paste0(" drinks an average of \n", round(plot_data$`Av. Coffees per day`, 1), " cups of coffee per day")), "and scores \n", round(plot_data$`Productivity Index`, 2), " points on the productivity index."),
                
                hoverinfo = 'text',
                
                marker = list(color='blue')
                
            ) %>%
            layout(title = 'Effect of Surname Length on Workplace Productivity',
                   xaxis = list(title = "Surname Length in letters"),
                   yaxis = list(title = "Productivity index"),
                   showlegend = FALSE)
        
        fig
        
        
    })
    

    
    output$downloadcv <- downloadHandler(
        filename = "CV_Hie-Hei.pdf",
        content = function(file) {
            file.copy("www/CV_Mio_Hienstorfer_Heitmann.pdf", file)
        }
    )
    
    output$surname_var_desc_table <- renderTable(var_desc_table)
    
    onclick("button_surname_detail", {
        js$scrollCallback()
    })
    
    output$surname_reg_table <- renderTable(reg_data)
    
    
    output$var1_sel <- renderUI({
        tagList(
            sliderInput("from_var1", "From", 
                        value = mean(X[,input$var1_ols]),
                        min = min(X[,input$var1_ols]),
                        max = max(X[,input$var1_ols]) ),
            
            sliderInput("to_var1","To", 
                        value= mean(X[,input$var1_ols]),
                        min = min(X[,input$var1_ols]),
                        max = max(X[,input$var1_ols]) ),
            
            numericInput("step_var1", "Steps",
                         value = 4,
                         min  = 1,
                         max = 100,
                         step = 1)
        )
    })
    
    output$var2_sel <- renderUI({
        tagList(
            sliderInput("from_var2", "From", 
                        value = mean(X[,input$var1_ols]),
                        min = min(X[,input$var1_ols]),
                        max = max(X[,input$var1_ols]) ),
            
            sliderInput("to_var2","To", 
                        value= mean(X[,input$var1_ols]),
                        min = min(X[,input$var1_ols]),
                        max = max(X[,input$var1_ols]) ),
            
            numericInput("steps_var2", "Steps",
                         value = 4,
                         min  = 1,
                         max = 100,
                         step = 1)
        )
    })
    
    
    ols_surname_simulate <- reactive({
        
        
        df_scen <- ols_qoi_sim(
            X = X,
            coef = model$reg_out$Coefficients, 
            vcov = model$VarCov,
            nsim = 1000,
            scenario1_min = input$from_var1,
            scenario1_max = input$to_var1,
            scenario1_step = input$step_var1,
            name_var1 = input$var1_ols,
            df = plot_data)
        
        df_scen$steps <- input$step_var1
        
        return(df_scen)
        
    })
    
    sim_para_text_prep <- reactive({
      
      start <- "A person with "
      
     `Surname Length` =  paste0(median(plot_data$`Surname Length`), " letters in his/her surname")

     Age = paste0(median(plot_data$Age), " years of age")
     
    `Av. Coffees per day` = paste0("an average coffee consumption of ", round(median(plot_data$`Av. Coffees per day`), 2), " cups per day")
    
      `Delayed Gratification` = ifelse(mean(plot_data$`Delayed Gratification`) > 0.5, "an ability to delay gratification", "no ability to delay gratification")
      
      IQ = paste0("an IQ of ",round(mean(plot_data$IQ), 0))
      
      `eats Breakfast` =  ifelse(mean(plot_data$`eats Breakfast`) > 0.5, "a habbit of eating breakfast", "a habbit of not eating breakfast")
      
      
      text_df <- cbind.data.frame(`Surname Length`, Age, `Av. Coffees per day`, `Delayed Gratification`, IQ, `eats Breakfast`)
      
      text_df[, input$var1_ols] <- NULL
      
      end <- paste0("shows the following characteristics given the values for ", input$var1_ols, ":") 
      
      
      text <- paste(start, text_df[1,1], ", ",text_df[1,2], ", ", text_df[1,3],", ", text_df[1,4]," and ", text_df[1,5], " ", end)
      
      
    })
    
    output$sim_para_text <- renderText({
      
      sim_para_text_prep()
      
    })
    
    output$sim_plot_surname <- renderPlot({
      
      color_dots <- ifelse(input$check_see_data == FALSE, "white", "black")
      
        name_var1_gg <- paste0("`", input$var1_ols, "`")
        
        steps <- unique(ols_surname_simulate()$steps)
        
        if(steps <= 2){
          
          ggplot()+
            geom_point(data = plot_data,
                       aes_string(x = name_var1_gg,
                                  y = "`Productivity Index`"),
                       color = color_dots)+
            geom_point(data = ols_surname_simulate(),
                       aes(x = scenario,
                           y = mean),
                       color = "blue")+
            geom_errorbar(data = ols_surname_simulate(),
                          aes(x = scenario,
                              ymin = lwr,
                              ymax = upr),
                          color = "blue")+
            ylab("Productivity Index")+
            xlab(name_var1_gg)+
            ggtitle(paste0("Effect of ", input$var1_ols, " on Productivity"))+
            theme_classic()
          
        
        } else if (steps > 2){
          
          ggplot(data = ols_surname_simulate(),
                 aes(x = scenario,
                     y = mean))+
            geom_point(data = plot_data,
                       aes_string(x = name_var1_gg,
                                  y = "`Productivity Index`"),
                       color = color_dots)+
              geom_line(color = "blue")+
              geom_line(aes(y = lwr),linetype = "dashed", color = "blue")+
              geom_line(aes(y = upr), linetype = "dashed", color = "blue")+

              ylab("Productivity Index")+
              xlab(name_var1_gg)+
              ggtitle(paste0("Effect of ", input$var1_ols, " on Productivity"))+
              theme_classic()
        
        }
          
          
          
    })
    
    
    
    
    #### Classification accuracy in German Bundestag
    
    filter_ca <- reactive({
        
        df_merged %>%
            filter(date >= input$ca_slider[1] & date <= input$ca_slider[2])
        
    })
    
    
    
    #browseURL("https://lgatto.github.io/VisualisingBiomolecularData/interactive-visualisation.html")
    output$plot_ca <- renderDygraph({
        
        
        dy_data <- filter_ca() %>% select(
          accuracy_list
          #,SPD_vote_share, 
          #Union_vote_share
          ) 
          # %>% mutate(accuracy_list = accuracy_list * 100)
        
        dy_data <- xts(dy_data, order.by= filter_ca()$date)
        
        dygraph(dy_data, main = "Polarisation between Union and SPD") %>%
            dySeries("accuracy_list", label = "Polarisation") %>%
            #dySeries("SPD_vote_share", label = "Votes SPD", color = "red") %>%
            #dySeries("Union_vote_share", label = "Votes CDU", color = "orange") %>%
            dyAxis("y", label = "Percent")
            # %>% dyEvent(unique(filter_ca()$election_date), 
            #        unique(filter_ca()$cabinet_name), 
            #        labelLoc = "bottom")

        # plot <- ggplot(data = filter_ca(),
        #        aes(x = date,
        #            y = accuracy_list))+
        #     geom_line()+
        #     geom_vline(data = parlgov_elections,
        #                aes(xintercept = start_date))+
        #     labs(x = "Date",
        #          y = "Polarisation CDU/ CSU vs. SPD",
        #          title = "Polarisation of German Volksparteien by classification accuracy")+
        #     ylim(0,1)+
        #     theme_classic()
        #     
        # plot
    })
    
    
    ## kitas in cologne
    
    filter_schools <- reactive({
        
        if (!is.na(input$na)){
            schools_df_omitted <- schools_df_omitted %>%
                filter(SCHUELER <= input$no_pupils[2] & SCHUELER >= input$no_pupils[1] & ORT %in% input$ort_school & SCHULFORM %in% input$type_school & einwohner <= input$size_munip[2] & einwohner >= input$size_munip[1])
        
        } else {
            
            schools_df_omitted <- schools_df_omitted %>%
                filter(SCHUELER <= input$no_pupils[2] & SCHUELER >= input$no_pupils[1] & ORT %in% input$ort_school & SCHULFORM %in% input$type_school & is.na(einwohner))
            
            }
        
        return(schools_df_omitted)
        
        
    })
    
    output$schoolmap <- renderLeaflet({
        
        leaflet(data = filter_schools()) %>%
            # addProviderTiles(providers$Stadia.Outdoors,
            #                  options = providerTileOptions(noWrap = TRUE)) %>%
            addTiles() %>%
            addMarkers(~lon,
                       ~lat,
                       label = ~ paste0("The ", NAME, " at ",
                                       ADRESSE, ", ",ORT, " is a ", SCHULFORM , " and has ", SCHUELER, " pupils."))
        
    })
    

})
