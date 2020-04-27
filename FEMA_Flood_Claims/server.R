
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$GG_Total_Nation_Summed_Claims<-renderPlotly({
        ggplotly(
        filter.raw.df %>% 
            filter(., yearofloss >= input$nation_slider[1], yearofloss <= input$nation_slider[2]) %>% 
            group_by(., yearofloss) %>% 
            summarise(., Annual_Claims = sum(amountpaidtotal)) %>% 
            ggplot(., aes(x = yearofloss, y = Annual_Claims)) +
            geom_line() +
            labs(title = 'Annual Claims', x = '', y = '$') + 
            scale_y_continuous(labels = scales::comma) +
            theme_bw() + 
            theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                  axis.text=element_text(size = axis_text_sz, face = 'bold'),
                  axis.title=element_text(size = axis_title_sz))
        )
        })
    
    output$GG_Accumulation_for_Nation<-renderPlotly({
        ggplotly(
                if (input$nation_radio == 0){
                    Accumulate_DF %>% 
                        filter(., yearofloss >= input$nation_slider[1], yearofloss <= input$nation_slider[2]) %>% 
                        group_by(., state, yearofloss) %>% 
                        summarise(., Accumulated_Claims = sum(accumulated_loss)) %>%
                        ggplot(., aes(x = yearofloss, y = Accumulated_Claims, fill = state)) +
                        geom_area(show.legend = FALSE) + 
                        labs(title = 'Accumulated Claims', x = '', y = '$') +
                        scale_y_continuous(labels = scales::comma) +
                        theme_bw() + 
                        theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                              axis.text=element_text(size = axis_text_sz, face = 'bold'),
                              axis.title=element_text(size = axis_title_sz))
                } else if (input$nation_radio == 1) {
                    Accumulate_DF %>% 
                        filter(., yearofloss >= input$nation_slider[1], yearofloss <= input$nation_slider[2]) %>% 
                        group_by(., floodzone, yearofloss) %>% 
                        summarise(., Accumulated_Claims = sum(accumulated_loss)) %>%
                        ggplot(., aes(x = yearofloss, y = Accumulated_Claims, fill = floodzone)) +
                        geom_area(show.legend = FALSE) + 
                        labs(title = 'Accumulated Claims', x = '', y = '$') +
                        scale_y_continuous(labels = scales::comma) +
                        theme_bw() + 
                        theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                              axis.text=element_text(size = axis_text_sz, face = 'bold'),
                              axis.title=element_text(size = axis_title_sz))
                } else if (input$nation_radio == 2){
                    Accumulate_DF %>% 
                        filter(., yearofloss >= input$nation_slider[1], yearofloss <= input$nation_slider[2]) %>% 
                        group_by(., region_name, yearofloss) %>% 
                        summarise(., Accumulated_Claims = sum(accumulated_loss)) %>%
                        ggplot(., aes(x = yearofloss, y = Accumulated_Claims, fill = region_name)) +
                        geom_area(show.legend = FALSE) + 
                        labs(title = 'Accumulated Claims', x = '', y = '$') +
                        scale_y_continuous(labels = scales::comma) +
                        theme_bw() + 
                        theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                              axis.text=element_text(size = axis_text_sz, face = 'bold'),
                              axis.title=element_text(size = axis_title_sz))
                } else {
                    Accumulate_DF %>% 
                        filter(., yearofloss >= input$nation_slider[1], yearofloss <= input$nation_slider[2]) %>% 
                        group_by(., division_name, yearofloss) %>% 
                        summarise(., Accumulated_Claims = sum(accumulated_loss)) %>%
                        ggplot(., aes(x = yearofloss, y = Accumulated_Claims, fill = division_name)) +
                        geom_area(show.legend = FALSE) + 
                        labs(title = 'Accumulated Claims', x = '', y = '$') +
                        scale_y_continuous(labels = scales::comma) +
                        theme_bw() + 
                        theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                              axis.text=element_text(size = axis_text_sz, face = 'bold'),
                              axis.title=element_text(size = axis_title_sz)) 
                    }
                )
        })
    
    output$GG_Summed_Claim_Cost_by_Top_10_States<-renderPlotly({
        ggplotly(
        Top_Ten_State_Names_Table %>% 
            mutate(., Summed_Flood_Claims = Total_Summed_Loss) %>% 
            ggplot(., aes(x = reorder(state, -Summed_Flood_Claims), y = Summed_Flood_Claims)) +
            geom_bar(stat = 'identity') + 
            labs(title = 'Summed Flood Claims', x = '', y = '$') + 
            scale_y_continuous(labels = scales::comma) +
            theme_bw() + 
            theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                  axis.text=element_text(size = axis_text_sz, face = 'bold'),
                  axis.title=element_text(size = axis_title_sz))
        )
        })
    
    output$GG_Total_State_Summed_Claims<-renderPlotly({
        ggplotly(
        filter.raw.df %>% 
            filter(., yearofloss >= input$state_slider[1], yearofloss <= input$state_slider[2], state == input$selected) %>% 
            group_by(., yearofloss) %>% 
            summarise(., Annual_Claims = sum(amountpaidtotal)) %>% 
            ggplot(., aes(x = yearofloss, y = Annual_Claims)) +
            geom_line() +
            labs(title = 'Annual Claims', x = '', y = '$') + 
            scale_y_continuous(labels = scales::comma) +
            theme_bw() + 
            theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                  axis.text=element_text(size = axis_text_sz, face = 'bold'),
                  axis.title=element_text(size = axis_title_sz))
        )
        })
    
    output$GG_Accumulation_for_State<-renderPlotly({
        ggplotly(
        Accumulate_DF %>% 
            filter(., yearofloss >= input$state_slider[1], yearofloss <= input$state_slider[2], state == input$selected) %>% 
            group_by(., yearofloss, floodzone) %>% 
            summarise(., Accumulated_Claims = sum(accumulated_loss)) %>% 
            ggplot(., aes(x = yearofloss, y = Accumulated_Claims, fill = floodzone)) +
            geom_area() + 
            labs(title = 'Accumulated Claims', x = '', y = '$') + 
            scale_y_continuous(labels = scales::comma) +
            theme_bw() + 
            theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                  axis.text=element_text(size = axis_text_sz, face = 'bold'),
                  axis.title=element_text(size = axis_title_sz))
        )
        })
    
    output$GG_Summed_Claim_Cost_by_Top_10_Counties<-renderPlotly({
        ggplotly(
            Top_Ten_County_Names_Table %>%
                filter(., state == input$selected) %>% 
                mutate(., Summed_Flood_Claims = Total_Summed_Loss) %>% 
                ggplot(., aes(x = reorder(county_name, -Summed_Flood_Claims), y = Summed_Flood_Claims)) +
                geom_bar(stat = 'identity') + 
                labs(title = 'Summed Flood Claims', x = '', y = '$') + 
                scale_y_continuous(labels = scales::comma) +
                theme_bw() + 
                theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                      axis.text=element_text(size = axis_text_sz, face = 'bold'),
                      axis.title=element_text(size = axis_title_sz))
        )
        })
  
      output$GG_Amount_PD<-renderPlotly({
        GG_Amount_PD = 
            major_storms %>% 
            ggplot(., aes(x = Year, y = Amount_PD, size = Amount_PD, text = paste0(Event, "<br>", Amount_PD), group = 1)) +
            geom_point() +
            labs(title = 'FEMA Flood Claims for Major Storms', x = '', y = '$') + 
            scale_y_continuous(limits = c(0,2e+10), labels = scales::comma) +
            theme_bw() + 
            theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                  axis.text=element_text(size = axis_text_sz, face = 'bold'),
                  axis.title=element_text(size = axis_title_sz))
        ggplotly(GG_Amount_PD, tooltip = 'text')
    })
      
      output$GG_Avg_PD_Losses<-renderPlotly({
          GG_Avg_PD_Losses = major_storms %>% 
              ggplot(., aes(x = Year, y = Avg_PD_Losses, text = paste0(Event, "<br>", Avg_PD_Losses), group = 1)) +
              geom_point() +
              geom_smooth(method = loess, se = FALSE, formula = 'y ~ x') +
              labs(title = 'Average Paid Losses of Major Storms', x = '', y = '$') + 
              scale_y_continuous(labels = scales::comma) +
              theme_bw() + 
              theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                    axis.text=element_text(size = axis_text_sz, face = 'bold'),
                    axis.title=element_text(size = axis_title_sz))
          ggplotly(GG_Avg_PD_Losses, tooltip = "text")
      })
      
      output$GG_Accumulation_for_Nation_Regression<-renderPlotly({
          if (input$regression == 0) {
              GG_Accumulation_for_Nation_Regression =  Accumulate_DF %>% 
                  filter(., !is.na(yearofloss), !is.nan(yearofloss), yearofloss != Inf, yearofloss != -Inf, yearofloss != '') %>% 
                  group_by(., yearofloss) %>% 
                  summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
                  mutate(., yearofloss = yearofloss - min(yearofloss)) %>% 
                  filter(., yearofloss > 0, accumulated_loss > 0) %>% 
                  ggplot(.) +
                  geom_point(aes(x = yearofloss, y = accumulated_loss)) + 
                  geom_smooth(colour = 'blue', size = 0.5, aes(x = yearofloss, y = accumulated_loss), method="lm", formula= (y ~ x), se=FALSE, linetype = 1) + 
                  labs(title = 'Accumulated Loss Regression Analysis', x = 'Years Since Program Inception', y = '$') + 
                  scale_y_continuous(labels = scales::comma) +
                  theme_bw() + 
                  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                        axis.text=element_text(size = axis_text_sz, face = 'bold'),
                        axis.title=element_text(size = axis_title_sz))
          } else if (input$regression == 1) {
              GG_Accumulation_for_Nation_Regression =  Accumulate_DF %>% 
                  filter(., !is.na(yearofloss), !is.nan(yearofloss), yearofloss != Inf, yearofloss != -Inf, yearofloss != '') %>% 
                  group_by(., yearofloss) %>% 
                  summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
                  mutate(., yearofloss = yearofloss - min(yearofloss)) %>% 
                  filter(., yearofloss > 0, accumulated_loss > 0) %>% 
                  ggplot(.) +
                  geom_point(aes(x = yearofloss, y = accumulated_loss)) + 
                  geom_line(colour = 'red', linetype = 1, aes(x = yearofloss, y = alpha*exp( as.numeric(beta) * yearofloss) + theta)) +
                  labs(title = 'Accumulated Loss Regression Analysis', x = 'Years Since Program Inception', y = '$') + 
                  scale_y_continuous(labels = scales::comma) +
                  theme_bw() + 
                  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                        axis.text=element_text(size = axis_text_sz, face = 'bold'),
                        axis.title=element_text(size = axis_title_sz))
          } else {
              GG_Accumulation_for_Nation_Regression =  Accumulate_DF %>% 
                  filter(., !is.na(yearofloss), !is.nan(yearofloss), yearofloss != Inf, yearofloss != -Inf, yearofloss != '') %>% 
                  group_by(., yearofloss) %>% 
                  summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
                  mutate(., yearofloss = yearofloss - min(yearofloss)) %>% 
                  filter(., yearofloss > 0, accumulated_loss > 0) %>% 
                  ggplot(.) +
                  geom_point(aes(x = yearofloss, y = accumulated_loss)) + 
                  geom_smooth(colour = 'green', size = 0.5, aes(x = yearofloss, y = accumulated_loss), method="lm", formula= (y ~ x + I(x^2) + I(x^3)), se=FALSE, linetype = 1) + 
                  labs(title = 'Accumulated Loss Regression Analysis', x = 'Years Since Program Inception', y = '$') + 
                  scale_y_continuous(labels = scales::comma) +
                  theme_bw() + 
                  theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                        axis.text=element_text(size = axis_text_sz, face = 'bold'),
                        axis.title=element_text(size = axis_title_sz))
          }
          ggplotly()
          })
    
      output$GG_Accumulation_for_Nation_Standardized<-renderPlotly({
          GG_Accumulation_for_Nation_Standardized = 
              Accumulate_DF %>% 
              group_by(., yearofloss) %>% 
              summarise(., accumulated_loss = sum(accumulated_loss)/69751802599) %>% #total paid throughout program
              ggplot(., aes(x = yearofloss, y = accumulated_loss)) +
              geom_line() + 
              labs(title = 'Standardized Accumulated Loss for the Nation', x = '', y = '($) / (Total $)') + 
              scale_y_continuous(labels = scales::comma) +
              theme_bw() + 
              theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                    axis.text=element_text(size = axis_text_sz, face = 'bold'),
                    axis.title=element_text(size = axis_title_sz))
          ggplotly()
      })
      
      output$GG_Standardized_Accumulation_State<-renderPlotly({
          Accumulate_DF_0_to_1 %>% 
              ggplot(., aes(x = yearofloss, y = standardized_accumulation, color = state)) +
              geom_line(show.legend = FALSE) +
              labs(title = 'Standardized Accumulated Loss for All States', x = '', y = '($) / (Total $)') +
              theme_bw() + 
              theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                    axis.text=element_text(size = axis_text_sz),
                    axis.title=element_text(size = axis_title_sz,face="bold"),
                    legend.position='none')
          ggplotly()
      })
      
      # show data using DataTable
      output$table_cleaned <- DT::renderDataTable({
          datatable(filter.raw.df, rownames=FALSE)
      })
      
      # show data using DataTable
      output$table_manipulated <- DT::renderDataTable({
          datatable(Accumulate_DF, rownames=FALSE)
      })
      
      # show data using DataTable
      output$table_major_storms <- DT::renderDataTable({
          datatable(major_storms, rownames=FALSE)
      })
      
      output$text_blurb <- renderText({
          paste(START_HERE_TEXT_pt2_1, START_HERE_TEXT_pt2_2, START_HERE_TEXT_pt2_3, sep="<br>")
      })
      
    }
)


