
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
            group_by(., yearofloss) %>% 
            summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
            ggplot(., aes(x = yearofloss, y = accumulated_loss)) +
            geom_line() + 
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
    
    }
)


