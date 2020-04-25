
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$GG_Total_Nation_Summed_Claims<-renderPlotly({
        ggplotly(
        filter.raw.df %>% 
            filter(., yearofloss >= input$nation_slider[1], yearofloss <= input$nation_slider[2]) %>% 
            group_by(., yearofloss) %>% 
            summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
            ggplot(., aes(x = yearofloss, y = Total_Summed_Loss)) +
            geom_line() +
            labs(title = 'Annual NFIP Claims Across the Nation', x = 'year', y = '$') + 
            scale_y_continuous(labels = scales::comma) +
            theme_bw() + 
            theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                  axis.text=element_text(size = axis_text_sz),
                  axis.title=element_text(size = axis_title_sz,face="bold"))
        )
        })
    
    output$GG_Accumulation_for_Nation<-renderPlotly({
        ggplotly(
        Accumulate_DF %>% 
            filter(., yearofloss >= input$nation_slider[1], yearofloss <= input$nation_slider[2]) %>% 
            group_by(., yearofloss) %>% 
            summarise(., accumulated_loss = sum(accumulated_loss)) %>% 
            ggplot(., aes(x = yearofloss, y = accumulated_loss)) +
            geom_line() + 
            scale_y_continuous(labels = scales::comma) +
            theme_bw() + 
            theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                  axis.text=element_text(size = axis_text_sz),
                  axis.title=element_text(size = axis_title_sz,face="bold"))
        )
        })
    
    output$GG_Summed_Claim_Cost_by_Top_10_States<-renderPlotly({
        ggplotly(
        Top_10_DF %>% 
            filter(., yearofloss >= input$nation_slider[1], yearofloss <= input$nation_slider[2]) %>% 
            group_by(., state) %>% 
            summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
            ggplot(., aes(x = reorder(state, -Total_Summed_Loss), y = Total_Summed_Loss)) +
            geom_bar(stat = 'identity') + 
            labs(title = 'Total Money Claimed to Date for Top 10 States', x = 'State', y = '$') + 
            scale_y_continuous(labels = scales::comma) +
            theme_bw() + 
            theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                  axis.text=element_text(size = axis_text_sz),
                  axis.title=element_text(size = axis_title_sz,face="bold"))
        )
        })
    
    output$GG_Total_State_Summed_Claims<-renderPlotly({
        ggplotly(
        filter.raw.df %>% 
            filter(., yearofloss >= input$state_slider[1], yearofloss <= input$state_slider[2], state == input$selected) %>% 
            group_by(., yearofloss) %>% 
            summarise(., Total_Summed_Loss = sum(amountpaidtotal)) %>% 
            ggplot(., aes(x = yearofloss, y = Total_Summed_Loss)) +
            geom_line() +
            labs(title = 'Annual NFIP Claims', x = 'year', y = '$') + 
            scale_y_continuous(labels = scales::comma) +
            theme_bw() + 
            theme(plot.title = element_text(size = title_text_sz, hjust = 0.5),
                  axis.text=element_text(size = axis_text_sz),
                  axis.title=element_text(size = axis_title_sz,face="bold"))
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
                  axis.text=element_text(size = axis_text_sz),
                  axis.title=element_text(size = axis_title_sz,face="bold"))
        )
        })
    
    }
)


