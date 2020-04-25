
dashboardPage(
    dashboardHeader(title = 'FEMA Flood Claims'),
    dashboardSidebar(
        sidebarMenu(
            menuItem('Dashboard', tabName = 'dashboard', icon = icon('map')),
            menuItem('Data', tabName = 'data', icon = icon('database'))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'dashboard', 
                    fluidRow(
                        tabBox(
                            title = "", id = "tabset1", width = '100%',
                            
                            tabPanel(h3("Start Here"), 
                                     "First tab content"),
                            
                            tabPanel(h3("Our Nation"), 
                                     sliderInput("nation_slider", sep = "", label = h3("Date Range"), min = 1970, max = 2018, value = c(1970,2019)),
                                     radioButtons("nation_radio", label = h3("Geographic Region"), choices = list("States" = 1, "Region" = 2), selected = 1),
                                     plotlyOutput("GG_Total_Nation_Summed_Claims"),
                                     plotlyOutput("GG_Accumulation_for_Nation"),
                                     plotlyOutput('GG_Summed_Claim_Cost_by_Top_10_States')
                                     ),
                            
                            tabPanel(h3("Our States"),
                                     sliderInput("state_slider", sep = "", label = h3("Date Range"), min = 1970, max = 2018, value = c(1970,2019)),
                                     selectizeInput("selected",
                                                    "State Acronym",
                                                    State_Names),
                                     plotlyOutput("GG_Total_State_Summed_Claims"),
                                     plotlyOutput("GG_Accumulation_for_State")
                            ),
                            
                            tabPanel(h3("Our Story"), "Tab content 4")
                        )
                    )),
            tabItem(tabName = 'data',
                    fluidRow(
                        tabBox(
                            title = "",
                            # The id lets us use input$tabset1 on the server to find the current tab
                            id = "tabset1", width = '100%',
                            tabPanel(h3("Raw"), "Tab content 1"),
                            tabPanel(h3("Filtered"), "Tab content 2"),
                            tabPanel(h3("Manipulated"), "Tab content 3"),
                            tabPanel(h3("Summed"), "Tab content 4")
                            )
                    )
            )
        )
    )
)


