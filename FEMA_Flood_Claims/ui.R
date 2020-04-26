
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
                                     h3(START_HERE_TEXT_pt1),
                                     br(),
                                     h3(START_HERE_TEXT_pt2)),
                            
                            tabPanel(h3("Our Nation"), 
                                     fluidRow(
                                         column(2, sliderInput("nation_slider", sep = "", label = h3("Date Range"), min = 1970, max = 2019, value = c(1970,2019))),
                                         column(2, radioButtons("nation_radio", label = h3("Filter"), choices = list("States" = 0, "Flood Zone" = 1), selected = 0))
                                     ),
                                     h3(OUR_NATION_TEXT_pt1),
                                     h3(OUR_NATION_TEXT_pt2),
                                     h3(OUR_NATION_TEXT_pt3),
                                     h3(OUR_NATION_TEXT_pt4),
                                     plotlyOutput("GG_Accumulation_for_Nation"),
                                     plotlyOutput("GG_Total_Nation_Summed_Claims"),
                                     plotlyOutput('GG_Summed_Claim_Cost_by_Top_10_States')
                                     ),
                            
                            tabPanel(h3("Our States"),
                                     fluidRow(
                                         column(2, sliderInput("state_slider", sep = "", label = h3("Date Range"), min = 1970, max = 2019, value = c(1970,2019))),
                                         column(2, selectizeInput("selected", "State Acronym", State_Names))
                                     ),
                                     h3(OUR_STATES_TEXT_pt1),
                                     h3(OUR_STATES_TEXT_pt2),
                                     h3(OUR_STATES_TEXT_pt3),
                                     h3(OUR_STATES_TEXT_pt4),
                                     plotlyOutput("GG_Accumulation_for_State"),
                                     plotlyOutput("GG_Total_State_Summed_Claims")
                            ),
                            
                            tabPanel(h3("Our Story"), 
                                     h3(OUR_STORY_pt1),
                                     br(),
                                     h3(OUR_STORY_pt2),
                                     br(),
                                     h3(OUR_STORY_pt3),
                                     br(),
                                     h3(OUR_STORY_pt4),
                                     br(),
                                     h3(OUR_STORY_pt5),
                                     br(),
                                     h3(OUR_STORY_pt6),
                                     br(),
                                     h3(OUR_STORY_pt7),
                                     br(),
                                     h3(OUR_STORY_pt8),
                                     br(),
                                     h3(OUR_STORY_pt9)
                                     )
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


