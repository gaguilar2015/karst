


ui <- shinydashboardPlus::dashboardPage(
  md = TRUE,
  skin = "black-light",
  dashboardHeader(
    title = 'TMNR KAP Dashboard',
    titleWidth = '350px'
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("globe-americas")),
      menuItem("Field Metrics", tabName = "metrics", icon = icon("chart-bar"),
               menuSubItem("Progress", tabName = "progress", icon = icon("bars-progress")),
               menuSubItem("HH Response", tabName = "response", icon = icon("thumbs-up"))
      ),
      menuItem("Quality Checks", tabName = "quality", icon = icon("check-double"),
               menuSubItem("Non-Respondents", tabName = "nonrespondents", icon = icon("table")),
               menuSubItem("Missing Result", tabName = "missing_result", icon = icon("table")),
               menuSubItem("Missing Info", tabName = "missing_head", icon = icon("table"))
      ),
      menuItem("Visuals", tabName = "skills", icon = icon("chalkboard-teacher"),
               menuSubItem("Interviewers", tabName = "int_chart", icon = icon("chart-simple")),
               menuSubItem("TMNR Knowledge", tabName = "tmnr_chart", icon = icon("chart-simple"))
      ),
      actionButton(
        "sign_out",
        "Sign Out",
        icon = icon("sign-out-alt"),
        class = "pull-right"
      )
    ),
    minified = F, collapsed = F
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          infoBox("Submitted Interviews", color = 'olive', fill = TRUE,
                  icon = icon('house'), total_interviews),
          infoBox("Completed Households", color = 'olive',  fill = TRUE,
                  icon = icon('house-circle-check'), completed_households),
          infoBox("Response Rate", 
                  color = 'olive', fill = TRUE, icon = icon('thumbs-up'), response_rate),
          infoBox("Communities Started", color = 'light-blue',  fill = TRUE,
                  icon = icon('tents'), communities_started),
          infoBox("Communities Completed", color = 'light-blue',  fill = TRUE,
                  icon = icon('tents'), communities_completed),
          infoBox("Members/HH", color = 'light-blue',  fill = TRUE,
                  icon = icon('house-user'), 3)
        ),
        fluidRow(
          box(
            title = "Interviews by CTV", solidHeader = T,
            plotOutput("ctv_plot", height = 320)
          ),
          box(
            title = "Interviews by Result", solidHeader = T,
            plotOutput("result_plot", height = 320)
          )
            
        )
        
      ),
      tabItem(
        tabName = 'map',
        fluidRow(
          column(4),
          column(4, selectInput('community', 'Community', choices = communities_names)),
          column(4, selectInput('basemap', 'Base Map', choices = bmap_choices))
        ),
        leafletOutput('map', height = '75vh')
      ),
      tabItem(
        tabName = 'progress',
        reactableOutput('progress')
      ),
      tabItem(
        tabName = 'response',
        reactableOutput('response')
      ),
      tabItem(
        tabName = 'nonrespondents',
        reactableOutput('nonrespondents')
      ),
      tabItem(
        tabName = 'missing_result',
        reactableOutput('missing_result')
      ),
      tabItem(
        tabName = 'missing_info',
        reactableOutput('missing_info')
      ),
      tabItem(
        tabName = 'int_chart',
        plotOutput('int_chart')
      ),
      tabItem(
        tabName = 'tmnr_chart',
        plotOutput('tmnr_chart')
      )
    )
  )
)
