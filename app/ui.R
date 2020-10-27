shinyUI(
  ui = bs4DashPage(
    useShinyFeedback(),
    tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css"),
    sidebar = bs4DashSidebar(disable = TRUE),
    body = bs4DashBody(
                bs4TabCard(id = "tabs", width = 12, 
                           closable = FALSE, 
                           collapsible = TRUE, 
                           side = "right",
                           title = "R Package Download Tracker",
                           bs4TabPanel(tabName = "About", active = TRUE,
                                         htmlOutput("intro"),
                                         fluidRow(
                                           column(2,
                                                  uiOutput('time_about')),
                                           column(10,
                                                  shinycssloaders::withSpinner(reactableOutput("table_spk_about")))
                                         )
                                         
                                       ),
                           bs4TabPanel(tabName = "Trending", 
                                       fluidRow(
                                         column(2, 
                                                uiOutput('time_trending')),
                                         column(10, 
                                                shinycssloaders::withSpinner(reactableOutput("table_spk_trending")))
                                       )), 
                           bs4TabPanel(tabName = "Top Downloaded", 
                                       fluidRow(
                                         column(2,
                                                uiOutput("selector_period_ui"),
                                                selectizeInput(inputId = "selector_period",
                                                               label = "Period",
                                                               choices = c("Last Day" = "last-day",
                                                                           "Last Week" = "last-week",
                                                                           "Last Month" = "last-month"),
                                                               selected = "last-month"),
                                                uiOutput('time_top_download')
                                                ),
                                         column(10,
                                                shinycssloaders::withSpinner(reactableOutput("table_spk_top_download")))
                                         )),
                           bs4TabPanel(tabName = "Your Packages", 
                                       fluidRow(
                                         column(2,
                                                uiOutput('selector_pkg_ui'),
                                                column(12, align = "right",
                                                       uiOutput('btn_apply_ui')),
                                                br(),
                                                uiOutput('time_your_pkg')
                                                ),
                                         column(10,
                                                shinycssloaders::withSpinner(reactableOutput("table_spk_your_pkg")))
                                       ))
               )
    )
  )
)


