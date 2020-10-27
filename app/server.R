shinyServer(function(input, output, session) {
  #### Reactive Values ---------------------------------------------------------
  values = reactiveValues(status = 0,
                          df_external = NULL,
                          df_plot_fake = NULL,
                          selected_pkg = NULL,
                          last_time_generateFakeData = NULL)
  
  #### Tab: About --------------------------------------------------------------
  output$intro = renderUI({
    "this is how it should look like"
  })

  observe({
    # schedule to generate fake data
    invalidateLater(period_generateFakeData_millisec, session)
    
    df_plot_fake = isolate(values$df_plot_fake)
    df_plot_fake_new = getFakeDataForAbout(df_plot_fake)
    values$df_plot_fake = df_plot_fake_new
    if (is.null(isolate(values$last_time_generateFakeData))){
      values$last_time_generateFakeData = Sys.time()
    } else{
      values$last_time_generateFakeData = isolate(values$last_time_generateFakeData) + 86400
    }
  })
  output$time_about = renderUI({
    req(!is.null(values$last_time_generateFakeData))
    paste0(
      h6(strong("Last Updated Time")),
      tags$p(format(as.POSIXlt(values$last_time_generateFakeData, tz = "UTC"), usetz = TRUE)),
      h6(strong("Next Scheduled Update Time")), 
      tags$p(format(as.POSIXlt(values$last_time_generateFakeData, tz = "UTC") + 86400, usetz = TRUE))
    ) %>% HTML(.)
  })
  output$table_spk_about = renderReactable({
    req(!is.null(values$df_plot_fake))
    values$df_plot_fake %>%
      select(Package, Title, Topic, Count, `Today's Count`) %>%
      printReactable(., arrange_var = "Package", arrange_type = "asc")
  })

  #### Shared between tabs (Trending, Top Downloaded, Your Packages) -----------
  observe({
    # handling initial step
    if (values$status == 0 & !is.null(input$selector_pkg)){
      values$selected_pkg = input$selector_pkg
      values$status = 1
    } 
  })
  observe({
    # schedule to get data through web scraping and api 
    invalidateLater(period_getRealData_millisec, session)
    message("[Start: getExternalData()]")
    t1 = Sys.time()
    df_all = tryCatch({ getExternalData() },
                      error = function(e) { "error" }) 
    if (class(df_all) == "list"){
      values$df_external = df_all
    } 
    message(paste0("[ Spent ", round(Sys.time() - t1, 2), " sec]"))
    message("[End: getExternalData()]")
  })
  output$time_your_pkg <-output$time_top_download <- output$time_trending <- renderUI({
    req(!is.null(values$df_external))
    paste0(h6(strong("Last Updated Time")),
           tags$p(format(as.POSIXlt(values$df_external$last_time_getRealData, tz = "UTC"), usetz = TRUE)),
           h6(strong("Next Scheduled Update Time")), 
           tags$p(format(as.POSIXlt(values$df_external$last_time_getRealData, tz = "UTC") + period_getRealData_sec, usetz = TRUE))) %>%  
      HTML(.)
  })
  
  #### Tab: Trending -----------------------------------------------------------
  output$table_spk_trending = renderReactable({
    req(!is.null(values$df_external))
    df_plot_trending = prepareFinalPlotData_Trending(values$df_external)

    df_plot_trending %>% 
      select(Package, Title, Topic, Count, `Today's Count`, increase) %>% 
      printReactable(., arrange_var = "increase", arrange_type = "desc")
  })
  
  #### Tab: Top Downloaded -----------------------------------------------------
  df_plot_top_download = reactive({
    req(!is.null(values$df_external) & !is.null(input$selector_period))
    if (input$selector_period == "last-month"){
      df_top_download = values$df_external$df_top_download_last_month
    } else if (input$selector_period == "last-week"){
      df_top_download = values$df_external$df_top_download_last_week
    } else {
      df_top_download = values$df_external$df_top_download_last_day
    }
    
    prepareFinalPlotData_TopDownload(values$df_external, df_top_download)
  })
  output$table_spk_top_download = renderReactable({
    req(!is.null(df_plot_top_download()))
    
    start_date = df_plot_top_download()$start_date[1]
    end_date = df_plot_top_download()$end_date[1]
    
    df_plot_top_download() %>% 
      select(Package, Title, Topic, Count, `Today's Count`, downloads) %>% 
      printReactable(., arrange_var = "downloads", arrange_type = "desc", start_date = start_date, end_date = end_date)
  })
  
  #### Tab: Your Packages ------------------------------------------------------
  output$selector_pkg_ui = renderUI({
    req(!is.null(values$df_external))
    selectizeInput(inputId = 'selector_pkg',
                   label = HTML(paste0("Packages (Today's total ",
                                  tags$a(href = "https://cran.r-project.org//web/packages/",
                                         target = "_blank",
                                         formatC(nrow(values$df_external$df_title), format="d", big.mark=",")),
                                  ")")),
                   selected = c("DT", "gt", "reactable", "flextable", 
                                "huxtable", "kableExtra", "magrittr"),
                   choices = sort(values$df_external$df_title$Package),
                   options = list(maxOptions = nrow(values$df_external$df_title)),
                   multiple = TRUE)
  })
  observe({
    req(values$status == 1)
    updateSelectizeInput(session, inputId = "selector_pkg",
                         selected = isolate(values$selected_pkg),
                         choices = sort(values$df_external$df_title$Package))
  })
  output$btn_apply_ui = renderUI({
    req(!is.null(input$selector_pkg))
    shinyWidgets::actionBttn(
      inputId = "btn_apply",
      label = "Apply",
      style = "jelly",
      color = "warning"
    )
  })
  observe({
    if (is.null(input$selector_pkg)){
      showFeedbackWarning(
        inputId = "selector_pkg",
        text = "Please select at least one package."
      ) 
    } else {
      hideFeedback("selector_pkg")
    }
  })
  
  
  observeEvent(input$btn_apply, {
    values$selected_pkg = input$selector_pkg
  })
  df_plot_your_pkg = reactive({
    req(!is.null(values$selected_pkg))
    prepareFinalPlotData_YourPackage(values$df_external, values$selected_pkg) 
  })
  output$table_spk_your_pkg = renderReactable({
    req(!is.null(df_plot_your_pkg()))
    df_plot_your_pkg() %>% 
        select(Package, Title, Topic, Count, `Today's Count`, Status) %>% 
        printReactable(., arrange_var = "Package", arrange_type = "asc")
  })
})








