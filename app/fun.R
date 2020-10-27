#### Generate Fake Data --------------------------------------------------------
getFakeDataForAbout = function(df_plot_fake){
  if (is.null(df_plot_fake)){
    today_utc = as.Date(format(as.POSIXct(Sys.time()), tz = "UTC", usetz = TRUE))
    set.seed(123)
    df_long_fake_1 = tibble(
      date = seq.Date(from = today_utc - 30, 
                      to = today_utc - 1, by = "1 day"),
      count = rpois(30, 20),
      Package = "myPackage_1"
    )
    set.seed(321)
    df_long_fake_2 = tibble(
      date = seq.Date(from = today_utc - 30, 
                      to = today_utc - 1, by = "1 day"),
      count = rpois(30, 100),
      Package = "myPackage_2"
    )
    df_long_fake = bind_rows(list(df_long_fake_1, df_long_fake_2))
  } else {
    df_plot_fake_origin = df_plot_fake %>% 
      select(date, count, Package) %>% 
      tidyr::unnest(., cols = c("date", "count")) %>% 
      ungroup(.)
    new_data_1 = tibble(
      date = max(df_plot_fake_origin$date) + 1, 
      count = rpois(1, 20), 
      Package = "myPackage_1"
    )
    new_data_2 = tibble(
      date = max(df_plot_fake_origin$date) + 1, 
      count = rpois(1, 100), 
      Package = "myPackage_2"
    )
    
    df_long_fake = bind_rows(list(df_plot_fake_origin, new_data_1, new_data_2)) %>% 
      filter(date != min(date))
  }
  pkg_topic_fake = tibble(Package = c("myPackage_1", "myPackage_2"), 
                          Topic = c("Others", "Others"))
  pkg_title_fake = tibble(Package = c("myPackage_1", "myPackage_2"), 
                          Title = c("title for myPackage_1", "title for myPackage_2"))
  df_plot_about = prepareData(df_long_fake, pkg_topic_fake, pkg_title_fake)
  
  return(df_plot_about)
}

#### Get Data ------------------------------------------------------------------
getExternalData = function(){
  # from web scraping
  df_title = getPackageTitle()
  df_topic = getPackageTopic()
  
  # from api
  df_trending = getTrendingPackage()
  df_top_download_last_day = getTopNDownloadedPackage(100, "last-day")
  df_top_download_last_week = getTopNDownloadedPackage(100, "last-week")
  df_top_download_last_month = getTopNDownloadedPackage(100, "last-month")
  
  # record time
  last_time_getRealData = format(as.POSIXct(Sys.time()), tz = "UTC", usetz = TRUE)
  
  return(list(df_title = df_title, 
              df_topic = df_topic,
              df_trending = df_trending, 
              df_top_download_last_day = df_top_download_last_day, 
              df_top_download_last_week = df_top_download_last_week, 
              df_top_download_last_month = df_top_download_last_month,
              last_time_getRealData = last_time_getRealData))
}
getPackageTitle = function(){
  url = "https://cran.r-project.org/web/packages/available_packages_by_name.html"
  webpage = xml2::read_html(url)
  output = rvest::html_nodes(webpage,'td:nth-child(-n+2)') %>% 
    rvest::html_text(trim=TRUE)  
  output = output[output != ""]
  
  df_title = tibble("Package" = output[(1:length(output))%%2 != 0], 
                    "Title" = output[(1:length(output))%%2 == 0])
  return(df_title)
}
getPackageTopic = function(){
  url = "https://cran.r-project.org/web/views/"
  webpage = xml2::read_html(url)
  topics = rvest::html_nodes(webpage,'td a') %>% 
    rvest::html_text(trim=TRUE) 
  
  df_topic = bind_rows(future.apply::future_lapply(topics, getPackageByTopic)) %>% 
    group_by(Package) %>% 
    summarise(Topic = paste0(Topic, collapse = "|"), .groups = 'drop') %>% 
    ungroup(.)
  return(df_topic)
}
getPackageByTopic = function(topic){
  url = paste0("https://cran.r-project.org/web/views/", topic, ".html")
  webpage = xml2::read_html(url)
  pkgs = rvest::html_nodes(webpage, xpath = '//h3[.="Related links:"]/preceding-sibling::ul/li/a') %>% 
    rvest::html_text(trim=TRUE) 
  
  df_topic_one = tibble(Topic = topic,
                        Package = pkgs)
  return(df_topic_one)
}
getTrendingPackage = function(){
  r = httr::GET("https://cranlogs.r-pkg.org/trending")
  ls = httr::content(r, "parsed")
  df_trending = bind_rows(ls) %>% 
    rename(Package = package) %>% 
    mutate(increase = round(as.numeric(increase), 2),
           trending = "Trending")
  return(df_trending)
}
getTopNDownloadedPackage = function(top_n, period){
  r = httr::GET(paste0("http://cranlogs.r-pkg.org/top/", period, "/", top_n))
  ls = httr::content(r, "parsed")
  start_date = ls$start %>% as.Date(.)
  end_date = ls$end %>% as.Date(.)
  df_top_download = bind_rows(ls$downloads) %>% 
    rename(Package = package) %>% 
    mutate(start_date = start_date,
           end_date = end_date,
           downloads = round(as.numeric(downloads), 2),
           top_download = paste0("Top Downloaded (", period, ")"))
  
  return(df_top_download)
}

#### Data Preparation ----------------------------------------------------------
prepareDataForOnePackage = function(df_one){
  df_plot_one = tibble(
    date = list(df_one$date),
    count = list(df_one$count),
    `Today's Count` = df_one$count[which.max(as.Date(df_one$date))]
  )
  df_plot_one$data = mapply(function(xx, yy){
    mapply(
      function(x,y,z) {list(date = x, count = y)},
      xx %>% as.list(.), yy %>% as.list(.),
      SIMPLIFY = FALSE)
  }, df_plot_one$date,
  df_plot_one$count,
  SIMPLIFY = FALSE)
  
  df_plot_one$Count = mapply(function(w){
    # browser()
    list(data = w)
  },
  df_plot_one$data, SIMPLIFY = FALSE)
  
  return(df_plot_one)
}
prepareData = function(df_long, pkg_topic, pkg_base){
  # browser()
  df_nest = df_long %>% 
    group_by(Package) %>% 
    tidyr::nest()
  
  df_nest$df_plot = future.apply::future_lapply(df_nest$data, prepareDataForOnePackage)
  
  df_plot = df_nest %>% 
    select(-data) %>% 
    tidyr::unnest(df_plot)
  
  df_plot = merge(data.table::data.table(df_plot, key = "Package"),
                  data.table::data.table(pkg_topic, key = "Package"), 
                  all.x = TRUE, by = "Package")
  df_plot = merge(data.table::data.table(df_plot, key = "Package"),
                  data.table::data.table(pkg_base, key = "Package"), 
                  all.x = TRUE, by = "Package")
  df_plot = df_plot %>% 
    mutate(Topic = ifelse(is.na(Topic), "Others", Topic))
  
  return(df_plot)
}
prepareFinalPlotData_Trending = function(df_external){
  # browser()
  df_title = df_external$df_title
  df_topic = df_external$df_topic
  df_trending = df_external$df_trending
  
  df_long <- cranlogs::cran_downloads(df_trending$Package, 
                                      when = "last-month") %>%
    rename(Package = package)
  df_plot_trending = prepareData(df_long, df_topic, df_title)
  df_plot_trending = merge(data.table::data.table(df_plot_trending, key = "Package"), 
                           data.table::data.table(df_trending, key = "Package"), 
                           all.x = TRUE, by = "Package")
  return(df_plot_trending)
}
prepareFinalPlotData_TopDownload = function(df_external, df_top_download){
  df_title = df_external$df_title
  df_topic = df_external$df_topic
  df_long <- cranlogs::cran_downloads(df_top_download$Package, 
                                      when = "last-month") %>% 
    rename(Package = package)
  df_plot_top_download = prepareData(df_long, df_topic, df_title)
  df_plot_top_download = merge(data.table::data.table(df_plot_top_download, key = "Package"), 
                               data.table::data.table(df_top_download, key = "Package"), 
                               all.x = TRUE, by = "Package")
  return(df_plot_top_download)
}
prepareFinalPlotData_YourPackage = function(df_external, selected_pkg){
  df_title = df_external$df_title
  df_topic = df_external$df_topic
  df_trending = df_external$df_trending
  df_top_download_last_day = df_external$df_top_download_last_day
  df_top_download_last_week = df_external$df_top_download_last_week
  df_top_download_last_month = df_external$df_top_download_last_month
  
  df_long <- cranlogs::cran_downloads(selected_pkg, 
                                      when = "last-month") %>% 
    rename(Package = package)
  df_plot = prepareData(df_long, df_topic, df_title)
  df_plot = merge(data.table::data.table(df_plot, key = "Package"), 
                  data.table::data.table(df_trending, key = "Package"), 
                  all.x = TRUE, by = "Package") 
  df_plot = merge(data.table::data.table(df_plot, key = "Package"), 
                  data.table::data.table(df_top_download_last_month, key = "Package"), 
                  all.x = TRUE, by = "Package")
  df_plot = merge(data.table::data.table(df_plot, key = "Package"), 
                  data.table::data.table(df_top_download_last_week, key = "Package"), 
                  all.x = TRUE, by = "Package")
  df_plot = merge(data.table::data.table(df_plot, key = "Package"), 
                  data.table::data.table(df_top_download_last_day, key = "Package"), 
                  all.x = TRUE, by = "Package")
  df_plot = df_plot %>% 
    mutate(trending = ifelse(is.na(trending), "", trending)) %>% 
    mutate_at(vars(starts_with("top_download")), function(x) ifelse(is.na(x), "", x)) %>% 
    tidyr::unite(Status, trending, starts_with("top_download"), sep = "|")
  
  return(df_plot)
}

#### React Table ---------------------------------------------------------------
printReactable = function(df, arrange_var, arrange_type, height = "780",...){
  dots = list(...)
  
  # browser()
  reactable(df, 
            showSortable = TRUE,
            fullWidth = TRUE,
            height = height,
            highlight = TRUE,
            searchable = TRUE, 
            defaultSortOrder = arrange_type,
            defaultSorted = arrange_var,
            theme = reactableTheme(
              borderColor = "#dfe2e5",
              stripedColor = "#f6f8fa",
              highlightColor = "hsl(0, 0%, 96%)",
              cellPadding = "20px 12px",
              style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
              headerStyle = list(
                "color" = "hsl(0, 0%, 98%)",
                "background-color" = 'hsl(205, 93%, 16%)',
                "&:hover[aria-sort]" = list(background = "#0275D8"),
                "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#0275D8"),
                borderColor = "#555"
              )
            ),
            defaultColDef = colDef(
              headerStyle = "align-self: flex-end; font-weight:bold;"
            ),
            columns = list(
              Topic = colDef(minWidth = 60,
                             sortable = TRUE,
                             cell = function(value, index) {
                               # browser()
                               # add hyperlink to all the topics in one cell
                               if (value != "Others"){
                                 all_types = strsplit(value, "[|]") %>% unlist(.)
                                 urls = sprintf("https://cran.r-project.org/web/views/%s.html", all_types)
                                 ls = list()
                                 for (i in 1:(length(all_types)*2 - 1)){
                                   if (i %% 2 == 0){
                                     ls[[i]] = htmltools::tags$a(", ")
                                   } else{
                                     ls[[i]] = htmltools::tags$a(href = urls[(i + 1)/2], target = "_blank",
                                                                 as.character(all_types[(i + 1)/2]))
                                   }
                                 }
                                 tagList(ls)
                               } else{
                                 htmltools::tags$p(value)
                               } 
                             }),
              Package = colDef(minWidth = 60,
                               sortable = TRUE,
                               cell = function(value, index) {
                                 # add hyperlink
                                 url <- sprintf("https://cran.r-project.org//web/packages/%s/index.html", value)
                                 htmltools::tags$a(href = url, target = "_blank", as.character(value))
                               }
              ),
              `Today's Count` = colDef(
                name = "Last Day Downloads",
                format = colFormat(separators = TRUE, digits = 0)
              ),
              downloads = colDef(
                name = paste0("Total Downloads (", format(dots$start_date, "%m/%d"), 
                              "~", format(dots$end_date, "%m/%d"), ")"),
                format = colFormat(separators = TRUE, digits = 0)
              ),
              increase = colDef(
                name = "Score",
                format = colFormat(separators = TRUE, digits = 0)
              ),
              Count = colDef(
                name = "Downloads",
                minWidth = 150,
                align = "center",
                filterable = FALSE,
                sortable = FALSE,
                cell = function(value, index) {
                  dataui::dui_sparkline(
                    data = value$data,
                    height = 100,
                    margin = list(left = 50, top = 10),
                    valueAccessor =  htmlwidgets::JS("(d) => d.count"),
                    renderTooltip = htmlwidgets::JS(htmltools::HTML("
                      function (_ref) {
                        // console.log(_ref)
                        var datum = _ref.datum;
                        // console.log(datum)
    
                        return React.createElement(
                          'div',
                          {style: {margin: 0, padding: 0}},
                          datum.date && React.createElement(
                            'div',
                            {style: { fontWeight: 'bold', fontSize: '0.8em', padding: '0px 0px',
                                      textAlign: 'right'
                            }},
                            datum.date
                          ),
                          React.createElement(
                            'div',
                            {style: { fontWeight: 'bold', fontSize: '0.8em', padding: '0px 0px',
                                      textAlign: 'right'
                            }},
                            datum.count.toLocaleString()
                         )
                        );
                      }")),
                    components = list(
                      dataui::dui_sparklineseries( stroke = "#000000"),
                      dataui::dui_sparkpointseries(
                        points = list("all"),
                        stroke = "#000000",
                        fill = "#000000",
                        size = 1
                      ),
                      dataui::dui_sparkpointseries(
                        points = list("min"),
                        fill = "#0275D8",
                        size = 5
                      ),
                      dataui::dui_sparkpointseries(
                        points = list("max"),
                        fill = "#ff0000",
                        size = 5
                      ) ,
                      dataui::dui_tooltip(components = list(
                        dataui::dui_sparkverticalrefline(
                          strokeDasharray = "4,4",
                          stroke = gray.colors(10)[3]
                        ),
                        dataui::dui_sparkpointseries(fill = "#fff",
                                                     stroke = gray.colors(10)[3])
                      ))
                    )
                  )
                }
              ), 
              Status = colDef(
                cell = function(value, index) {
                  # style the different status in one cell
                  values = strsplit(value, "[|]") %>% unlist(.)
                  ls = list()
                  for (i in 1:length(values)){
                    if (values[i] == "Trending"){
                      classes = "tag num-low"
                      ls[[i]] = tags$span(class = classes, values[i])
                    } else if (values[i] == "Top Downloaded (last-month)"){
                      classes = "tag num-high"
                      ls[[i]] = tags$span(class = classes, values[i])
                    } else if (values[i] == "Top Downloaded (last-week)"){
                      classes = "tag num-high"
                      ls[[i]] = tags$span(class = classes, values[i])
                    } else if (values[i] == "Top Downloaded (last-day)"){
                      classes = "tag num-high"
                      ls[[i]] = tags$span(class = classes, values[i])
                    } else {
                      ls[[i]] = tags$span()
                    }
                  }
                  tagList(ls)
                }
              )
            )
  )
}

