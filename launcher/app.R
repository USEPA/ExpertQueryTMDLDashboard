library(bsicons)
library(bslib)
library(DT)
library(ggplot2)
library(later)
library(plotly)
library(scales)
library(shiny)
library(shinyjs)
library(shinythemes)
library(htmltools)
library(dplyr)
library(tidyr)
library(lubridate)

# Register app-local static resources
shiny::addResourcePath("appwww", "www")

# Load app data once at startup
source("R/data_load.R")
load_all_app_data()

message("startup: plain Shiny app loaded")
message("data exists? ", exists("filt.df", envir = EQ_cache, inherits = FALSE))

# UI
ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  tags$head(
    tags$link(rel = "stylesheet", href = "appwww/styles.css"),
    tags$script(HTML("$(document).on('click', 'a', function(e) { e.stopPropagation(); });")),
    tags$style(HTML("
      .accordion-button {
        background-color: #005EA2;
        color: white;
      }
      .accordion-button:not(.collapsed) {
        background-color: #005EA2;
        color: white;
      }
    "))
  ),
  
  shiny::includeHTML("www/header.html"),
  
  page_sidebar(
    title = div(
      br(),
      tags$h1("National Summary of TMDLs in ATTAINS", style = "margin-bottom: 0;"),
      tags$h3(htmlOutput("update.tmdls"))
    ),
    
    sidebar = sidebar(
      sliderInput(
        "year", "Year:",
        min = 1975,
        max = lubridate::year(Sys.Date()),
        value = c(1975, lubridate::year(Sys.Date())),
        sep = ""
      ),
      selectInput("region", "Region:", choices = character(0), multiple = TRUE),
      selectInput("state", "State:", choices = character(0), multiple = TRUE),
      selectInput("pollgroup", "Pollutant Group:", choices = character(0), multiple = TRUE),
      selectInput("pollutant", "Pollutant:", choices = character(0), multiple = TRUE),
      selectInput("addparam", "Addressed Parameter:", choices = character(0), multiple = TRUE),
      selectInput("actagency", "Action Agency:", choices = character(0), multiple = TRUE),
      actionButton("update", "Update"),
      actionButton("clear", "Clear")
    ),
    
    navset_card_underline(
      title = "Tabs:",
      
      nav_panel(
        "Summary",
        accordion(
          open = c("desc", "count", "instruct"),
          accordion_panel(
            title = span("TMDL Count", style = "font-size: 16px"),
            icon = bsicons::bs_icon("123"),
            tags$p(htmlOutput("tmdl1")),
            value = "count"
          ),
          accordion_panel(
            title = "Description",
            icon = bsicons::bs_icon("file-earmark-text"),
            tags$p(htmlOutput("cwa")),
            value = "desc"
          ),
          accordion_panel(
            title = "Dashboard Instructions",
            icon = bsicons::bs_icon("person-workspace"),
            tags$p(htmlOutput("instructions")),
            value = "instruct"
          )
        )
      ),
      
      nav_panel(
        "Filtered TMDL Results",
        radioButtons(
          inputId = "filt_select",
          label = "Select TMDL Count Method:",
          choices = c("By Unique Pollutant/Assessment Unit/Action ID" = "reactive_df"),
          selected = "reactive_df",
          width = "100%"
        ),
        downloadButton("download_df", "Download Data"),
        DTOutput("table")
      ),
      
      nav_panel(
        "TMDL Production History",
        radioButtons(
          inputId = "prod_select",
          label = "Select TMDL Count Method:",
          choices = c("By Unique Pollutant/Assessment Unit/Action ID" = "reactive_df"),
          selected = "reactive_df",
          width = "100%"
        ),
        accordion(
          accordion_panel(
            title = "Filled Area Graph",
            icon = bsicons::bs_icon("graph-up"),
            plotly::plotlyOutput("historyplot")
          ),
          accordion_panel(
            title = "Data Table",
            icon = bsicons::bs_icon("table"),
            downloadButton("download_prodhist", "Download Data"),
            DTOutput("prodhist")
          )
        )
      ),
      
      nav_panel(
        "Annual TMDL Production",
        radioButtons(
          inputId = "annual_select",
          label = "Select TMDL Count Method:",
          choices = c("By Unique Pollutant/Assessment Unit/Action ID" = "reactive_df"),
          selected = "reactive_df",
          width = "100%"
        ),
        accordion(
          accordion_panel(
            title = "Bar Graph",
            icon = bsicons::bs_icon("bar-chart"),
            plotly::plotlyOutput("annual")
          ),
          accordion_panel(
            title = "Data Table",
            icon = bsicons::bs_icon("table"),
            downloadButton("download_annual", "Download Data"),
            DTOutput("annualtable")
          )
        )
      ),
      
      nav_panel(
        "Pollutants",
        radioButtons(
          inputId = "poll_radio",
          label = "Select TMDL Count Method:",
          choices = c(
            "By Unique Pollutant/Assessment Unit" = "wb_df",
            "By Unique Pollutant/Assessment Unit/Action ID" = "reactive_df"
          ),
          selected = "wb_df",
          width = "100%"
        ),
        accordion(
          accordion_panel(
            title = "Pie Graph",
            icon = bsicons::bs_icon("pie-chart"),
            plotly::plotlyOutput("pie"),
            uiOutput("back")
          ),
          accordion_panel(
            title = "Data Table",
            icon = bsicons::bs_icon("table"),
            downloadButton("download_pollutant", "Download Data"),
            DTOutput("pietable")
          )
        )
      ),
      
      nav_panel(
        "TMDLs By State",
        radioButtons(
          inputId = "state_select",
          label = "Select TMDL Count Method:",
          choices = c(
            "By Unique Pollutant/Assessment Unit" = "wb_df",
            "By Unique Pollutant/Assessment Unit/Action ID" = "reactive_df"
          ),
          selected = "wb_df",
          width = "100%"
        ),
        accordion(
          accordion_panel(
            title = "Bar Graph",
            icon = bsicons::bs_icon("bar-chart"),
            plotly::plotlyOutput("bystate")
          ),
          accordion_panel(
            title = "Data Table",
            icon = bsicons::bs_icon("table"),
            downloadButton("download_state", "Download Data"),
            DTOutput("statetable")
          )
        )
      )
    )
  ),
  
  shiny::includeHTML("www/footer.html")
)

server <- function(input, output, session) {
  data_ready  <- reactiveVal(FALSE)
  reactive_df <- reactiveVal(NULL)
  original_df <- reactiveVal(NULL)
  
  session$onFlushed(function() {
    later::later(function() {
      if (!isTRUE(EQ_cache$loaded)) {
        load_all_app_data()
      }
      
      if (!exists("filt.df", envir = EQ_cache, inherits = FALSE)) {
        stop("Object 'filt.df' not found after loading data")
      }
      
      if (!is.null(EQ_cache$filt.df)) {
        reactive_df(EQ_cache$filt.df)
        original_df(EQ_cache$filt.df)
      }
      
      data_ready(TRUE)
      
      if (!is.null(EQ_cache$max_year) && is.finite(EQ_cache$max_year)) {
        updateSliderInput(
          session, "year",
          min = 1975,
          max = EQ_cache$max_year,
          value = c(1975, EQ_cache$max_year)
        )
      }
      
      if (!is.null(EQ_cache$states_regions)) {
        updateSelectInput(session, "region", choices = sort(unique(EQ_cache$states_regions$region)))
        updateSelectInput(session, "state", choices = sort(unique(EQ_cache$states_regions$state)))
      }
      
      if (!is.null(EQ_cache$pollutants_groups)) {
        updateSelectInput(session, "pollgroup", choices = sort(unique(EQ_cache$pollutants_groups$pollutantGroup)))
        updateSelectInput(session, "pollutant", choices = sort(unique(EQ_cache$pollutants_groups$pollutant)))
      }
      
      if (!is.null(EQ_cache$parameters)) {
        updateSelectInput(session, "addparam", choices = sort(unique(EQ_cache$parameters$addressedParameter)))
      }
      
      if (!is.null(EQ_cache$act_agencies)) {
        updateSelectInput(session, "actagency", choices = sort(unique(EQ_cache$act_agencies)))
      } else if (!is.null(EQ_cache$filt.df$actionAgency)) {
        updateSelectInput(session, "actagency", choices = sort(unique(EQ_cache$filt.df$actionAgency)))
      }
    }, delay = 0)
  }, once = TRUE)
  
  observe({
    req(data_ready())
    selected_region <- input$region
    sr <- EQ_cache$states_regions
    
    if (is.null(selected_region) || length(selected_region) == 0) {
      updateSelectInput(session, "state", choices = sort(unique(sr$state)))
    } else {
      filtered_states <- sort(unique(sr$state[sr$region %in% selected_region]))
      updateSelectInput(session, "state", choices = filtered_states)
    }
  })
  
  observe({
    req(data_ready())
    selected_pollgroup <- input$pollgroup
    pg <- EQ_cache$pollutants_groups
    
    if (is.null(selected_pollgroup) || length(selected_pollgroup) == 0) {
      updateSelectInput(session, "pollutant", choices = sort(unique(pg$pollutant)))
    } else {
      filtered_pollutants <- sort(unique(pg$pollutant[pg$pollutantGroup %in% selected_pollgroup]))
      updateSelectInput(session, "pollutant", choices = filtered_pollutants)
    }
  })
  
  observe({
    req(data_ready())
    selected_pollgroup <- input$pollgroup
    selected_pollutant <- input$pollutant
    
    pg_tbl   <- EQ_cache$addparameters_filter_pg
    poll_tbl <- EQ_cache$addparameters_filter_poll
    
    if (is.null(selected_pollgroup) || length(selected_pollgroup) == 0) {
      filtered_pg_params <- sort(unique(pg_tbl$addressedParameter))
    } else {
      filtered_pg_params <- sort(unique(pg_tbl$addressedParameter[pg_tbl$pollutantGroup %in% selected_pollgroup]))
    }
    
    if (is.null(selected_pollutant) || length(selected_pollutant) == 0) {
      filtered_poll_params <- sort(unique(poll_tbl$addressedParameter))
    } else {
      filtered_poll_params <- sort(unique(poll_tbl$addressedParameter[poll_tbl$pollutant %in% selected_pollutant]))
    }
    
    comb_param_filter <- intersect(filtered_pg_params, filtered_poll_params)
    updateSelectInput(session, "addparam", choices = comb_param_filter)
  })
  
  observeEvent(input$update, {
    req(data_ready())
    temp_df <- original_df()
    
    if (!is.null(input$year)) {
      temp_df <- temp_df[temp_df$fiscalYearEstablished >= input$year[1] &
                           temp_df$fiscalYearEstablished <= input$year[2], ]
    }
    
    if (!is.null(input$region) && length(input$region) > 0) {
      temp_df <- temp_df[temp_df$region %in% input$region, ]
    }
    
    if (!is.null(input$state) && length(input$state) > 0) {
      temp_df <- temp_df[temp_df$state %in% input$state, ]
    }
    
    if (!is.null(input$pollgroup) && length(input$pollgroup) > 0) {
      temp_df <- temp_df[temp_df$pollutantGroup %in% input$pollgroup, ]
    }
    
    if (!is.null(input$pollutant) && length(input$pollutant) > 0) {
      temp_df <- temp_df[temp_df$pollutant %in% input$pollutant, ]
    }
    
    if (!is.null(input$addparam) && length(input$addparam) > 0) {
      params <- EQ_cache$parameters %>%
        dplyr::filter(addressedParameter %in% input$addparam)
      
      temp_df <- temp_df[temp_df$addressedParameters %in% params$addressedParameters, ]
    }
    
    if (!is.null(input$actagency) && length(input$actagency) > 0) {
      temp_df <- temp_df[temp_df$actionAgency %in% input$actagency, ]
    }
    
    reactive_df(temp_df)
  })
  
  observeEvent(input$clear, {
    req(data_ready())
    reactive_df(EQ_cache$filt.df)
    original_df(EQ_cache$filt.df)
    
    updateSliderInput(session, "year", min = 1995, max = EQ_cache$max_year, value = c(1995, EQ_cache$max_year))
    updateSelectInput(session, "region", selected = character(0))
    updateSelectInput(session, "state", selected = character(0))
    updateSelectInput(session, "pollgroup", selected = character(0))
    updateSelectInput(session, "pollutant", selected = character(0))
    updateSelectInput(session, "actagency", selected = character(0))
  })
  
  wb_df <- reactive({
    req(reactive_df())
    reactive_df() %>%
      dplyr::select(region, state, pollutant, pollutantGroup, assessmentUnitId, assessmentUnitName) %>%
      dplyr::distinct()
  })
  
  wbtally_df <- reactive({
    req(wb_df())
    wb_df() %>%
      dplyr::group_by(state) %>%
      dplyr::arrange(state) %>%
      dplyr::tally()
  })
  
  output$table <- renderDT({
    req(reactive_df())
    datatable(
      reactive_df() %>%
        dplyr::mutate(planSummaryLink = paste0('<a href="', planSummaryLink, '" target="_blank">', planSummaryLink, "</a>")) %>%
        dplyr::rename(
          Region = region,
          State = state,
          `Fiscal Year Established` = fiscalYearEstablished,
          Pollutant = pollutant,
          `Pollutant Group` = pollutantGroup,
          `Action ID` = actionId,
          `Assessment Unit ID` = assessmentUnitId,
          `Plan Summary Link` = planSummaryLink
        ) %>%
        dplyr::distinct(),
      filter = "top",
      escape = FALSE,
      extensions = "FixedHeader",
      rownames = FALSE
    )
  })
  
  output$update.tmdls <- renderText({
    req(data_ready())
    paste0(" Data last updated on ", EQ_cache$update.tmdls, ".")
  })
  
  output$tmdl1 <- renderText({
    req(reactive_df())
    
    count <- reactive_df() %>%
      dplyr::select(assessmentUnitId, pollutant, actionId) %>%
      dplyr::n_distinct() %>%
      formatC(big.mark = ",")
    
    count2 <- reactive_df() %>%
      dplyr::select(assessmentUnitId, pollutant) %>%
      dplyr::n_distinct() %>%
      formatC(big.mark = ",")
    
    paste0(
      "In this filtered data set there are : <br><br>",
      "<b>", count2, "</b> unique combinations of assessmentUnitId and pollutant<br>",
      "<i>Number of TMDLs (i.e., waterbody/pollutant combinations) currently in place.</i><br><br>",
      "<b>", count, "</b> unique combinations of actionId, assessmentUnitId and pollutant<br>",
      "<i>Number of TMDLs established, including TMDLs that were later revised and other variations.</i><br><br>",
      "Use radio buttons at the top of each tab to select the count method. ",
      "Some figures can only be displayed using the actionId/assessmentUnitId/pollutant because a single fiscalYearEstablished is required to plot the TMDL."
    )
  })
  
  output$cwa <- renderText({
    HTML(paste0(
      "Under section 303(d) of the CWA, EPA approves or disapproves state submissions of ",
      "Total Maximum Daily Loads (TMDLs) for impaired waters. A TMDL is the sum of the ",
      "individual Wasteload allocations (WLAs) for point sources , load allocations (LAs) for ",
      "non-point sources and natural background.",
      "<br><br>",
      "It is crucial to be aware of data complexities due to database structure or legacy data that were imported into the current ATTAINS database. ",
      "Users are cautioned against direct comparisons between states due to differences in data entry, TMDL development, and assessment methodologies.",
      "<br><br>",
      "Data Source: ",
      '<a href="https://owapps.epa.gov/expertquery/national-downloads" target="_blank">Expert Query National Downloads</a><br>',
      "Data Cleaning: ",
      '<a href="DataCleaning.html" target="_blank">Workflow</a><br>',
      "Definitions: ",
      '<a href="https://www.ecfr.gov/current/title-40/chapter-I/subchapter-D/part-130/section-130.2" target="_blank">40 C.F.R. 130.2(i)</a>'
    ))
  })
  
  output$instructions <- renderText({
    HTML(paste0(
      "<ul>",
      "<b>Filters:</b><br>",
      "<li>All filters to refine data included in the summary tables and figures are found on the left side of the dashboard.</li>",
      "<li>The “Year” filter is a slider input and will include the oldest and most recent years shown above the circle markers.</li>",
      "<li>The “Region” allows multiple selections.</li>",
      "<li>The “State” filter will show all possible values if no “Region” selections have been made. If one or more “Region” selections are made, the available “State” options will include only the values relevant to the selected Regions.</li>",
      "<li>The “Pollutant Group” filter allows multiple selections.</li>",
      "<li>The “Pollutant” filter will show all possible values if no “Pollutant Group” selections have been made (over 1,000 options). If one or more “Pollutant Group” selections are made, the available “Pollutant” options will include only the values relevant to the selected Pollutant Groups.</li>",
      "<li>The “Addressed Parameter” filter allows multiple selections. If no “Pollutant Group” or “Pollutant” selections have been made, all “Addressed Parameter” values are shown. If “Pollutant Group” or “Pollutant” selections have been made, only “Addressed Parameters” associated with the chosen Pollutant Groups or Pollutants are shown.</li>",
      "<li>The “Action Agency” filter allows multiple selections. If no selection(s) have been made, results from all actionAgency values are shown.</li>",
      "<li>Other than the previously described relationships between Region/State, Pollutant Group/Pollutant, and Pollutant Group/Pollutant/Addressed Parameter, all filters function as “AND” operators. For example, filtering for “Region: 5”, “State: MN”, “Pollutant Group: AMMONIA”, and “Year: 1980-2000” would only return results that matched all of those filters.</li>",
      "<li>After all selections are made, the user needs to click the “Update” button to ensure all filters are applied. If any modifications to the filters are made, the user will need to click “Update” again to apply them.</li>",
      "<li>To remove all filters and start over, the user should click the “Clear” button shown below all the other filter options.</li>",
      "<br>",
      "<b>Downloads:</b><br>",
      "<li>All summary tables and figures in the dashboard can be downloaded.</li>",
      "<li>To download summary tables, click the “Download Data” button above the table. This will download the data as a .csv file.</li>",
      "<li>To download figures, click on the camera icon in the upper right hand corner of the figure. This will save the figure as a static .png image. No interactive functionality will be retained.</li>",
      "<br>",
      "<b>Questions:</b><br>",
      "<li>Contact the ATTAINS Team at <a href='mailto:ATTAINS@epa.gov' target='_blank'>ATTAINS@epa.gov</a> with any questions, issues or suggestions.</li>",
      "</ul>"
    ))
  })
  
  output$download_df <- downloadHandler(
    filename = function() paste("filtered_TMDLs", format(Sys.Date(), "_%m_%d_%Y"), ".csv", sep = ""),
    content = function(file) utils::write.csv(reactive_df(), file)
  )
  
  count_df <- reactive({
    req(reactive_df())
    
    reactive_df() %>%
      dplyr::select(state, fiscalYearEstablished, pollutant, assessmentUnitId, actionId) %>%
      dplyr::distinct() %>%
      dplyr::group_by(fiscalYearEstablished) %>%
      dplyr::summarize(TMDLCOUNT = length(pollutant), .groups = "drop") %>%
      tidyr::complete(fiscalYearEstablished = tidyr::full_seq(input$year[1]:input$year[2], 1)) %>%
      dplyr::filter(!is.na(fiscalYearEstablished)) %>%
      dplyr::arrange(fiscalYearEstablished) %>%
      dplyr::mutate(
        TMDLCOUNT = ifelse(is.na(TMDLCOUNT), 0, TMDLCOUNT),
        CUMMULATIVETMDLS = cumsum(TMDLCOUNT)
      )
  })
  
  current_category <- reactiveVal()
  
  observeEvent(session$clientData$output_pie_width, {
    observeEvent(plotly::event_data("plotly_click", source = "pollutant_pie"), {
      ed <- plotly::event_data("plotly_click", source = "pollutant_pie")
      if (!is.null(ed) && "customdata" %in% names(ed) && length(ed$customdata) > 0) {
        current_category(ed$customdata[[1]])
      }
    }, ignoreInit = TRUE)
  }, once = TRUE)
  
  pies_data <- reactive({
    df <- switch(input$poll_radio,
                 "reactive_df" = reactive_df(),
                 "wb_df" = wb_df()
    )
    
    if (!length(current_category())) {
      return(dplyr::count(df, pollutantGroup))
    }
    
    df %>%
      dplyr::filter(pollutantGroup %in% current_category()) %>%
      dplyr::count(pollutant)
  })
  
  output$download_pollutant <- downloadHandler(
    filename = function() paste("pollutant", format(Sys.Date(), "_%m_%d_%Y"), ".csv", sep = ""),
    content = function(file) write.csv(pies_data(), file)
  )
  
  output$pietable <- renderDT({
    if (!length(current_category())) {
      pie.df <- pies_data() %>% dplyr::rename(`Pollutant Group` = pollutantGroup, `TMDL Count` = n)
    } else {
      pie.df <- pies_data() %>% dplyr::rename(`Pollutant` = pollutant, `TMDL Count` = n)
    }
    
    datatable(pie.df, escape = FALSE, rownames = FALSE)
  })
  
  output$pie <- renderPlotly({
    poll_select <- current_category()
    
    d <- setNames(pies_data(), c("labels", "values")) %>%
      dplyr::mutate(
        percent = round(values / sum(values), 5),
        labs_w_vals = paste(labels, " (", scales::comma(values), ")", sep = "")
      )
    
    plotly::plot_ly(
      data = d,
      labels = ~labs_w_vals,
      values = ~values,
      type = "pie",
      textinfo = "none",
      hoverinfo = "label+value+percent",
      customdata = ~labels,
      source = "pollutant_pie"
    ) %>%
      plotly::layout(
        title = list(
          text = ifelse(is.null(poll_select),
                        "TMDLs by Pollutant Group",
                        paste("TMDLs by Pollutant for ", poll_select, sep = "")),
          x = 0.5,
          xanchor = "center",
          yanchor = "bottom",
          xref = "paper",
          yref = "paper",
          y = 1.2
        ),
        annotations = list(
          x = 0.5,
          y = -0.4,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          text = ifelse(is.null(poll_select),
                        "click on a Pollutant Group in Plot <br> to see the relative contribution of its Pollutants",
                        "")
        ),
        margin = list(b = 100, t = 100),
        showlegend = TRUE
      ) %>%
      plotly::event_register("plotly_click")
  })
  
  output$back <- renderUI({
    if (length(current_category())) {
      actionButton("reset", "Back to TMDLs by Pollutant Group", icon("chevron-left"))
    }
  })
  
  observeEvent(input$reset, current_category(NULL))
  
  output$annual <- renderPlotly({
    plotly::plot_ly(count_df(), x = ~fiscalYearEstablished, y = ~TMDLCOUNT, type = "bar") %>%
      plotly::layout(
        title = "TMDLs Produced Per Year",
        xaxis = list(title = "Fiscal Year"),
        yaxis = list(title = "Number of TMDLs Produced")
      )
  })
  
  annual_data <- reactive({
    count_df() %>%
      dplyr::select(fiscalYearEstablished, TMDLCOUNT) %>%
      dplyr::rename(`Fiscal Year Established` = fiscalYearEstablished, `TMDL Count` = TMDLCOUNT)
  })
  
  output$annualtable <- renderDT({
    datatable(data = annual_data(), escape = FALSE, rownames = FALSE)
  })
  
  output$download_annual <- downloadHandler(
    filename = function() paste("annual_TMDLs", format(Sys.Date(), "_%m_%d_%Y"), ".csv", sep = ""),
    content = function(file) write.csv(annual_data(), file)
  )
  
  prod_data <- reactive({
    count_df() %>%
      dplyr::select(fiscalYearEstablished, TMDLCOUNT, CUMMULATIVETMDLS) %>%
      dplyr::rename(
        `Fiscal Year Established` = fiscalYearEstablished,
        `Annual TMDL Count` = TMDLCOUNT,
        `Cummulative TMDL Count` = CUMMULATIVETMDLS
      )
  })
  
  output$prodhist <- renderDT({
    datatable(prod_data(), escape = FALSE, rownames = FALSE)
  })
  
  output$download_prodhist <- downloadHandler(
    filename = function() paste("productionhist_TMDLs", format(Sys.Date(), "_%m_%d_%Y"), ".csv", sep = ""),
    content = function(file) write.csv(reactive_df(), file)
  )
  
  state_df <- reactive({
    switch(input$state_select,
           "reactive_df" = reactive_df(),
           "wb_df" = wb_df()
    ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(state) %>%
      dplyr::arrange(state) %>%
      dplyr::tally()
  })
  
  output$statetable <- renderDT({
    datatable(
      state_df() %>% dplyr::rename(`TMDL Count` = n, State = state),
      escape = FALSE,
      rownames = FALSE
    )
  })
  
  output$download_state <- downloadHandler(
    filename = function() paste("data-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(state_df(), file)
  )
  
  output$bystate <- renderPlotly({
    df <- state_df()
    
    plotly::plot_ly(
      df,
      x = ~n,
      y = ~state,
      type = "bar",
      text = ~state,
      textposition = "outside"
    ) %>%
      plotly::layout(
        title = "TMDLs by State",
        yaxis = list(title = "State", showticklabels = FALSE),
        xaxis = list(title = "Number of TMDLs")
      )
  })
  
  output$historyplot <- renderPlotly({
    df <- count_df()
    
    plotly::plot_ly(
      x = ~df$fiscalYearEstablished,
      y = ~df$CUMMULATIVETMDLS,
      type = "scatter",
      mode = "none",
      fill = "tonexty",
      fillcolor = "#D55E00",
      name = "Cummulative TMDLs"
    ) %>%
      plotly::add_trace(
        x = ~df$fiscalYearEstablished,
        y = ~df$TMDLCOUNT,
        type = "scatter",
        mode = "none",
        fill = "tozeroy",
        fillcolor = "#0072B2",
        name = "Annual TMDLs"
      ) %>%
      plotly::layout(
        title = list(text = "Annual TMDL Production", xref = "paper", anchor = "center"),
        xaxis = list(title = "Fiscal Year Established"),
        yaxis = list(
          title = "Number of TMDLs",
          range = list(0, max(df$CUMMULATIVETMDLS) + 0.2 * max(df$CUMMULATIVETMDLS))
        )
      )
  })
}

shinyApp(ui = ui, server = server)