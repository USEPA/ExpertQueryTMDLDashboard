library(shiny)
library(shinyjs)
library(DT)
library(ggplot2)
library(plotly)
library(bslib)
library(scales)
library(shinythemes)
library(bsicons)

# Resolve the installed app directory (works for installed package and dev)
pkg_app_dir <- getOption(
  "TMDLDash.app_dir",
  default = tryCatch(system.file("app", package = "TMDLDash"), error = function(e) "")
)

if (!nzchar(pkg_app_dir) || !dir.exists(pkg_app_dir)) {
  # Fallbacks for development (e.g., devtools::load_all(), running from source)
  pkg_path <- tryCatch(getNamespaceInfo("TMDLDash", "path"), error = function(e) "")
  candidates <- c(
    file.path(pkg_path, "inst", "app"),
    file.path(getwd(),  "inst", "app")
  )
  pkg_app_dir <- candidates[dir.exists(candidates)][1]
}

if (!nzchar(pkg_app_dir) || !dir.exists(pkg_app_dir)) {
  stop("pkg_app_dir could not be determined; ensure inst/app exists and run_app() sets TMDLDash.app_dir.")
}

options(shiny.fullstacktrace = TRUE)
message("App init start: ", Sys.time())
message("pkg_app_dir: ", pkg_app_dir)

# Per-process cache for large data (shared across sessions in this R worker)
EQ_cache <- new.env(parent = emptyenv())
# UI
ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),            # or 4 if you used BS4 classes
  # Load your app-local CSS from www/
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  # If header.html is just a banner fragment
  shiny::includeHTML("header.html"),               # make sure it has no <html>/<head>/<body>
  page_sidebar(
    # url options
    tags$head(
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
    # title and update information
    title = div(
      br(),
      tags$h1("National Summary of TMDLs in ATTAINS", style = "margin-bottom: 0;"),
      tags$h3(htmlOutput("update.tmdls"))
    ),
    # create sidebar for user inputs
    sidebar = sidebar(
      sliderInput("year", "Year:", min = 1975, max = max_year, value = c(1975, max_year), sep = ""),
      selectInput("region", "Region:", choices = sort(unique(states_regions$region)), selected = NULL, multiple = TRUE),
      selectInput("state", "State:", choices = sort(unique(states_regions$state)), selected = NULL, multiple = TRUE),
      selectInput("pollgroup", "Pollutant Group:", choices = sort(unique(pollutants_groups$pollutantGroup)), selected = NULL, multiple = TRUE),
      selectInput("pollutant", "Pollutant:", choices = sort(unique(pollutants_groups$pollutant)), selected = NULL, multiple = TRUE),
      selectInput("addparam", "Addressed Parameter:", choices = sort(unique(parameters$addressedParameter)), selected = NULL, multiple = TRUE),
      selectInput("actagency", "Action Agency:", choices = sort(unique(act_agencies)), selected = NULL, multiple = TRUE),
      actionButton("update", "Update"),
      actionButton("clear", "Clear")
    ),
    # create tabs
    navset_card_underline(
      # title for all tabs
      title = "Tabs:",
      # create filtered results panel
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
          choices = c(
            "By Unique Pollutant/Assessment Unit/Action ID" = "reactive_df"
          ),
          selected = "reactive_df",
          width = "100%"
        ),
        downloadButton("download_df", "Download Data"),
        DTOutput("table")
      ),
      # create tmdl production history tab
      nav_panel(
        "TMDL Production History",
        radioButtons(
          inputId = "prod_select",
          label = "Select TMDL Count Method:",
          choices = c(
            "By Unique Pollutant/Assessment Unit/Action ID" = "reactive_df"
          ),
          selected = "reactive_df",
          width = "100%"
        ),
        # add bar graph panel
        accordion(
          accordion_panel(
            title = "Filled Area Graph",
            icon = bsicons::bs_icon("graph-up"),
            plotly::plotlyOutput("historyplot")
          ),
          # add data table panel
          accordion_panel(
            title = "Data Table",
            icon = bsicons::bs_icon("table"),
            downloadButton("download_prodhist", "Download Data"),
            DTOutput("prodhist")
          )
        )
      ),
      # create annual tmdl production panel
      nav_panel(
        "Annual TMDL Production",
        radioButtons(
          inputId = "annual_select",
          label = "Select TMDL Count Method:",
          choices = c(
            "By Unique Pollutant/Assessment Unit/Action ID" = "reactive_df"
          ),
          selected = "reactive_df",
          width = "100%"
        ),
        # add bar graph panel
        accordion(
          accordion_panel(
            title = "Bar Graph",
            icon = bsicons::bs_icon("bar-chart"),
            plotly::plotlyOutput("annual")
          ),
          # add data table panel
          accordion_panel(
            title = "Data Table",
            icon = bsicons::bs_icon("table"),
            downloadButton("download_annual", "Download Data"),
            DTOutput("annualtable")
          )
        )
      ),
      # create pollutant panel
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
        # add pie graph panel
        accordion(
          accordion_panel(
            title = "Pie Graph",
            icon = bsicons::bs_icon("pie-chart"),
            plotly::plotlyOutput("pie"), uiOutput("back")
          ),
          # add data table panel
          accordion_panel(
            title = "Data Table",
            icon = bsicons::bs_icon("table"),
            downloadButton("download_pollutant", "Download Data"),
            DTOutput("pietable")
          )
        )
      ),
      # create tmdls by state panel
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
        # add bar graph panel
        accordion(
          accordion_panel(
            title = "Bar Graph",
            icon = bsicons::bs_icon("bar-chart"),
            plotly::plotlyOutput("bystate")
          ),
          # add data table panel
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
  shiny::includeHTML("footer.html")
)


# Server
server <- function(input, output, session) {
  # Reactive flags/values initialized empty
  data_ready  <- reactiveVal(FALSE)
  reactive_df <- reactiveVal(NULL)
  original_df <- reactiveVal(NULL)
  
  # Load the large data once, after the UI is first rendered
  session$onFlushed(function() {
    if (!isTRUE(EQ_cache$loaded)) {
      message("startup: loading EQ_data start")
      
      # Resolve the .RData path robustly for a package
      path <- system.file("extdata", "EQ_data.RData", package = "TMDLDash")
      stopifnot(nzchar(path) && file.exists(path))
      env <- new.env(parent = emptyenv())
      load(path, envir = env)
      
      # Move all loaded objects into the cache (names must match your .RData)
      list2env(as.list(env), envir = EQ_cache)
      EQ_cache$loaded <- TRUE
      
      # Initialize your reactive datasets from the loaded objects
      reactive_df(EQ_cache$filt.df)
      original_df(EQ_cache$filt.df)
      
      data_ready(TRUE)
      message("startup: loading EQ_data end")
    }
  }, once = TRUE)

  # create dynamic filter so region selection will limit states available
  observe({
    req(data_ready())  # wait for data to load
    
    selected_region <- input$region
    sr <- EQ_cache$states_regions
    
    if (is.null(selected_region) || length(selected_region) == 0) {
      updateSelectInput(session, "state", choices = sort(unique(sr$state)))
    } else {
      filtered_states <- sort(unique(sr$state[sr$region %in% selected_region]))
      updateSelectInput(session, "state", choices = filtered_states)
    }
  })


  # create dynamic filter so pollutant group selection will limit pollutants available
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

  # create dynamic filter so pollutant group and pollutant selection will limit addressed parameters available
  observe({
    req(data_ready())  # ensure large data is loaded
    
    selected_pollgroup <- input$pollgroup
    selected_pollutant <- input$pollutant
    
    pg_tbl   <- EQ_cache$addparameters_filter_pg
    poll_tbl <- EQ_cache$addparameters_filter_poll
    
    # filter addressed parameters by pollutant group
    if (is.null(selected_pollgroup) || length(selected_pollgroup) == 0) {
      filtered_pg_params <- sort(unique(pg_tbl$addressedParameter))
    } else {
      filtered_pg_params <- sort(unique(
        pg_tbl$addressedParameter[pg_tbl$pollutantGroup %in% selected_pollgroup]
      ))
    }
    
    # filter addressed parameters by pollutant
    if (is.null(selected_pollutant) || length(selected_pollutant) == 0) {
      filtered_poll_params <- sort(unique(poll_tbl$addressedParameter))
    } else {
      filtered_poll_params <- sort(unique(
        poll_tbl$addressedParameter[poll_tbl$pollutant %in% selected_pollutant]
      ))
    }
    
    # intersect both filters and update the dropdown
    comb_param_filter <- intersect(filtered_pg_params, filtered_poll_params)
    updateSelectInput(session, "addparam", choices = comb_param_filter)
  })

  # update reactive df based on user inputs
  observeEvent(input$update, {
    req(data_ready())
    temp_df <- original_df()

    if (!is.null(input$year)) {
      temp_df <- temp_df[temp_df$fiscalYearEstablished >= input$year[1] & temp_df$fiscalYearEstablished <= input$year[2], ]
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
      params <- parameters %>%
        dplyr::filter(addressedParameter %in% input$addparam)

      temp_df <- temp_df[temp_df$addressedParameters %in% params$addressedParameters, ]
    }
    
    if (!is.null(input$actagency) && length(input$actagency) > 0) {
      temp_df <- temp_df[temp_df$actionAgency %in% input$actagency, ]
    }


    reactive_df(temp_df)
  })

  # reset reactive df to original df (remove all user inputs)
  observeEvent(input$clear, {
    req(data_ready())
    reactive_df(EQ_cache$filt.df)
    original_df(EQ_cache$filt.df)
    # updateCheckboxInput(session, "counttype", selected = "waterbody")

    updateSliderInput(session, "year", min = 1995, max = EQ_cache$max_year, value = c(1995, EQ_cache$max_year))

    updateSelectInput(session, "region", selected = "")

    updateSelectInput(session, "state", selected = "")

    updateSelectInput(session, "pollgroup", selected = "")

    updateSelectInput(session, "pollutant", selected = "")
    
    updateSelectInput(session, "actagency", selected = "")
  })

  # create reactive df to count waterbody and pollutant combinations
  wb_df <- reactive({
    req(reactive_df())
    
    df <- reactive_df() %>%
      dplyr::select(
        region, state, pollutant, pollutantGroup, assessmentUnitId,
        assessmentUnitName
      ) %>%
      dplyr::distinct()

    return(df)
  })

  # create reactive waterbody tally
  wbtally_df <- reactive({
    req(wb_df())
    
    df <- wb_df() %>%
      dplyr::group_by(state) %>%
      dplyr::arrange(state) %>%
      dplyr::tally()

    return(df)
  })

  # create output table for filtered (by user input) tmdls for 'Filtered TMDL Results' tab
  output$table <- renderDT({
    req(reactive_df())
    
    datatable(
      reactive_df() %>%
        dplyr::mutate(planSummaryLink = paste0('<a href="', planSummaryLink, '" target="_blank">', planSummaryLink, "</a>")) %>%
        dplyr::rename(
          Region = region,
          State = state,
          "Fiscal Year Established" = fiscalYearEstablished,
          Pollutant = pollutant,
          "Pollutant Group" = pollutantGroup,
          "Action ID" = actionId,
          "Assessment Unit ID" = assessmentUnitId,
          "Plan Summary Link" = planSummaryLink
        ) %>%
        dplyr::distinct(),
      filter = "top",
      escape = FALSE,
      extensions = "FixedHeader",
      rownames = FALSE
    )
  })


  # date update
  output$update.tmdls <- renderText({
    req(data_ready())
    
    paste0(" Data last updated on ", update.tmdls, ".")
  })

  # tmdls version one
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
      "In this filtered data set there are : ", "<br>", "<br>",
      "<b>", count2, "</b>",
      " unique combinations of assessmentUnitId and pollutant",  "<br>",
      "<i>", "Number of TMDLs (i.e., waterbody/pollutant combinations) currently in place.", "</i>",
      "<br>", "<br>",
      "<b>", count, "</b>",
      "  unique combinations of actionId, assessmentUnitId and pollutant", "<br>",
      "<i>", "Number of TMDLs established, including TMDLs that were later revised and other variations.", "</i>",
      "<br>", "<br>",
      "Use radio buttons at the top of each tab to select the count method. ",
      "Some figures can only be displayed using the actionId/assessmentUnitId/pollutant because a single fiscalYearEstablished is required to plot the TMDL."
    )
  })

  # count explanation cwa
  output$cwa <- renderText({
    HTML(paste0(
      "Under section 303(d) of the CWA, EPA approves or disapproves state submissions of ",
      "Total Maximum Daily Loads (TMDLs) for impaired waters. A TMDL is the sum of the ",
      "individual Wasteload allocations (WLAs) for point sources , load allocations (LAs) for ",
      "non-point sources and natural background.",
      "<br>", "<br>",
      "It is crucial to be aware of data complexities due to database structure or legacy data that were imported into the current ATTAINS database. ", 
      "Users are cautioned against direct comparisons between states due to differences in data entry, TMDL development, and assessment methodologies.",
      "<br>", "<br>",
      "Data Source: ",
      '<a href="', "https://owapps.epa.gov/expertquery/national-downloads", '" target="_blank">',
      "Expert Query National Downloads", "</a>", "<br>",
      "Data Cleaning: ", '<a href="', "DataCleaning.html", '" target="_blank">',
      "Workflow", "</a>", "<br>",
      "Definitions: ",
      '<a href="', "https://www.ecfr.gov/current/title-40/chapter-I/subchapter-D/part-130/section-130.2", '" target="_blank">',
      "40 C.F.R. 130.2(i)", "</a>"
      
    ))
  })

  # add user instructions for dashboard
  output$instructions <- renderText({
    HTML(paste0(
      "<ul>",
      "<b>Filters:</b>",
      "<br>",
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
      "<b>Downloads:</b>",
      "<br>",
      "<li>All summary tables and figures in the dashboard can be downloaded.</li>",
      "<li>To download summary tables, click the “Download Data” button above the table. This will download the data as a .csv file.</li>",
      "<li>To download figures, click on the camera icon in the upper right hand corner of the figure. This will save the figure as a static .png image. No interactive functionality will be retained.</li>",
      "<br>",
      "<b>Questions:</b>",
      "<br>",
      "<li>Contact the ATTAINS Team at <a href='mailto:ATTAINS@epa.gov' target='_blank'>ATTAINS@epa.gov</a> with any questions, issues or suggestions.</li>",
      "</ul>"
    ))
  })
  
  # create download button for filtered tmdl results tab
  output$download_df <- downloadHandler(
    filename = function() {
      paste("filtered_TMDLs", format(Sys.Date(), "_%m_%d_%Y"), ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(reactive_df(), file)
    }
  )

  # create reactive df to count annual and cummulative tmdls
  count_df <- reactive({
    req(reactive_df())
    
    df <- reactive_df() %>%
      dplyr::select(state, fiscalYearEstablished, pollutant, assessmentUnitId, actionId) %>%
      dplyr::distinct() %>%
      dplyr::group_by(fiscalYearEstablished) %>%
      dplyr::summarize(TMDLCOUNT = length(pollutant)) %>%
      tidyr::complete(fiscalYearEstablished = tidyr::full_seq(input$year[1]:input$year[2], 1)) %>%
      dplyr::filter(!is.na(fiscalYearEstablished)) %>%
      dplyr::arrange(fiscalYearEstablished) %>%
      dplyr::mutate(
        TMDLCOUNT = ifelse(is.na(TMDLCOUNT), 0, TMDLCOUNT),
        CUMMULATIVETMDLS = cumsum(TMDLCOUNT)
      )

    return(df)
  })


  # create reactive value for pollutant group selection (user input) to use in "Pollutants" tab plot and table
  current_category <- reactiveVal()

  # observe user click to select category (pollutant group)
  observeEvent(session$clientData$output_pie_width, {
    observeEvent(plotly::event_data("plotly_click", source = "pollutant_pie"), {
      ed <- plotly::event_data("plotly_click", source = "pollutant_pie")
      if (!is.null(ed) && "customdata" %in% names(ed) && length(ed$customdata) > 0) {
        current_category(ed$customdata[[1]])
      }
    }, ignoreInit = TRUE)
  }, once = TRUE)

  # create reactive df to count tmdls by pollutant group, unless a pollutant group is selected to use for "Pollutants" plot and table
  pies_data <- reactive({
    df <- switch(input$poll_radio,
      "reactive_df" = reactive_df(),
      "wb_df" = wb_df()
    )

    if (!length(current_category())) {
      return(dplyr::count(df, pollutantGroup))
    }
    # if pollutant group is selected, count by pollutant
    df %>%
      dplyr::filter(pollutantGroup %in% current_category()) %>%
      dplyr::count(pollutant)
  })

  # create download button for pollutant results tab
  output$download_pollutant <- downloadHandler(
    filename = function() {
      paste("pollutant", format(Sys.Date(), "_%m_%d_%Y"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pies_data(), file)
    }
  )

  # create data table for "Pollutants" tab
  output$pietable <- renderDT({
    if (!length(current_category())) {
      pie.df <- pies_data() %>%
        dplyr::rename(
          "Pollutant Group" = pollutantGroup,
          "TMDL Count" = n
        )
    }

    if (length(current_category())) {
      pie.df <- pies_data() %>%
        dplyr::rename(
          "Pollutant" = pollutant,
          "TMDL Count" = n
        )
    }

    datatable(pie.df,
      escape = FALSE,
      rownames = FALSE
    )
  })


  # create dynamic pie chart for "Pollutants" tab
  output$pie <- renderPlotly({
    poll_select <- current_category()

    d <- setNames(pies_data(), c("labels", "values")) %>%
      dplyr::mutate(
        percent = round(values / sum(values), 5),
        labs_w_vals = paste(labels, " (", scales::comma(values), ")", sep = "")
      )


    pie_plot <- plotly::plot_ly(
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
            paste("TMDLS by Pollutant for ", poll_select, sep = "")
          ),
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
            ""
          )
        ),
        margin = list(b = 100, t = 100),
        showlegend = TRUE
      ) %>%
      plotly::event_register("plotly_click")

    return(pie_plot)
  })



  # create back button for pollutant pie chart
  output$back <- renderUI({
    if (length(current_category())) {
      actionButton("reset", "Back to TMDLS by Pollutant Group", icon("chevron-left"))
    }
  })

  # observe clear for current category for pollutants
  observeEvent(input$reset, current_category(NULL))


  # create annaul tmdls plot
  output$annual <- renderPlotly({
    bar_annual <- plotly::plot_ly(count_df(), x = ~fiscalYearEstablished, y = ~TMDLCOUNT, type = "bar") %>%
      plotly::layout(
        title = "TMDLS Produced Per Year",
        xaxis = list(title = "Fiscal Year"),
        yaxis = list(title = "Number of TMDLs Produced")
      )

    bar_annual
  })

  # create reactive df for annual tmdls
  annual_data <- reactive({
    count_df() %>%
      dplyr::select(fiscalYearEstablished, TMDLCOUNT) %>%
      dplyr::rename(
        "Fiscal Year Established" = fiscalYearEstablished,
        "TMDL Count" = TMDLCOUNT
      )
  })

  # create annual tmdls table
  output$annualtable <- renderDT({
    datatable(
      data = annual_data(),
      escape = FALSE,
      rownames = FALSE
    )
  })

  # create download button for annual tmdl results tab
  output$download_annual <- downloadHandler(
    filename = function() {
      paste("annual_TMDLs", format(Sys.Date(), "_%m_%d_%Y"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(annual_data(), file)
    }
  )

  # create reactive df for production history
  prod_data <- reactive({
    count_df() %>%
      dplyr::select(fiscalYearEstablished, TMDLCOUNT, CUMMULATIVETMDLS) %>%
      dplyr::rename(
        "Fiscal Year Established" = fiscalYearEstablished,
        "Annual TMDL Count" = TMDLCOUNT,
        "Cummulative TMDL Count" = CUMMULATIVETMDLS
      )
  })

  # create production history data table
  output$prodhist <- renderDT({
    datatable(prod_data(),
      escape = FALSE,
      rownames = FALSE
    )
  })

  # create download button for production history results tab
  output$download_prodhist <- downloadHandler(
    filename = function() {
      paste("productionhist_TMDLs", format(Sys.Date(), "_%m_%d_%Y"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_df(), file)
    }
  )


  # create reactive state/tmdls data frame
  state_df <- reactive({
    df <- switch(input$state_select,
      "reactive_df" = reactive_df(),
      "wb_df" = wb_df()
    ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(state) %>%
      dplyr::arrange(state) %>%
      dplyr::tally()

    return(df)
  })

  # create data table for state and tmdl counts
  output$statetable <- renderDT({
    datatable(
      state_df() %>%
        dplyr::rename(
          "TMDL Count" = n,
          "State" = state
        ),
      escape = FALSE,
      rownames = FALSE
    )
  })
  
 # download handler for state and tmdl counts
   output$download_state <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(state_df(), file)
    }
  )


  # create state tmdl count bar plot
  output$bystate <- renderPlotly({
    df <- state_df()

    plot <- plotly::plot_ly(df,
      x = ~n,
      y = ~state,
      type = "bar",
      text = ~state,
      textposition = "outside"
    ) %>%
      plotly::layout(
        title = "TMDLs by State",
        yaxis = list(
          title = "State",
          showticklabels = FALSE
        ),
        xaxis = list(title = "Number of TMDLs")
      )

    plot
  })

  # create stacked area plot for annual and cummulative tmdls
  output$historyplot <- renderPlotly({
    df <- count_df()

    plot <- plotly::plot_ly(
      x = ~ df$fiscalYearEstablished,
      y = ~ df$CUMMULATIVETMDLS,
      type = "scatter",
      mode = "none",
      fill = "tonexty",
      fillcolor = "#D55E00",
      name = "Cummulative TMDLs"
    ) %>%
      plotly::add_trace(
        x = ~ df$fiscalYearEstablished,
        y = ~ df$TMDLCOUNT,
        type = "scatter",
        mode = "none",
        fill = "tozeroy",
        fillcolor = "#0072B2",
        name = "Annual TMDLs"
      ) %>%
      plotly::layout(
        title = list(
          text = "Annual TMDL Production",
          xref = "paper",
          anchor = "center"
        ),
        xaxis = list(title = "Fiscal Year Established"),
        yaxis = list(
          title = "Number of TMDLs",
          range = list(0, max(df$CUMMULATIVETMDLS) + 0.2 * max(df$CUMMULATIVETMDLS))
        )
      )
    plot
  })
}

# Run the app
shinyApp(ui = ui, server = server)
