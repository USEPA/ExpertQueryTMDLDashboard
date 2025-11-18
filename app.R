library(shiny)
library(shinyjs)
library(DT)
library(ggplot2)
library(plotly)
library(bslib)
library(scales)
library(shinythemes)
library(bsicons)

# Bug fixes/To do list:
# fix top banner so that gray section collapses when not selected
#   expand Y axis to 250 k for TMDL Production History
# pollutant/pollutant group filter is not working (pollutant list should be filtered based on selected pollutant group)
# addressed parameters available to search should filter based on pollutant/pollutant group
# add download all button (to download all tables/plots in one zip file)

load("EQ_data.RData")

# UI
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  ),
  shiny::includeHTML("app/header.html"),
  page_sidebar(
    # url options
    tags$head(
      tags$script(HTML("$(document).on('click', 'a', function(e) { e.stopPropogation(); });")),
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
      tags$h3(htmlOutput("update"))
    ),
    # create sidebar for user inputs
    sidebar = sidebar(
      sliderInput("year", "Year:", min = 1975, max = max_year, value = c(1975, max_year), sep = ""),
      selectInput("region", "Region:", choices = sort(unique(states_regions$region)), selected = NULL, multiple = TRUE),
      selectInput("state", "State:", choices = sort(unique(states_regions$state)), selected = NULL, multiple = TRUE),
      selectInput("pollgroup", "Pollutant Group:", choices = sort(unique(pollutants_groups$pollutantGroup)), selected = NULL, multiple = TRUE),
      selectInput("pollutant", "Pollutant:", choices = sort(unique(pollutants_groups$pollutant)), selected = NULL, multiple = TRUE),
      selectInput("addparam", "Addressed Parameter:", choices = sort(unique(parameters$addressedParameter)), selected = NULL, multiple = TRUE),
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
            "By Unique Pollution/Assessment Unit/Action ID" = "reactive_df"
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
            "By Unique Pollution/Assessment Unit/Action ID" = "reactive_df"
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
            "By Unique Pollution/Assessment Unit/Action ID" = "reactive_df"
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
            "By Unique Pollution/Assessment Unit/Action ID" = "reactive_df",
            "By Unique Pollution/Assessment Unit" = "wb_df"
          ),
          selected = "reactive_df",
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
            "By Unique Pollution/Assessment Unit/Action ID" = "reactive_df",
            "By Unique Pollution/Assessment Unit" = "wb_df"
          ),
          selected = "reactive_df",
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
  shiny::includeHTML("app/footer.html")
)


# Server
server <- function(input, output, session) {
  # force links to open in browser
  options(shiny.launch.browser = TRUE)

  # create dynamic filter so region selection will limit states available
  observe({
    selected_region <- input$region
    if (is.null(selected_region) || length(selected_region) == 0) {
      updateSelectInput(session, "state", choices = sort(unique(states_regions$state)))
    } else {
      filtered_states <- sort(unique(states_regions$state[states_regions$region %in% selected_region]))
      updateSelectInput(session, "state", choices = filtered_states)
    }
  })


  # create dynamic filter so pollutant group selection will limit pollutants available
  observe({
    selected_pollgroup <- input$pollgroup
    if (is.null(selected_pollgroup) || length(selected_pollgroup) == 0) {
      updateSelectInput(session, "pollutant", choices = sort(unique(pollutants_groups$pollutant)))
    } else {
      filtered_pollutants <- sort(unique(pollutants_groups$pollutant[pollutants_groups$pollutantGroup %in% selected_pollgroup]))
      updateSelectInput(session, "pollutant", choices = sort(unique(filtered_pollutants)))
    }
  })

  # create dynamic filter so pollutant group and pollutant selection will limit addressed parameters available
  observe({
    selected_pollgroup <- input$pollgroup
    selected_pollutant <- input$pollutant
    
    # # debugging: print selected inputs
    # print(paste("Selected Pollutant Group:", paste(selected_pollgroup, collapse=", ")))
    # print(paste("Selected Pollutant:", paste(selected_pollutant, collapse=", ")))
    # 
    
    # filter addressed parameters by pollutant group
    if (is.null(selected_pollgroup) || length(selected_pollgroup) == 0) {
      filtered_pg_params <- sort(unique(addparameters_filter_pg$addressedParameter))
    } else {
      filtered_pg_params <- sort(unique(addparameters_filter_pg$addressedParameter[addparameters_filter_pg$pollutantGroup %in% selected_pollgroup]))
    }
    
    # filter addressed paramters by pollutant 
    if (is.null(selected_pollutant) || length(selected_pollutant) == 0) {
      filtered_poll_params <- sort(unique(addparameters_filter_poll$addressedParameter))
    } else {
      filtered_poll_params <- sort(unique(addparameters_filter_poll$addressedParameter[addparameters_filter_poll$pollutant %in% selected_pollutant]))
    }
    
    # compare the two lists and retain only those that are included in both
    comb_param_filter <- intersect(filtered_pg_params, filtered_poll_params)
    
    # # debugging: print the filtered lists
    # print(paste("Filtered by Pollutant Group:", paste(filtered_pg_params, collapse=", ")))
    # print(paste("Filtered by Pollutant:", paste(filtered_poll_params, collapse=", ")))
    # print(paste("Combined Filter:", paste(comb_param_filter, collapse=", ")))
    
    # update drop down menu for addressed parameters
    updateSelectInput(session, "addparam", choices = comb_param_filter)
  })
  

  # create reactive df for plots and tables
  reactive_df <- reactiveVal(filt.df)

  # create original df so underlying data for app can be reset
  original_df <- reactiveVal(filt.df)

  # update reactive df based on user inputs
  observeEvent(input$update, {
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


    reactive_df(temp_df)
  })

  # reset reactive df to original df (remove all user inputs)
  observeEvent(input$clear, {
    reactive_df(filt.df)
    original_df(filt.df)

    # updateCheckboxInput(session, "counttype", selected = "waterbody")

    updateSliderInput(session, "year", min = 1995, max = max_year, value = c(1995, max_year))

    updateSelectInput(session, "region", selected = "")

    updateSelectInput(session, "state", selected = "")

    updateSelectInput(session, "pollgroup", selected = "")

    updateSelectInput(session, "pollutant", selected = "")
  })

  # create reactive df to count waterbody and pollutant combinations
  wb_df <- reactive({
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
    df <- wb_df() %>%
      dplyr::group_by(state) %>%
      dplyr::arrange(state) %>%
      dplyr::tally()

    return(df)
  })

  # create output table for filtered (by user input) tmdls for 'Filtered TMDL Results' tab
  output$table <- renderDT({
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
      extensions = "FixedHeader"
    )
  })


  # date update
  output$update <- renderText({
    paste0(" The Expert Query National TMDL Profile was last updated on ", update.tmdls, ".")
  })

  # tmdls version one
  output$tmdl1 <- renderText({
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
      "<b>", count, "</b>",
      "  unique combinations of actionId, assessmentUnitId and pollutant", "<br>",
      "<b>", count2, "</b>",
      " unique combinations of assessmentUnitId and pollutant", "<br>", "<br>",
      "All of the other tabs in this dashboard count TMDLs as unique combinations of ",
      "actionId, assessmentUnitId and pollutant"
    )
  })

  # count explanation cwa
  output$cwa <- renderText({
    HTML(paste0(
      "Under section 303(d) of the CWA, EPA approves or disapproves state submissions of ",
      "Total Maximum Daily Loads (TMDLs) for impaired waters. A TMDL is the sum of the ",
      "individual Wasteload allocations (WLAs) for point sources , load allocations (LAs) for ",
      "non-point sources and natural background (",
      '<a href="', "https://www.ecfr.gov/current/title-40/chapter-I/subchapter-D/part-130/section-130.2", '" target="_blank">',
      "40 C.F.R. 130.2(i)", "</a>", ").", "<br>", "<br>",
      "Data Source: ",
      '<a href="', "https://owapps.epa.gov/expertquery/national-downloads", '" target="_blank">',
      "Expert Query National Downloads", "</a>"
    ))
  })

  # add user instructions for dashboard
  output$instructions <- renderText({
    HTML(paste0(
      "This section will contain instructions for using the dashboard."
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
  observe({
    cd <- event_data("plotly_click")$customdata[[1]]
    if (isTRUE(cd %in% categories)) current_category(cd)
  })

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
      escape = FALSE
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
      customdata = ~labels
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
      event_register("plotly_click")

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
      escape = FALSE
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
      escape = FALSE
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
      escape = FALSE
    )
  })
  
 # download handler for state and tmdl counts
   output$download_state <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file)
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
