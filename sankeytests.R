library(plotly)

labels_with_values <- c(paste0("Raw Data: ", "<br>", formatC(orig.n, big.mark = ",")),
                        paste0("Duplicates Removed: ", "<br>", formatC(orig.distinct.n, big.mark = ",")),
                        paste0("Missing Data Removed: ", "<br>", formatC(orig.distinct.n-orig.noauidpoll.n, big.mark = ",")),
                        )
node_labels <- c(
  "Raw Data",
  "Distinct Records",
  "Missing Data Removed",
  "Final Clean Data",
)

# Define labels for flows
flow_labels <- c(
  "remove duplicates",
  "remove records missing key data",
  "condense to one row per unique actionId/assessmentUnitIdentifier/pollutant combination",
)


fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = node_labels,
    color = c("red", "darkorange", "purple", "darkblue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    ),
    hoverinfo = "none"
  ),
  
  link = list(
    source = c(0, 1, 2, 3),  # Adjusted source indices
    target = c(1, 2, 3, 4),  # Adjusted target indices
    value =  c(orig.n, orig.distinct.n, orig.distinct.n-orig.noauidpoll.n, filt.df.n),  # Values for each step
    color = c("pink", "orange", "lavender", "lightblue"),
    hovertemplate = paste(flow_labels, "<br>Value: %{value} records<br>")
  )
)

fig <- fig %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 10
  )
)

fig
