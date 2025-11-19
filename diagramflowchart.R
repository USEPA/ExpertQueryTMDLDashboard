# Install DiagrammeR if you haven't already
# install.packages("DiagrammeR")

library(DiagrammeR)

# Define the number of records at each step
raw_data <- orig.n
dups_removed <- orig.distinct.n
misskey_removed <- (orig.distinct.n - orig.noauidpoll.n)
final_clean <- filt.df.n


# Create a flow diagram with scaled box widths
grViz(sprintf("
digraph flowchart {
  node [fontname = Helvetica, shape = box, style = filled, fillcolor = lightblue]
  A [label = 'Raw Data\nRecords: %d', width = %.2f, height = %.2f]
  B [label = 'Unique Rows\nRecords: %d', width = %.2f, height = %.2f]
  C [label = 'Complete Rows\nRecords: %d', width = %.2f, height = %.2f]
  D [label = 'Clean Data \nRecords: %d', width = %.2f, height = %.2f]
  
  A -> B [label = '   duplicate rows removed']
  B -> C [label = '   rows missing pollutant or assesmentUnitIdentifier removed']
  C -> D [label = '   consolidate addressedParameters for one row per actionId/assessmentUnitIdentifier/pollutant']
}
", raw_data, 8, 4,
dups_removed, 8 * dups_removed/raw_data, 4 * dups_removed/raw_data,
misskey_removed, 8 * misskey_removed/raw_data, 4 * misskey_removed/raw_data,
final_clean, 8 * final_clean/raw_data, 4 * misskey_removed/raw_data))
