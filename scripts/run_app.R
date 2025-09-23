# AlloMate Shiny App Runner
# This script runs the AlloMate Shiny application

# Load required library
library(shiny)

# Run the Shiny app
# The app directory contains global.R, ui.R, and server.R
shiny::runApp(
  appDir = "app",
  host = "127.0.0.1",
  port = 3838,
  launch.browser = TRUE
)
