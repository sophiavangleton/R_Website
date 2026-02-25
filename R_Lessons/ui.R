#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(
  
  titlePanel("Adaptive R Lessons Quiz"),
  
  uiOutput("question_ui"),
  uiOutput("feedback_ui"),
  uiOutput("results_ui")
)
