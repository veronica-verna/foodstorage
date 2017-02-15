shinyApp(
  ui = navbarPage("United",
                  theme = shinytheme("united"),
                  tabPanel("Plot", "Plot tab contents..."),
                  navbarMenu("More",
                             tabPanel("Summary", "Summary tab contents..."),
                             tabPanel("Table", "Table tab contents...")
                  )
  ),
  server = function(input, output) { }
)