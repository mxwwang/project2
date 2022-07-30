

function(input, output) {
  
  # Page 2
  output$cat_plots <- renderPlot(
    plots_by_category(input$category)
  )
  
  
  # Page 3
  output$boro_plots <- renderPlot(
    boro_plot(input$boro,input$stat)
  )
  
  
  output$corr_matrix <- renderPlot(
    corr_matrix_plot(input$boro, input$sdate)
  )
 
  
  # Page 4
  output$covid_comp <- renderPlot(
    covid_comp_plot(input$boro1,input$boro2, input$cstat)
  )
  
  output$boro_comp <- renderPlot(
    boro_comp_plot(input$boro1,input$boro2, input$cat)
  )

  }