shinyServer(function(input, output, session) {
  output$uiHTML <- renderUI({
    apps <- c("NationalTest5", "passwdShinyApp", "RColor", "sykehus", "userFootprint", "whoVisitMe")
    Link <- paste("http://188.166.116.72:3838/", apps[sample(1:length(apps), 1)], sep = "")
    Iframe <- '<iframe style="width:100%; height:100vh; border: none;" src="Link"></iframe>'
    HTML(gsub("Link", Link, Iframe))
  })
})
