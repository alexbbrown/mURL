require(shiny)

shinyUI(pageWithSidebar(
	div(textInput("url","url"),actionButton("load_url", "Load URL")),
	div("sidebar"),
	div("body",
	  tableOutput("transferTable")
	)
))