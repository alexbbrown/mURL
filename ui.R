require(shiny)

urls <- list(
	war_and_peace <- "http://www.gutenberg.org/files/2600/2600-h/2600-h.htm"
	)

shinyUI(pageWithSidebar(
	div(
		textInput("url","url",value=urls[[1]]),
		actionButton("load_url", "Load URL")),
	div("sidebar"),
	div("body",
	  tableOutput("transferTable")
	)
))