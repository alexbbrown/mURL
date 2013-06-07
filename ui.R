require(shiny)

urls <- c(
	war_and_peace = "http://www.gutenberg.org/files/2600/2600-h/2600-h.htm"
	)

shinyUI(pageWithSidebar(
	div(
		selectInput("url","url",choices=urls,multiple=T,selected=names(urls)[[1]]),
		tags$a(href="#", class="action-button shiny-bound-input", id="load_url", "Load URL")),
	div("sidebar"),
	div("body",
	  tableOutput("transferTable"),
	  plotOutput("hists",height=200)
	)
))