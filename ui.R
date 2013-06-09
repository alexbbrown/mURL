require(shiny)

urls <- c(
	`War and peace` = "http://www.gutenberg.org/cache/epub/2600/pg2600.txt",
	`Huckleberry finn` = "http://www.gutenberg.org/cache/epub/76/pg76.txt",
	`Beowulf` = "http://www.gutenberg.org/cache/epub/16328/pg16328.txt",
	`Ulysses` = "http://www.gutenberg.org/cache/epub/4300/pg4300.txt",
	`Grimm's Fairy Tales` = "http://www.gutenberg.org/cache/epub/2591/pg2591.txt"
	)

shinyUI(pageWithSidebar(
	headerPanel("demo of mURL - asynchronous URL fetching for responsive shiny"),
	
	sidebarPanel(selectInput("url","url",choices=urls,multiple=T,selected=names(urls)[[1]]),
			tags$a(href="#", class="action-button shiny-bound-input", id="load_url", "Load URL"),
			div(class="alert","warning: gutenberg will forbid you pretty quickly"),
			uiOutput("transferTable")
		),
	mainPanel(
		htmlOutput("results")
	)
))