require(shiny)
require(RCurl)
require(plyr)
require(ggplot2)
require(devtools)
#install_github("httr","alexbbrown",ref="asynch")
require(httr)

source("multi.R")

print.reactivevalues=function(x)print(names(x))

shinyServer(function(input, output, session) {
  
	murl <- new_multi_controller(session);
	
	# detect new url requests and dispatch
	newUrlObserver <- observe({
		
		input$load_url # listen to action button
		urls <- isolate(input$url) 	# get list to download. doesn't check for already downloaded
		
		if(length(urls)==0||is.null(urls)||urls=="") return(NULL)
    lapply(urls,function(url) {
	  	cat(file=stderr(),"adding new URL",url,"\n")
    
  		new_download <- queue_download(url,murl)
    })
	})
	
	# multi progress monitor - in the form of a progress table
	# not required but interesting.
	output$transferTable = renderUI({
		murl$progressCount
		if (length(murl$fetchers)==0) return(div("all downloads complete"))
		
		div(
			div(paste(length(murl$fetchers),"concurrent downloads")),
			do.call(div,
				llply(murl$fetchers,function(x) {
					status <- simpleStatus(x$deferred_httr$curl)
					s<-summarize(status,
					  url=effective.url,
					  res=as.character(response.code),
					  time=round(total.time),
					  percent=round(100*size.download/content.length.download)
					)
					div(width="100%",height="1em",
							div(style=paste0("width:",with(status,round(100*size.download/content.length.download)),"%;","background-color:teal;overflow:hidden;heignt:1em"),
							status$effective.url))
				})
			)
		)
	})
	
	 # what outputs have we already created?
  output_names <- character(0)

	output$results = renderUI({    
    cat(file=stderr(),"generating an output for each download\n")
    
    do.call(div,lapply(murl$completed,function(fetcher) {
      if (!fetcher$uid %in% output_names) {
        output[[fetcher$uid]] <- renderPlot({
          print(file=stderr(),"Trying to draw\n")
          firstFetcher <- fetcher
          output_names <<- c(output_names,fetcher$uid)
          wordlengths<-function(x)ldply(table(attr(gregexpr("[A-Za-z]+",x)[[1]],"match.length")))
          print(qplot(data=wordlengths(firstFetcher$content),x=as.numeric(as.character(.id)),y=V1,geom="bar",stat="identity")+labs(title=sub(".*/","",fetcher$url),y="count",x="word length"))
        }) 
      }   
      div(id=fetcher$uid,class="shiny-plot-output",style="display:inline-block;width:150px;height:150px")

    }))
	})
	
})
