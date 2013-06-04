require(shiny)
require(RCurl)
require(plyr)
require(ggplot2)
require(devtools)
install_github("httr","alexbbrown",ref="asynch")

nullNa <- function(x)lapply(x,function(x)ifelse(is.null(x),NA,x))

simpleStatus <- function(handle) {
	ldply(list(handle),function(x)as.data.frame(nullNa(getCurlInfo(x))))
}

print.reactivevalues=function(x)print(names(x))

new.fetcher <- function(url) {
	reactiveValues(
		url = url,
    deferred_httr = NULL,
		contentDownloaded = 0,
		content = NULL
	)
}

shinyServer(function(input, output, session) {
	
	murl <- list(
		multiHandle = getCurlMultiHandle(),
		multiControl = reactiveValues(
				complete = FALSE, # should probably be 2 to start
				downloadCount = 0 # just indicates progress
		),
		# Should have a fetcher for each URL in flight.
		fetcher = new.fetcher(NULL)
	)
	
	# detect new url requests and dispatch
	newUrlObserver <- observe({
		# doesn't seem to be asynchronous?
		url <- isolate(input$url) # listen to action button
		input$load_url
		if(is.null(url)||url=="") return(NULL)
		if(url %in% murl$fetcher$url) return(NULL)

		deferred_httr <- GET_deferred(url)
		
		murl$fetcher$url <- url
		murl$fetcher$deferred_httr <- deferred_httr

		push(murl$multiHandle, deferred_httr$curl)

 		murl$multiControl$complete <- FALSE
	})
	
	# URLworker is concerned with the continuing fetch work of the multi
	# curl handle.  It also tickles completed fetches so they can react.
	# It currently uses a timer, but could use select in a parallel universe.
	urlWorker <- observe({
		if (is.null(murl$fetcher$url)||murl$multiControl$complete == TRUE) return(NULL)
		# do a little more work.  Can we get it to do an intermediate amount of work?
		status <- curlMultiPerform(murl$multiHandle, multiple = FALSE)
		
		if(status$numHandlesRemaining > 0) {
			invalidateLater(1,session)
			# libcurl knows when curlhandles are complete, but that property is not exported
			# this code uses the contentlength to detect completeness
		} else {
			# all tasks are complete.  Trigger completeness tag
			# should be more selective - trigger each as they complete.
			completeness <- with(simpleStatus(murl$fetcher$deferred_httr$curl),size.download==content.length.download)
			
			if (completeness[1]) {
				# decode content (half-done - needs header)
				murl$fetcher$content <- content(as="text",murl$fetcher$deferred_httr$response())					
			} 
			
			murl$multiControl$complete <<- TRUE
		}
		# downloadcount is used to inform any progress monitors
		murl$multiControl$downloadCount <<- isolate(murl$multiControl$downloadCount)+1
	})
	
	# progress monitor - in the form of a progress table
	output$transferTable = renderTable({
		murl$multiControl$downloadCount
		if(is.null(murl$fetcher$deferred_httr)) return(NULL)
		summarize(simpleStatus(murl$fetcher$deferred_httr$curl),
		  url=effective.url,
		  res=as.character(response.code),
		  time=total.time,
		  percent=100*size.download/content.length.download
		)
	})
	
	output$hists = renderPlot({
		# just one for now
		if (FALSE==murl$multiControl$complete) return(NULL)

		wordlengths<-function(x)ldply(table(attr(gregexpr("[A-Za-z]+",x)[[1]],"match.length")))
    print(qplot(data=wordlengths(murl$fetcher$content),x=as.numeric(as.character(.id)),y=V1,geom="bar",stat="identity"))
	})
})
