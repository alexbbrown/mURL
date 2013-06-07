require(shiny)
require(RCurl)
require(plyr)
require(ggplot2)
require(devtools)
#install_github("httr","alexbbrown",ref="asynch")
require(httr)

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
		fetchers = list()
	)
	
	# detect new url requests and dispatch
	newUrlObserver <- observe({
		# doesn't seem to be asynchronous?
		url <- isolate(input$url) # listen to action button
		
		print(file=stderr(),input$load_url)
		
		cat(file=stderr(),"adding new URL",url,"\n")
		
		# let's 
		
		if(is.null(url)||url=="") return(NULL)
		if(url %in% murl$fetcher$url) return(NULL)

		deferred_httr <- GET_deferred(url)
		new_fetcher <- new.fetcher(url)
		new_fetcher$deferred_httr <- deferred_httr

		push(murl$multiHandle, deferred_httr$curl)

		murl$fetchers <<- c(murl$fetchers,list(new_fetcher))

 		murl$multiControl$complete <- FALSE

		# downloadcount is used to inform any progress monitors and the urlWorker
		murl$multiControl$downloadCount <<- isolate(murl$multiControl$downloadCount)+1

	})
	
	# URLworker is concerned with the continuing fetch work of the multi
	# curl handle.  It also tickles completed fetches so they can react.
	# It currently uses a timer, but could use select in a parallel universe.
	urlWorker <- observe({
		cat(file=stderr(),"first look [",isolate(murl$multiControl$downloadCount),"]\n")
		
		murl$multiControl$complete
		
		if (length(murl$fetchers)==0||murl$multiControl$complete == TRUE) return(NULL)
		# do a little more work.  Can we get it to do an intermediate amount of work?
		cat(file=stderr(),"doing some work [",isolate(murl$multiControl$downloadCount),"]\n")
		
		status <- curlMultiPerform(murl$multiHandle, multiple = FALSE)
		
		if(status$numHandlesRemaining > 0) {
			invalidateLater(1,session)
			# libcurl knows when curlhandles are complete, but that property is not exported
			# this code uses the contentlength to detect completeness
		} else {
			# all tasks are complete.  Trigger completeness tag
			# should be more selective - trigger each as they complete.
			
			firstFetcher <- murl$fetchers[[1]]
			
			completeness <- with(simpleStatus(firstFetcher$deferred_httr$curl),size.download==content.length.download)
			
			if (completeness[1]) {
				# decode content (half-done - needs header)
				firstFetcher$content <- content(as="text",firstFetcher$deferred_httr$response())					
			} 
			
			murl$multiControl$complete <<- TRUE
		}
		# downloadcount is used to inform any progress monitors
		murl$multiControl$downloadCount <<- isolate(murl$multiControl$downloadCount)+1
	})
	
	# progress monitor - in the form of a progress table
	output$transferTable = renderTable({
		murl$multiControl$downloadCount
		if (length(murl$fetcher)==0) return(data.frame(a="nothing"))
		
		ldply(murl$fetchers,function(x) {
			summarize(simpleStatus(x$deferred_httr$curl),
			  url=effective.url,
			  res=as.character(response.code),
			  time=total.time,
			  percent=100*size.download/content.length.download
			)
		}
		)
	})
	
	output$hists = renderPlot({
		# just one for now
		if (FALSE==murl$multiControl$complete) return(NULL)
		firstFetcher <- murl$fetchers[[1]]
		wordlengths<-function(x)ldply(table(attr(gregexpr("[A-Za-z]+",x)[[1]],"match.length")))
    print(qplot(data=wordlengths(firstFetcher$content),x=as.numeric(as.character(.id)),y=V1,geom="bar",stat="identity"))
	})
})
