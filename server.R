require(shiny)
require(httr)
require(RCurl)
require(plyr)
require(ggplot2)

simpleStatus <- function(mhandle) {
	nullNa <- function(x)lapply(x,function(x)ifelse(is.null(x),NA,x))
	
	ldply(mhandle[[1]]@subhandles,function(x)as.data.frame(nullNa(getCurlInfo(x))))
}

print.reactivevalues=function(x)print(names(x))

new.fetcher <- function(url) {
	reactiveValues(
		complete = FALSE,
		url = url,
		buffer = NULL,
		content = NULL,
		header = NULL,
		asyncRequestHandle = NULL,
		contentDownloaded = 0 # an object containing the actual handle
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

		murl$fetcher$url <- url
		# suitable for text only :-)
		# single request (per invocation of newUrlObserver) only
		
		buffer <- binaryBuffer()
		murl$fetcher$buffer <- buffer
    writefunction <- getNativeSymbolInfo("R_curl_write_binary_data")$address
    writedata <- buffer@ref
		headerfunction <- (murl$fetcher$header<-basicHeaderGatherer())$update
		
		murl$fetcher$asyncRequestHandle = 
		  getURLAsynchronous(url, 
		                     write = writefunction,
												 file=writedata,
                         multiHandle = murl$multiHandle, 
                         perform = FALSE,
 												 headerFunction=headerfunction
		  )

 		murl$multiControl$complete <- FALSE
	})
	
	# URLworker is concerned with the continuing fetch work of the multi
	# curl handle.  It also tickles completed fetches so they can react.
	# It currently uses a timer, but could use select in a parallel universe.
	urlWorker <- observe({
		if (is.null(murl$fetcher$url)||murl$multiControl$complete == TRUE) return(NULL)
		# do a little more work.  Can we get it to do an intermediate amount of work?
		status <- curlMultiPerform(murl$multiHandle, multiple = FALSE)
		murl$fetcher$asyncRequestHandle <<- murl$fetcher$asyncRequestHandle
		
		if(status$numHandlesRemaining > 0) {
			invalidateLater(1,session)
			# libcurl knows when curlhandles are complete, but that property is not exported
			# this code uses the contentlength to detect completeness
		} else {
			# all tasks are complete.  Trigger completeness tag
			# should be more selective - trigger each as they complete.
			completeness <- with(simpleStatus(murl$fetcher$asyncRequestHandle),size.download==content.length.download)
			
			if (completeness[1]) {
				# decode content (half-done - needs header)
				murl$fetcher$content <- content(httr:::response(url="foo",content=as(murl$fetcher$buffer, "raw")),as="text")
			} 
			
			murl$multiControl$complete <<- TRUE
		}
		# downloadcount is used to inform any progress monitors
		murl$multiControl$downloadCount <<- isolate(murl$multiControl$downloadCount)+1
	})
	
	# progress monitor - in the form of a progress table
	output$transferTable = renderTable({
		murl$multiControl$downloadCount
		if(is.null(murl$fetcher$asyncRequestHandle)) return(NULL)
		summarize(simpleStatus(murl$fetcher$asyncRequestHandle),
		  url=effective.url,
		  res=as.character(response.code),
		  time=total.time,
		  percent=size.download/content.length.download
		)
	})
	
	output$hists = renderPlot({
		# just one for now
		if (FALSE==murl$multiControl$complete) return(NULL)

		wordlengths<-function(x)ldply(table(attr(gregexpr("[A-Za-z]+",x)[[1]],"match.length")))
    print(qplot(data=wordlengths(murl$fetcher$content),x=as.numeric(as.character(.id)),y=V1,geom="bar",stat="identity"))
	})
})
