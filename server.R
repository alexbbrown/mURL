require(shiny)
require(httr)
require(RCurl)
require(plyr)

simpleStatus <- function(mhandle) {
	nullNa <- function(x)lapply(x,function(x)ifelse(is.null(x),NA,x))
	
	ldply(mhandle[[1]]@subhandles,function(x)as.data.frame(nullNa(getCurlInfo(x))))
}

shinyServer(function(input, output, session) {
	
	murl <- reactiveValues(
		url = "",
		multiHandle = getCurlMultiHandle(),
		partialcontent = NULL,
		content = NULL,
		complete = TRUE,
		multiRequestHandle = NULL,
		downloadCount = 0
	)
	
	# get a new URL and dispatch it
	newUrlObserver <- observe({
		# doesn't seem to be asynchronous?
		url <- isolate(input$url) # listen to action button
		input$load_url
		
		if(is.null(url)||url=="") return(NULL)
		if (url == isolate(murl$url)) return(NULL)
		
		murl$url <- url
		# suitable for text only :-)
		# single request (per invocation of newUrlObserver) only
		writefn = function(x)1 #function(x) murl$partialcontent <- paste0(murl$partialcontent, x)
		
		murl$multiRequestHandle = 
		  getURLAsynchronous(url, 
		                     write = writefn,
                         multiHandle = murl$multiHandle, 
                         perform = 0,
 												 header=FALSE)

 		murl$complete <- FALSE
	})
	
	urlWorker <- observe({
		if (is.null(murl$url)) return(NULL)
		if (murl$complete == TRUE) return(NULL)
		status <- curlMultiPerform(murl$multiHandle, multiple = FALSE)
		murl$multiRequestHandle <<- murl$multiRequestHandle
		if(status$numHandlesRemaining > 0) {
			invalidateLater(1,session)
			murl$downloadCount <<- isolate(murl$downloadCount)+1
			
		} else {
			murl$complete <<- TRUE
		}
	})
	
	output$transferTable = renderTable({
		murl$downloadCount
		if(is.null(murl$multiRequestHandle)) return(NULL)
		simpleStatus(murl$multiRequestHandle)
	})
})
