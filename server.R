require(shiny)
require(httr)
require(RCurl)
require(plyr)

simpleStatus <- function(mhandle) {
	nullNa <- function(x)lapply(x,function(x)ifelse(is.null(x),NA,x))
	
	ldply(mhandle[[1]]@subhandles,function(x)as.data.frame(nullNa(getCurlInfo(x))))
}

shinyServer(function(input, output, session) {
	
	murl <- list(
		multiHandle = getCurlMultiHandle(),
		fetcher = reactiveValues(
			url = "",
			partialcontent = "",
			buffer = NULL,
			content = NULL,
			complete = TRUE,
			asyncRequestHandle = NULL,
			contentDownloaded = 0, # an object containing the actual handle
			downloadCount = 0 # just indicates progress
		)
	)
	
	# get a new URL and dispatch it
	newUrlObserver <- observe({
		# doesn't seem to be asynchronous?
		url <- isolate(input$url) # listen to action button
		input$load_url
		
		if(is.null(url)||url=="") return(NULL)
		if (url == isolate(murl$fetcher$url)) return(NULL)
		
		murl$fetcher$url <- url
		# suitable for text only :-)
		# single request (per invocation of newUrlObserver) only
		writefn <- function(x)1 #function(x) murl$partialcontent <- paste0(murl$partialcontent, x)
		writefn <- function(x)murl$fetcher$partialcontent <- paste0(murl$fetcher$partialcontent, x)
		
		buffer <- binaryBuffer()
		murl$fetcher$buffer <- buffer
    writefunction <- getNativeSymbolInfo("R_curl_write_binary_data")$address
    writedata <- buffer@ref
		
		
		murl$fetcher$asyncRequestHandle = 
		  getURLAsynchronous(url, 
		                     write = writefunction,
												 file=writedata,
                         multiHandle = murl$multiHandle, 
                         perform = 0,
 												 header=FALSE)

 		murl$fetcher$complete <- FALSE
	})
	
	# URLworker is a global object on the 
	urlWorker <- observe({
		if (is.null(murl$fetcher$url)) return(NULL)
		if (murl$fetcher$complete == TRUE) return(NULL)
		status <- curlMultiPerform(murl$multiHandle, multiple = FALSE)
		murl$fetcher$asyncRequestHandle <<- murl$fetcher$asyncRequestHandle
		
		if(status$numHandlesRemaining > 0) {
			invalidateLater(1,session)
			murl$fetcher$downloadCount <<- isolate(murl$fetcher$downloadCount)+1
			
		} else {
			
			if (completeness[1]) {
				completeness <- with(simpleStatus(murl$fetcher$asyncRequestHandle),size.download==content.length.download)
				# decode content (half-done - needs header)
				content <- content(httr:::response(url="foo",content=as(murl$fetcher$buffer, "raw")),as="text")
			} 
			
			murl$fetcher$complete <<- TRUE
			murl$fetcher$downloadCount <<- isolate(murl$fetcher$downloadCount)+1
		}
	})
	
	output$transferTable = renderTable({
		murl$fetcher$downloadCount
		if(is.null(murl$fetcher$asyncRequestHandle)) return(NULL)
		simpleStatus(murl$fetcher$asyncRequestHandle)
	})
})
