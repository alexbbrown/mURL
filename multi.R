# URLworker is concerned with the continuing fetch work of the multi
# curl handle.  It also tickles completed fetches so they can react.
# It currently uses a timer, but could use select in a parallel universe.
makeUrlWorker <- function(murl,session)observe({
#	cat(file=stderr(),"first look [",isolate(murl$progressCount),"]\n")
	
	murl$complete # start trigger
	
	if (murl$complete == TRUE||length(murl$fetchers)==0) return(NULL)
	# do a little more work.  Can we get it to do an intermediate amount of work?
#	cat(file=stderr(),"doing some work [",isolate(murl$progressCount),"]\n")
	
	status <- curlMultiPerform(murl$multiHandle, multiple = FALSE)

	if (status$numHandlesRemaining != isolate(murl$lastHandlesRemaining) ||
	  murl$startedCount != murl$lastStartedCount) {
			
		murl$fetchers <- Filter(function(fetcher){
			complete <- with(simpleStatus(isolate(fetcher$deferred_httr$curl)),size.download==content.length.download)
		
			if (complete) {
        cat(file=stderr(),"completed download of a url\n")

				# decode content when complete.  this triggers the next step (consumer)
				fetcher$content <- content(as="text",isolate(fetcher$deferred_httr$response()))
				pop(murl$multiHandle, isolate(fetcher$deferred_httr$curl))
				murl$completedCount <- isolate(murl$completedCount) + 1
			
				murl$completed <<- c(murl$completed, list(fetcher)) # should convert to non-reactive here
			}
			return(!all(complete))
		},murl$fetchers)
	}
	
	murl$lastHandlesRemaining <<- status$numHandlesRemaining
	murl$lastStartedCount <<- isolate(murl$startedCount)
	
	if (status$numHandlesRemaining > 0) {
		invalidateLater(1,session)
		# libcurl knows when curlhandles are complete, but that property is not exported
		# this code uses the contentlength to detect completeness
	} else {
		# all tasks are complete.  Trigger completeness tag
		# should be more selective - trigger each as they complete.			
		murl$complete <<- TRUE
	}
	# progressCount is used to inform any progress monitors
	murl$progressCount <<- isolate(murl$progressCount)+1
})

#' Create a new multi url controller for the shiny server
new_multi_controller <- function(session){
	murl <- reactiveValues(
		multiHandle = getCurlMultiHandle(),
		complete = FALSE, # should probably be 2 to start
		progressCount = 0, # just indicates progress
		startedCount = 0, # used to help decide to check completeness
	    completedCount = 0,
		fetchers = list(),
		completed = list(),
		downloads = list(),
		lastStartedCount = 0, # used to help decide to check completeness
		lastHandlesRemaining = 0 # used to help decide to check completeness
	)
	murl$urlWorker <- makeUrlWorker(murl,session)
	murl
}

#' Tell the multi controller to start a new url for download
#' @param url the url to download
#' @param murl the multi url controller
queue_download <- function(url, murl) {
	deferred_httr <- GET_deferred(url)
	new_fetcher <- new.fetcher(url)
	new_fetcher$deferred_httr <- deferred_httr

	push(murl$multiHandle, deferred_httr$curl)

	murl$fetchers <- c(isolate(murl$fetchers),list(new_fetcher))
	
	murl$downloads <- c(list(new_fetcher),isolate(murl$downloads))
	
	cat(file=stderr(),"count",length(isolate(murl$fetchers)),"\n")
	
	murl$complete <- FALSE

	# progressCount is used to inform any progress monitors and the urlWorker
	murl$progressCount <- isolate(murl$progressCount) + 1
  # startedCount can be used to update reactives
	murl$startedCount <- isolate(murl$startedCount) + 1
	
	new_fetcher
}

#' Create a new easy handle with data to allow it to signal completion
#' and extract content
#'
#' @param url the url to download
new.fetcher <- function(url) {
	reactiveValues(
		url = url,             # the original URL being downloaded
		uid = tempfile("",""), # unique ID for disambiguating and for naming outputs
    deferred_httr = NULL,  # the 'handle' for this asynchronous get.  contains status.
    complete = FALSE,			 # has the download finished
		content = NULL         # content.  set when complete, NULL otherwise
	)
}

nullNa <- function(x)lapply(x,function(x){
  if (length(x)==0) return(NA)
  if (length(x)>1) return(paste(x,collapse=", "))
  if (is.null(x)) return(NA)
  x
})

simpleStatus <- function(handle) {
	ldply(list(handle),function(x)as.data.frame(nullNa(getCurlInfo(x))))
}