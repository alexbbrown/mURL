#' Create a new multi url controller for the shiny server
new_multi_controller <- function()list(
	multiHandle = getCurlMultiHandle(),
	multiControl = reactiveValues(
			complete = FALSE, # should probably be 2 to start
			progressCount = 0, # just indicates progress
			startedCount = 0, # used to help decide to check completeness
      completedCount = 0,
			fetchers = list(),
			completed = list(),
			downloads = list()
	),
	# Should have a fetcher for each URL in flight.
	
	lastStartedCount = 0, # used to help decide to check completeness
	lastHandlesRemaining = 0 # used to help decide to check completeness
)

#' Tell the multi controller to start a new url for download
#' @param url the url to download
#' @param murl the multi url controller
queue_download <- function(url, murl) {
	deferred_httr <- GET_deferred(url)
	new_fetcher <- new.fetcher(url)
	new_fetcher$deferred_httr <- deferred_httr

	push(murl$multiHandle, deferred_httr$curl)

	murl$multiControl$fetchers <- c(isolate(murl$multiControl$fetchers),list(new_fetcher))
	
	murl$multiControl$downloads <- c(list(new_fetcher),isolate(murl$multiControl$downloads))
	
	cat(file=stderr(),"count",length(isolate(murl$multiControl$fetchers)),"\n")
	
	murl$multiControl$complete <- FALSE

	# progressCount is used to inform any progress monitors and the urlWorker
	murl$multiControl$progressCount <- isolate(murl$multiControl$progressCount) + 1
  # startedCount can be used to update reactives
	murl$multiControl$startedCount <- isolate(murl$multiControl$startedCount) + 1
	
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