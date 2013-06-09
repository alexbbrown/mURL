require(shiny)
require(RCurl)
require(plyr)
require(ggplot2)
require(devtools)
dev_mode()
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
	
	downloads <- list()
	
	murl <- list(
		multiHandle = getCurlMultiHandle(),
		multiControl = reactiveValues(
				complete = FALSE, # should probably be 2 to start
				downloadCount = 0 # just indicates progress
		),
		# Should have a fetcher for each URL in flight.
		fetchers = list(),
		completed = list(),
		
		startedCount = 0, # used to help decide to check completeness
		lastStartedCount = 0, # used to help decide to check completeness
		lastHandlesRemaining = 0 # used to help decide to check completeness
	)
	
	queue_download <- function(url) {
		deferred_httr <- GET_deferred(url)
		new_fetcher <- new.fetcher(url)
		new_fetcher$deferred_httr <- deferred_httr

		push(murl$multiHandle, deferred_httr$curl)

		murl$fetchers <<- c(murl$fetchers,list(new_fetcher))
		
		cat(file=stderr(),"count",length(murl$fetchers),"\n")
		
 		murl$multiControl$complete <- FALSE

		# downloadcount is used to inform any progress monitors and the urlWorker
		murl$multiControl$downloadCount <<- isolate(murl$multiControl$downloadCount)+1
		murl$startedCount <- murl$startedCount + 1;
		
		new_fetcher
	}
	
	# detect new url requests and dispatch
	newUrlObserver <- observe({
		# doesn't seem to be asynchronous?
		input$load_url # watch the action
		url <- isolate(input$url) # listen to action button
				
		cat(file=stderr(),"adding new URL",url,"\n")
		
		if(is.null(url)||url=="") return(NULL)
		if(url %in% murl$fetcher$url) return(NULL)

		new_download <- queue_download(url)
		
		downloads <<- c(list(new_download),downloads)
	})
	
	# URLworker is concerned with the continuing fetch work of the multi
	# curl handle.  It also tickles completed fetches so they can react.
	# It currently uses a timer, but could use select in a parallel universe.
	urlWorker <- observe({
	#	cat(file=stderr(),"first look [",isolate(murl$multiControl$downloadCount),"]\n")
		
		murl$multiControl$complete
		
		if (length(murl$fetchers)==0||murl$multiControl$complete == TRUE) return(NULL)
		# do a little more work.  Can we get it to do an intermediate amount of work?
	#	cat(file=stderr(),"doing some work [",isolate(murl$multiControl$downloadCount),"]\n")
		
		status <- curlMultiPerform(murl$multiHandle, multiple = FALSE)

		if (status$numHandlesRemaining != murl$lastHandlesRemaining ||
			  murl$startedCount != murl$lastStartedCount) {
				
			murl$fetchers <<- Filter(function(fetcher){
				complete <- with(simpleStatus(isolate(fetcher$deferred_httr$curl)),size.download==content.length.download)
			
				if (complete) {
					# decode content when complete.  this triggers the next step (consumer)
					fetcher$content <- content(as="text",isolate(fetcher$deferred_httr$response()))
					pop(murl$multiHandle, isolate(fetcher$deferred_httr$curl))
					murl$completed <- c(murl$completed, list(fetcher)) # should convert to non-reactive here
				}
				return(!all(complete))
			},murl$fetchers)
		}
		
		murl$lastHandlesRemaining <<- status$numHandlesRemaining
		murl$lastStartedCount <<- murl$startedCount
		
		if (status$numHandlesRemaining > 0) {
			invalidateLater(1,session)
			# libcurl knows when curlhandles are complete, but that property is not exported
			# this code uses the contentlength to detect completeness
		} else {
			# all tasks are complete.  Trigger completeness tag
			# should be more selective - trigger each as they complete.			
			murl$multiControl$complete <<- TRUE
		}
		# downloadcount is used to inform any progress monitors
		murl$multiControl$downloadCount <<- isolate(murl$multiControl$downloadCount)+1
	})
	
	# progress monitor - in the form of a progress table
	output$transferTable = renderUI({
		murl$multiControl$downloadCount
		if (length(murl$fetchers)==0) return(div("all downloads complete"))
		
		z<-div(div(paste(length(murl$fetchers),"concurrent downloads")),
			do.call(div,
		llply(murl$fetchers,function(x) {
			status <- simpleStatus(x$deferred_httr$curl)
			s<-summarize(status,
			  url=effective.url,
			  res=as.character(response.code),
			  time=round(total.time),
			  percent=round(100*size.download/content.length.download)
			)
			div(width="100%",#style="background-color:gray",
					div(style=paste0("width:",with(status,round(100*size.download/content.length.download)),"%;","background-color:teal;"),
					status$effective.url))
		}
		)))
		
		z
	})
	
	output$results = renderUI({
	  plotOutput("hists",height=200)
	})
	
	output$hists = renderPlot({
		# just one for now
		print(file=stderr(),"Trying to draw\n")
		if (FALSE==murl$multiControl$complete) return(NULL)
		print(file=stderr(),"Drawing\n")
		firstFetcher <- downloads[[1]]
		wordlengths<-function(x)ldply(table(attr(gregexpr("[A-Za-z]+",x)[[1]],"match.length")))
    print(qplot(data=wordlengths(firstFetcher$content),x=as.numeric(as.character(.id)),y=V1,geom="bar",stat="identity"))
	})
})
