require(shiny)
require(RCurl)
require(plyr)
require(ggplot2)
require(devtools)
#install_github("httr","alexbbrown",ref="asynch")
require(httr)

nullNa <- function(x)lapply(x,function(x){
  if (length(x)==0) return(NA)
  if (length(x)>1) return(paste(x,collapse=", "))
  if (is.null(x)) return(NA)
  x
})

simpleStatus <- function(handle) {
	ldply(list(handle),function(x)as.data.frame(nullNa(getCurlInfo(x))))
}

print.reactivevalues=function(x)print(names(x))

new.fetcher <- function(url) {
	reactiveValues(
		url = url,
    deferred_httr = NULL,
		contentDownloaded = 0,
		content = NULL,
    uid = tempfile("","")
	)
}

shinyServer(function(input, output, session) {
	
	downloads <- list()
	
  # what outputs have we already created?
  output_names <- character(0)
  
	murl <- list(
		multiHandle = getCurlMultiHandle(),
		multiControl = reactiveValues(
				complete = FALSE, # should probably be 2 to start
				progressCount = 0, # just indicates progress
				startedCount = 0, # used to help decide to check completeness
        completedCount = 0
		),
		# Should have a fetcher for each URL in flight.
		fetchers = list(),
		completed = list(),
		
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

		# progressCount is used to inform any progress monitors and the urlWorker
		murl$multiControl$progressCount <<- isolate(murl$multiControl$progressCount) + 1
    # startedCount can be used to update reactives
		murl$multiControl$startedCount <- isolate(murl$multiControl$startedCount) + 1
		
		new_fetcher
	}
	
	# detect new url requests and dispatch
	newUrlObserver <- observe({
		# doesn't seem to be asynchronous?
		input$load_url # watch the action
		urls <- isolate(input$url) # listen to action button
				
		
		if(length(urls)==0||is.null(urls)||urls=="") return(NULL)
    lapply(urls,function(url) {
	  	cat(file=stderr(),"adding new URL",url,"\n")
  		#if(url %in% murl$fetcher$url) return(NULL)
    
  		new_download <- queue_download(url)
		
  		downloads <<- c(list(new_download),downloads)
    })
	})
	
	# URLworker is concerned with the continuing fetch work of the multi
	# curl handle.  It also tickles completed fetches so they can react.
	# It currently uses a timer, but could use select in a parallel universe.
	urlWorker <- observe({
	#	cat(file=stderr(),"first look [",isolate(murl$multiControl$progressCount),"]\n")
		
		murl$multiControl$complete # start trigger
		
		if (murl$multiControl$complete == TRUE||length(murl$fetchers)==0) return(NULL)
		# do a little more work.  Can we get it to do an intermediate amount of work?
	#	cat(file=stderr(),"doing some work [",isolate(murl$multiControl$progressCount),"]\n")
		
		status <- curlMultiPerform(murl$multiHandle, multiple = FALSE)

		if (status$numHandlesRemaining != murl$lastHandlesRemaining ||
			  murl$multiControl$startedCount != murl$lastStartedCount) {
				
			murl$fetchers <<- Filter(function(fetcher){
				complete <- with(simpleStatus(isolate(fetcher$deferred_httr$curl)),size.download==content.length.download)
			
				if (complete) {
          cat(file=stderr(),"completed download of a url\n")
 
					# decode content when complete.  this triggers the next step (consumer)
					fetcher$content <- content(as="text",isolate(fetcher$deferred_httr$response()))
					pop(murl$multiHandle, isolate(fetcher$deferred_httr$curl))
					murl$multiControl$completedCount <- isolate(murl$multiControl$completedCount) + 1
				
					murl$completed <<- c(murl$completed, list(fetcher)) # should convert to non-reactive here
				}
				return(!all(complete))
			},murl$fetchers)
		}
		
		murl$lastHandlesRemaining <<- status$numHandlesRemaining
		murl$lastStartedCount <<- isolate(murl$multiControl$startedCount)
		
		if (status$numHandlesRemaining > 0) {
			invalidateLater(1,session)
			# libcurl knows when curlhandles are complete, but that property is not exported
			# this code uses the contentlength to detect completeness
		} else {
			# all tasks are complete.  Trigger completeness tag
			# should be more selective - trigger each as they complete.			
			murl$multiControl$complete <<- TRUE
		}
		# progressCount is used to inform any progress monitors
		murl$multiControl$progressCount <<- isolate(murl$multiControl$progressCount)+1
	})
	
	# progress monitor - in the form of a progress table
	output$transferTable = renderUI({
		murl$multiControl$progressCount
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
	  if(murl$multiControl$completedCount == 0) return(div("Waiting..."))
    
    cat(file=stderr(),"updating number of outputs\n")
    
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
