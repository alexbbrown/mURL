mURL
====

mURL is an exploration of the use of cURL to enable asynchronous downloads in server.R

By Alex B Brown

RStudio's shiny can use used to build an interactive front-end to data which is fetched (by the server-side app) from other 'foreign' web-services.  Often this means the server has to download multiple documents from multiple foreign web-servers for multiple users, often in response to user choices.

The foreign web services it depends upon may not always be able to complete the transaction immediately, this can be due to : overloaded resources on the foreign server, available bandwidth in between, deliberate throttling, file-size, overhead of authentication, or in a not infrequent case, simple failure.

A naive use of the varied HTTP download functionality in R would tend to result in synchronous downloads, which would cause the shiny application to block (stop working) for the current user (and possibly all users) until the content fetch completed (or failed).

---

Shiny's reactive model points at a solution to this problem - instead of performing a synchronous download, initiate the request (in response to user input) and hand control back to shiny server.  A background function (or other mechanism) can monitor the state of the download.  Once the download is complete, a 'result' resultValue can be invalidated to indicate that an output function can process the downloaded data.

----

This example is a working example of asynchronous downloading.  It is not at all a clean design, since it serves simultaneously as an experiment, a demo, and a dream of more power to come.

The goal
========

The end goal (as yet unrealised) is a shiny extension which can deliver the asynchronous power that HTTP (and CURL) can offer.  

This would enable:
 * deferred downloads 
 * non-blocking downloads - shiny remains interactive during download
 * parallelised downloads - multiple connections download at same time 
 * progressive downloads - partially downloaded data may be useful (or pretty)
 * spidering or nested downloads - partial downloads may be mined on-the-fly for more links to download

These can help support a shiny application which itself communicates with web resources or 
web services which may have a need for multiple high latency low content requests.

Current state
=============

The current design is a result of a conflict between the (unsatisfied) needs of a final design and the simple test case of a single asynchronous download.  It's just a proof of concept.

It currently features:

 * Single asynchronous download
 * Progress monitor
 * Output based upon download completion

Design
======

The following proposed design is psuedo code and leaves out error handling, debouncing etc.

````
urlContent <- reactive(myurl = NULL)
urlMonitor <- mURL::newMonitor()

# a single input is bound to a single request
myURLqueuer <- observe({
	queue(urlMonitor, "myurl", GET(input$myurl))
})

# ... other inputs

# all live inputs are updated
commonURLMonitor <- observe({
	completedRequests <- workAndReturnCompleteness(urlMonitor)
	lapply(names(completedKeys), function(x)urlContent[[x]] = content(completedRequests[x]))
	invalidateLater(10)
})

# a single url is bound to a single URL
output$myplot <- renderPlot() {
	if(!is.null(urlContent$myurl))
	plot(read.csv(urlContent$myurl))
}

# ... other outputs
````

commonURLMonitor may actually be an internal property of urlMonitor, rather than being exposed to the developer.

Design questions
================

libcurl has a number of options for asynchronous downloads.  This is further complicated by the needs of rCurl to manage object liveness.  In summary:

Easy
----
'easy' synchronous rCurl (as used by httr today) uses one easy_handle, possibly repeatedly.  

The handle's HTTP/1.1 connection *may* be re-used which can help performance. (1) (2)

handles may be duped to start new connections.

The elegant Httr uses the Easy interface, although could trivially modify it to support multi by splitting the make_request function into a front and back end, and when GET(...,async=TRUE), returning the handle and a continuation for the decoder logic.  The handle can then be passed into the multi-interface.

Multi
-----
'multi' uses multiple easy_handles, usually duped from a template handle.  These are queued onto one multi-handle.  Optionally the user supply the required easy handles (see httr comment above)

The advantage of multi is that the multi handle can be invoked once to 'perform' partial or complete downloads on all the easy handles.  This is likely to result in reduced complexity and runtime cost in R applications.

multi can be used to implement single asynchronous connections, or even single synchronous ones.

Since multiple handles are used, HTTP/1.1 connections are not re-used by default, although it would be possible to implement a pool.

It's perfectly possible to have multiple multi handles, so there is a design decision between using the multi property purely for single asynchrony, and using a global multi handle to queue all requests on.

getURLAsynchronous
------------------

getURLAsynchronous is RCurl's interface onto Curl's multi-mode.  It allows multiple urls to be dispatched in a single call. but is limited in that it can only apply the same curl options to all of them.  

to add more urls to a multi connection later on (in response to new user input, or data discovered on download), additional calls to getURLAsynchronous can be made on the same multi handle.  These additional calls can have different curl options.

RCurl's interface complicate understanding of the relationship between multis and handles because it also has a role in object liveness tracking.  On each call it returns the multi-handle, plus the list of the new easy handles it has created to handle the requests, but does not include easy handles that were already attached to the multi.  Use with caution.

This leads to an design decision between 

(1) This may be particularly important if the user and server must mutually authenticate for each connection, as happens in one web API that the author uses.
(2) It may be perfectly possible to enable deferred and asynchronous downloads using the easy interface.  The Multi interface has the advantage that it can update all managed handles (for that multi) in a single call to libcurl. 

libCurl
-------

Exposing more of libCurl via RCurl may improve performance - for instance by exposing `curl_multi_info_read` completed connnections can be more easily detected.  Exposing curl_multi_fdset may allow the shiny runlook to attach selects to the fdset - allowing reactives to wake up only when there is real new work to be done (for the moment its done by polling).

Comments on speed
=================

Unexplored things: currently a single asynchronous download is about 50% slower than a single synchronous one.  This may be improved by better choice of buffer size, use of selects etc in the future.  This slow performance may not be a problem if it allows better parallelisation and responsiveness in the shiny application. 


References
==========

 * http://cran.r-project.org/web/packages/RCurl/RCurl.pdf - the RCurl manual
 * http://curl.haxx.se/libcurl/c/ - the libCurl API
 * http://www.omegahat.org/RCurl/RCurlJSS.pdf - author's lengthy article on RCurl with good coverage, examples and analysis of multi.
 * http://www.omegahat.org/RCurl/xmlParse.html - Partial example of nested downloads using multi.  Helpful background material but not self-sufficient
 * http://cran.r-project.org/web/packages/httr/ - Hadley Wickham's httr
 * http://www.rstudio.com/shiny/ - Rstudio's shiny