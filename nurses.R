
# DES.R:  R routines for discrete-event simulation (DES), with an example

# matrix version; data frame allows character event types, but much too slow

# all data is stored in an R environment variable that will be referrred
# to as simlist below

# the simlist will consist of the following components:
#
#       currtime:  current simulated time
#       evnts:  the events list, a matrix
#       reactevent:  event handler, user-supplied; creates new
#                    events upon the occurrence of an old one;
#                    e.g. job arrival triggers either start of 
#                    service for the job or queuing it; call form is
#                    reactevent(evnt,simlist)
#       dbg:  if TRUE, will print evnts above after each event
#             scheduling action, and enter R browser for single-stepping
#             etc.

# the application code can add further application-specific data to
# simlist, e.g. total job queuing time 

# each event will be represented by a matrix row consisting of: 
# 
#    occurrence time
#    event type (user-defined numeric code)
#
# and application-specific information, if any

# library functions (do not alter):
# 
#       newsim:  create a new simlist
#       insevnt:  insert a new event into evnts in the simlist
#       schedevnt:  schedule a new event (determine its occurrence time
#                   and call insevnt())
#       getnextevnt:  pulls the earliest event from the event list,
#                     process it, and update the current simulated
#                     time
#       mainloop:  as the name implies
#       appendtofcfsqueue:  append job to a FCFS queue
#       delfcfsqueue:  delete head of a FCFS queue

# outline of a typical application:

#    mysim <- newsim()    create the simlist
#    set reactevent in mysim
#    set application-specific variables in mysim, if any
#    set the first event in mysim$evnts
#    mainloop(mysim,mysimtimelim)
#    print results

# create a simlist, which will be the return value, an R environment
newsim <- function(dbg=F) {
  simlist <- new.env()
  simlist$currtime <- 0.0  # current simulated time
  simlist$evnts <- NULL  # event list
  simlist$dbg <- dbg
  simlist
}

# insert event evnt into evnts in simlist
insevnt <- function(evnt,simlist) {
  # if the event list is empty, set it to consist of evnt and return
  if (is.null(simlist$evnts)) {
    simlist$evnts <- matrix(evnt,nrow=1)
    return()
  }
  # otherwise, find insertion point
  inspt <- binsearch(simlist$evnts[,1],evnt[1])
  # now "insert," by reconstructing the matrix; we find what portion of
  # the current matrix should come before evnt and what portion should 
  # come after it, then string everything together
  before <- if (inspt == 1) NULL else simlist$evnts[1:(inspt-1),]
  nr <- nrow(simlist$evnts)
  after <- if (inspt <= nr) simlist$evnts[inspt:nr,] else NULL  
  simlist$evnts <- rbind(before,evnt,after)  
  rownames(simlist$evnts) <- NULL
}

# schedule new event in evnts in simlist; evnttime is the time at
# which the event is to occur; evnttype is the event type; appdata is
# a vector of numerical application-specific data
schedevnt <- function(evnttime,evnttype,simlist,appdata=NULL) {
  evnt <- c(evnttime,evnttype,appdata)
  insevnt(evnt,simlist)  
}

# start to process next event (second half done by application
# programmer via call to reactevnt() from mainloop())
getnextevnt <- function(simlist) {
  head <- simlist$evnts[1,]
  # delete head
  if (nrow(simlist$evnts) == 1) simlist$evnts <- NULL else 
    simlist$evnts <- simlist$evnts[-1,,drop=F]  
  return(head)
}

# main loop of the simulation
mainloop <- function(simlist,simtimelim) {
  while(simlist$currtime < simtimelim) {
    head <- getnextevnt(simlist)  
    # update current simulated time
    simlist$currtime <- head[1]  
    # process this event (programmer-supplied ftn)
    simlist$reactevent(head,simlist)  
    if (simlist$dbg) {
      print("event occurred:")
      print(head)
      print("events list now")
      print(simlist$evnts)
      browser()
    }
  }
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; this could be replaced
# by faster C code
binsearch <- function(x,y) {
  n <- length(x)
  lo <- 1
  hi <- n
  while(lo+1 < hi) {
    mid <- floor((lo+hi)/2)
    if (y == x[mid]) return(mid)
    if (y < x[mid]) hi <- mid else lo <- mid
  }
  if (y <= x[lo]) return(lo)
  if (y < x[hi]) return(hi)
  return(hi+1)
}

# appendtofcfsqueue() and delfcfsqueuehead() below assume the
# application code has one or more queues, each queue stored as a
# list-of-lists, with each individual list being the information for one
# queued job; note that one must initialize the list-of-lists as NULL 

# appends jobtoqueue to the given queue, assumed of the above form;
# the new, longer list is returned
appendtofcfsqueue <- function(queue,jobtoqueue) {
  lng <- length(queue)
  queue[[lng+1]] <- jobtoqueue
  queue
}

# deletes head of queue; assumes list-of-lists structure as decribed
# above; returns the head and new queue
delfcfsqueuehead <- function(queue) {
  qhead <- queue[[1]]
  newqueue <- queue[-1]
  # careful!--an empty list is not NULL  
  if (length(queue) == 1) newqueue <- NULL
  list(qhead=qhead,newqueue=newqueue)
}

# test; M/M/1 queue--exponential ("Markov" job interarrivals,
# exponential service times, 1 server
mm1 <- function(meaninterarrv,meansrv,timelim,dbg=F) {
  simlist <- newsim(dbg)
  simlist$reactevent <- mm1react  # defined below
  simlist$arrvrate <- 1 / meaninterarrv
  simlist$srvrate <- 1 / meansrv
  simlist$totjobs <- 0
  simlist$totwait <- 0.0
  simlist$queue <- NULL
  simlist$srvrbusy <- F
  # defining job numbers is good practice, always invaluable during
  # debugging
  simlist$jobnum <- 0
  # event type codes: 1 for arrival, 2 for service completion;
  # set up first event, including info on this job's arrival time for
  # later use in finding mean wait until job done
  timeto1starrival <- rexp(1,simlist$arrvrate)
  jobnum <- simlist$jobnum + 1
  simlist$jobnum <- jobnum
  schedevnt(timeto1starrival,1,simlist,c(timeto1starrival,jobnum))
  mainloop(simlist,timelim)
  # should print out 1 / (srvrate - arrvrate)
  cat("mean wait:  ")
  print(simlist$totwait / simlist$totjobs)
}

# what new events are triggered by the occurrence of an old one?
mm1react <- function(evnt,simlist) {
  etype <- evnt[2]
  if (etype == 1) {  # job arrival
    # schedule next arrival
    timeofnextarrival <- simlist$currtime + rexp(1,simlist$arrvrate)
    jobnum <- simlist$jobnum + 1
    simlist$jobnum <- jobnum
    schedevnt(timeofnextarrival,1,simlist,c(timeofnextarrival,jobnum))
    # start newly-arrived job or queue it
    if (!simlist$srvrbusy) {  # start job service
      simlist$srvrbusy <- T
      srvduration <- rexp(1,simlist$srvrate)
      schedevnt(simlist$currtime+srvduration,2,simlist,evnt[3:4])
    } else {  # add to queue
      simlist$queue <- appendtofcfsqueue(simlist$queue,evnt)
    }
  } else if (etype == 2) {  # job completion
    # bookkeeping
    simlist$totjobs <- simlist$totjobs + 1
    simlist$totwait <- simlist$totwait + simlist$currtime - evnt[3]
    simlist$srvrbusy <- F
    # check queue for waiting jobs
    if (!is.null(simlist$queue)) {
      tmp <- delfcfsqueuehead(simlist$queue)
      job <- tmp$qhead
      simlist$queue <- tmp$newqueue
      # start job service
      simlist$srvrbusy <- T
      srvduration <- rexp(1,simlist$srvrate)
      schedevnt(simlist$currtime+srvduration,2,simlist,job[3:4])
    }
  } 
}

nurses <- function(n,q,r,d,p,timelim,dbg=F) {
  simlist <- newsim(dbg)
  simlist$reactevent <- nursesreact  # defined below
  simlist$n <- n
  simlist$q <- q
  simlist$r <- r
  simlist$d <- d
  simlist$p <- p
  simlist$totjobs <- 0
  simlist$totwait <- 0.0
  simlist$ncalls <- 0
  simlist$nurses <- 1
  simlist$statedur <- matrix(rep(0, n*(q+n+1)),nrow = n, ncol = (q+n+1),byrow = TRUE)
  simlist$lasttime <- 0
  simlist$queue <- NULL
  simlist$srvrbusy <- F
  # defining job numbers is good practice, always invaluable during
  # debugging
  simlist$jobnum <- 0
  # event type codes: 1 for arrival, 2 for service completion;
  # set up first event, including info on this job's arrival time for
  # later use in finding mean wait until job done
  timeto1starrival <- runif(1,0,(2*simlist$r))
  jobnum <- simlist$jobnum + 1
  simlist$jobnum <- jobnum
  schedevnt(timeto1starrival,1,simlist,c(timeto1starrival,jobnum))
  mainloop(simlist,timelim)
  # should print out 1 / (srvrate - arrvrate)
  cat("mean wait:  ")
  print(simlist$totwait / simlist$totjobs)
  #cat("last event occurs at:  ")
  #print(simlist$lasttime)
  Pi <- simlist$statedur / simlist$lasttime
  cat("Pi:  ")  
  print(Pi)
  
  rejectratio = 0
  for (index in 1:simlist$n)
  {
    rejectratio = rejectratio + Pi[index,index + simlist$q + 1]
  }
  cat("reject call ratio:  ")
  print(rejectratio)
  cat("idle time ratio:  ")
  idletime <- 0
  nursetotaltime <- 0
  idleratio <- 0
  for (i in 1:simlist$nurses)
  {
    for (j in 1:i)
    {
      idletime <- idletime + (i+1-j)*Pi[i,j]
    }
  }
  for (i in 1:simlist$nurses)
  {
    for (j in 1:( simlist$q + i + 1))
    {
      nursetotaltime <- nursetotaltime + i*Pi[i,j]
    }
  }
  idleratio = idletime/nursetotaltime
  print(idleratio)
  return(c(rejectratio, idleratio))
}

nursesreact <- function(evnt,simlist) {
  etype <- evnt[2]
  if (etype == 1) # job arrival
  {  
    simlist$statedur[simlist$nurses,simlist$ncalls+1] <- simlist$statedur[simlist$nurses,simlist$ncalls+1] - simlist$lasttime + simlist$currtime
    simlist$lasttime <- simlist$currtime
    if ((simlist$nurses == simlist$n) && (simlist$ncalls == (simlist$n +simlist$q))) # no arrival
    {
      #do nothing
    }
    else if (simlist$ncalls == (simlist$nurses + simlist$q)) #activate a new nurse, block a call
    {
      simlist$nurses <- simlist$nurses + 1     
    }
    else #add call into the quue
    {
      simlist$ncalls <- simlist$ncalls + 1
      simlist$queue <- appendtofcfsqueue(simlist$queue,evnt) # put the arrival into queue
    }
    arrduration <- runif(1,0,(2*simlist$r)) #next call arrival
    jobnum <- simlist$jobnum + 1 
    simlist$jobnum <- jobnum
    timeofnextarrival <- simlist$currtime + arrduration
    schedevnt(timeofnextarrival,1,simlist,c(timeofnextarrival,jobnum))
    
    arrduration <- arrduration - simlist$p
    timeofnurseleave <- simlist$currtime
    while (arrduration > 0) #schedule the nurse leave
    {
      timeofnurseleave <- timeofnurseleave + simlist$p
      schedevnt(timeofnurseleave,3,simlist,c(0,0))
      arrduration <- arrduration - simlist$p
    }

  } 
  else if (etype == 2) #job complete
  {  # job completion
    simlist$statedur[simlist$nurses,simlist$ncalls+1] <- simlist$statedur[simlist$nurses,simlist$ncalls+1] - simlist$lasttime + simlist$currtime
    simlist$lasttime <- simlist$currtime
    if (simlist$ncalls >= 1)
    {
      simlist$ncalls <- simlist$ncalls - 1
      simlist$totjobs <- simlist$totjobs + 1
      simlist$totwait <- simlist$totwait + simlist$currtime - evnt[3]
      tmp <- delfcfsqueuehead(simlist$queue)
      job <- tmp$qhead
      simlist$queue <- tmp$newqueue      
    }
    
  } 
  else if (etype == 3) #a nurse leaves
  {
    simlist$statedur[simlist$nurses,simlist$ncalls+1] <- simlist$statedur[simlist$nurses,simlist$ncalls+1] - simlist$lasttime + simlist$currtime
    simlist$lasttime <- simlist$currtime
    if (simlist$nurses > 1 && (simlist$nurses > simlist$ncalls))
    {
      simlist$nurses <- simlist$nurses - 1      
    }
  }
  #schedule call departure
  
  if (simlist$ncalls > 0) # wrong
  {
    srvduration <- rexp(1,(simlist$nurses/simlist$d))
    head <- simlist$evnts[1,]
    if ((simlist$currtime + srvduration) < head[1])
    {
      tmp <- delfcfsqueuehead(simlist$queue)# check the arrival time for the first user in the queue
      job <- tmp$qhead
      schedevnt(simlist$currtime+srvduration,2,simlist,job[3:4])
    }
  }
}

#nurses <- function(n,q,r,d,p,timelim,dbg=F)
# n: maximal nurse No. q: maximal queued calls. r: mean arrival time. d: mean service time. 
# p: nurse leaves if in a duration of p no new arrival
nurses(5,5,2,10,1,100000)

test <- function()
{
  Rcallratio = rep(0,10);
  Idleratio = rep(0,10);
  for (i in (1:10))
  {
    ans <- nurses(5,5,2,10,0.3 + (i-1)*0.4,100000)
    Rcallratio[i] = ans[1]
    Idleratio[i] = ans[2]
  }
  print(Rcallratio)
  print(Idleratio)
}

test()
