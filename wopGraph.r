################# All functions #################
#Assign start date to V(g)$startDate - getDividedDates("13-01-2014","28-02-2014",g1)
#gMx[1,] : gets each record
#gMx[1,][1]:get the start vertex
#gMx[1,2][1] : gets the dependent of the first child


#Check if the parentId(pid) is a start node.
isStartVertex=function(gMx,pid)
{  
 	startFlag='Y'
	for (i in 1:dim(gMx)[1])
	{
		#print(i)
		if ((gMx[i,2][1] == pid) & (startFlag=='Y')) 
		{	
			startFlag='N'
		}
	}
	return(startFlag)
}

#Get the next edge
getNextEdge=function(gMx,pid)
{
	return (gMx[pid,])
}

#Get the parentnode in the edge
getEdgeParent=function(gMx,pid)
{
	return (gMx[pid,2][1])
}

#Get the dependent node in the edge
getEdgeDependent=function(gMx,pid)
{
	return (gMx[pid,2][1])
}

#Get the process Flows(read the matrix to get the Flows)
getProcessFlows=function(gWop)
{
  procFlow=matrix(NA,8,8)
  gMx=get.edges(gWop,1:length(E(gWop)))
  for(pid in 1:dim(gMx)[1])
  {
    startFlag=isStartVertex(gMx,pid)
    if (startFlag == 'Y')
    {
    	procFlow[pid,1:2] = c(pid,getEdgeDependent(gMx,pid))
    	#cat ("rec entered:",pid,getEdgeDependent(gMx,pid),"\n")
    }
    else
    {	
        if (length(which(procFlow[,2] == pid)) > 1)
        {
          for (i in 1:dim(procFlow)[1])
          {        
            if (pid %in% procFlow[i,] & (pid != "NA"))
            {	
	          	curLen= length(na.omit(procFlow[i,]))
    	     	procFlow[i,(curLen+1)] = getEdgeDependent(gMx,pid)
    	     	#cat ("  rec entered for",pid,":",getEdgeDependent(gMx,pid),"\n")
            }
          }
        }
        else
        {
        	for (i in 1:dim(procFlow)[1])
        	{
        		curLen= length(na.omit(procFlow[i,])) 
        		#cat("error info",i,curLen,pid,"\n")
        		if ((pid != "NA") & (curLen != 0))
        		{
        			if (procFlow[i,curLen] == pid)
        			{
        				procFlow[i,(curLen+1)] = getEdgeDependent(gMx,pid)
        				#cat ("  rec entered for",pid,":",getEdgeDependent(gMx,pid),"\n")
        			}	
        		}
        	}
 		}
 	}
  }
  procFlow=procFlow[rowSums(is.na(procFlow))!=dim(procFlow)[1],] 	
  return(procFlow)
}

#Get the process Start dates during the entire order activity.
getProcessStartDate=function(gProcFlow
                            ,calendarDate
                            ,orderStartDate
                            ,orderEndDate
                            ,FChrs
                            ,ALhrs
                            ,FQChrs
                            ,RChrs
                            ,RQChrs
                            ,CChrs
                            ,CQChrs
                            ,PChrs
                            ,ABChrs)
{
  #cat (" in wGraph","\n")
  procFCStartDate=c()
  procALStartDate=c()
  procRCStartDate=c()
  procCCStartDate=c()
  procFQCStartDate=c()
  procRQCStartDate=c()
  procCQCStartDate=c()
  procPCStartDate=c()
  procABCStartDate=c()

  procFCEndDate=c()
  procALEndDate=c()
  procRCEndDate=c()
  procCCEndDate=c()
  procFQCEndDate=c()
  procRQCEndDate=c()
  procCQCEndDate=c()
  procPCEndDate=c()
  procABCEndDate=c()

  tempList=list(estDays=numeric()
               ,proportion=numeric()
               ,estPropInDays=numeric()
               ,procStartDate=date()
               ,procEndDate=date()
               ,isActive=character())
  
  tempList$estDays = c(FChrs/24,ALhrs/24,RChrs/24,CChrs/24,FQChrs/24,RQChrs/24,CQChrs/24,PChrs/24,ABChrs/24)
  orderDuration=as.integer(as.Date(orderEndDate) - as.Date(orderStartDate))

  #cat ("Read the path from gProcFlow","\n")
  for (pathId in 1:dim(gProcFlow)[1])
  {
    #cat ("Path read :",gProcFlow[pathId,],"\n")
    #cat("Get the path total estimated days...","\n")
    pathTotal=0
    for (nodeId in 1: dim(gProcFlow)[2])
    {
      if (is.na(gProcFlow[pathId,nodeId]) == FALSE)
      {
        nodeVal=gProcFlow[pathId,nodeId]
        pathTotal=pathTotal + tempList$estDays[nodeVal]
      }
    }
    
    #cat("Get the proportion(in% & inDays) & start & end dates of each process in the path","\n")
    for (nodeId in 1: dim(gProcFlow)[2])
    {
      if (is.na(gProcFlow[pathId,nodeId]) == FALSE)
      {
        nodeVal=gProcFlow[pathId,nodeId]

        #cat("Error @ ",tempList$estDays,"\n")
        if (tempList$estDays[nodeVal] == 0)
        {  tempList$proportion[nodeVal] = 0 }
        else
        {  tempList$proportion[nodeVal] = (tempList$estDays[nodeVal]/pathTotal) * 100 }

        if (tempList$proportion[nodeVal] == 0)
        {  tempList$estPropInDays[nodeVal] = 0  } 
        else
        {  tempList$estPropInDays[nodeVal] = (tempList$proportion[nodeVal]*orderDuration)/100 }
        
        
        #cat(" ## Get the start and end dates per order","\n")
        if (nodeId == 1)
        {
          tempList$procStartDate[nodeVal] = as.character(orderStartDate)
          calcDate=as.Date(tempList$procStartDate[nodeVal]) + tempList$estPropInDays[nodeVal]
          tempList$procEndDate[nodeVal] = as.character(calcDate)
        }
        else
        {
          prevNodeVal=gProcFlow[pathId,nodeId-1]
          tempList$procStartDate[nodeVal]= tempList$procEndDate[prevNodeVal]
          calcDate=as.Date(tempList$procStartDate[nodeVal]) + tempList$estPropInDays[nodeVal]
          tempList$procEndDate[nodeVal] = as.character(calcDate) 
        }

        #cat("## Check if the process is active on the given dates","\n")  
        if ((calendarDate >= tempList$procStartDate[nodeVal]) & 
            (calendarDate <= tempList$procEndDate[nodeVal]))
        {
          tempList$isActive[nodeVal]='Y'
        } 
        else
        {
          tempList$isActive[nodeVal]='N'
        }

        #cat("##  Store the process StartDates so that we can do a min of it at the end","\n")
        if (nodeVal == 1)
        { procFCStartDate= c(tempList$procStartDate[nodeVal] ,procFCStartDate) }  
        else  
        { if (nodeVal == 2)
          { procALStartDate= c(tempList$procStartDate[nodeVal] ,procALStartDate) }
          else
          { if (nodeVal == 3)
            { procRCStartDate= c(tempList$procStartDate[nodeVal] ,procRCStartDate) }
            else
            { if (nodeVal == 4)
              { procCCStartDate= c(tempList$procStartDate[nodeVal] ,procCCStartDate) }   
              else
              { if (nodeVal == 5)
                { procFQCStartDate= c(tempList$procStartDate[nodeVal] ,procFQCStartDate) } 
                else
                { if (nodeVal == 6)
                  { procRQCStartDate= c(tempList$procStartDate[nodeVal] ,procRQCStartDate) } 
                  else
                  { if (nodeVal == 7)
                    { procCQCStartDate= c(tempList$procStartDate[nodeVal] ,procCQCStartDate) }
                    else
                    { if (nodeVal == 8)
                      { procPCStartDate= c(tempList$procStartDate[nodeVal] ,procPCStartDate) } 
                      else
                      { procABCStartDate= c(tempList$procStartDate[nodeVal] ,procABCStartDate)
                      }}}}}}}}

        #cat("##  Store the process EndDates so that we can do a max of it at the end","\n")
        if (nodeVal == 1)
        { procFCEndDate= c(tempList$procEndDate[nodeVal] ,procFCEndDate) }  
        else  
        { if (nodeVal == 2)
          { procALEndDate= c(tempList$procEndDate[nodeVal] ,procALEndDate) }
          else
          { if (nodeVal == 3)
            { procRCEndDate= c(tempList$procEndDate[nodeVal] ,procRCEndDate) }
            else
            { if (nodeVal == 4)
              { procCCEndDate= c(tempList$procEndDate[nodeVal] ,procCCEndDate) }   
              else
              { if (nodeVal == 5)
                { procFQCEndDate= c(tempList$procEndDate[nodeVal] ,procFQCEndDate) } 
                else
                { if (nodeVal == 6)
                  { procRQCEndDate= c(tempList$procEndDate[nodeVal] ,procRQCEndDate) } 
                  else
                  { if (nodeVal == 7)
                    { procCQCEndDate= c(tempList$procEndDate[nodeVal] ,procCQCEndDate) }
                    else
                    { if (nodeVal == 8)
                      { procPCEndDate= c(tempList$procEndDate[nodeVal] ,procPCEndDate) } 
                      else
                      { procABCEndDate= c(tempList$procEndDate[nodeVal] ,procABCEndDate)
                      }}}}}}}}

      }
    }
    #cat ("==> Details for path ",pathId,"are ","\n"
    #                            ,"pathTotal: ",pathTotal,"\n"
    #                            ,"prop%: ",tempList$proportion,"\n"
    #                            ,"propDays: ",tempList$estPropInDays,"\n"
    #                            ,"procStartDate: ",tempList$procStartDate,"\n"
    #                            ,"procEndDate: ",tempList$procEndDate,"\n")
    
  }

  
  ###                EXTRACT THE INFO NEEDED               ###

  #cat(" Extract all info to send to wopFunction","\n")
  procStartDates=c(max(na.omit(procFCStartDate))
                  ,max(na.omit(procALStartDate))
                  ,max(na.omit(procRCStartDate))
                  ,max(na.omit(procCCStartDate))  
                  ,max(na.omit(procFQCStartDate))
                  ,max(na.omit(procRQCStartDate))
                  ,max(na.omit(procCQCStartDate))
                  ,max(na.omit(procPCStartDate))
                  ,max(na.omit(procABCStartDate)))
  procEndDates=c(max(na.omit(procFCEndDate))
                ,max(na.omit(procALEndDate))
                ,max(na.omit(procRCEndDate))
                ,max(na.omit(procCCEndDate))
                ,max(na.omit(procFQCEndDate))
                ,max(na.omit(procRQCEndDate))
                ,max(na.omit(procCQCEndDate))
                ,max(na.omit(procPCEndDate))
                ,max(na.omit(procABCEndDate)))

  ### To get the waiting/slack duration for each process.
  procSlackDays =c(as.integer(orderEndDate-as.Date(procEndDates[1]))
                  ,as.integer(orderEndDate-as.Date(procEndDates[2]))
                  ,as.integer(orderEndDate-as.Date(procEndDates[3]))
                  ,as.integer(orderEndDate-as.Date(procEndDates[4]))
                  ,as.integer(orderEndDate-as.Date(procEndDates[5])) 
                  ,as.integer(orderEndDate-as.Date(procEndDates[6])) 
                  ,as.integer(orderEndDate-as.Date(procEndDates[7])) 
                  ,as.integer(orderEndDate-as.Date(procEndDates[8])) 
                  ,as.integer(orderEndDate-as.Date(procEndDates[9])))
   
  ### To get the Boolean Variables to decide if variable is active on that calendar date.
  procActiveFlags=c(tempList$isActive[1]
                   ,tempList$isActive[2]
                   ,tempList$isActive[3]
                   ,tempList$isActive[4]
                   ,tempList$isActive[5]
                   ,tempList$isActive[6]
                   ,tempList$isActive[7]
                   ,tempList$isActive[8]
                   ,tempList$isActive[9])

  procDetails=c(procStartDates,procEndDates,procSlackDays,procActiveFlags)
  #cat (" @amit in wGraph sending: start dates are :",procStartDates,"\n")
  #cat (" @amit in wGraph sending: slack days are :",procSlackDays,"\n")
  #cat (" out of wGraph","\n")
  return(procDetails)   
}






