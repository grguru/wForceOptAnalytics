#*****************************************************
#Load relevant libararies


#*****************************************************
#Load relevant files
loadFile=function(arg1,arg2)
{ 
  if (arg2=="csv")
  {
    file=tbl_df(read.csv(arg1,
                        ,sep = "," 
                        ,header=TRUE
                        ,na.strings=c(".","","NA","?")
                        ,strip.white=TRUE
                        ,fill=TRUE  
                        ,stringsAsFactors=FALSE))  
    return(file)
  }
  else if (arg1=="xlsx")
       {
        fileName=loadWorkbook(arg1)
        file=tbl_df(readWorksheet(fileName,
                                       sheet=arg2,
                                       header=TRUE))
        return(file)
       }
      else
       { cat ("You got the wrong file type",arg2) }
}

#Extract all Delivered projects
preProcessData=function(file,processFlag,nrec)
{
  if (processFlag=="demand")
  {
    orderData=processDemandData(file)
    orderDelDf=data.frame()
    orderDelDf=filter(orderData,Production.Status=="Delivered"); 
    #orderDelDf=orderDelDf[1:nrec,]  #amit
    return(orderDelDf)
  }
  else
  {
    skillData=processSkillData(file)
    return(skillData)
  }
}

processSkillData=function(file)
{
  file$S.No=NULL
  file$EmpID=NULL                  
  file$Employee.Name=NULL                   
  file$Total.Calendar.days=NULL
  file$Total.working.days=NULL
  file$Unit=NULL
  file$Process=as.factor(file$Process)                    
  file$Month=as.factor(file$Month)    
  return(file)    
}

#Variable Type Conversion, #Converting all complex hours estimates to pure hr estimates
#Remove the unwanted columns , #Add columns
processDemandData=function(file)
{
  file$Series.ID=as.factor(file$Series.ID)
  file$Resource.ID=as.factor(file$Resource.ID)                      
  file$Project.Type=as.factor(file$Project.Type)                   
  file$Source.Type=as.factor(file$Source.Type)                    
  file$Category=as.factor(file$Category)                       
  file$Production.Status=as.factor(file$Production.Status)              
  file$Production.Approval.Date=as.Date(file$Production.Approval.Date,format="%d-%b-%y")       
  file$Production.Start.Date=as.Date(file$Production.Start.Date,format="%d-%b-%y")          
  file$Actual.Delivery.Date=as.Date(file$Actual.Delivery.Date,format="%d-%b-%y") 
  file$Delivery.Character.Count=as.integer(file$Delivery.Character.Count) 

  for (i in 1:nrow(file))
  {
    file$FChrs[i] = as.integer(unlist(strsplit(file$Formatting.Coding[i],":"))[[1]])              
    file$ALhrs[i] = as.integer(unlist(strsplit(file$Ancient.Language[i],":"))[[1]])      
    file$PChrs[i] = as.integer(unlist(strsplit(file$Proofing..Compare[i],":"))[[1]])  
    file$FQChrs[i] = as.integer(unlist(strsplit(file$Formatting.QC[i],":"))[[1]])  
    file$RChrs[i] = as.integer(unlist(strsplit(file$Reference.Coding[i],":"))[[1]])  
    file$RQChrs[i] = as.integer(unlist(strsplit(file$Reference.QC[i],":"))[[1]])     
    file$CChrs[i] = as.integer(unlist(strsplit(file$Citation.Coding[i],":"))[[1]])  
    file$CQChrs[i] = as.integer(unlist(strsplit(file$Citation.QC[i],":"))[[1]])  
    file$ABChrs[i] = as.integer(unlist(strsplit(file$Alpha...Beta.Check[i],":"))[[1]])  
  }
  #This code can be removed but some problem above..no patience
  file$FChrs = as.integer(file$FChrs)              
  file$ALhrs = as.integer(file$ALhrs)      
  file$PChrs = as.integer(file$PChrs) 
  file$FQChrs = as.integer(file$FQChrs) 
  file$RChrs = as.integer(file$RChrs)  
  file$RQChrs = as.integer(file$RQChrs)   
  file$CChrs = as.integer(file$CChrs) 
  file$CQChrs = as.integer(file$CQChrs)  
  file$ABChrs = as.integer(file$ABChrs)
  file$Project.Start.Delay =as.integer(as.Date(file$Production.Start.Date)-as.Date(file$Production.Approval.Date))  
  file$Diff.In.Char.Count =as.integer(file$Total.Estimated.Character.Count-file$Delivery.Character.Count)

#@amit -starts
#  file = file%>%filter((Series.ID == "CHRTNLIBPRESSMISC") & (Resource.ID == "WRKMNNGYRLF"))
  return(file)
}

### Create a new dataframe with the volume of all orders on a daily basis
#This analysis is only to monitor the order details....No of orders in parallel
#Calculating the proportion of each function in the entire order(in terms of estimated hours)
getOrderVolStats=function(orderData,gProcFlow)
{
  minProjDate=min(na.omit(orderData$Production.Start.Date))
  maxProjDate=max(na.omit(orderData$Actual.Delivery.Date))
  dateSequence=as.Date(seq(ymd(as.Date(minProjDate)),ymd(as.Date(maxProjDate)),by='day'))
  
  #Initialize Dates
  tempDf=data.frame()
  orderData$Proc.FC.StartDate="1899-01-01"
  orderData$Proc.AL.StartDate="1899-01-01"
  orderData$Proc.RC.StartDate="1899-01-01"
  orderData$Proc.CC.StartDate="1899-01-01"
  orderData$Proc.FQC.StartDate="1899-01-01"
  orderData$Proc.RQC.StartDate="1899-01-01"
  orderData$Proc.CQC.StartDate="1899-01-01"
  orderData$Proc.PC.StartDate="1899-01-01"
  orderData$Proc.ABC.StartDate="1899-01-01"
  orderData$Proc.FC.EndDate="1899-01-01"
  orderData$Proc.AL.EndDate="1899-01-01"
  orderData$Proc.RC.EndDate="1899-01-01"
  orderData$Proc.CC.EndDate="1899-01-01"
  orderData$Proc.FQC.EndDate="1899-01-01"
  orderData$Proc.RQC.EndDate="1899-01-01"
  orderData$Proc.CQC.EndDate="1899-01-01"
  orderData$Proc.PC.EndDate="1899-01-01"
  orderData$Proc.ABC.EndDate="1899-01-01"

  #Initialize SlackDays & Flags
  orderData$Proc.FC.SlackDays=0
  orderData$Proc.AL.SlackDays=0
  orderData$Proc.RC.SlackDays=0
  orderData$Proc.CC.SlackDays=0
  orderData$Proc.FQC.SlackDays=0
  orderData$Proc.RQC.SlackDays=0
  orderData$Proc.CQC.SlackDays=0
  orderData$Proc.PC.SlackDays=0
  orderData$Proc.ABC.SlackDays=0
  orderData$Proc.FC.isActive="N"
  orderData$Proc.AL.isActive="N"
  orderData$Proc.RC.isActive="N"
  orderData$Proc.CC.isActive="N"
  orderData$Proc.FQC.isActive="N"
  orderData$Proc.RQC.isActive="N"
  orderData$Proc.CQC.isActive="N"
  orderData$Proc.PC.isActive="N"
  orderData$Proc.ABC.isActive="N"

  for (z in 1:length(dateSequence))
  {
    cat(z,".Date =>",as.character(dateSequence[z]),"\n") 
     
    for (i in 1:nrow(orderData))
    { 
        #cat(" @amit1:",orderData$S.No[i],orderData$Series.ID[i],orderData$Resource.ID[i],"\n")
        orderData$calendarDate[i]=as.character(as.Date(dateSequence[z]))
        if ((as.Date(dateSequence[z]) >= orderData$Production.Start.Date[i]) &
            (as.Date(dateSequence[z]) <= orderData$Actual.Delivery.Date[i]))
        {
            orderData$orderInProcessFlag[i]='Y'
            #Get individual Process Start Dates & Slack days
            procDetails=getProcessStartDate(gProcFlow
                                       ,orderData$calendarDate[i]
                                       ,orderData$Production.Start.Date[i]
                                       ,orderData$Actual.Delivery.Date[i]
                                       ,orderData$FChrs[i]
                                       ,orderData$ALhrs[i]
                                       ,orderData$FQChrs[i]
                                       ,orderData$RChrs[i]
                                       ,orderData$RQChrs[i]
                                       ,orderData$CChrs[i]
                                       ,orderData$CQChrs[i]
                                       ,orderData$PChrs[i]
                                       ,orderData$ABChrs[i])
            
            #cat (" @amit in wFunction receiving: start dates are: ",procDetails,"\n")
            #cat ("length of procDetails",length(procDetails),"\n")

            #Add the proc Details to the dataframe
            orderData$Proc.FC.StartDate[i]=procDetails[1]
            orderData$Proc.AL.StartDate[i]=procDetails[2]
            orderData$Proc.RC.StartDate[i]=procDetails[3]
            orderData$Proc.CC.StartDate[i]=procDetails[4]
            orderData$Proc.FQC.StartDate[i]=procDetails[5]
            orderData$Proc.RQC.StartDate[i]=procDetails[6]
            orderData$Proc.CQC.StartDate[i]=procDetails[7]
            orderData$Proc.PC.StartDate[i]=procDetails[8]
            orderData$Proc.ABC.StartDate[i]=procDetails[9]

            orderData$Proc.FC.EndDate[i]=procDetails[10]
            orderData$Proc.AL.EndDate[i]=procDetails[11]
            orderData$Proc.RC.EndDate[i]=procDetails[12]
            orderData$Proc.CC.EndDate[i]=procDetails[13]
            orderData$Proc.FQC.EndDate[i]=procDetails[14]
            orderData$Proc.RQC.EndDate[i]=procDetails[15]
            orderData$Proc.CQC.EndDate[i]=procDetails[16]
            orderData$Proc.PC.EndDate[i]=procDetails[17]
            orderData$Proc.ABC.EndDate[i]=procDetails[18]

            orderData$Proc.FC.SlackDays[i]=procDetails[19]
            orderData$Proc.AL.SlackDays[i]=procDetails[20]
            orderData$Proc.RC.SlackDays[i]=procDetails[21]
            orderData$Proc.CC.SlackDays[i]=procDetails[22]
            orderData$Proc.FQC.SlackDays[i]=procDetails[23]
            orderData$Proc.RQC.SlackDays[i]=procDetails[24]
            orderData$Proc.CQC.SlackDays[i]=procDetails[25]
            orderData$Proc.PC.SlackDays[i]=procDetails[26]
            orderData$Proc.ABC.SlackDays[i]=procDetails[27]

            orderData$Proc.FC.isActive[i]=procDetails[28]
            orderData$Proc.AL.isActive[i]=procDetails[29]
            orderData$Proc.RC.isActive[i]=procDetails[30]
            orderData$Proc.CC.isActive[i]=procDetails[31]
            orderData$Proc.FQC.isActive[i]=procDetails[32]
            orderData$Proc.RQC.isActive[i]=procDetails[33]
            orderData$Proc.CQC.isActive[i]=procDetails[34]
            orderData$Proc.PC.isActive[i]=procDetails[35]
            orderData$Proc.ABC.isActive[i]=procDetails[36]

        }
        else
        {
            orderData$orderInProcessFlag[i]='N'
        }
    }
    tempDf=rbind(tempDf,orderData) 
  }
  
  #cat("Filtering the columns-finally","\n")
  orderDataStats = tempDf%>%
      filter(orderInProcessFlag=='Y')%>%
      select(calendarDate
            ,Series.ID
            ,Resource.ID
            ,Project.Type
            ,Source.Type
            ,Category
            ,Production.Status
            ,Diff.In.Char.Count
            ,Production.Approval.Date
            ,Production.Start.Date
            ,Actual.Delivery.Date
            ,Project.Start.Delay
            ,Proc.FC.StartDate,Proc.FC.EndDate,Proc.FC.isActive,Proc.FC.SlackDays
            ,Proc.AL.StartDate,Proc.AL.EndDate,Proc.AL.isActive,Proc.AL.SlackDays
            ,Proc.RC.StartDate,Proc.RC.EndDate,Proc.RC.isActive,Proc.RC.SlackDays
            ,Proc.CC.StartDate,Proc.CC.EndDate,Proc.CC.isActive,Proc.CC.SlackDays
            ,Proc.FQC.StartDate,Proc.FQC.EndDate,Proc.FQC.isActive,Proc.FQC.SlackDays
            ,Proc.RQC.StartDate,Proc.RQC.EndDate,Proc.RQC.isActive,Proc.RQC.SlackDays
            ,Proc.CQC.StartDate,Proc.CQC.EndDate,Proc.CQC.isActive,Proc.CQC.SlackDays
            ,Proc.PC.StartDate,Proc.PC.EndDate,Proc.PC.isActive,Proc.PC.SlackDays
            ,Proc.ABC.StartDate,Proc.ABC.EndDate,Proc.ABC.isActive,Proc.ABC.SlackDays)

  names(orderDataStats)=
           c("CalDate"
            ,"SerID"
            ,"ResID"
            ,"PrjType"
            ,"SrcType"
            ,"Category"
            ,"ProdStatus"
            ,"CCntDelta"
            ,"OAppDate"
            ,"OStDate"
            ,"ODelDate"
            ,"PSDelay"
            ,"FCSDate","FCEDate","FCFlag","FCWaitDays"
            ,"ALSDate","ALEDate","ALFlag","ALWaitDays"
            ,"RCSDate","RCEDate","RCFlag","RCWaitDays"
            ,"CCSDate","CCEDate","CCFlag","CCWaitDays"
            ,"FQCSDate","FQCEDate","FQCFlag","FQCWaitDays"
            ,"RQCSDate","RQCEDate","RQCFlag","RQCWaitDays"
            ,"CQCSDate","CQCEDate","CQCFlag","CQCWaitDays"
            ,"PCSDate","PCEDate","PCFlag","PCWaitDays"
            ,"ABCSDate","ABCEDate","ABCFlag","ABCWaitDays"
            )
  return(orderDataStats)
}