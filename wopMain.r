setwd("D:\\MyJukeBox\\Data Analytics\\08. MyClasses\\workdir")
rm(list=ls())

#***************************************************#
library(igraph)
source("wopGraph.r")

library(dplyr)
library(lubridate)
library(elastic)
source("wopFunction.r")
#***************************************************#
#Load the Orders file,Graphs file
orderDemand=loadFile("Order.csv","csv")
gWop=read.graph("processFlowTemplate.txt",format="dimacs",directed=TRUE)
gProcFlow=getProcessFlows(gWop)

#Data PreProcessing
processFlag="demand"
orderData=preProcessData(orderDemand,processFlag,100) 

#Get orders volume and process proportion & Backup the file
orderDataStats=getOrderVolStats(orderData,gProcFlow)		
write.csv(orderDataStats,file="FinalOrderDetails.csv")   #<=== USE THIS FILE..Gita...

