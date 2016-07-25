#### Epilepsy Tracking Data Analysis App

library(shiny)
source("tracking_script.R")

#allow for a 190Mb raw data spreadsheet to be uploaded,
#this can be increased as needed
options(shiny.maxRequestSize = 220*1024^2)


shinyServer(function(input, output) {
  
  #Creates table that will just show the total distance moved
  #per well per group per time period
  output$ds_table <- renderDataTable({
    #assign variables from UI inputs
    inBef_File <- input$bef_file
    inAft_File <- input$aft_file
    inRow <- input$row
    inGroup <- input$group
    
    #Make sure uploaded files are not null
    if (is.null(inBef_File))
      return(NULL)
    
    if (is.null(inAft_File))
      return(NULL)
    
    #read in raw  data files remove duplicate rows also remove extra rows
    #created from bug in zebrabox
    befdata <- read.delim(inBef_File$datapath, header = TRUE, sep = "\t")
    befdata <- subset(befdata, an != 0)
    befdata <- subset(befdata, befdata$end <= 1800)
    befdata <- befdata[ order(befdata$start, befdata$animal), ]
    aftdata <- read.delim(inAft_File$datapath, header = TRUE, sep = "\t")
    aftdata <- subset(aftdata, an != 0)
    aftdata <- subset(aftdata, aftdata$end <= 5400)
    aftdata <- aftdata[ order(aftdata$start, aftdata$animal), ]
    
    #Add PTZ column, Well number column, and distance sum column
    #and merge data
    befdata$PTZ <- "before"
    aftdata$PTZ <- "after"
    rawdata <- rbind(befdata, aftdata)
    remove(befdata, aftdata)
    rawdata$distsum <- rawdata$smldist + rawdata$lardist
    rawdata$wellnum <- 1:96
    rawdata <- rawdata[ , -c(1:7,9:24), drop = FALSE]
    rawdata1 <- subset(rawdata, rawdata$end <= 1800)
    rawdata2 <- subset(rawdata, rawdata$end > 1800)
    rawdata1$per <- "Period 1"
    rawdata2$per <- ifelse (rawdata2$end <= 3600, "Period 2", "Period 3")
    rawdata <- rbind(rawdata1, rawdata2)
    remove(rawdata1, rawdata2)
    
    #Use for loop to assign data frames by well number to A1-H12 and merge data frames by row
    A <- NULL
    B <- NULL
    C <- NULL
    D <- NULL
    E <- NULL
    F <- NULL
    G <- NULL
    H <- NULL
    ds.table <- as.data.frame(NULL)
    
    for (i in 1:12)
    {
      Ai <- assign(paste("A",i,sep=""), subset(rawdata, rawdata$wellnum == i))
      if (sum(Ai[,"distsum"]) >= input$mindist)
      {
        A <- rbind(A, Ai)
      }
      Bi <- assign(paste("B",i,sep=""), subset(rawdata, rawdata$wellnum == i+12))
      if (sum(Bi[,"distsum"]) >= input$mindist)
      {
        B <- rbind(B, Bi)
      }
      Ci <- assign(paste("C",i,sep=""), subset(rawdata, rawdata$wellnum == i+24))
      if (sum(Ci[,"distsum"]) >= input$mindist)
      {
        C <- rbind(C, Ci)
      }
      Di <- assign(paste("D",i,sep=""), subset(rawdata, rawdata$wellnum == i+36))
      if (sum(Di[,"distsum"]) >= input$mindist)
      {
        D <- rbind(D, Di)
      }
      Ei <- assign(paste("E",i,sep=""), subset(rawdata, rawdata$wellnum == i+48))
      if (sum(Ei[,"distsum"]) >= input$mindist)
      {
        E <- rbind(E, Ei)
      }
      Fi <- assign(paste("F",i,sep=""), subset(rawdata, rawdata$wellnum == i+60))
      if (sum(Fi[,"distsum"]) >= input$mindist)
      {
        F <- rbind(F, Fi)
      }
      Gi <- assign(paste("G",i,sep=""), subset(rawdata, rawdata$wellnum == i+72))
      if (sum(Gi[,"distsum"]) >= input$mindist)
      {
        G <- rbind(G, Gi)
      }
      Hi <- assign(paste("H",i,sep=""), subset(rawdata, rawdata$wellnum == i+84))
      if (sum(Hi[,"distsum"]) >= input$mindist)
      {
        H <- rbind(H, Hi)
      }
    }
    remove(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,Ai)
    remove(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,Bi)
    remove(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,Ci)
    remove(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,Di)
    remove(E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,Ei)
    remove(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,Fi)
    remove(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,Gi)
    remove(H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,Hi)
    
    #Average total distance per well per time period
    #and merge distance sum data frames and output table
    if (is.null(A) == FALSE)
    {
      A$Group <- "A"
      A.ds <- distsum(A)
      ds.table <- rbind(ds.table, A.ds)
    } else 
      A.ds <- NULL
    if (is.null(B) == FALSE)
    {B$Group <- "B"
    B.ds <- distsum(B)
    ds.table <- rbind(ds.table, B.ds)
    } else 
      B.ds <- NULL
    if(is.null(C) == FALSE)
    {
      C$Group <- "C"
      C.ds <- distsum(C)
      ds.table <- rbind(ds.table, C.ds)
    } else
      C.ds <- NULL
    if(is.null(D) == FALSE)
    {
      D$Group <- "D"
      D.ds <- distsum(D)
      ds.table <- rbind(ds.table, D.ds)
    } else
      D.ds <- NULL
    if(is.null(E) == FALSE)
    {
      E$Group <- "E"
      E.ds <- distsum(E)
      ds.table <- rbind(ds.table, E.ds)
    } else
      E.ds <- NULL
    if(is.null(F) == FALSE)
    {
      F$Group <- "F"
      F.ds <- distsum(F)
      ds.table <- rbind(ds.table, F.ds)
    } else
      F.ds <- NULL
    if(is.null(G) == FALSE)
    {
      G$Group <- "G"
      G.ds <- distsum(G)
      ds.table <- rbind(ds.table, G.ds)
    } else
      G.ds <- NULL
    if(is.null(H) == FALSE)
    {
      H$Group <- "H"
      H.ds <- distsum(H)
      ds.table <- rbind(ds.table, H.ds)
    } else
      H.ds <- NULL
    ds.table <- ds.table[ order(ds.table$Period, -xtfrm(ds.table$PTZ),
                                ds.table$`Well Number`), ]
   
    
    
    #If user hasnt selected groups calculate average 
    #and Standard Error by row
if (inGroup == FALSE)
{
    output$data_table <- renderDataTable({
      #Create the data table with average and standard error
      mse.table <- as.data.frame(NULL)
      
      if (is.null(A.ds) == FALSE)
      {
        A.mse <- distave(A.ds)
        mse.table <- rbind(mse.table, A.mse)
      }
      if (is.null(B.ds) == FALSE)
      {
        B.mse <- distave(B.ds)
        mse.table <- rbind(mse.table, B.mse)
      }
      if (is.null(C.ds) == FALSE)
      {
        C.mse <- distave(C.ds)
        mse.table <- rbind(mse.table, C.mse)
      }
      if (is.null(D.ds) == FALSE)
      {
        D.mse <- distave(D.ds)
        mse.table <- rbind(mse.table, D.mse)
      }
      if (is.null(E.ds) == FALSE)
      {
        E.mse <- distave(E.ds)
        mse.table <- rbind(mse.table, E.mse)
      }
      if (is.null(F.ds) == FALSE)
      {
        F.mse <- distave(F.ds)
        mse.table <- rbind(mse.table, F.mse)
      }
      if (is.null(G.ds) == FALSE)
      {
        G.mse <- distave(G.ds)
        mse.table <- rbind(mse.table, G.mse)
      }
      if (is.null(H.ds) == FALSE)
      {
        H.mse <- distave(H.ds)
        mse.table <- rbind(mse.table, H.mse)
      }
      mse.table <- mse.table[ order(mse.table$Period, -xtfrm(mse.table$PTZ),
                                    mse.table$Group), ]
      mse.table
      })
}
else if (inGroup == TRUE)
{
  ##Take user input and create merged data frames to work with
  if (is.null(input$g1) == FALSE)
  {
    g1 <- mget(input$g1)
    g1.red <- Reduce(rbind, g1)
    g1.red$Group <- "g1"
  }
  else
    g1 <- NULL
  if (is.null(input$g2) == FALSE)
  {
    g2 <- mget(input$g2)
    g2.red <- Reduce(rbind, g2)
    g2.red$Group <- "g2"
  }
  else
    g2 <- NULL
  if (is.null(input$g3) == FALSE)
  {
    g3 <- mget(input$g3)
    g3.red <- Reduce(rbind, g3)
    g3.red$Group <- "g3"
  }
  else
    g3 <- NULL
  if (is.null(input$g4) == FALSE)
  {
    g4 <- mget(input$g4)
    g4.red <- Reduce(rbind, g4)
    g4.red$Group <- "g4"
  }
  else
    g4 <- NULL
  
  ##Use distsum and distave functions to get average and standard errors
  output$data_table <- renderDataTable({
    if (is.null(g3)&is.null(g4)) {
      g1.ds <- distsum(g1.red)
      g2.ds <- distsum(g2.red)
      g1.mse <- distave(g1.ds)
      g2.mse <- distave(g2.ds)
      mse.table <- rbind(g1.mse, g2.mse)
      mse.table <- mse.table[ order(mse.table$Period, -xtfrm(mse.table$PTZ),
                                    mse.table$Group), ]
    }
    else if (is.null(g4)) {
      g1.ds <- distsum(g1.red)
      g2.ds <- distsum(g2.red)
      g3.ds <- distsum(g3.red)
      g1.mse <- distave(g1.ds)
      g2.mse <- distave(g2.ds)
      g3.mse <- distave(g3.ds)
      mse.table <- rbind(g1.mse, g2.mse, g3.mse)
      mse.table <- mse.table[ order(mse.table$Period, -xtfrm(mse.table$PTZ),
                                    mse.table$Group), ]
    }
    else {
      g1.ds <- distsum(g1.red)
      g2.ds <- distsum(g2.red)
      g3.ds <- distsum(g3.red)
      g4.ds <- distsum(g4.red)
      g1.mse <- distave(g1.ds)
      g2.mse <- distave(g2.ds)
      g3.mse <- distave(g3.ds)
      g4.mse <- distave(g4.ds)
      mse.table <- rbind(g1.mse, g2.mse, g3.mse, g4.mse)
      mse.table <- mse.table[ order(mse.table$Period, -xtfrm(mse.table$PTZ),
                                    mse.table$Group), ]
    }
  
  mse.table
  })
}
    ds.table
  })
})