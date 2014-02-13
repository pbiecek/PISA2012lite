## server.r

#setwd("/Users/pbiecek/camtasia/Dropbox/TJA Fellowship Biecek/reports/PISAinFocusSummaryApp/app/")
#setwd("c:/_Przemek_/Dropbox/TJA Fellowship Biecek/reports/PISAinFocusSummaryApp/app")
library(ggplot2)
library(RColorBrewer)
kol9 <- brewer.pal(n=9, name="RdYlBu")

load("labels05.rda")
load("AllAvgSdsDec05.rda")
minStudents = 30
minSchools = 5

#input = list(variable="Poland",variable1="Germany",subject="MATH",range=c(450,650))

shinyServer(function(input, output) {
  
  output$TwoCnts <- renderPlot({
    inRange <- mean( AllAvgSds[[paste0(input$subject, "avg")]][input$variable,]  <= input$range[2] & AllAvgSds[[paste0(input$subject, "avg")]][input$variable,]  >= input$range[1], na.rm=TRUE)
    
    levs1 <- nchar(colnames(AllAvgSds[[paste0(input$subject, "avg")]]))
    gr1 <- labels[levs1 == 1]; gr1[1] = input$variable
    gr2 <- labels[levs1 == 1]; gr2[1] = input$variable1
    pl <- plotSlopeHtree(val1 = AllAvgSds[[paste0(input$subject, "avg")]][input$variable,levs1 == 1], val2 = AllAvgSds[[paste0(input$subject, "avg")]][input$variable1,levs1 == 1],
                         gr1 = gr1, gr2 = gr1, 
                         lab1 = gr1, lab2 = gr2, 
                         col1 = factor(labels[levs1 == 1]), col2 = factor(labels[levs1 == 1]),
                         lev1 = c(1.5,rep(1,9)), lev2 = c(1.5,rep(1,9)),
                         rang = input$range)+ scale_size_continuous(range=c(5,10))
    if (inRange < 0.01){
      df <- data.frame()
      pl <- ggplot(df) + geom_blank() + xlim(0, 10) + ylim(input$range[1], input$range[2]) + theme_bw()
    }

    if (inRange < 0.7)
      pl <- pl + ggtitle("NOTE THAT MOST OF AVERAGES FALL OUTSIDE OF THIS PLOT!\n CHANGE THE VERTICAL AXIS ON THE LEFT PANEL!") + theme(plot.title=element_text(size=20))
    
    print(pl)
    
      
  }, height=800)

  output$TwoCnts2 <- renderPlot({
    inRange <- mean( AllAvgSds[[paste0(input$subject, "avg")]][input$variable,]  <= input$range[2] & AllAvgSds[[paste0(input$subject, "avg")]][input$variable,]  >= input$range[1], na.rm=TRUE)
    
    levs1 <- nchar(sapply(strsplit(labels, split=" "), '[', 1))
    gr1 <- labels; gr1[1] = input$variable
    gr2 <- labels; gr2[1] = input$variable1

    pl<- plotSlopeHtree(val1 = AllAvgSds[[paste0(input$subject, "avg")]][input$variable,], val2 = AllAvgSds[[paste0(input$subject, "avg")]][input$variable1,],
                        gr1 = gr1, gr2 = gr1, 
                        lab1 = gr1, lab2 = gr2, 
                        col1 = factor(substr(labels,1,1)), col2 = factor(substr(labels,1,1)),
                        lev1 = (3-levs1)/2, lev2 = (3-levs1)/2,
                        rang = input$range)+ scale_size_continuous(range=c(3.5,10))
    
    if (inRange < 0.01){
      df <- data.frame()
      pl <- ggplot(df) + geom_blank() + xlim(0, 10) + ylim(input$range[1], input$range[2]) + theme_bw()
    }
    if (inRange < 0.7)
      pl <- pl + ggtitle("NOTE THAT MOST OF AVERAGES FALL OUTSIDE OF THIS PLOT!\n CHANGE THE VERTICAL AXIS ON THE LEFT PANEL!") + theme(plot.title=element_text(size=20))
    
    print(pl)
    
  }, height=800)

  output$Trees <- renderPlot({
    eMeans <- AllAvgSds[[paste0(input$subject, "avg")]][input$variable,]
    eSd    <- AllAvgSds[[paste0(input$subject, "sd")]][input$variable,]
    eSize  <- AllAvgSds[["struct"]][input$variable,]
    eStud  <- AllAvgSds[["studs"]][input$variable,]
    eSchool <- AllAvgSds[["schools"]][input$variable,]
    
    par(mar=c(1,10,20,10))
    plot(seq_along(eMeans), eMeans, pch=19, cex=2*sqrt(eSize/median(eSize, na.rm=TRUE)), las=1, xaxt="n", xlab="", ylab="", main="", type="n", ylim=input$range, yaxt="n")
    axis(2, las=1, seq(round(input$range[1],-1), input$range[2], 10))
    axis(4, las=1, seq(round(input$range[1],-1), input$range[2], 10))
    abline(h=seq(round(input$range[1],-1), input$range[2], 20), col="grey", lty=3)
    for (i in 1:9) {
      tx <- grep(names(eMeans), pattern=paste("^", i, "[^0]", sep=""))
      lines(range(tx), eMeans[paste(i)]*c(1,1), col=kol9[i])
      for (txx in tx) {
        if (!is.na(eStud[txx]) && eStud[txx] >= minStudents && eSchool[txx] >= minSchools) {
          points(txx, eMeans[txx], pch=19, cex=sqrt(2*sqrt(eSize[txx]/median(eSize, na.rm=TRUE))), col=kol9[i])
          if (input$showCI)
            lines(txx*c(1,1), eMeans[txx] + c(-1,1)*eSd[txx], col=kol9[i])
        }
      }
    }

    axis(3, 1, input$variable, las=2, cex.axis=0.95, col.axis="brown")
    points(1, eMeans[1], pch=19, cex=sqrt(2*sqrt(eSize["."]/median(eSize, na.rm=TRUE))), col="brown")
    if (input$showCI)
      lines(c(1,1), eMeans["."] + c(-1,1)*eSd["."], col="brown")
    
    for (ii in seq_along(eMeans))
      if (nchar(names(eMeans)[ii]) > 1)
        axis(3, ii, labels[ii], las=2, cex.axis=0.95, 
             col.axis=kol9[as.numeric(substr(labels[ii], 1, 1))])
    
    }, height=800)
  
  output$DownloadZone <- renderText({
    cname <- gsub(input$variable, pattern="[^A-Za-z]", replacement="")
    
    HTML(paste0("<a href='occupationsPISA2012.xls'>Here you can download data as Excel file,<br/><br/>",
                "<a href='OccupationsPISA2012.pdf'>Here you can download graphical one page country profiles.<br/><br/>"))
  })

  output$ColorTrees <- renderPlot({
    inRange <- mean( AllAvgSds[[paste0(input$subject, "avg")]][input$variable,]  <= input$range[2] & AllAvgSds[[paste0(input$subject, "avg")]][input$variable,]  >= input$range[1], na.rm=TRUE)
    
    flatHtree <- data.frame(
      level = c(0,0.4,1.9)[nchar(sapply(strsplit(labels, split=" "), '[', 1))+1],
      average = AllAvgSds[[paste0(input$subject, "avg")]][input$variable,],
      nameLong = sapply(sapply(strsplit(labels, split=" "), '[', -1), paste, collapse=" "),
      struct = AllAvgSds[["struct"]][input$variable,],
      studs = AllAvgSds[["studs"]][input$variable,],
      schools = AllAvgSds[["schools"]][input$variable,],
      color = factor(substr(labels, 1, 1)), stringsAsFactors = FALSE)
    flatHtree[1,3] = input$variable
    flatHtree <- flatHtree[flatHtree$studs >= 30 & flatHtree$schools >=5 , ]

    pl <- plotFlatHtree(flatHtree, 
                        x = "level", y = "average", size = "struct", label="nameLong", color = "color",
                        range = input$range)+ scale_size_continuous(range=c(3.5,12))
    
    if (inRange < 0.01){
      df <- data.frame()
      pl <- ggplot(df) + geom_blank() + xlim(0, 10) + ylim(input$range[1], input$range[2]) + theme_bw()
    }
    
    if (inRange < 0.7)
      pl <- pl + ggtitle("NOTE THAT MOST OF AVERAGES FALL OUTSIDE OF THIS PLOT!\n CHANGE THE VERTICAL AXIS ON THE LEFT PANEL!") + theme(plot.title=element_text(size=20))
    
    print(pl)
  }, height=800)

  output$Map <- renderPlot({
    level = nchar(sapply(strsplit(labels, split=" "), '[', 1))
    coefs <- cbind(
      apply(AllAvgSds[[paste0(input$subject, "avg")]], 1, function(x) quantile(x[level==1], .25, na.rm=TRUE)),
      apply(AllAvgSds[[paste0(input$subject, "avg")]], 1, function(x) x[1]),
      apply(AllAvgSds[[paste0(input$subject, "avg")]], 1, function(x) quantile(x[level==1], .75, na.rm=TRUE)))
    
    plot(coefs[,2], coefs[,3] - coefs[,1], type="p", pch=19,
         xlab="average occupation score", ylab="spread / interquartile range of occupation scores", las=1,
         bty="n", xaxt="n", yaxt="n")
    abline(h=seq(0,70,10), col="grey", lty=3)
    abline(v=seq(350,650,25), col="grey", lty=3)
    text(coefs[,2], coefs[,3] - coefs[,1]+0.8, rownames(coefs), cex=0.85)
    axis(1, col="white")
    axis(2, col="white", las=1)
    axis(3, col="white")
    axis(4, col="white", las=1)
    
  }, height=800) 
})


createExcelFiles <- function() {
  load("labels05.rda")
  load("AllAvgSdsDec05.rda")
  
  labs <- sapply(strsplit(labels, split=" "), '[', 1)
  labs[1] <- "."
  names(labels) = labs
  
  AllAvgSdsN <- AllAvgSds
  for (i in seq_along(AllAvgSds)) {
    AllAvgSdsN[[i]] <- round(AllAvgSds[[i]][,c(labs[nchar(labs) == 1], labs[nchar(labs) == 2])],2)
    nams <- labels[c(labs[nchar(labs) == 1], labs[nchar(labs) == 2])]
    nams <- paste0("ISCO ", substr(paste0(sapply(strsplit(nams, ' '), '[', 1), "xxxx"), 1, 4), " ",
                   sapply(sapply(strsplit(nams, ' '), '[', -1), paste, collapse=" "))
    nams[1] <- "Country"
    
    colnames(AllAvgSdsN[[i]]) <- nams
    AllAvgSdsN[[i]] <- AllAvgSdsN[[i]][order(rownames(AllAvgSdsN[[i]] )),][-4,] # remove austria due to lack of data
  }
  
  library("xlsx")
  write.xlsx(AllAvgSdsN[[4]], file="occupationsPISA2012dec.xlsx", sheetName="MATH averages", append=FALSE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[5]], file="occupationsPISA2012dec.xlsx", sheetName="READ averages", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[6]], file="occupationsPISA2012dec.xlsx", sheetName="SCIE averages", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[7]], file="occupationsPISA2012dec.xlsx", sheetName="MATH sd", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[8]], file="occupationsPISA2012dec.xlsx", sheetName="READ sd", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[9]], file="occupationsPISA2012dec.xlsx", sheetName="SCIE sd", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[1]], file="occupationsPISA2012decC.xlsx", sheetName="population share", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[2]], file="occupationsPISA2012dec.xlsx", sheetName="number of students", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[3]], file="occupationsPISA2012dec.xlsx", sheetName="number od schools", append=TRUE, showNA=FALSE)
}






plotFlatHtree <- function(flatHtree, x, y, size, label, color, range) {
  bp <- ggplot(aes_string(x = x, y = y, size = size, label=label, color = color), data=flatHtree)
  bp + geom_point() + theme_bw() +  scale_color_brewer(palette="RdYlBu") +
    geom_text(size=(2.8-sqrt(flatHtree[,x]))*2.8, hjust=0, vjust=0.5, x=0.1 + flatHtree[,x]) + 
    scale_x_continuous(limits = c(0, 3.2)) + 
    scale_y_continuous("", limits = c(range[1], range[2])) + 
    theme(plot.title = element_text(face="bold", size=14), 
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(face="bold", size=12, angle=90),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          legend.position = "none", 
          legend.title = element_blank(), 
          legend.text = element_text(size=12),
          panel.border = element_rect(linetype = "dotted", colour = "white"),
          legend.key = element_blank() 
    )
}

plotSlopeHtree <- function(val1, val2, gr1, gr2, lab1, lab2, col1="black", col2="black", 
                           lev1=1, lev2=1, rang=range(c(val1, val2), na.rm=TRUE)) {
  flatHtree <- rbind(
    data.frame(cnt = 0, avg = val1, lab= lab1, color=col1, level=lev1, gr=gr1),
    data.frame(cnt = 1, avg = val2, lab= lab2, color=col2, level=lev2, gr=gr2))
  
  ggplot(data = flatHtree, aes(x = cnt, y = avg, group=gr, color=color)) + 
    geom_line(lwd=2) +
    geom_text(aes(label = lab, x=cnt*1.4 - 0.2 , hjust = 1-cnt, size=level)) + 
    scale_size_continuous(range=c(3,7)) + 
    theme_bw()+
    scale_color_brewer(palette = "RdYlBu") + 
    scale_x_continuous("", limits = c(-3,4)) + 
    scale_y_continuous("", limits = rang) + 
    theme(plot.title = element_text(face = "bold", size = 14), 
          axis.title.x = element_blank(), axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(), axis.title.y = element_text(face = "bold", size = 12, angle = 90), panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), legend.position = "none", 
          legend.title = element_blank(), legend.text = element_text(size = 12), 
          panel.border = element_rect(linetype = "dotted", colour = "white"), legend.key = element_blank())
  
}

getHFlatAverages <- function(Htree, vname, cname, CI = NULL, level=0) {
  inds <- which(sapply(Htree, class) == "Hgroup")
  pre <- NULL
  if (length(inds) > 0) {
    pre <- do.call(rbind, 
                   lapply(inds, function(x) getHFlatAverages(Htree[[x]], vname, cname, CI=CI, level+1))
    )
  }
  if (!any(is.na(c(Htree[[vname]]["average"], Htree[cname])))) {
    CIs <- NULL
    if (!is.null(CI))
      CIs <- quantile(Htree[[vname]][["replicates"]], c(1-CI, 1+CI)/2, na.rm=TRUE)
    pre <- rbind(pre,
                 as.data.frame(c(level=level, Htree[[vname]]["average"], Htree[cname], CIs)))
  }
  pre
}

