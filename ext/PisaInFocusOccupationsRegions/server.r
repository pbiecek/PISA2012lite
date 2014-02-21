## server.r

#setwd("/Users/pbiecek/camtasia/Dropbox/TJA Fellowship Biecek/reports/PISAinFocusSummaryApp/appReg/")
#setwd("c:/_Przemek_/Dropbox/TJA Fellowship Biecek/reports/PISAinFocusSummaryApp/app")

#regCodes <- readLines("regionsCodes.txt")
#regNames <- substr(regCodes, 7, 55)
#names(regNames) <- substr(regCodes, 1, 5)

load("regNames.rda")

library(ggplot2)
library(RColorBrewer)
kol9 <- brewer.pal(n=9, name="RdYlBu")
kol9 <- c("#a60C00", "#E63600", "#E67100", "#E6A700", "#46a604", "#00B6E6", "#00AAE6", "#008Dc6", "#0063a6")
kol10 <- c("#777777", "#a60C00", "#E63600", "#E67100", "#E6A700", "#46a604", "#00B6E6", "#00AAE6", "#008Dc6", "#0063a6", "#000000")

load("labels05.rda")
labels[30] <- "6 Skilled agricultural, forestry and fishery workers"
labels[34] <- "7 Craft and related trades workers"
labels[40] <- "8 Plant and machine operators, and assemblers"

load("AllAvgSdsDec05b_regions.rda")
AllAvgSdsREG <- AllAvgSds
load("AllAvgSdsDec05.rda")
minStudents = 30
minSchools = 5

#input = list(variable="Poland",variable1="Germany",subject="MATH",range=c(450,650))

addSpace <- function(x, delta) {
  orderOnThisLevel <- rev(order(x))
  lastp <- x[orderOnThisLevel[1]]
  for (tmpi in orderOnThisLevel[-1]) {
    if (x[tmpi] > lastp - delta) 
      x[tmpi] <- lastp - delta
    lastp <- x[tmpi]
  }
  x
}

shinyServer(function(input, output) {
  
  output$TwoCnts <- renderPlot({
    datasource <- AllAvgSds
    datasource2 <- AllAvgSds
    cntName <- input$variable
    cntNameS <- input$variable
    cntName2 <- input$variable1
    cntName2S <- input$variable1
    # regions
    if (input$variable == "Belgium" && input$variableBelgium != "All") { datasource <- AllAvgSdsREG
                                                                         cntName <- paste0("BEL.",input$variableBelgium)
                                                                         cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableBelgium]))
    }
    if (input$variable == "Canada" && input$variableCanada != "All") { datasource <- AllAvgSdsREG
                                                                       cntName <- paste0("CAN.",input$variableCanada)
                                                                       cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableCanada]))
    }
    if (input$variable == "Spain" && input$variableSpain != "All") { datasource <- AllAvgSdsREG
                                                                     cntName <- paste0("ESP.",input$variableSpain)
                                                                     cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableSpain]))
    }
    if (input$variable == "United Kingdom" && input$variableUK != "All") { datasource <- AllAvgSdsREG
                                                                           cntName <- paste0("GBR.",input$variableUK)
                                                                           cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableUK]))
    }
    if (input$variable == "Italy" && input$variableItaly != "All") { datasource <- AllAvgSdsREG
                                                                     cntName <- paste0("ITA.",input$variableItaly)
                                                                     cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableItaly]))
    }
    if (input$variable1 == "Belgium" && input$variable2Belgium != "All") { datasource2 <- AllAvgSdsREG
                                                                           cntName2 <- paste0("BEL.",input$variable2Belgium)
                                                                           cntName2S <- paste0(input$variable1,".",names(regNames[regNames == input$variable2Belgium]))
    }
    if (input$variable1 == "Canada" && input$variable2Canada != "All") { datasource2 <- AllAvgSdsREG
                                                                         cntName2 <- paste0("CAN.",input$variable2Canada)
                                                                         cntName2S <- paste0(input$variable1,".",names(regNames[regNames == input$variable2Canada]))
    }
    if (input$variable1 == "Spain" && input$variable2Spain != "All") { datasource2 <- AllAvgSdsREG
                                                                       cntName2 <- paste0("ESP.",input$variable2Spain)
                                                                       cntName2S <- paste0(input$variable1,".",names(regNames[regNames == input$variable2Spain]))
    }
    if (input$variable1 == "United Kingdom" && input$variable2UK != "All") { datasource2 <- AllAvgSdsREG
                                                                             cntName2 <- paste0("GBR.",input$variable2UK)
                                                                             cntName2S <- paste0(input$variable1,".",names(regNames[regNames == input$variable2UK]))
    }
    if (input$variable1 == "Italy" && input$variable2Italy != "All") { datasource2 <- AllAvgSdsREG
                                                                       cntName2 <- paste0("ITA.",input$variable2Italy)
                                                                       cntName2S <- paste0(input$variable1,".",names(regNames[regNames == input$variable2Italy]))
    }
    
    isubject <- toupper(substr(input$subject,1,4))
    inRange <- mean( datasource[[paste0(isubject, "avg")]][cntName,]  <= input$range[2] & datasource[[paste0(isubject, "avg")]][cntName,]  >= input$range[1], na.rm=TRUE)
    
    levs1 <- nchar(colnames(datasource[[paste0(isubject, "avg")]]))
    gr1 <- labels[levs1 == 1]; gr1[1] = paste0(" ",cntNameS)
    gr2 <- labels[levs1 == 1]; gr2[1] = paste0(" ",cntName2S)
    val1 = datasource[[paste0(isubject, "avg")]][cntName,levs1 == 1]
    val2 = datasource2[[paste0(isubject, "avg")]][cntName2,levs1 == 1]
    val12 = val1
    val22 = val2
    delta <- diff(input$range)/100
    
    val12[2:length(val12)] <- addSpace(val12[2:length(val12)], delta*1.5)
    val22[2:length(val22)] <- addSpace(val22[2:length(val22)], delta*1.5)
    
    pl <- plotSlopeHtree(val1 = val1, val2 = val2,
                         gr1 = gr1, gr2 = gr1, 
                         lab1 = gr1, lab2 = gr2, 
                         col1 = factor(substr(labels,1,1)[levs1 == 1]), col2 = factor(substr(labels,1,1)[levs1 == 1]),
                         lev1 = c(1.5,rep(1,9)), lev2 = c(1.5,rep(1,9)),
                         rang = input$range,
                         val12 = val12, val22 = val22)+ scale_size_continuous(range=c(5,10))
    if (inRange < 0.01){
      df <- data.frame()
      pl <- ggplot(df) + geom_blank() + xlim(0, 10) + ylim(input$range[1], input$range[2]) + theme_bw()
    }

    if (inRange < 0.7)
      pl <- pl + ggtitle("NOTE THAT MOST OF AVERAGES FALL OUTSIDE OF THIS PLOT!\n CHANGE THE VERTICAL AXIS ON THE LEFT PANEL!") + theme(plot.title=element_text(size=20))
    
    print(pl)
    
      
  }, height=800)

  output$TwoCnts2 <- renderPlot({
    isubject <- toupper(substr(input$subject,1,4))
    inRange <- mean( AllAvgSds[[paste0(isubject, "avg")]][input$variable,]  <= input$range[2] & AllAvgSds[[paste0(isubject, "avg")]][input$variable,]  >= input$range[1], na.rm=TRUE)
    
    levs1 <- nchar(sapply(strsplit(labels, split=" "), '[', 1))
    gr1 <- labels; gr1[1] = input$variable
    gr2 <- labels; gr2[1] = input$variable1

    val1 =  AllAvgSds[[paste0(isubject, "avg")]][input$variable,]
    val2 = AllAvgSds[[paste0(isubject, "avg")]][input$variable1,]
    val12 = val1
    val22 = val2
    delta <- diff(input$range)/100
    
    val12[which(!is.na(val12))] <- addSpace(val12[which(!is.na(val12))], delta*1.5)
    val22[which(!is.na(val22))] <- addSpace(val22[which(!is.na(val22))], delta*1.5)
    
    pl <- plotSlopeHtree(val1 =val1, val2 = val2,
                        gr1 = gr1, gr2 = gr1, 
                        lab1 = gr1, lab2 = gr2, 
                        col1 = factor(substr(labels,1,1)), col2 = factor(substr(labels,1,1)),
                        lev1 = (3-levs1)/2, lev2 = (3-levs1)/2,
                        rang = input$range,
                        val12 = val12, val22 = val22)+ scale_size_continuous(range=c(3.5,10))
    
    if (inRange < 0.01){
      df <- data.frame()
      pl <- ggplot(df) + geom_blank() + xlim(0, 10) + ylim(input$range[1], input$range[2]) + theme_bw()
    }
    if (inRange < 0.7)
      pl <- pl + ggtitle("NOTE THAT MOST OF AVERAGES FALL OUTSIDE OF THIS PLOT!\n CHANGE THE VERTICAL AXIS ON THE LEFT PANEL!") + theme(plot.title=element_text(size=20))
    
    print(pl)
    
  }, height=800)

  output$Trees <- renderPlot({
    isubject <- toupper(substr(input$subject,1,4))
    eMeans <- AllAvgSds[[paste0(isubject, "avg")]][input$variable,]
    eSd    <- AllAvgSds[[paste0(isubject, "sd")]][input$variable,]
    eSize  <- AllAvgSds[["struct"]][input$variable,]
    eStud  <- AllAvgSds[["studs"]][input$variable,]
    eSchool <- AllAvgSds[["schools"]][input$variable,]
    
#    par(mar=c(1,10,20,15))
    par(omd=c(0.1,0.75,0.01,0.65), mar=c(0,0,0,0))
    plot(seq_along(eMeans), eMeans, pch=19, cex=2*sqrt(eSize/median(eSize, na.rm=TRUE)), las=1, xaxt="n", xlab="", ylab="", main="", type="n", ylim=input$range, yaxt="n")
    axis(2, las=1, seq(round(input$range[1],-1), input$range[2], 10))
    #axis(4, las=1, seq(round(input$range[1],-1), input$range[2], 10))
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
    
    for (ii in 1:9)
      axis(4, las=1, eMeans[paste(ii)], labels[grep(labels, pattern=paste0("^", ii, " "))], cex.axis=0.95, col.axis=kol9[ii])
    axis(3, 1, input$variable, las=2, cex.axis=0.95, col.axis="#777777")
    points(1, eMeans[1], pch=19, cex=sqrt(2*sqrt(eSize["."]/median(eSize, na.rm=TRUE))), col="#777777")
    if (input$showCI)
      lines(c(1,1), eMeans["."] + c(-1,1)*eSd["."], col="#777777")
    
    for (ii in seq_along(eMeans))
      if (nchar(names(eMeans)[ii]) > 1)
        axis(3, ii, labels[ii], las=2, cex.axis=0.95, 
             col.axis=kol9[as.numeric(substr(labels[ii], 1, 1))])
    
    }, height=800)
  
  output$DownloadZone <- renderText({
    cname <- gsub(input$variable, pattern="[^A-Za-z]", replacement="")
    
    HTML(paste0("<br/><br/>",
                "<a href='http://www.oecd.org/pisa/pisaproducts/pisainfocus/PISA-in-Focus-n36-(eng)-FINAL.pdf'>Here you can download the Pisa In Focus article</a>,</br></br>",
                "<a href='occupationsPISA2012.xls'>Here you can download data as Excel file</a>,<br/><br/>",
                "<a href='OccupationsPISA2012.pdf'>Here you can download graphical one page country profiles</a>.<br/><br/>"))
  })

  output$ColorTrees <- renderPlot({
    datasource <- AllAvgSds
    cntName <- input$variable
    cntNameS <- input$variable
    # regions
    if (input$variable == "Belgium" && input$variableBelgium != "All") { datasource <- AllAvgSdsREG
                                                                     cntName <- paste0("BEL.",input$variableBelgium)
                                                                     cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableBelgium]))
    }
    if (input$variable == "Canada" && input$variableCanada != "All") { datasource <- AllAvgSdsREG
                                                                     cntName <- paste0("CAN.",input$variableCanada)
                                                                     cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableCanada]))
    }
    if (input$variable == "Spain" && input$variableSpain != "All") { datasource <- AllAvgSdsREG
                                                                     cntName <- paste0("ESP.",input$variableSpain)
                                                                     cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableSpain]))
    }
    if (input$variable == "United Kingdom" && input$variableUK != "All") { datasource <- AllAvgSdsREG
                                                                     cntName <- paste0("GBR.",input$variableUK)
                                                                     cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableUK]))
    }
    if (input$variable == "Italy" && input$variableItaly != "All") { datasource <- AllAvgSdsREG
                                                                     cntName <- paste0("ITA.",input$variableItaly)
                                                                     cntNameS <- paste0(input$variable,".",names(regNames[regNames == input$variableItaly]))
    }
    
    isubject <- toupper(substr(input$subject,1,4))
    inRange <- mean( datasource[[paste0(isubject, "avg")]][cntName,]  <= input$range[2] & datasource[[paste0(isubject, "avg")]][cntName,]  >= input$range[1], na.rm=TRUE)
    
    flatHtree <- na.omit(data.frame(
      level = c(0,0.4,1.9)[nchar(sapply(strsplit(labels, split=" "), '[', 1))+1],
      average = datasource[[paste0(isubject, "avg")]][cntName,],
      nameLong = sapply(sapply(strsplit(labels, split=" "), '[', -1), paste, collapse=" "),
      struct = datasource[["struct"]][cntName,],
      studs = datasource[["studs"]][cntName,],
      schools = datasource[["schools"]][cntName,],
      color = factor(substr(labels, 1, 1)), stringsAsFactors = FALSE))
    flatHtree[1,3] = cntNameS
    flatHtree <- flatHtree[flatHtree$studs >= 30 & flatHtree$schools >=5 , ]

    delta <- diff(input$range)/100
    flatHtree$average2 <- flatHtree$average

   # remove overlaps
   flatHtree$average2[which(flatHtree$level == 0.4)] <- addSpace(flatHtree$average2[which(flatHtree$level == 0.4)], delta*1.9)
   flatHtree$average2[which(flatHtree$level == 1.9)] <- addSpace(flatHtree$average2[which(flatHtree$level == 1.9)], delta*1.3)
   
    pl <- plotFlatHtree(flatHtree, 
                        x = "level", y = "average", size = "struct", label="nameLong", color = "color",
                        range = input$range, y2="average2") + scale_size_continuous(range=c(3.5,12)) 
     
    if (inRange < 0.01){
      df <- data.frame()
      pl <- ggplot(df) + geom_blank() + xlim(0, 10) + ylim(input$range[1], input$range[2]) + theme_bw()
    }
    
    if (inRange < 0.7)
      pl <- pl + ggtitle("NOTE THAT MOST OF AVERAGES FALL OUTSIDE OF THIS PLOT!\n CHANGE THE VERTICAL AXIS ON THE LEFT PANEL!") + theme(plot.title=element_text(size=20))
    
    print(pl)
  }, height=800)

  output$Map <- renderPlot({
    isubject <- toupper(substr(input$subject,1,4))
    level = nchar(sapply(strsplit(labels, split=" "), '[', 1))
    coefs <- cbind(
      apply(AllAvgSds[[paste0(isubject, "avg")]], 1, function(x) quantile(x[level==1], .25, na.rm=TRUE)),
      apply(AllAvgSds[[paste0(isubject, "avg")]], 1, function(x) x[1]),
      apply(AllAvgSds[[paste0(isubject, "avg")]], 1, function(x) quantile(x[level==1], .75, na.rm=TRUE)))
    
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
  
  load("AllAvgSdsDec05b_regions.rda")
  AllAvgSdsREG <- AllAvgSds
  load("AllAvgSdsDec05.rda")
  
  labs <- sapply(strsplit(labels, split=" "), '[', 1)
  labs[1] <- "."
  names(labels) = labs
  
  AllAvgSdsN <- AllAvgSds
  for (i in seq_along(AllAvgSds)) {
    tmp <- round(AllAvgSdsREG[[i]][,c(labs[nchar(labs) == 1], labs[nchar(labs) == 2])],2)
    for (k in 1:nrow(tmp))
      rownames(tmp)[k] <- paste0(substr(rownames(tmp)[k], 1, 3), ".", names(regNames[regNames == substr(rownames(tmp)[k], 5, 25)]))
    tmp <- tmp[!is.na(sapply(strsplit(rownames(tmp), split="\\."), '[', 2)), ]

    tmp2 <- AllAvgSds[[i]][,c(labs[nchar(labs) == 1], labs[nchar(labs) == 2])]
    tmp2 <- tmp2[order(rownames(tmp2)),][-4,]
    
    AllAvgSdsN[[i]] <- 
      round(rbind(tmp2,tmp),2)
    
    nams <- labels[c(labs[nchar(labs) == 1], labs[nchar(labs) == 2])]
    nams <- paste0("ISCO ", substr(paste0(sapply(strsplit(nams, ' '), '[', 1), "xxxx"), 1, 4), " ",
                   sapply(sapply(strsplit(nams, ' '), '[', -1), paste, collapse=" "))
    nams[1] <- "Country"
    
    colnames(AllAvgSdsN[[i]]) <- nams
    AllAvgSdsN[[i]] <- AllAvgSdsN[[i]]
  }
  
  for (k in 4:9)
    AllAvgSdsN[[k]][AllAvgSdsN[[2]] < 35 | AllAvgSdsN[[3]] < 5] = NA
  
  library("xlsx")
  write.xlsx(AllAvgSdsN[[4]], file="occupationsPISA2012decReg.xlsx", sheetName="MATH averages", append=FALSE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[5]], file="occupationsPISA2012decReg.xlsx", sheetName="READ averages", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[6]], file="occupationsPISA2012decReg.xlsx", sheetName="SCIE averages", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[7]], file="occupationsPISA2012decReg.xlsx", sheetName="MATH sd", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[8]], file="occupationsPISA2012decReg.xlsx", sheetName="READ sd", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[9]], file="occupationsPISA2012decReg.xlsx", sheetName="SCIE sd", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[1]], file="occupationsPISA2012decReg.xlsx", sheetName="population share", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[2]], file="occupationsPISA2012decReg.xlsx", sheetName="number of students", append=TRUE, showNA=FALSE)
  write.xlsx(AllAvgSdsN[[3]], file="occupationsPISA2012decReg.xlsx", sheetName="number od schools", append=TRUE, showNA=FALSE)
  
}






plotFlatHtree <- function(flatHtree, x, y, size, label, color, range, y2) {
  bp <- ggplot(aes_string(x = x, y = y, size = size, label=label, color = color), data=flatHtree)
  bp + geom_point() + theme_bw() +
    scale_color_manual(values=kol10) +
    #    scale_color_brewer(palette = "RdYlBu") + 
    geom_text(aes_string(y = y2),size=(2.8-sqrt(flatHtree[,x]))*2.8, hjust=0, vjust=0.5, x=0.1 + flatHtree[,x]) + 
    scale_x_continuous(limits = c(0, 3.2)) + 
    scale_y_continuous("", limits = c(range[1], range[2])) + 
    geom_text(x=-0.1, y=range[2]+2, label="PISA score", size=6, colour="#777777", hjust=0, vjust=0.5) + 
    geom_text(x=0.38, y=range[2]+2, label="Results by\nparents' major occupational groups", size=6, colour="#777777", hjust=0, vjust=0.5) + 
    geom_text(x=1.88, y=range[2]+2, label="Results by\nparents' detailed occupational groups", size=6, colour="#777777", hjust=0, vjust=0.5)  + 
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
                           lev1=1, lev2=1, rang=range(c(val1, val2), na.rm=TRUE), val12, val22) {
  flatHtree <- rbind(
    data.frame(cnt = 0, avg = val1, lab= lab1, color=col1, level=lev1, gr=gr1, avg2=val12),
    data.frame(cnt = 1, avg = val2, lab= lab2, color=col2, level=lev2, gr=gr2, avg2=val22))
  
  ggplot(data = flatHtree, aes(x = cnt, y = avg, group=gr, color=color)) + 
    geom_line(lwd=2) +
    geom_text(aes(label = lab, x=cnt*1.4 - 0.2 , hjust = 1-cnt, size=level, y=avg2)) + 
    scale_size_continuous(range=c(3,7)) + 
    theme_bw()+
#    scale_color_brewer(palette = "RdYlBu") + 
    scale_color_manual(values=kol10) + 
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

