list.of.packages <- c("shiny", "ggplot2","colourpicker")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){
  if (!require("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")}
  BiocManager::install("scales")
  install.packages('shiny')
  install.packages("gplots")
  install.packages("colourpicker")
  
}

library(shiny)
library(ggplot2)
library(colourpicker)
library(gplots)

git.dir   = "https://raw.githubusercontent.com/johncsantiago/Lactate-Correlation-Manuscript/master/Data/"
cpmdata   = read.csv(paste0(git.dir,"LacCorCpmData.csv"), row.names = 1)
groups    = read.csv(paste0(git.dir,"Metadata.csv"), row.names = 1)
annot     = read.csv(paste0(git.dir,"AnnotationData.csv"),row.names=1)

groups=groups[order((paste0(substr(row.names(groups),1,2),(3-as.numeric(substr(row.names(groups),3,3))))),decreasing=T),]
meancpm=cpmdata[,1:12]
sdcpm=cpmdata[,1:12]
colnames(meancpm)=unique(groups$Group)
colnames(sdcpm)=unique(groups$Group)
i=1
while(i<=ncol(meancpm)){
  meancpm[,colnames(meancpm)[i]]=apply(cpmdata[,row.names(groups)[groups$Group==colnames(meancpm)[i]]],1,mean)
  sdcpm[,colnames(sdcpm)[i]]=apply(cpmdata[,row.names(groups)[groups$Group==colnames(sdcpm)[i]]],1,sd)
  i=i+1
}

clicktemp=data.frame(samplename=1,sample=1,cpm=1)
clicktemp=clicktemp[0,]
clickreset=clicktemp

ui <- fluidPage(
  titlePanel("Human Liver Data: Plot Specific Gene Expression Data"),
  sidebarPanel(
    h5("Description: "),
    p("Plot the mean gene expression data and standard deviation for any specific gene in the human liver dataset. Options allow figure customization. Click on data points to get raw cpm data for replicates."),
    br(),
    selectizeInput("gene", label = "Select Gene", 
                   choices = annot[,2],
                   options = list(create = TRUE,
                   ##maxOptions = 5,
                   placeholder = 'select a gene name'),
                   selected = "LARP1"),
    p("Select Gene: Use gene symbol if available. autocompletes"),
    br(), 
    
    selectInput("colselect", h5("Select Line Color"), 
                choices = list("Choose Condition" = 0,
                               "Lean Viable"      = 1, 
                               "Lean Nonviable"   = 2,
                               "Fatty Viable"     = 3,
                               "Fatty Nonviable"  = 4),
                    selected = 0),
    p("Select Line Color: choose line and fill colors for selected sample condition data."),
    br(), 
    
    conditionalPanel(
      condition = "input.colselect == 1",
      colourInput("LV.line.color", "Line Color",
                  "darkgreen",showColour = 'background'),
      colourInput("LV.fill.color", "Line Color",
                  "limegreen",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 2",
      colourInput("LN.line.color", "Line Color",
                  "firebrick3",showColour = 'background'),
      colourInput("LN.fill.color", "Line Color",
                  "red",showColour = 'background')),
      
      conditionalPanel(
        condition = "input.colselect == 3",
        colourInput("FV.line.color", "Line Color",
                    "royalblue3",showColour = 'background'),
        colourInput("FV.fill.color", "Line Color",
                    "deepskyblue",showColour = 'background')),
      
      conditionalPanel(
        condition = "input.colselect == 4",
        colourInput("FN.line.color", "Line Color",
                    "gold3",showColour = 'background'),
        colourInput("FN.fill.color", "Line Color",
                    "yellow",showColour = 'background')),
    
    sliderInput("size", h5("Size"), min = 0, max = 5, value = 2, step=.1),
    p("Size: select size for points and line thickness."),
    br(),
    
    selectInput("rawdata", h5("Include CPM Data"),
                choices = list("Include"   = 1,
                               "Exclude"   = 2),
                selected="Include"),
    p("Include CPM Data: select whether to include the raw data traces or not"),
    
    sliderInput("back", h5("Background"), min = 0, max = 1, value = 1,step=.01),
    p("Background: select the plot background color across black to white gradient"),
    br(), 
    width = 3),
  mainPanel(
    plotOutput(outputId = "plot", 
             click = "clicked",
             height=700, width=700))
  
  )


server = shinyServer(function(input, output) {

  mean.data=reactive({
    gene.name=row.names(annot[annot[,2]==input$gene,])
    gene.data=meancpm[gene.name,]
    x=c(1:3,5:7,9:11,13:15)
    gene.data=rbind(gene.data,x)
    return(gene.data)
  })
  
  sd.data=reactive({
    gene.name=row.names(annot[annot[,2]==input$gene,])
    gene.data=sdcpm[gene.name,]
    return(gene.data)
  })
  
  clickdata=reactive({
    gene.mean=mean.data()
    temp=t(gene.mean)
    colnames(temp)=c("Y","X")
    cdata=data.frame(samplename=row.names(temp),sample=temp[,"X"], cpm=temp[,"Y"])
    row.names(cdata)=row.names(temp)
    cdata=nearPoints(cdata,input$clicked,
                     xvar="sample",yvar="cpm")
    clicktemp<<-rbind(clicktemp,cdata)
    if(nrow(clicktemp)>1){
      if(clicktemp[nrow(clicktemp),1]==clicktemp[nrow(clicktemp)-1,1]){
        clicktemp<<-clickreset
      }
    }
    return(clicktemp)
  })
  
  output$plot <- renderPlot({
    gene.mean = mean.data()
    gene.mean = gene.mean[1,]
    gene.sd   = sd.data()
    highlight=clickdata()
    maxlim=max(gene.mean+gene.sd)
    minlim=min(gene.mean-gene.sd)
    times=c(" 0H", " 3H", " 6H")
    x=c(1:3,5:7,9:11,13:15)
    arrow.colors=c(rep(input$LV.line.color,3),
                   rep(input$LN.line.color,3),
                   rep(input$FV.line.color,3),
                   rep(input$FN.line.color,3))
    
    sdy=gene.mean[1,1]-gene.sd[1,1]
    sdy1=gene.mean[1,1]+gene.sd[1,1]
    gene.name=row.names(annot[annot[,2]==input$gene,])
    
    par(mar=c(5,4,4,5))
    
    plot.new()
    col.pan <- colorpanel(101, "black","white")
    # Change the plot region color
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = col.pan[(input$back*100)+1]) # Color
    par(new = TRUE)
    
    cats=unique(substring(colnames(meancpm),1,2))
    actualdata=matrix(rep(NA,36),ncol=3)
    colnames(actualdata)=c(1:3)
    ##actualdata=actualdata[0,]
    xcoords=actualdata
    i=1
    while(i<=4){
      j=1
      while(j<=3){
        temp=cpmdata[gene.name,row.names(groups)[paste0(groups$fat,groups$viability,groups$replicate)==paste0(cats[i],j)]]
        temp=(temp[1,order(substring(colnames(temp),3,3))])
        actualdata[((i-1)*3)+j,1:length(temp)]=as.numeric(temp)
        temp=(c(((i-1)*4)+1,((i-1)*4)+2,((i-1)*4)+3))
        xcoords[((i-1)*3)+j,]=temp
        j=j+1
      }
      i=i+1
    }


    
    
    plot(x=NA, y=NA, 
         xlim = c(0,15), 
         ylim = c(minlim,maxlim),
         lty=2,
         ylab= "Mean CPM", xlab="",
         main=input$gene, xaxt="n",
         cex.axis=1.25, cex.lab=1.25)
    
    axis(side=1, at=x, labels = paste0(substring(colnames(gene.mean),1,2), times[as.numeric(substring(colnames(gene.mean),3,3))]),las=2, cex.axis=1.25)

    if(input$rawdata==1){
      i=1
      while(i<=12){
        lines(x=xcoords[i,], y=actualdata[i,],
              col=col.pan[((((.6-input$back)/abs(.6-input$back))+1)*50)+1],
              lwd=input$size*.4,lty=2)
        i=i+1
      }
    }


        
    lines(x=1:3, y=gene.mean[1:3],
          col=input$LV.line.color,
          lwd=input$size*.75)
    
    lines(x=5:7, y=gene.mean[4:6],
          col=input$LN.line.color,
          lwd=input$size*.75)
    lines(x=9:11, y=gene.mean[7:9],
          col=input$FV.line.color,
          lwd=input$size*.75)
    lines(x=13:15, y=gene.mean[10:12],
          col=input$FN.line.color,
          lwd=input$size*.75)

    i=1
    while(i<=12){
      sdy=gene.mean[1,i]-gene.sd[1,i]
      sdy1=gene.mean[1,i]+gene.sd[1,i]
      arrows(x0=x[i], y0=sdy, x1=x[i], y1=sdy1, 
             code=3, angle=90, length=0.1,
             col=arrow.colors[i],
             lwd=input$size*.75)
      i=i+1
    }
    
    points(x=1:3, y=gene.mean[1:3],
           col = input$LV.line.color,
           bg  = input$LV.fill.color,
           cex=input$size, pch=21)    
    points(x=5:7, y=gene.mean[4:6],
           col = input$LN.line.color,
           bg  = input$LN.fill.color,
           cex=input$size, pch=21)
    points(x=9:11, y=gene.mean[7:9],
           col = input$FV.line.color,
           bg  = input$FV.fill.color,
           cex=input$size, pch=21)
    points(x=13:15, y=gene.mean[10:12],
           col = input$FN.line.color,
           bg  = input$FN.fill.color,
           cex=input$size, pch=21)
    
    points(x=highlight$sample[nrow(highlight)], 
           y=highlight$cpm[nrow(highlight)],
           col = "yellow",
           bg  = "red",
           cex=input$size+1, pch=21)

    if(nrow(highlight)>0){
      if(highlight$samplename[nrow(highlight)]!="FN3"){
      legend(x=highlight$sample[nrow(highlight)]+1.5, 
             y=highlight$cpm[nrow(highlight)]+((maxlim-minlim)*.125),
             legend=c(paste0((row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][1])," CPM: ",
                             signif(cpmdata[gene.name,row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][1]],3),
                             "\n",(row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][2])," CPM: ",
                             signif(cpmdata[gene.name,row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][2]],3),
                             "\n",(row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][3])," CPM: ",
                             signif(cpmdata[gene.name,row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][3]],3))),
             xjust = 0.5,
             yjust = 1,
             x.intersp = -0.5,
             y.intersp = 1,
             adj = c(0, 0.2),
             cex=.6, bg="khaki",xpd=T)
      }
      if(highlight$samplename[nrow(highlight)]=="FN3"){
        legend(x=highlight$sample[nrow(highlight)]+1.5, 
               y=highlight$cpm[nrow(highlight)]+((maxlim-minlim)*.125),
               legend=c(paste0((row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][1])," CPM: ",
                               signif(cpmdata[gene.name,row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][1]],3),
                               "\n",(row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][2])," CPM: ",
                               signif(cpmdata[gene.name,row.names(groups)[groups$Group==highlight$samplename[nrow(highlight)]][2]],3))),
               xjust = 0.5,
               yjust = 1,
               x.intersp = -0.5,
               y.intersp = 1,
               adj = c(0, 0.2),
               cex=.6, bg="khaki",xpd=T)
      }
    }

    
    legend(x=16, y=minlim+((maxlim-minlim)*.95), 
           legend=unique(substr(colnames(gene.mean),1,2)),xpd=T,
           fill=c(input$LV.fill.color, input$LN.fill.color, 
                  input$FV.fill.color, input$FN.fill.color),
           bg=col.pan[(input$back*100)+1],
           text.col=col.pan[((((.6-input$back)/abs(.6-input$back))+1)*50)+1])
  }, res=96)
  
})



shinyApp(ui = ui, server = server, options)