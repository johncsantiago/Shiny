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
library(DT)

#########################
## Hepatocyt data only ##
#########################

##countdata=read.csv("https://raw.githubusercontent.com/johncsantiago/SData/master/FetalAdultRatData/AF_Cells_raw_counts.csv", row.names = 1)
##countdata$ensembl=row.names(countdata)
##annot=countdata[,c("Gene.name","ensembl")]
##colnames(annot)=c("symbol","ensembl")
##groups=read.csv(""https://raw.githubusercontent.com/johncsantiago/SData/master/FetalAdultRatData/AF_Cells_metadata.csv", row.names = 1)
##groups$Group=row.names(groups)
##countdata=countdata[,1:16]

##x <- countdata[,row.names(groups)]
##group <- factor(groups$Group)
##y <- DGEList(counts=x,group=group)
##keep <- filterByExpr(y)
##y <- y[keep,,keep.lib.sizes=FALSE] 
##z <- calcNormFactors(y, method = "TMM") 
##cpmdata=cpm(z)

# Design matrix
##design<-model.matrix(~0+(substring(colnames(z$counts),1,1)))
##colnames(design) <- c(unique(substring(colnames(z$counts),1,1)))

##z = estimateGLMCommonDisp(z,design, verbose=F)
##z = estimateGLMTrendedDisp(z,design)
##z = estimateGLMTagwiseDisp(z,design)
##fit <- glmFit(z, design)

##compare <- makeContrasts(A-D,levels=design)
##lrt <- glmLRT(fit,contrast=as.vector(compare))
##G_X_E<-topTags(lrt, n=nrow(cpmdata),adjust.method="BH", sort.by="PValue")
##Adult_vs_Dual=G_X_E$table

##compare <- makeContrasts(A-L,levels=design)
##lrt <- glmLRT(fit,contrast=as.vector(compare))
##G_X_E<-topTags(lrt, n=nrow(cpmdata),adjust.method="BH", sort.by="PValue")
##Adult_vs_LAP=G_X_E$table

##compare <- makeContrasts(A-S,levels=design)
##lrt <- glmLRT(fit,contrast=as.vector(compare))
##G_X_E<-topTags(lrt, n=nrow(cpmdata),adjust.method="BH", sort.by="PValue")
##Adult_vs_Single=G_X_E$table

##compare <- makeContrasts(D-L,levels=design)
##lrt <- glmLRT(fit,contrast=as.vector(compare))
##G_X_E<-topTags(lrt, n=nrow(cpmdata),adjust.method="BH", sort.by="PValue")
##Dual_vs_LAP=G_X_E$table

##compare <- makeContrasts(D-S,levels=design)
##lrt <- glmLRT(fit,contrast=as.vector(compare))
##G_X_E<-topTags(lrt, n=nrow(cpmdata),adjust.method="BH", sort.by="PValue")
##Dual_vs_Single=G_X_E$table

##compare <- makeContrasts(L-S,levels=design)
##lrt <- glmLRT(fit,contrast=as.vector(compare))
##G_X_E<-topTags(lrt, n=nrow(cpmdata),adjust.method="BH", sort.by="PValue")
##LAP_vs_Single=G_X_E$table

##all.deanalysis=data.frame(AD_logFC = Adult_vs_Dual[row.names(cpmdata),"logFC"],
##                          AD_FDR   = Adult_vs_Dual[row.names(cpmdata),"FDR"],
##                          AL_logFC = Adult_vs_LAP[row.names(cpmdata),"logFC"],
##                          AL_FDR   = Adult_vs_LAP[row.names(cpmdata),"FDR"],
##                          AS_logFC = Adult_vs_Single[row.names(cpmdata),"logFC"],
##                          AS_FDR   = Adult_vs_Single[row.names(cpmdata),"FDR"],
##                          DL_logFC = Dual_vs_LAP[row.names(cpmdata),"logFC"],
##                          DL_FDR   = Dual_vs_Single[row.names(cpmdata),"FDR"],
##                          DS_logFC = Dual_vs_Single[row.names(cpmdata),"logFC"],
##                          DS_FDR   = Dual_vs_Single[row.names(cpmdata),"FDR"],
##                          LS_logFC = LAP_vs_Single[row.names(cpmdata),"logFC"],
##                          LS_FDR   = LAP_vs_Single[row.names(cpmdata),"FDR"])

##write.csv(cpmdata,"/Users/johncsantiago/Documents/GitHub/SData/FetalAdultRatData/AF_Cells_CPM.csv")
##write.csv(annot,"/Users/johncsantiago/Documents/GitHub/SData/FetalAdultRatData/AF_Cells_annot.csv")
##write.csv(all.deanalysis,"/Users/johncsantiago/Documents/GitHub/SData/FetalAdultRatData/AF_Cells_All_DEanalysis.csv")

#############################
## Tissue/Colony Data Only ##
#############################

##countdata=read.csv("https://raw.githubusercontent.com/johncsantiago/SData/master/FetalAdultRatData/AF_Genewiz_all_counts.csv", row.names = 1)
##colnames(countdata)=paste0(substring(colnames(countdata),6,nchar(colnames(countdata))),"_",substring(colnames(countdata),2,4))
##groups=read.csv("https://raw.githubusercontent.com/johncsantiago/SData/master/FetalAdultRatData/AF_Genewiz_metadata.csv", row.names = 1)
##row.names(groups)=paste0(substring(row.names(groups),5,nchar(row.names(groups))),"_",substring(row.names(groups),1,3))

##countdata=countdata[,substring(colnames(countdata),7,9)!="246"]
##groups=groups[substring(row.names(groups),7,9)!="246",]

##x <- countdata[,row.names(groups)]
##group <- factor(groups$adultfetal)
##y <- DGEList(counts=x,group=group)
##keep <- filterByExpr(y)
##y <- y[keep,,keep.lib.sizes=FALSE] 
##z <- calcNormFactors(y, method = "TMM") 
##cpmdata=cpm(z)

# Design matrix
##design<-model.matrix(~0+(substring(colnames(z$counts),1,1)))
##colnames(design) <- c(unique(substring(colnames(z$counts),1,1)))

##z = estimateGLMCommonDisp(z,design, verbose=F)
##z = estimateGLMTrendedDisp(z,design)
##z = estimateGLMTagwiseDisp(z,design)
##fit <- glmFit(z, design)

##compare <- makeContrasts(A-F,levels=design)
##lrt <- glmLRT(fit,contrast=as.vector(compare))
##G_X_E<-topTags(lrt, n=nrow(cpmdata),adjust.method="BH", sort.by="PValue")
##Adult_vs_Fetal=G_X_E$table

##write.csv(cpmdata,"/Users/johncsantiago/Documents/GitHub/SData/FetalAdultRatData/AF_Genewiz_CPM.csv")
##write.csv(groups,"/Users/johncsantiago/Documents/GitHub/SData/FetalAdultRatData/AF_Genewiz_metadata.csv")
##write.csv(Adult_vs_Fetal,"/Users/johncsantiago/Documents/GitHub/SData/FetalAdultRatData/AF_Genewiz_DEanalysis.csv")



git.dir    = "https://raw.githubusercontent.com/johncsantiago/SData/master/FetalAdultRatData/"
cpmdata    = read.csv(paste0(git.dir,"AF_Cells_CPM.csv"),row.names=1)
groups     = read.csv(paste0(git.dir,"AF_Cells_metadata.csv"),row.names=1)
annot      = read.csv(paste0(git.dir,"AF_Cells_annot.csv"),row.names=1)
deanalysis = read.csv(paste0(git.dir,"/AF_Cells_All_DEanalysis.csv"),row.names=1)
##annot=annot[row.names(cpmdata),]
CH.cpmdata = read.csv(paste0(git.dir,"AF_Genewiz_CPM.csv"),row.names=1)
CH.DE      = read.csv(paste0(git.dir,"AF_Genewiz_DEanalysis.csv"),row.names=1)
CH.groups=read.csv(paste0(git.dir,"AF_Genewiz_metadata.csv"), row.names = 1)
colnames(CH.groups)=c("group","replicate")
CH.groups$group[CH.groups$group=="Adult"]="Adult Host"
CH.groups$group[CH.groups$group=="Fetal"]="Fetal Colony"
groups=groups[,c("group","replicate")]
all.groups=rbind(groups,CH.groups)

all.cpmdata=annot[unique(c(row.names(cpmdata),row.names(CH.DE))),]
all.cpmdata[row.names(cpmdata),3:18]=cpmdata
all.cpmdata[row.names(CH.cpmdata),19:26]=CH.cpmdata
all.cpmdata=all.cpmdata[,3:26]

cpmdata=all.cpmdata
groups=all.groups
groups$group=factor(groups$group, levels=c(unique(groups$group)))
annot=annot[row.names(cpmdata),]
all.deanalysis=annot[unique(c(row.names(cpmdata),row.names(CH.DE))),]
all.deanalysis[row.names(deanalysis),3:14]=deanalysis
all.deanalysis[row.names(CH.DE),15:16]=CH.DE[,c("logFC","FDR")]
colnames(all.deanalysis)[15:16]=c("CH_logFC","CH_FDR")
deanalysis=all.deanalysis[,c(3:16)]

gene.choices=annot[,1]
##annot[row.names(deanalysis)[order(deanalysis[,input$choice.order])],1]

ui <- fluidPage(
  titlePanel("Rat Adult-Fetal Hepatocyte Data: Plot Specific Gene Expression Data"),
  sidebarPanel(
    h5("Description: "),
    p("Generates a boxplot for any specific gene in the Rat Adult-Fetal Hepatocyte dataset. Options allow figure customization. Click inside box to highlight significant FDR values in the corresponding deanalysis table sample column"),
    br(),
    uiOutput("geneControls"),
    p("Select Gene: Use gene symbol if available. autocompletes"),
    br(), 
    selectInput("choice.order", h5("Choose Signifigance Order"),
                choices = list("Host~Colony"   = "CH_FDR",
                               "Adult~Dual"    = "AD_FDR",
                               "Adult~LAP"     = "AL_FDR",
                               "Adult~Single"  = "AS_FDR",
                               "Dual~LAP"      = "DL_FDR",
                               "Dual~Single"   = "DS_FDR",
                               "LAP~Single"    = "LS_FDR"),
                selected="CH_FDR"),
    p("Choose Signifigance Order: Organize the 'Select Gene' drop down menu order by FDR observed for a comparison between specific conditions"),
    br(),
    
    selectInput("colselect", h5("Select Fill Color"), 
                choices = list("Choose Condition" = 0,
                               "Adult"            = 1, 
                               "Dual"             = 2,
                               "LAP"              = 3,
                               "Single"           = 4,
                               "Adult Host"       = 5,
                               "Fetal Colony"     = 6),
                selected = 0),
    p("Select Line Color: choose box and point colors for selected sample condition data."),
    br(), 
    
    conditionalPanel(
      condition = "input.colselect == 1",
      colourInput("A.box.color", "Line Color",
                  "forestgreen",showColour = 'background'),
      colourInput("A.point.color", "Line Color",
                  "limegreen",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 2",
      colourInput("D.box.color", "Line Color",
                  "firebrick",showColour = 'background'),
      colourInput("D.point.color", "Line Color",
                  "red",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 3",
      colourInput("L.box.color", "Line Color",
                  "dodgerblue",showColour = 'background'),
      colourInput("L.point.color", "Line Color",
                  "deepskyblue",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 4",
      colourInput("S.box.color", "Line Color",
                  "gold3",showColour = 'background'),
      colourInput("S.point.color", "Line Color",
                  "yellow",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 5",
      colourInput("AH.box.color", "Line Color",
                  "lightslategrey",showColour = 'background'),
      colourInput("AH.point.color", "Line Color",
                  "lightgrey",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 6",
      colourInput("FC.box.color", "Line Color",
                  "plum",showColour = 'background'),
      colourInput("FC.point.color", "Line Color",
                  "plum1",showColour = 'background')),
    
    sliderInput("size", h5("Size"), min = 0, max = 5, value = 2, step=.1),
    p("Size: select line thickness for box and point borders."),
    br(),
    
    fluidRow(
      column(6,
             selectInput("rawdata", h5("Include CPM Data"),
                         choices = list("Include"   = 1,
                                        "Exclude"   = 2),
                         selected="Include"),
             p("Include CPM Data: select whether to overlay the raw data points or not"),
      ),
      
      column(6,
             sliderInput("back", h5("Background"), min = 0, max = 1, value = 1,step=.01),
             p("Background: select the plot background color across black to white gradient"),
      ),
      br(),
    ),
    width = 3),
  
  mainPanel(
    plotOutput(outputId = "plot", 
               click = "clicked",
               height=600, width=900),
    br(),
    DTOutput('data', width = 750),
    tableOutput("data1"),
    p("Table of FDR values generated in the indicated comparison")
  )
  
)

server = shinyServer(function(input, output) {
  
  xdata=reactiveVal()
  observeEvent(input$clicked,{
    xdata(input$clicked$x)
  })
  
  ydata=reactiveVal()
  observeEvent(input$clicked,{
    ydata(input$clicked$y)
  })
  
  output$geneControls =  renderUI({
    gene.choices<<-annot[row.names(deanalysis)[order(deanalysis[,input$choice.order])],1]
    selectizeInput("gene", label = "Select Gene", 
                   choices = gene.choices,
                   options = list(create = TRUE,
                                  ##maxOptions = 5,
                                  placeholder = 'select a gene name'),
                   selected=gene.choices[1])
  })
  
  clicksample=reactiveVal()
  observeEvent(input$clicked,{
    clicksample(NA)
    geneID=annot[annot[,1]==input$gene,2]
    upperhinge=data.frame(
      Adult  = fivenum(as.numeric(cpmdata[geneID,1:4]))[4],
      Dual   = fivenum(as.numeric(cpmdata[geneID,5:8]))[4],
      LAP    = fivenum(as.numeric(cpmdata[geneID,9:12]))[4],
      Single = fivenum(as.numeric(cpmdata[geneID,13:16]))[4],
      'Adult Host' = fivenum(as.numeric(cpmdata[geneID,c(17,19,21,23)]))[4],
      'Fetal Colony' = fivenum(as.numeric(cpmdata[geneID,c(18,20,22,24)]))[4])
    
    lowerhinge=data.frame(
      Adult  = fivenum(as.numeric(cpmdata[geneID,1:4]))[2],
      Dual   = fivenum(as.numeric(cpmdata[geneID,5:8]))[2],
      LAP    = fivenum(as.numeric(cpmdata[geneID,9:12]))[2],
      Single = fivenum(as.numeric(cpmdata[geneID,13:16]))[2],
      'Adult Host' = fivenum(as.numeric(cpmdata[geneID,c(17,19,21,23)]))[2],
      'Fetal Colony' = fivenum(as.numeric(cpmdata[geneID,c(18,20,22,24)]))[2])
    
    colnames(upperhinge)=unique(groups$group)
    colnames(lowerhinge)=unique(groups$group)

    if(!(is.na(upperhinge[,1]))){
    if(input$clicked$x <= 1.4 &
       input$clicked$x >=  .6 &
       input$clicked$y <= upperhinge$Adult &
       input$clicked$y >= lowerhinge$Adult){
      clicksample("Adult")
    }
    if(input$clicked$x<=2.4 &
       input$clicked$x>=1.6 &
       input$clicked$y <= upperhinge$Dual &
       input$clicked$y >= lowerhinge$Dual){
      clicksample("Dual")
    }
    if(input$clicked$x<=3.4 &
       input$clicked$x>=2.6 &
       input$clicked$y <= upperhinge$LAP &
       input$clicked$y >= lowerhinge$LAP){
      clicksample("LAP")
    }
    if(input$clicked$x<=4.4 &
       input$clicked$x>=3.6 &
       input$clicked$y <= upperhinge$Single &
       input$clicked$y >= lowerhinge$Single){
      clicksample("Single")
    }
    }
    if(!(is.na(upperhinge$'Adult Host'))){
    if(input$clicked$x<=5.4 &
       input$clicked$x>=4.6 &
       input$clicked$y <= upperhinge[,5] &
       input$clicked$y >= lowerhinge[,5]){
      clicksample("Adult Host")
    }
    if(input$clicked$x<=6.4 &
       input$clicked$x>=5.6 &
       input$clicked$y <= upperhinge[,6] &
       input$clicked$y >= lowerhinge[,6]){
      clicksample("Fetal Colony")
    }
    }
  })
  
  output$plot <- renderPlot({
    gene.name=row.names(annot[annot[,1]==input$gene,])
    minlim=min(as.numeric(na.omit(cpmdata[gene.name,])))
    maxlim=max(as.numeric(na.omit(cpmdata[gene.name,])))
    
    par(mar=c(2,4,4,8))
    plot.new()
    col.pan <- colorpanel(101, "black","white")
    # Change the plot region color
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = col.pan[(input$back*100)+1]) # Color
    
    box.outline= col.pan[((((.6-input$back)/abs(.6-input$back))+1)*50)+1]
    par(new = TRUE)
    boxplot(as.numeric(cpmdata[gene.name,])~(groups$group),
            col=c(input$A.box.color,input$D.box.color,
                  input$L.box.color,input$S.box.color,
                  input$AH.box.color,input$FC.box.color),
            border=box.outline, xlab="",lwd=input$size,ylab="CPM")
    if(input$rawdata==1){
      
      points(x=rep(1,4),y=as.numeric(cpmdata[gene.name,1:4]),
             pch=21,bg=input$A.point.color,lwd=input$size *.7, 
             cex=input$size, col = box.outline)
      points(x=rep(2,4),y=as.numeric(cpmdata[gene.name,5:8]),
             pch=21,bg=input$D.point.color,lwd=input$size *.7, 
             cex=input$size, col = box.outline)
      points(x=rep(3,4),y=as.numeric(cpmdata[gene.name,9:12]),
             pch=21,bg=input$L.point.color,lwd=input$size *.7, 
             cex=input$size, col = box.outline)
      points(x=rep(4,4),y=as.numeric(cpmdata[gene.name,13:16]),
             pch=21,bg=input$S.point.color,lwd=input$size *.7, 
             cex=input$size, col = box.outline)
      points(x=rep(5,4),y=as.numeric(cpmdata[gene.name,c(17,19,21,23)]),
             pch=21,bg=input$AH.point.color,lwd=input$size *.7, 
             cex=input$size, col = box.outline)
      points(x=rep(6,4),y=as.numeric(cpmdata[gene.name,c(18,20,22,24)]),
             pch=21,bg=input$FC.point.color,lwd=input$size *.7, 
             cex=input$size, col = box.outline)
    }
    
    ## if(length(intersect(clickID,groups$group)==1)){
    ## legend(x=xpos, 
    ##      y=ypos+((maxlim-minlim)*.125),
    ##    legend=boxdata,
    ##  xjust = 0.5,
    ##yjust = 1,
    ##x.intersp = -0.5,
    ##y.intersp = 1,
    ##adj = c(0, 0.2),
    ##cex=.6, bg="khaki",xpd=T)
    ##}
    
    
    legend(x=6.8, y=minlim+((maxlim-minlim)*.975), 
           legend=unique(groups$group),xpd=T,
           fill=c(input$A.box.color, input$D.box.color, 
                  input$L.box.color, input$S.box.color,
                  input$AH.box.color, input$FC.box.color),
           bg=col.pan[(input$back*100)+1],
           text.col=col.pan[((((.6-input$back)/abs(.6-input$back))+1)*50)+1])
  }, res=96)
  
  output$data <- DT::renderDataTable({
    
    gene.name=row.names(annot[annot[,1]==input$gene,])
    temp=deanalysis[gene.name,]
    boxdata=data.frame(logFC=signif(as.numeric(temp[c(1,3,5,7,9,11,13)]),3),
                       FDR=signif(as.numeric(temp[c(2,4,6,8,10,12,14)]),5))
    row.names(boxdata)=unique(substring(colnames(deanalysis),1,2))
    ##boxdata$FDR[boxdata$FDR>.05]=1
    
    sig.table=data.frame(Adult  = as.numeric(c("",boxdata[c("AD","AL","AS"),"FDR"],"","")), 
                         Dual   = as.numeric(c(boxdata["AD","FDR"],"",boxdata[c("DL","DS"),"FDR"],"","")), 
                         LAP    = as.numeric(c(boxdata[c("AL","DL"),"FDR"],"",boxdata["LS","FDR"],"","")), 
                         Single = as.numeric(c(boxdata[c("AS","DS","LS"),"FDR"],"","","")),
                         'Adult Host' = as.numeric(c("","","","","",boxdata[c("CH"),"FDR"])),
                         'Fetal Colony' = as.numeric(c("","","","",boxdata[c("CH"),"FDR"],"")))
    
    row.names(sig.table)=unique(groups$group)
    colnames(sig.table)=unique(groups$group)
    
    use.table=data.frame(Adult  = as.numeric(c(1,boxdata[c("AD","AL","AS"),"FDR"],1,1)), 
                         Dual   = as.numeric(c(boxdata["AD","FDR"],1,boxdata[c("DL","DS"),"FDR"],1,1)), 
                         LAP    = as.numeric(c(boxdata[c("AL","DL"),"FDR"],1,boxdata["LS","FDR"],1,1)), 
                         Single = as.numeric(c(boxdata[c("AS","DS","LS"),"FDR"],1,1,1)),
                         'Adult Host' = as.numeric(c(1,1,1,1,1,boxdata[c("CH"),"FDR"])),
                         'Fetal Colony' = as.numeric(c(1,1,1,1,boxdata[c("CH"),"FDR"],1)))

    row.names(use.table)=unique(groups$group)
    colnames(use.table)=unique(groups$group)
    
    use.table[use.table>.05]=-1
    use.table[use.table>-1]=0
    use.table=use.table+1
    
    df = (sig.table)
    

    xpos=xdata()
    ypos=ydata()
    clickID=clicksample()
    if(length(intersect(clickID,groups$group)==1)){
      datatable(df, options = list(dom = 't', autoHideNavigation=T)) %>%
        formatStyle(colnames(df),color='grey') %>%
        formatStyle(clickID,backgroundColor = 
                      styleRow(c(1:6)[use.table[,clickID]==1], c('yellow'))) %>% 
        formatStyle(clickID, fontWeight = 'bold', color='black') %>%
        formatStyle(colnames(df)[1], 
                    backgroundColor = styleRow(c(1),c('darkgrey'))) %>%
        formatStyle(colnames(df)[2], 
                    backgroundColor = styleRow(c(2),c('darkgrey'))) %>%
        formatStyle(colnames(df)[3], 
                    backgroundColor = styleRow(c(3),c('darkgrey'))) %>%
        formatStyle(colnames(df)[4], 
                    backgroundColor = styleRow(c(4),c('darkgrey'))) %>%
        formatStyle(colnames(df)[5], 
                    backgroundColor = styleRow(c(5),c('darkgrey'))) %>%
        formatStyle(colnames(df)[6], 
                    backgroundColor = styleRow(c(6),c('darkgrey'))) %>%
        formatStyle(colnames(df)[1:4], border = styleRow(c(1:4),'2px solid black')) %>% 
        formatStyle(colnames(df)[5:6], border = styleRow(c(5:6),'2px solid black')) %>%
        formatStyle(clickID, border = styleRow(c(1:6)[use.table[,clickID]==1],'3px solid red'))
    }else{
      datatable(df, options = list(dom = 't', autoHideNavigation=T)) %>%
        formatStyle(colnames(df),color='grey') %>%
        formatStyle(colnames(df)[1], 
                    backgroundColor = styleRow(c(1),c('darkgrey'))) %>%
        formatStyle(colnames(df)[2], 
                    backgroundColor = styleRow(c(2),c('darkgrey'))) %>%
        formatStyle(colnames(df)[3], 
                    backgroundColor = styleRow(c(3),c('darkgrey'))) %>%
        formatStyle(colnames(df)[4], 
                    backgroundColor = styleRow(c(4),c('darkgrey'))) %>%
        formatStyle(colnames(df)[5], 
                    backgroundColor = styleRow(c(5),c('darkgrey'))) %>%
        formatStyle(colnames(df)[6], 
                    backgroundColor = styleRow(c(6),c('darkgrey'))) %>%
        formatStyle(colnames(df)[1:4], border = styleRow(c(1:4),'2px solid black')) %>% 
        formatStyle(colnames(df)[5:6], border = styleRow(c(5:6),'2px solid black'))
    }
  })
  
  output$data2 <- renderTable({
    input$clicked
  })
  
})



shinyApp(ui = ui, server = server, options)