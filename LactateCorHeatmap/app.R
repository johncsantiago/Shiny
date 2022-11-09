list.of.packages <- c("shiny", "gplots","heatmaply","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){
  install.packages('shiny')
  install.packages("gplots")
  install.packages("heatmaply")
  install.packages("plotly")
}

library(shiny)
library(gplots)
library(heatmaply)
library(plotly)

git.dir   = "https://raw.githubusercontent.com/johncsantiago/Lactate-Correlation-Manuscript/master/Data/"
LTD       = read.csv(paste0(git.dir,"LiverTraitData.csv"), row.names = 1)
cpmdata   = read.csv(paste0(git.dir,"LacCorCpmData.csv"), row.names = 1)
groups    = read.csv(paste0(git.dir,"Metadata.csv"), row.names = 1)
annot     = read.csv(paste0(git.dir,"AnnotationData.csv"),row.names=1)
##cor.data  = read.csv(paste0(git.dir,"LacCorData.csv"), row.names = 1)
##cor.p     = read.csv(paste0(git.dir,"LacCorP.csv"), row.names = 1)
tpmdata   = read.csv(paste0(git.dir,"LacCorTpmData.csv"), row.names = 1)
ProbeList = read.csv(paste0(git.dir,"NanostringProbeList.csv"))
ProbeList = ProbeList[,2]


##FunctionalGroups
FG=c("AF1","AF2","AF3","AF4","AF5","IF1",
     "IF2","LF1","LF2","LF3")
names(FG)=c("FV12","LV12","LV13","FV13","LV11","FN11",
            "FN12","LN11","FN13","LN13")

cpm.0H=cpmdata[,names(FG)]
cpm.0H=cpm.0H[apply(cpm.0H,1,sum)>0,]
tpm.0H=tpmdata[,names(FG)]


group.means=data.frame(High.Performance=apply(cpmdata[,c("FV12","LV12","LV13","FV13","LV11")],1,mean), Intermediate.Performance=apply(cpmdata[,c("FN11","FN12")],1,mean), Low.Performance= apply(cpmdata[,c("LN11","FN13","LN13")],1,mean))

fcs=log2(group.means[,"High.Performance"]/group.means[,"Low.Performance"])
names(fcs)=row.names(cpmdata)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("  Heatmap of Trait Correlating Genes"),
  sidebarLayout(position="left",
                sidebarPanel(
                  selectInput("trait", h5("Trait"),
                              choices = list("Lactate" = "alactate.3H",
                                             "Macrosteatosis" = "macro",
                                             "Microsteatosis" = "micro",
                                             "Sex"            = "male"),
                              selected = "alactate.3H"),

                  selectInput("pos.neg", h5("Pos. or Neg. Cor."),
                              choices = list("Positive" = 1,
                                             "Negative" = 2,
                                             "Both" = 3),
                              selected = 3),

                  fluidRow(
                    column(6, sliderInput("cpm.cutoff", h5("Min. CPM"), 
                              min = 0, max = 100, value = 0, step = 5)),
                    column(6, sliderInput("tpm.cutoff", h5("Max. TPM"), 
                              min = 0, max = 100, value = 5, step = 5))),
                  fluidRow(
                    column(6, sliderInput("cor.cutoff", h5("Cor."), 
                              min = 0, max = 100, value = 80, step = 1)),
                    column(6, sliderInput("p.cutoff", h5("Cor. P"), 
                              min = 0, max = 1, value = .05, step = .01))),
                  fluidRow(
                    conditionalPanel(
                      condition = "input.trait == 'alactate.3H'",
                      column(6, sliderInput("fc", h5("Fold Change"), 
                                min = 1, max = 5, value = 2, step = .1))
                    ),
                    
                    column(6, checkboxInput("probe", h5("Has Probe"),
                              value = TRUE))),
                  
                  width = 4),
                
                mainPanel(
                  plotlyOutput(outputId = "distPlot",
                             height=700, width=500))
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  
    cor.info=reactive({
      
      cpm.cutoff = input$cpm.cutoff
      cor.cutoff = (input$cor.cutoff)/100
      sig.cutoff = input$p.cutoff
      pos.or.neg = input$pos.neg
      tpm.cutoff = input$tpm.cutoff
      fc.cutoff  = input$fc
      cor.trait  = input$trait
      
      trait=LTD[LTD$time==0,]
      trait=trait[,c("libraryID",cor.trait)]
      trait=na.omit(trait)
      row.names(trait)=
        paste0(substring(trait$libraryID,1,2),1,substring(trait$libraryID,3,3))
      trait=trait[order(trait[,2]),]
      temp=trait[,2]
      names(temp)=row.names(trait)
      trait=temp
      cpm.0H=cpm.0H[,names(trait)]
      tpm.0H=tpm.0H[,names(trait)]
      
      trait.cor=function(gene.data){
        calc.cor=cor(trait,gene.data)
        return(calc.cor)
      }
      trait.cor.p=function(gene.data){
        calc.cor=cor.test(trait,gene.data)
        return(calc.cor[[3]])
      }
      
      cor.data = apply(cpm.0H,1,trait.cor)
      cor.p    = apply(cpm.0H,1,trait.cor.p)  
      temp=data.frame(cor.data=cor.data,cor.p=cor.p)
      return(temp)
    })

    newdata=reactive({
      temp=cor.info()
      cor.data=temp[,1]
      cor.p=temp[,2]
      names(cor.data)=row.names(temp)
      names(cor.p)=row.names(temp)
      
      
      cpm.cutoff = input$cpm.cutoff
      cor.cutoff = (input$cor.cutoff)/100
      sig.cutoff = input$p.cutoff
      pos.or.neg = input$pos.neg
      tpm.cutoff = input$tpm.cutoff
      fc.cutoff  = input$fc
      cor.trait  = input$trait
      
      trait=LTD[LTD$time==0,]
      trait=trait[,c("libraryID",cor.trait)]
      trait=na.omit(trait)
      row.names(trait)=
        paste0(substring(trait$libraryID,1,2),1,substring(trait$libraryID,3,3))
      trait=trait[order(trait[,2]),]
      temp=trait[,2]
      names(temp)=row.names(trait)
      trait=temp
      cpm.0H=cpm.0H[,names(trait)]
      tpm.0H=tpm.0H[,names(trait)]
      
      ##trim for genes that meet cpm.cutoff
      cpm.genes=apply(cpm.0H,1,min)
      cpm.genes=names(cpm.genes[cpm.genes>cpm.cutoff])
      cordata=cpm.0H[cpm.genes,]
      
      tpm.genes = apply(tpm.0H,1,max)
      tpm.genes = names(tpm.genes[tpm.genes>tpm.cutoff])
      cordata=cordata[intersect(row.names(cordata),(tpm.genes)),]
      
      ##trim for genes that meet sig.cutoff
      sig.genes=names(cor.p)[cor.p<sig.cutoff]
      cordata=cordata[intersect(row.names(cordata),sig.genes),]
      
      if(cor.trait=="alactate.3H"){
        cor.genes=names(fcs)[abs(fcs)>=log2(fc.cutoff)]
        cordata=cordata[intersect(row.names(cordata),cor.genes),]
      }

      
      ##trim for genes that meet the cor.cutoff in the right direction
      if(pos.or.neg== 1){
        cor.genes=names(cor.data)[cor.data>cor.cutoff]
        cordata=cordata[intersect(row.names(cordata),cor.genes),]
      }
      if(pos.or.neg== 2){
        cor.genes=names(cor.data)[cor.data<(cor.cutoff*-1)]
        cordata=cordata[intersect(row.names(cordata),cor.genes),]
      }
      if(pos.or.neg== 3){
        cor.genes=names(cor.data)[abs(cor.data)>cor.cutoff]
        cordata=cordata[intersect(row.names(cordata),cor.genes),]
      }
      
      hmdata = cordata[order(cor.data[row.names(cordata)]),]
      hmdata=t(scale(t(hmdata)))
      row.names(hmdata) = annot[row.names(hmdata),2]
      if(input$probe){
      hmdata=hmdata[intersect(row.names(hmdata),ProbeList),]
      }
      
      return(hmdata)
    })
    
    custom.hover =  reactive({
      mat=newdata()
      cor=cor.info()
      temp=annot[,1]
      names(temp)=annot[,2]
      row.names(mat)=temp[row.names(mat)]
      if(input$trait=="alactate.3H"){
      i=1
      while(i<=ncol(mat)){
        mat[,i]=paste0("CPM: ",
                signif(cpm.0H[row.names(mat),
                colnames(mat)[i]],4),
                "\nCor: ",
                signif(cor[row.names(mat),1],2),
                "\nAbs. FC: ",
                signif(2^abs(fcs[row.names(mat)]),2))
        i=i+1
      }
      }
      row.names(mat)=annot[row.names(mat),2]
      return(mat)
    })
    
    output$distPlot <- renderPlotly({
      hmdata=newdata()
      mat=custom.hover()
      heatmaply(hmdata,trace="none",col=RdYlBu(100)[100:1], scale="none",
                dendrogram = "none",Rowv=F,Colv=F,
                cexRow = .75, na.color="grey",
                labRow = row.names(hmdata),
                labCol = FG[colnames(hmdata)], 
                key = T, 
                custom_hovertext = mat)
    })

}


shinyApp(ui = ui, server = server)