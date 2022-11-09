list.of.packages <- c("shiny", "ggfortify")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){
  install.packages('shiny')
  install.packages("ggfortify")
}

library(shiny)
library(ggfortify)
library(heatmaply)
library(plotly)

git.dir = "https://raw.githubusercontent.com/johncsantiago/SData/master/FlySectionsProject/"
cpmdata = read.csv(paste0(git.dir,"cpmdata.csv"),row.names = 1)
groups  = read.csv(paste0(git.dir,"groups.csv"),row.names = 1)
convert=read.csv("https://raw.githubusercontent.com/DavidRandLab/Santiago-et-al-2021-BMC-Genomics/main/Data%20Files/FBgnConversionTable.csv",row.names=1)
temp=convert[,"Symbol"]
temp2=setdiff(row.names(cpmdata),convert[,2])
names(temp)=convert[,2]
names(temp2)=temp2
convert=c(temp,temp2)

clickdata=data.frame(Sample=1,
                    PC1=1,
                    PC2=1,
                    sdev=1,
                    Species=1, 
                    Sex=1, 
                    Treatment=1, 
                    Tissue=1,
                    Percent1=1,
                    Percent2=1)
clickdata=clickdata[0,]
hoverdata=clickdata
resetdata=clickdata


##mean.cpmdata=cpmdata[,1:length(unique(groups$Group))]
##colnames(mean.cpmdata)=unique(groups$Group)
##i=1
##while(i<=ncol(mean.cpmdata)){
  ##temp=cpmdata[,row.names(groups)[groups$Group==colnames(mean.cpmdata)[i]]]
  ##mean.cpmdata[,i]=apply(temp, 1, mean)
  ##i=i+1
##}


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Customize PCA"),
  sidebarLayout(position="left", sidebarPanel(
    fluidRow(
      column(6,
    checkboxGroupInput("species",
                       h5("Species"),
                       choices = list("OreR;OreR" = "OO",
                                      "sm21;OreR" = "SO"),
                       selected = ""),
      ),
    column(6,
    checkboxGroupInput("sex",
                       h5("Sex"),
                       choices = list("Male"   = "Male",
                                      "Female" = "Female"),
                       selected = ""),
    ),),
    
    fluidRow(
      column(6,
    checkboxGroupInput("treatment",
                       h5("Treatment"),
                       choices = list("Control" = "Control",
                                      "Rapa"    = "Rapa"),
                       selected = ""),
      ),
    column(6,
    checkboxGroupInput("tissue",
                       h5("Tissue"),
                       choices = list("Head"    = "Head",
                                      "Thorax"  = "Thorax",
                                      "Abdomen" = "Abdomen"),
                       selected = ""),
    ),),
    
    fluidRow(
      column(6,
    selectInput("shape", h5("Shape Variable"),
                choices = list("Species"   = "Species",
                               "Sex"       = "Sex",
                               "Treatment" = "Treatment",
                               "Tissue"    = "Tissue")),
    
      ),
    column(6,
    selectInput("color", h5("Color Variable"),
                choices = list("Species"   = "Species",
                               "Sex"       = "Sex",
                               "Treatment" = "Treatment",
                               "Tissue"    = "Tissue"),
                selected="Tissue"),
    ),),
    
    fluidRow(
      column(6,
    sliderInput("size", h5("Size"), min = 0, max = 10, value = 3, step=.5),
      ),
    column(6,
           sliderInput("tophits", h5("Number of PC Genes"), min = 10, max = 50, value = 25, step=5)
    ),),
    actionButton("clear","Clear Selected", class = "btn-lg btn-success"),
                
    width = 3),
    
    mainPanel(
      plotOutput(outputId = "distPlot",
                 click = "clicked",
                 hover = "hoverpoint"##,
                 ##height=600, width=600)
                 ),
      fluidRow(
        column(6,
      plotlyOutput(outputId = "topPC1hits", height = 550),
        ),
      column(6,
      plotlyOutput(outputId = "topPC2hits", height = 550))
      ),),
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  temp.groups=reactive({
    use.groups =  groups
    
    if(length(input$species) == 1){
      use.groups = use.groups[use.groups$Species == input$species,]
    }
    
    if(length(input$sex) == 1){
      use.groups = use.groups[use.groups$Sex == input$sex,]
    }
    
    if(length(input$treatment) == 1){
      use.groups = use.groups[use.groups$Treatment == input$treatment,]
    }
    
    if(length(input$tissue) == 1){
      use.groups = use.groups[use.groups$Tissue == input$tissue,]
    }
    
    if(length(input$tissue) == 2){
      use.groups = use.groups[use.groups$Tissue == input$tissue[1]|
                                use.groups$Tissue == input$tissue[2],]
    }

    return(use.groups)
  })
  
  newdata=reactive({
    use.groups=temp.groups()
    use.data   = cpmdata[,row.names(use.groups)]
    use.data   = use.data[apply(use.data,1,sum)>0,]
    pca <- prcomp(t(use.data), scale.=TRUE) 
    gr <- as.data.frame(use.groups)
    PC1=scale(pca$x[,1])
    PC2=scale(pca$x[,2])
    eigs <- pca$sdev^2
    ve=signif(((eigs / sum(eigs))*100)[1:2],4)
    ##distance=sqrt(((highlight()-PC1[1])^2)+((highlight()-PC2[1])^2))
    pca.data=data.frame(Sample=row.names(gr),
                        PC1=scale(pca$x[,1]),
                        PC2=scale(pca$x[,2]),
                        sdev=pca$sdev,
                        Species=gr$Species, 
                        Sex=gr$Sex, 
                        Treatment=gr$Treatment, 
                        Tissue=gr$Tissue,
                        Percent1=ve[1],
                        Percent2=ve[2])
    
    return(pca.data)
  })


  highlight=reactive({
    temp=newdata()
    clickdata<<-rbind(clickdata,nearPoints(temp,input$clicked, xvar="PC1", yvar="PC2", maxpoints=1))
    clickdata<<-clickdata[!(duplicated(clickdata) |
                            duplicated(clickdata, fromLast=TRUE)),]
    return(clickdata)
  })
  
  details=reactive({
    temp=newdata()
    ##hoverdata<<-resetdata
    hoverdata<<-rbind(hoverdata,nearPoints(temp,input$hoverpoint, xvar="PC1", yvar="PC2", maxpoints=1))
    return(hoverdata)
  })

  observeEvent(input$clear,{
    clickdata<<-resetdata
  })
  
  output$data <- renderTable({
    highlight()
  })

output$distPlot <- renderPlot({
  pca.data=newdata()
  labeltxt=details()
      ggplot(pca.data, aes(x = PC1, y = PC2)) +
        geom_point(aes_string(shape = input$shape, 
                       colour       = input$color),
                   size  = input$size,
                   alpha = .75) +
        labs(x=paste0("PC1 (",pca.data$Percent1[1],"%)"), 
             y=paste0("PC2 (",pca.data$Percent2[1],"%)")) +
        geom_point(data=highlight(), colour = "black", size = 6) +
        geom_point(data=highlight(), colour = "yellow", size = 5) +
        annotate("label",
                 x=hoverdata[nrow(hoverdata),"PC1"]+.25,
                 y=hoverdata[nrow(hoverdata),"PC2"]+.1,
                 label=hoverdata[nrow(hoverdata),"Sample"],
                 colour="black", fill = "yellow") ##, alpha=.7)
    }, res=96)
 


output$topPC1hits <- renderPlotly({
  use.groups = temp.groups()
  use.data   = cpmdata[,row.names(use.groups)]
  use.data   = use.data[apply(use.data,1,sum)>0,]
  pca.data <- prcomp(t(use.data), scale.=TRUE) 
  PCcontribute=pca.data$rotation[,1]
  PCcontribute=PCcontribute[order(PCcontribute,decreasing=T)]
  
  ##The column sum of squares of the loadings (pca$rotation) are the variances of PCs.
  #the squared factor loading is the percent of variance in that variable explained by the factor
  percents=(PCcontribute*PCcontribute)

  ##puts the genes in decreasing order
  percents=percents[order(percents,decreasing=T)]
  pca.genes=PCcontribute[names(percents)[1:input$tophits]]

  pca.genes=names(pca.genes[order(pca.genes)])
  PCorder=pca.data$x[,1]
  PCorder=PCorder[order(PCorder)]
  
  top.pca=cpmdata[pca.genes,names(PCorder)]
  row.names(top.pca)=
    convert[row.names(top.pca)]
  color.groups=groups[names(PCorder),]

  
  col.text=1
  if(ncol(top.pca)>18){
    col.text=1-((ncol(top.pca)-18)*.011)
  }
  
  heatmaply(top.pca,trace="none",col=RdYlBu(100)[100:1], scale="row",
            dendrogram = "none",Rowv=F,Colv=F,
            cexRow = .75, na.color="grey",
            labRow = row.names(top.pca),
            cexCol = col.text,
            key = T,
            column_text_angle = 90,
            col_side_colors=color.groups[,1:4],
            col_side_palette=Spectral,
            main="PC1")
  
})

output$topPC2hits <- renderPlotly({
  use.groups = temp.groups()
  use.data   = cpmdata[,row.names(use.groups)]
  use.data   = use.data[apply(use.data,1,sum)>0,]
  
  
  pca.data <- prcomp(t(use.data), scale.=TRUE) 
  PCcontribute=pca.data$rotation[,2]
  
  PCcontribute=PCcontribute[order(PCcontribute,decreasing=T)]
  
  ##The column sum of squares of the loadings (pca$rotation) are the variances of PCs.
  #the squared factor loading is the percent of variance in that variable explained by the factor
  percents=(PCcontribute*PCcontribute)
  
  ##puts the genes in decreasing order
  percents=percents[order(percents,decreasing=T)]
  pca.genes=PCcontribute[names(percents)[1:input$tophits]]
  
  pca.genes=names(pca.genes[order(pca.genes)])
  PCorder=pca.data$x[,2]
  PCorder=PCorder[order(PCorder)]
  
  top.pca=cpmdata[pca.genes,names(PCorder)]
  row.names(top.pca)=
    convert[row.names(top.pca)]
  color.groups=groups[names(PCorder),]

  col.text=1
  if(ncol(top.pca)>18){
    col.text=1-((ncol(top.pca)-18)*.011)
  }
  
  heatmaply(top.pca,trace="none",col=RdYlBu(100)[100:1], scale="row",
            dendrogram = "none",Rowv=F,Colv=F,
            cexRow = .75, na.color="grey",
            cexCol = col.text,
            labRow = row.names(top.pca),
            key = T,
            column_text_angle = 90,
            col_side_colors=color.groups[,1:4],
            col_side_palette=Spectral,
            main="PC2")
  
})

}


shinyApp(ui = ui, server = server, options)