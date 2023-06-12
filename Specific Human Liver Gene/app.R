list.of.packages <- c("shiny", "gplots","DT","colourpicker")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){
  install.packages('shiny')
  install.packages("gplots")
  install.packages("DT")
  install.packages("colourpicker")
}

library(shiny)
library(gplots)
library(ggplot2)
library(colourpicker)
library(DT)

##library(edgeR)
##countdata=read.csv("https://raw.githubusercontent.com/johncsantiago/Ohman-2021/master/Data/mgh_raw_countdata.csv",row.names=1)
##groups=read.csv("https://raw.githubusercontent.com/johncsantiago/Ohman-2021/master/Data/mgh_metadata.csv",row.names=1)

##EdgeR comparisons
##countdata=countdata[,row.names(groups)]
##countdata=countdata[,row.names(groups)]
##x <- countdata
##group <- factor(groups$Group)
##y <- DGEList(counts=x,group=group)
##keep <- filterByExpr(y)
##y <- y[keep,,keep.lib.sizes=FALSE] 
##z <- calcNormFactors(y, method = "TMM") 
##cpmdata=cpm(z)

# Design matrix
##design<-model.matrix(~0+group)
##colnames(design) <- (substring(colnames(design),6,8))

##z = estimateGLMCommonDisp(z,design, verbose=F)
##z = estimateGLMTrendedDisp(z,design)
##z = estimateGLMTagwiseDisp(z,design)
##fit <- glmFit(z, design)


##comp = c(
##Perfused vs Not-Perfused
##         "LV2-LV1 LV2-LV1.csv", 
##         "LV3-LV1 LV3-LV1.csv",
##         "LN2-LN1 LN2-LN1.csv", 
##         "LN3-LN1 LN3-LN1.csv",
##         "FV2-FV1 FV2-FV1.csv", 
##         "FV3-FV1 FV3-FV1.csv",
##         "FN2-FN1 FN2-FN1.csv", 
##         "FN3-FN1 FN3-FN1.csv",
##Viable vs Non-Viable
##         "LN1-LV1 LN1-LV1.csv",
##         "LN2-LV2 LN2-LV2.csv",
##         "LN3-LV3 LN3-LV3.csv",
##         "FN1-FV1 FN1-FV1.csv",
##         "FN2-FV2 FN2-FV2.csv",
##         "FN3-FV3 FN3-FV3.csv",
##Lean vs Fatty
##         "FV1-LV1 FV1-LV1.csv",
##         "FV2-LV2 FV2-LV2.csv",
##         "FV3-LV3 FV3-LV3.csv",
##         "FN1-LN1 FN1-LN1.csv",
##         "FN2-LN2 FN2-LN2.csv",
##         "FN3-LN3 FN3-LN3.csv")


##i=1
##while(i<=20){
##  temp=(strsplit(comp[i],split=" ")[[1]][1])
##  compare = makeContrasts(temp, levels=design)
##  lrt <- glmLRT(fit,contrast=as.vector(compare))		
##  G_X_E<-topTags(lrt,adjust.method="BH",n = nrow(z$counts), sort.by="PValue")
##  DEGs=G_X_E$table
##  write.csv(DEGs, paste0("/Users/johncsantiago/Documents/GitHub/Shiny/Data/Human Liver EdgeR Outputs/",strsplit(comp[i],split=" ")[[1]][2]))
##  i=i+1
##}

##edger.git.dir= "https://raw.githubusercontent.com/johncsantiago/Shiny/master/Data/Human%20Liver%20EdgeR%20Outputs/"

##edger.outs = c(
##Perfused vs Not-Perfused
##  "LV2-LV1.csv", 
##  "LV3-LV1.csv",
##  "LN2-LN1.csv", 
##  "LN3-LN1.csv",
##  "FV2-FV1.csv", 
##  "FV3-FV1.csv",
##  "FN2-FN1.csv", 
##  "FN3-FN1.csv",
##Viable vs Non-Viable
##  "LN1-LV1.csv",
##  "LN2-LV2.csv",
##  "LN3-LV3.csv",
##  "FN1-FV1.csv",
##  "FN2-FV2.csv",
##  "FN3-FV3.csv",
##Lean vs Fatty
##  "FV1-LV1.csv",
##  "FV2-LV2.csv",
##  "FV3-LV3.csv",
##  "FN1-LN1.csv",
##  "FN2-LN2.csv",
##  "FN3-LN3.csv")

##sigdata=data.frame(cpmdata[,1:20])
##colnames(sigdata)=edger.outs
##i=1
##while(i<=20){
##  temp= read.csv(paste0(edger.git.dir,edger.outs[i]),row.names = 1)
##  sigdata[,i]=temp[row.names(sigdata),"FDR"]
##  i=i+1
##}
##colnames(sigdata)=substring(colnames(sigdata),1,7)
##write.csv(sigdata,"/Users/johncsantiago/Documents/GitHub/Lactate-Correlation-Manuscript/Data/CombinedEdgeRData.csv")

##git.dir   = "https://raw.githubusercontent.com/johncsantiago/Lactate-Correlation-Manuscript/master/Data/"
##cpmdata   = read.csv(paste0(git.dir,"LacCorCpmData.csv"), row.names = 1)

##mean.cpm=cpmdata[,1:12]
##colnames(mean.cpm)=unique(groups$Group)
##i=1
##while(i<=12){
##  temp=cpmdata[,row.names(groups)[groups$Group==colnames(mean.cpm[i])]]
##  mean.cpm[,i]=apply(temp,1,mean)
##  i=i+1
##}
##write.csv(mean.cpm,"/Users/johncsantiago/Documents/GitHub/Lactate-Correlation-Manuscript/Data/mean.cpmdata.csv")

git.dir   = "https://raw.githubusercontent.com/johncsantiago/Lactate-Correlation-Manuscript/master/Data/"
LTD       = read.csv(paste0(git.dir,"LiverTraitData.csv"), row.names = 1)
cpmdata   = read.csv(paste0(git.dir,"LacCorCpmData.csv"), row.names = 1)
groups    = read.csv(paste0(git.dir,"Metadata.csv"), row.names = 1)
annot     = read.csv(paste0(git.dir,"AnnotationData.csv"),row.names=1)
sigdata   = read.csv(paste0(git.dir,"CombinedEdgeRData.csv"),row.names=1)
mean.cpm  = read.csv(paste0(git.dir,"mean.cpmdata.csv"),row.names=1)

groups[groups=="F"]="Fatty"
groups[groups=="L"]="Lean"
groups[groups=="V"]="Viable"
groups[groups=="N"]="Nonviable"
groups[groups=="1"]="1 Hour"
groups[groups=="2"]="3 Hour"
groups[groups=="3"]="6 Hour"
groups$condition=paste(groups$fat,groups$viability)

metadata=data.frame(All       = rep(FALSE, nrow(groups)),
                    Lean      = groups$fat=="Lean",
                    Fatty     = groups$fat=="Fatty",
                    Viable    = groups$viability=="Viable",
                    Nonviable = groups$viability=="Nonviable",
                    Time1     = groups$timepoint=="1 Hour",
                    Time2     = groups$timepoint=="3 Hour",
                    Time3     = groups$timepoint=="6 Hour")
row.names(metadata)=row.names(groups)

i=1
while(i<=ncol(sigdata)){
  sigdata[,i]=signif(sigdata[,i],3)
  i=i+1
}


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Lean and Steatotic Human Liver Data: Plot Specific Gene Expression Data"),
  sidebarPanel(
    h5("Description: "),
    p("Generates boxplots for any specific gene in the Lean and Steatotic Human Liver dataset. Options allow figure customization."),

    br(),
    uiOutput("geneControls"),
    p("Select Gene: Use gene symbol if available. autocompletes"),
    ##br(), 

    fluidRow(
    column(6,
    selectInput("choice.order", h5("DEG Order"),
                choices = list("LV 3H~0H"   = "LV2.LV1",
                               "LV 6H~0H"   = "LV3.LV1",
                               "LN 3H~0H"   = "LN2.LN1",
                               "LN 3H~0H"   = "LN3.LN1",
                               "FV 3H~0H"   = "FV2.FV1",
                               "FV 6H~0H"   = "FV3.FV1",
                               "FN 3H~0H"   = "FN2.FN1",
                               "FN 3H~0H"   = "FN3.FN1",
                               
                               "LN~LV 0H"   = "LN1.LV1",
                               "LN~LV 3H"   = "LN2.LV2",
                               "LN~LV 6H"   = "LN3.LV3",
                               
                               "FN~FV 0H"   = "FN1.FV1",
                               "FN~FV 3H"   = "FN2.FV2",
                               "FN~FV 6H"   = "FN3.FV3",
                              
                               "FV~LV 0H"   = "FV1.LV1",
                               "FV~LV 3H"   = "FV2.LV2",
                               "FV~LV 6H"   = "FV3.LV3",
                               
                               "FN~LN 0H"   = "FN1.LN1",
                               "FN~LN 3H"   = "FN2.LN2",
                               "FN~LN 6H"   = "FN3.LN3"),
                selected="LV2.LV1"),
    p("Choose Signifigance Order: Organize the 'Select Gene' drop down menu order by FDR observed for a comparison between specific conditions"),
    br(),
    ),
    column(6,
    
    checkboxGroupInput("cond.select", h5("Select Conditions to Exclude"), 
                choices = list("Lean"             = 2, 
                               "Fatty"            = 3,
                               "Viable"           = 4,
                               "Nonviable"        = 5,
                               "0 Hour"           = 6,
                               "3 Hour"           = 7,
                               "6 Hour"           = 8)),
    ##p("Select Any Conditions to Exclude"),
    ##br(), 
    ),),
    
    fluidRow(
      column(6,
    selectInput("colfactor", h5("Select Factor for Color"), 
                choices = list("Group"     = "condition",
                               "Steatosis" = "fat",
                               "Time"      = "timepoint", 
                               "Viability" = "viability"),
                selected = "Group"),
      ),
    column(6,
    
    selectInput("colselect", h5("Select Color"), 
                choices = list("Choose Condition" = 0,
                               "Color 1"            = 1, 
                               "Color 2"             = 2,
                               "Color 3"              = 3,
                               "Color 4"           = 4),
                selected = 0),
    ),),
    
    conditionalPanel(
      condition = "input.colselect == 1",
      colourInput("color1", "Color 1",
                  "brown",showColour = 'background')),

    
    conditionalPanel(
      condition = "input.colselect == 2",
      colourInput("color2", "Color 2",
                  "cornflowerblue",showColour = 'background')),

    conditionalPanel(
      condition = "input.colselect == 3",
      colourInput("color3", "Color 3",
                  "chartreuse4",showColour = 'background')),
    
    conditionalPanel(
      condition = "input.colselect == 4",
      colourInput("color4", "Color 4",
                  "darkgoldenrod1",showColour = 'background')),
    
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

  output$geneControls =  renderUI({
    gene.choices<<-annot[row.names(sigdata)[order(sigdata[,input$choice.order])],2]
    selectizeInput("gene", label = "Select Gene", 
                   choices = gene.choices,
                   options = list(create = TRUE,
                                  ##maxOptions = 5,
                                  placeholder = 'select a gene name'),
                   selected=gene.choices[1])
  })
  
  output$plot <- renderPlot({
    gene.name=row.names(annot[annot[,2]==input$gene,])
    if(length(gene.name)==1){
    minlim=min(as.numeric(na.omit(cpmdata[gene.name,])))
    maxlim=max(as.numeric(na.omit(cpmdata[gene.name,])))
    
    if(length(input$cond.select)==0){
      use.samples=row.names(metadata)[!metadata[,1]]
    }    
    
    if(length(input$cond.select)>0){
      use.samples=row.names(metadata)[apply(metadata[,c(1,as.numeric(input$cond.select))],1,sum)==0]
    }
   
    boxdata=data.frame(cpm   = as.numeric(cpmdata[gene.name,use.samples]),
                       color = groups[use.samples,input$colfactor],
                       group = groups[use.samples,"Group"])
    
    boxcolors = c(input$color1,input$color2,
                  input$color3,input$color4)
    p = ggplot(boxdata, aes(x=group, y=cpm, fill=color)) +
        geom_boxplot()
    p = p + geom_dotplot(binaxis='y', stackdir='center', dotsize=.75)
    p = p + theme(panel.background = element_rect(fill = 'oldlace', color = 'burlywood4'),
                  panel.grid.major = element_line(color = 'antiquewhite3', linetype = 'dotted'),
                  panel.grid.minor = element_line(color = 'antiquewhite3'))##, size = 2))
    p + scale_fill_manual(values=boxcolors)
    }
  }, res=96)

  
  
    
output$data <- DT::renderDataTable({
    
    gene.name=row.names(annot[annot[,2]==input$gene,])
    temp=as.vector(sigdata[gene.name,])
    names(temp)=colnames(sigdata)
    
    if(length(input$cond.select)==0){
      use.samples=row.names(metadata)[!metadata[,1]]
    }    
    
    if(length(input$cond.select)>0){
      use.samples=row.names(metadata)[apply(metadata[,c(1,as.numeric(input$cond.select))],1,sum)==0]
    }
    use.samples=unique(groups[use.samples,"Group"])
    sig.table=data.frame(FN1 = c(NA,as.numeric(temp[c("FN2.FN1","FN3.FN1","FN1.FV1","FN1.LN1")])), 
                         FN2 = c(as.numeric(temp[c("FN2.FN1")]),NA,NA,as.numeric(temp[c("FN2.FV2","FN2.LN2")])), 
                         FN3 = c(as.numeric(temp[c("FN3.FN1")]),NA,NA,as.numeric(temp[c("FN3.FV3","FN3.LN3")])),
                         FV1 = c(NA,as.numeric(temp[c("FV2.FV1","FV3.FV1","FN1.FV1","FV1.LV1")])), 
                         FV2 = c(as.numeric(temp[c("FV2.FV1")]),NA,NA,as.numeric(temp[c("FN2.FV2","FV2.LV2")])), 
                         FV3 = c(as.numeric(temp[c("FV3.FV1")]),NA,NA,as.numeric(temp[c("FN3.FV3","FV3.LV3")])),
                         LN1 = c(NA,as.numeric(temp[c("LN2.LN1","LN3.LN1","LN1.LV1","FN1.LN1")])), 
                         LN2 = c(as.numeric(temp[c("LN2.LN1")]),NA,NA,as.numeric(temp[c("LN2.LV2","FN2.LN2")])), 
                         LN3 = c(as.numeric(temp[c("LN3.LN1")]),NA,NA,as.numeric(temp[c("LN3.LV3","FN3.LN3")])),
                         LV1 = c(NA,as.numeric(temp[c("LV2.LV1","LV3.LV1","LN1.LV1","FV1.LV1")])), 
                         LV2 = c(as.numeric(temp[c("LV2.LV1")]),NA,NA,as.numeric(temp[c("LN2.LV2","FV2.LV2")])), 
                         LV3 = c(as.numeric(temp[c("LV3.LV1")]),NA,NA,as.numeric(temp[c("LN3.LV3","FV3.LV3")])))

    row.names(sig.table)= c("vs. 0H","vs. 3H","vs. 6H", "V. vs N.", "F. vs L.")
    sig.table=sig.table[,use.samples[order(use.samples)]]
    df = (sig.table)
    
      datatable(df, options = list(dom = 't', autoHideNavigation=T)) %>%
        formatStyle(colnames(df),color='black', fontSize = '70%') %>%
        formatStyle(na.omit(colnames(df)[df[1,]<.05]), 
                    backgroundColor = styleRow(c(1),c('yellow'))) %>% 
        formatStyle(na.omit(colnames(df)[df[2,]<.05]), 
                    backgroundColor = styleRow(c(2),c('yellow'))) %>% 
        formatStyle(na.omit(colnames(df)[df[3,]<.05]), 
                    backgroundColor = styleRow(c(3),c('yellow'))) %>% 
        formatStyle(na.omit(colnames(df)[df[4,]<.05]), 
                    backgroundColor = styleRow(c(4),c('yellow'))) %>% 
        formatStyle(na.omit(colnames(df)[df[5,]<.05]), 
                    backgroundColor = styleRow(c(5),c('yellow'))) %>% 
        
        formatStyle(colnames(df)[is.na(df[1,])], 
                    backgroundColor = styleRow(c(1),c('darkgrey'))) %>%
        formatStyle(colnames(df)[is.na(df[2,])], 
                    backgroundColor = styleRow(c(2),c('darkgrey'))) %>%
        formatStyle(colnames(df)[is.na(df[3,])], 
                    backgroundColor = styleRow(c(3),c('darkgrey'))) %>%

       formatStyle(colnames(df)[1:ncol(df)], border = styleRow(c(1:5),'2px solid black'))
  })
})



shinyApp(ui = ui, server = server, options)