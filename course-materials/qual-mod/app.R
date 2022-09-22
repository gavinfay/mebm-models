#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#library(data.table); library(Rpath)
library(tidyverse)
library(LoopAnalyst)
library(permute)
library(igraph)
library(googlesheets4)
#library(ggrepel)


#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------

# Using matrix inverse
adjoint3 <- function(A) det(A)*solve(A)

gs4_deauth()
#library(googlesheets)  
#url <- "https://docs.google.com/spreadsheets/d/1zr6ijYUlinkQ2hGEE9rwTJRaRTXg_eeNE8QkzsnlZFI/edit?usp=sharing"
url <- "https://docs.google.com/spreadsheets/d/1aa298cymHNpkmhxeV_XG8IZWTiIczhuyahjRcmTm3jY/edit?usp=sharing"
lookup <- data.frame(response = c("Positive","Negative","None"),
                     value = c(1,-1,0))

matmatch <- data.frame(qnum = 1:11,
                       i = c(3,4,6,2,1,1,2,6,3,4,5),
                       j = c(6,5,5,6,6,7,7,7,7,7,7))  

#gs_ls()
#be <- gs_title("GCRL_model")
#lab <- gs_ws_ls(be)
#mydat <- as_tibble(gs_read(ss=be, ws = lab[1], skip=0))
mydat <- googlesheets4::read_sheet(url)

nobs <- nrow(mydat)
nudat <- mydat %>% 
   mutate(id = 1:nrow(mydat)) %>% 
   select(id,everything()) %>% 
   gather(key = "question", value = "response", 4:(ncol(mydat)+1)) %>% 
   mutate(qnum = rep(1:11, each=nobs)) %>% 
   left_join(lookup) %>% 
   left_join(matmatch)

B <- matrix(c(-1,-1, 0, 0, 0, 0, 0,
              1,-1,-1, 0, 0, 0, 0,
              0, 1,-1,-1,-1, 0, 0,
              0, 0, 1,-1, 0, 0, 0,
              0, 0, 1, 0,-1, 0, 0,
              0, 0, 0, 0, 0,-1, 0,
              0, 0, 0, 0, 0, 0,-1), byrow=TRUE, nrow = 7)

Wstore <- matrix(NA,nrow=7,ncol=nobs)
Astore <- matrix(NA,nrow=7,ncol=nobs)
evaldat <- nudat
WW2 <- NULL
AA2 <- NULL
for (k in unique(nudat$id)) {
   A <- B
   temp <- filter(nudat,id==k)
   for (irow in 1:nrow(temp))
      A[temp$i[irow],temp$j[irow]] <- temp$value[irow]
   #}  
   
   adj_A <- adjoint3(A)
   adj_A
   Tmat <- LoopAnalyst::make.T(A,status=TRUE)
   #system.time(Tmat <- LoopAnalyst::make.T(A,status=TRUE))
   
   
   Wmat <- abs(adj_A)/Tmat
   Wmat
   
   adj_AA <- adj_A
   
   #image showing sign of adjoint and weights
   colfunc <- colorRampPalette(c("white", "steelblue"))
   image(1:7,1:7,t(Wmat[7:1,]),col = colfunc(7))
   text(1,0,"+",col="white",cex=2)
   pick <- which(adj_AA>0)
   x <- ceiling(pick/7)
   y <- 8-pick%%7
   y[y==8] <- 1
   text(x,y,"+",col="white",cex=1.5)
   pick <- which(adj_AA<0)
   x <- ceiling(pick/7)
   y <- 8-pick%%7
   y[y==8] <- 1
   text(x,y,"-",col="white",cex=1.5)
   
   Wstore[,k] <- rev(Wmat[,7])
   Astore[,k] <- adj_AA[,7] #rev(adj_AA[,7])
   AA2[[k]] <- adj_AA
   WW2[[k]] <- Wmat
}


#group_pic
#image showing sign of adjoint and weights
colfunc <- colorRampPalette(c("white", "steelblue"))
image(1:nobs,1:7,t(Wstore),col = colfunc(5))
#text(1,0,"+",col="white",cex=2)
pick <- which(Astore>0)
x <- ceiling(pick/7)
y <- 8-pick%%7
y[y==8] <- 1
text(x,y,"+",col="white",cex=1.5)
pick <- which(Astore<0)
x <- ceiling(pick/7)
y <- 8-pick%%7
y[y==8] <- 1
text(x,y,"-",col="white",cex=1.5)



g1 <- graph_from_adjacency_matrix( abs(t(A)) , diag = FALSE)
V(g1)$size = 20
plot(g1,edge.arrow.size=.4)

users <- unique(nudat$Name)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("MEBM: Qualitative Network Modeling"),
   
      mainPanel(
        
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Networks", 
                             selectInput("user", label = "Select model",
                                         choices = users),
                             plotOutput("networkplot")),
                    tabPanel("Perturbations", 
                             selectInput("puser", label = "Select model",
                                         choices = users),
                             plotOutput("perplot")),
                    tabPanel("Evaluation", plotOutput("evalplot"))#,
                    # tabPanel("Summary", verbatimTextOutput("summary")),
                    #tabPanel("Summary Table", tableOutput("table"))
        )
        #plotOutput("EwEplot")
      )
  # )
)

# Define server logic 
server <- function(input, output) {
   
  output$perplot <- renderPlot({
    
    k <- which(users==input$puser)
    
    labs <- c("Manager",
              "Habitat",
              "Fish",
              "Seabirds",
              "Fish",
              "ZP",
              "PP")
    lab2 <- unique(nudat$Name)
    
    par(mar=c(1,5,8,1),las=0)
    
    #image showing sign of adjoint and weights
    colfunc <- colorRampPalette(c("white", "steelblue"))
    image(1:7,1:7,t(WW2[[k]][7:1,]),col = colfunc(7),
          xlab = "",
          ylab = "", axes=F)
    text(1,0,"+",col="white",cex=2)
    pick <- which(AA2[[k]]>0)
    x <- ceiling(pick/7)
    y <- 8-pick%%7
    y[y==8] <- 1
    text(x,y,"+",col="white",cex=1.5)
    pick <- which(AA2[[k]]<0)
    x <- ceiling(pick/7)
    y <- 8-pick%%7
    y[y==8] <- 1
    text(x,y,"-",col="white",cex=1.5)
    box()
    #axis(1,labels=rep("",nobs),at=seq(0.5,nobs+0.5,1),tcl=-0.2)
    par(las=2)
    axis(2,labels=labs,at=seq(1,7,1),tcl=-0.2,cex=0.8)
    #par(las=0)
    axis(3,labels=rev(labs),at=seq(1,7,1),tcl=-0.2,cex=0.8)
    title(input$puser,line=6)
    
  })

  
   output$networkplot <- renderPlot({

       nudat2 <- filter(nudat, Name == input$user)
        k <- unique(nudat2$id)[1]
        A <- B
        temp <- filter(nudat2,id==k)
        for (irow in 1:nrow(temp))
           A[temp$i[irow],temp$j[irow]] <- temp$value[irow]
     
     g1 <- graph_from_adjacency_matrix( abs(t(A)) , diag = FALSE)
     V(g1)$size = 20
     plot(g1,edge.arrow.size=.4, main = input$user)
     
   #    if (length(unique(nudat$id))>2) 
   #       nudat <- filter(nudat,id>2)
   #    pick <- sample(unique(nudat$id),size = min(4,length(unique(nudat$id))), replace=FALSE)
   #    pick <- nudat$id
   #    nudat <- filter(nudat, id %in% pick)
   #    nobs <- length(unique(nudat$id))
   #    #Wstore <- matrix(NA,nrow=7,ncol=nobs)
   #    #Astore <- matrix(NA,nrow=7,ncol=nobs)
   #    gg <- NULL
   #    for (iplot in 1:nobs) {
   #    k <- unique(nudat$id)[iplot]
   #       A <- B
   #       temp <- filter(nudat,id==k)
   #       for (irow in 1:nrow(temp))
   #          A[temp$i[irow],temp$j[irow]] <- temp$value[irow]
   #    gg[[iplot]] <- graph_from_adjacency_matrix( abs(t(A)) , diag = FALSE)
   #    V(gg[[iplot]])$size = 20
   #    # #plot(g1,edge.arrow.size=.4)
   #    # k <- unique(nudat$id)[2]
   #    # A <- B
   #    # temp <- filter(nudat,id==k)
   #    # for (irow in 1:nrow(temp))
   #    #    A[temp$i[irow],temp$j[irow]] <- temp$value[irow]
   #    # g2 <- graph_from_adjacency_matrix( abs(t(A)) , diag = FALSE)
   #    # V(g2)$size = 20
   #    # #plot(g1,edge.arrow.size=.4)
   #    # if (nobs>2) {
   #    # k <- unique(nudat$id)[3]
   #    # A <- B
   #    # temp <- filter(nudat,id==k)
   #    # for (irow in 1:nrow(temp))
   #    #    A[temp$i[irow],temp$j[irow]] <- temp$value[irow]
   #    # g3 <- graph_from_adjacency_matrix( abs(t(A)) , diag = FALSE)
   #    # V(g3)$size = 20
   #    # }
   #    # #plot(g1,edge.arrow.size=.4)
   #    # if (nobs>3) {
   #    # k <- unique(nudat$id)[4]
   #    # A <- B
   #    # temp <- filter(nudat,id==k)
   #    # for (irow in 1:nrow(temp))
   #    #    A[temp$i[irow],temp$j[irow]] <- temp$value[irow]
   #    # g4 <- graph_from_adjacency_matrix( abs(t(A)) , diag = FALSE)
   #    # V(g4)$size = 20
   #    # }
   #    # #plot(g1,edge.arrow.size=.4)
   #    }
   #    
   #    par(mfrow=c(3,3), mar=c(0,0,0,0))
   #    for (iplot in 1:nobs) {
   #      plot(gg[[iplot]],edge.arrow.size=.4)
   #      text(0,0,unique(nudat$Name)[iplot])
   #    }
   #    # text(0,0,unique(nudat$Name)[1])
   #    # plot(g2,edge.arrow.size=.4)
   #    # text(0,0,unique(nudat$Name)[2])
   #    # if (nobs >2) plot(g3,edge.arrow.size=.4)
   #    # if (nobs >3) plot(g4,edge.arrow.size=.4)
   })
   
   output$evalplot <- renderPlot({
      
      Wstore <- matrix(NA,nrow=7,ncol=nobs)
      Astore <- matrix(NA,nrow=7,ncol=nobs)
      if (length(unique(evaldat$id))>2) 
         nudat <- filter(evaldat,id>2)
      #pick <- sample(unique(evaldat$id),size = max(5,length(unique(nudat$id))), replace=FALSE)
      pick <- evaldat$id
      nudat <- filter(evaldat, id %in% pick)
      nobs <- length(unique(nudat$id))
      Wstore <- matrix(NA,nrow=7,ncol=nobs)
      Astore <- matrix(NA,nrow=7,ncol=nobs)
      for (k in unique(nudat$id)) {
         A <- B
         temp <- filter(nudat,id==k)
         for (irow in 1:nrow(temp))
            A[temp$i[irow],temp$j[irow]] <- temp$value[irow]
         #}  
         adj_A <- adjoint3(A)
         adj_A
         Tmat <- LoopAnalyst::make.T(A,status=TRUE)
         #system.time(Tmat <- LoopAnalyst::make.T(A,status=TRUE))
         Wmat <- abs(adj_A)/Tmat
         Wmat
         
         adj_AA <- adj_A
         
         #image showing sign of adjoint and weights
         #colfunc <- colorRampPalette(c("white", "steelblue"))
         #image(1:7,1:7,t(Wmat[7:1,]),col = colfunc(7))
         #text(1,0,"+",col="white",cex=2)
         #pick <- which(adj_AA>0)
         #x <- ceiling(pick/7)
         #y <- 8-pick%%7
         #y[y==8] <- 1
         #text(x,y,"+",col="white",cex=1.5)
         #pick <- which(adj_AA<0)
         #x <- ceiling(pick/7)
         #y <- 8-pick%%7
         #y[y==8] <- 1
         #text(x,y,"-",col="white",cex=1.5)
         
         Wstore[,which(unique(nudat$id)==k)] <- rev(Wmat[,7])
         Astore[,which(unique(nudat$id)==k)] <- adj_AA[,7] #rev(adj_AA[,7])
         
      }
      labs <- c("Manager",
                "Habitat",
                "Fish",
                "Seabirds",
                "Fish",
                "ZP",
                "PP")
      lab2 <- unique(nudat$Name)
      #group_pic
      #image showing sign of adjoint and weights
      colfunc <- colorRampPalette(c("white", "steelblue"))
      par(mar=c(1,5,8,1),las=0)
      image(1:nobs,1:7,t(Wstore),col = colfunc(5),
            xlab = "",
            ylab = "", axes=F)
      box()
      #axis(1,labels=rep("",nobs),at=seq(0.5,nobs+0.5,1),tcl=-0.2)
      par(las=2)
      axis(2,labels=labs,at=seq(1,7,1),tcl=-0.2,cex=0.8)
      #par(las=0)
      axis(3,labels=lab2,at=seq(1,nobs),tcl=-0.2,cex=0.8)
      #mtext(side=3,"Model",line = 3)
      #text(1,0,"+",col="white",cex=2)
      pick <- which(Astore>0)
      x <- ceiling(pick/7)
      y <- 8-pick%%7
      y[y==8] <- 1
      text(x,y,"+",col="white",cex=1.5)
      pick <- which(Astore<0)
      x <- ceiling(pick/7)
      y <- 8-pick%%7
      y[y==8] <- 1
      text(x,y,"-",col="white",cex=1.5)
   })


}

# Run the application 
shinyApp(ui = ui, server = server)

