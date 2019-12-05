#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

   
  dane <- reactive({
    input$file
    if(is.null(input$file)) {
      return()
    }
 
   obiekt<-wczytywanie(input$file$datapath)
   # proba<-matrix(,ncol=4)
   # for(h in 1:length(xx[,1])){
   #   
   #   proba1<-scale3d(xx[h,],4,4,4)
   #   proba<-rbind(proba,proba1)
   # }
   # xx<-na.omit(proba)
   # colnames(xx) <- c('x','y','z','w')
   # rownames(xx) <- paste(seq(nrow(xx)))
   xx<-obiekt[[1]]
   vn<-obiekt[[2]]
   f<-obiekt[[3]]
    MP<-MacierzPowiazan(f)
   punkty<-oznaczonekrawedzie(f,xx,vn)
     
     ZbiorScian <- 1
       Zbior <- 2:length(f)

       while( length(Zbior) > 0){

         ss <- MP %>% filter((S1 %in% ZbiorScian & S2 %in% ZbiorScian==FALSE)
                             | (S1 %in% ZbiorScian==FALSE & S2 %in% ZbiorScian)) %>% slice(1)
         s1 <- intersect(ZbiorScian, ss %>% select(S1, S2) %>% unlist() %>% c())
         s2 <- setdiff(ss %>% select(S1, S2) %>% unlist() %>% c(), ZbiorScian)
         pp <- ss %>% select(P1, P2) %>% unlist() %>% c()
         katy<-kat(punkty,s1,s2,pp)
         # przesunięcie
         punkty[[s2]][,1:2]<- Przesuniecie(punkty,s1,s2,pp)
         # obrót
         punkty[[s2]]<-obracanie(punkty,s1,s2,pp,katy)
         ##################
         ZbiorScian <- append(ZbiorScian, s2)
         Zbior <- setdiff(Zbior, s2)


       }
  return(list(xx,vn,f,punkty))
  })
#   output$tekst<-renderPrint({
#     input$file
#     if(is.null(input$file)) {
#       return()
#     }
#     dane<-dane()
#     return(dane)
#   })
# })
  output$plot3d<-renderRglwidget({
    input$file
    if(!is.null(input$file))
    {
      dane<-dane()
      xx<-dane[[1]]
      vn<-dane[[2]]
      f<-dane[[3]]
      
      open3d(useNULL=T)
      rgl.bg(color = "white")
        rgl.viewpoint(theta = 15,phi =20,fov = 120,interactive = TRUE)
      # plot3d(NA,NA,NA,xlim = c(-5,5),ylim = c(-5,5),bty='n')
      for( i in 1:length(f)){
        lines3d(xx[f[[i]],1],xx[f[[i]],2],xx[f[[i]],3],color='black')
        lines3d(xx[f[[i]][1],1],xx[f[[i]][1],2],xx[f[[i]][1],3],color='black')
      }
      rglwidget()
      
    }
    
  })
  output$siatka<-renderPlot({
    input$file
    if(!is.null(input$file))
    {
   
      dane<-dane()
      punkty<-dane[[4]]
      # pdf("proba.pdf",width = 600,height = 300,paper = "a4")
      Rys(punkty)
      dev.copy(pdf,"myplot.pdf",width = 10/cm(1),height = 10/cm(1))
      dev.off()
      # Rys(punkty)
    }
  })
  
})
