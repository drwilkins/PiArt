#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);require(ggplot2);require(plotly);require(deldir)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel(windowTitle="PiArt",h1("PiArt: Learning geometric properties through artistic exploration",style="font-family: 'Courier New';color: #444444;")),
  p("Change a parameter and start exploring!",style="font-family: 'Courier New';color: #444444;"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("N",
                     "Number of Points:",
                     min = 1,
                     max = 5e3,
                     value = 8),
        radioButtons("plottype","Plot Type",c("Spiral")),
          conditionalPanel("input.plottype==Spiral",
          sliderInput("rotations","How many rotations?",min=0,max=20,value=1,step=.1),
          checkboxInput("radii","Show radii?",F))#end conditionalPanel
          
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("graf")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  theme_set(theme_bw())
   
output$graf<- renderPlotly({  
n=input$N #orig. default 100
print(input$plottype)
if(input$plottype=="Spiral"){
angulos=seq(from=0, by=input$rotations*2*pi/n, length.out=n)
radio=1:n#rep(1,8)#1:n
puntos=data.frame(x=radio*cos(angulos), y=radio*sin(angulos))
puntos$radius<-1:nrow(puntos)#sqrt(puntos$x^2+puntos$y^2)

g1<-ggplot() + 
  geom_point(aes(x = x, y = y), data=puntos)+coord_fixed()+ggtitle("")
if(input$radii==T){
g2<-g1+geom_segment(aes(x=x,y=y,xend=0,yend=0,frame=radius),data=puntos,size=.1)
}else{g2<-g1}
#print("test passed")
}#end spiral plot def

ggplotly(g2)
 
 
})


}

# Run the application 
shinyApp(ui = ui, server = server)

