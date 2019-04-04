#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
 
library(shiny)


business_scores=read.csv("../data/business_scores.csv")


plotposition1 = function(business_id){
  #layout(matrix(c(1,2,3,4,5,6,7),nrow=2))
  #plot(density(business_scores$topic_1_Facilities),xlab = "Scores on topic_1_Facilities")
  #abline(v=business_scores[business_id,2],col="red")
  paste("On topic_1_Facilities, you have beat ",round(sum(business_scores[,2]<business_scores[which(business_scores$business_id==business_id),2])/1886
                                                      *100,2),"% businesses.")}
plotposition2 = function(business_id){
  #plot(density(business_scores$topic_2_Courses),xlab = "Scores on topic_2_Courses")
  #abline(v=business_scores[business_id,3],col="red")
  paste("On topic_2_Courses, you have beat ",round(sum(business_scores[,3]<business_scores[which(business_scores$business_id==business_id),3])/1886
                                                   *100,2),"% businesses.")}
plotposition3 = function(business_id){
  #plot(density(business_scores$topic_3_Service_and_Accessories),xlab = "Scores on topic_3_Service_and_Accessories")
  #abline(v=business_scores[business_id,4],col="red")
  paste("On topic_3_Service_and_Accessories, you have beat ",round(sum(business_scores[,4]<business_scores[which(business_scores$business_id==business_id),4])/1886
                                                                   *100,2),"% businesses.")}
plotposition4 = function(business_id){
  #plot(density(business_scores$topic_4_Membership_and_Price),xlab = "Scores on topic_4_Membership_and_Price")
  #abline(v=business_scores[business_id,5],col="red")
  paste("On topic_4_Membership_and_Price, you have beat ",round(sum(business_scores[,5]<business_scores[which(business_scores$business_id==business_id),5])/1886
                                                                *100,2),"% businesses.")}
plotposition5 = function(business_id){
  #plot(density(business_scores$topic_5_Trainer),xlab = "Scores on topic_5_Trainer")
  #abline(v=business_scores[business_id,6],col="red")
  paste("On topic_5_Trainer, you have beat ",round(sum(business_scores[,6]<business_scores[which(business_scores$business_id==business_id),6])/1886
                                                   *100,2),"% businesses.")}
plotposition6 = function(business_id){
  #plot(density(business_scores$topic_6_Time),xlab = "Scores on topic_6_Time")
  #abline(v=business_scores[business_id,7],col="red")
  paste("On topic_6_Time, you have beat ",round(sum(business_scores[,7]<business_scores[which(business_scores$business_id==business_id),7])/1886
                                                *100,2),"% businesses.")}
plotposition7 = function(business_id){
  #plot(density(business_scores$topic_7_Environment),xlab = "Scores on topic_7_Environment")
  #abline(v=business_scores[business_id,8],col="red")
  paste("On topic_7_Environment, you have beat ",round(sum(business_scores[,8]<business_scores[which(business_scores$business_id==business_id),8])/1886
                                                       *100,2),"% businesses.")
}
  str = "The business id you input is not a gym business."
  Ntable = data.frame(str)
  row.names(Ntable) = NULL
  colnames(Ntable) = 'NOTICE !'
  
  suggest1 = function(business_id){
  business_summary = business_scores[business_scores$business_id==business_id,]
  sugg_table = as.data.frame(matrix(as.vector(business_summary)[11:38],7,4,byrow = TRUE))
  reviews = rep( business_summary$review_number_id,7)
  topic = as.vector(c('Facilities','Courses','Service&Accessories','Membership&Price','Trainers','Time','Environment'))
  score = round(business_summary[,2:8],5)
  sugg_table = cbind(topic,reviews,sugg_table,t(score))
  colnames(sugg_table) = c("topic","reviews","mention_times","positive","negative","neutral","score")
  sugg_table$Improvement = c(0.8225,1.0843,0.7164,1.1960,0.6984,0.8813,0.6448)
  row.names(sugg_table) = NULL
  return(sugg_table)}


ui <- fluidPage(
  
  # Application title
  headerPanel("Improve gym business now !"),
  sidebarPanel(
    numericInput("id", "Business id:", 458),
    helpText("Note: Input the your gym business id."),
    submitButton("Get suggestions!")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("How to use this app?",
               h4(textOutput("howtouse1")),
               h4(textOutput("howtouse2")),
               h4(textOutput("howtouse3")), 
               h4(textOutput("howtouse4")),
               h4(textOutput("howtouse5")),
               h4(textOutput("howtouse6")),
               h4(textOutput("howtouse7")),
               h4(textOutput("howtouse8")),
               h4(textOutput("howtouse9")),
               h4(textOutput("howtouse10")),
               h4(textOutput("howtouse11")),
               h4(textOutput("howtouse12"))
      ),
    
      tabPanel("Where is the gym in the business?",
               h4(textOutput("location1")),
               h4(textOutput("location2")),
               h4(textOutput("location3")),
               h4(textOutput("location4")),
               h4(textOutput("location5")),
               h4(textOutput("location6")),
               h4(textOutput("location7"))
               
      ),
      tabPanel("Suggestions wrt reviews",
               DT::dataTableOutput("sugges1"),
               h4(textOutput("Instruction"))
      ),
      
      tabPanel("Suggestions wrt attributes",
               imageOutput("myimage"),
               h4(textOutput("attr1")),
               h4(textOutput("attr2")),
               h4(textOutput("attr3")),
               h4(textOutput("attr4"))
      )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$howtouse1<-renderText({
     paste ("This app is to give gym business owners suggestions on how to improve their ratings on yelp.")})
    output$howtouse2<-renderText({
     paste('The design and suggestions mainly depend on reviews and attributes.Seven Main topics are talked by 
           all the reviews. Their subject and examples are :')})    
    output$howtouse3<-renderText({
     paste ("topic_1_Facilities:equipment,machine,weight,cardio,pool.")})      
    output$howtouse4<-renderText({
     paste ("topic_2_Courses:class, training, session, yoga, course.")})  
    output$howtouse5<-renderText({
      paste ("topic_3_Service_and_Accessories: service, locker, shower, desk, change.")})
    output$howtouse6<-renderText({
      paste ("topic_4_Membership_and_Price: membership, money ,contract, rate, bill.")})
    output$howtouse7<-renderText({
      paste ("topic_5_Trainer: trainer, instructor, coach, teacher, advisor.")})
    output$howtouse8<-renderText({
      paste ("topic_6_Time: time, day, month, morning, night.")})
    output$howtouse9<-renderText({
      paste ("topic_7_Environment: music, house, environment, design, smell.")})
    output$howtouse10<-renderText({
      paste ("The tab Where is your business gives the position of your own business, 
             how many business you have beat in the whole business on each topic.")})
    output$howtouse11<-renderText({
      paste ("The tab Suggestions wrt reviews gives your the summary of all your reviews, 
             and give quantative suggestions on how the improvement of score on each topic 
             influence your ratings.")})
    output$howtouse12<-renderText({
      paste ("The tab Suggestions wrt attributes gives suggestions of improving ratings wrt attributes")
})
    
  output$location1<-renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste("The business id you input is not a gym business.")
    }else{
      plotposition1(input$id)
    }
  })
  output$location2<-renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      plotposition2(input$id)
    }
  })
  output$location3<-renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      plotposition3(input$id)
    }
  })
  output$location4<-renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      plotposition4(input$id)
    }
  })
  output$location5<-renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      plotposition5(input$id)
    }
  })
  output$location6<-renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      plotposition6(input$id)
    }
  })
  output$location7<-renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      plotposition7(input$id)
    }
  })
  output$sugges1 <- DT::renderDataTable({
    if ((input$id %in% business_scores$business_id) == FALSE){
      DT::datatable(Ntable,options = list(paging = FALSE),rownames=FALSE) 
    }else{
      DT::datatable(suggest1(input$id),options = list(paging = FALSE),caption = 'Suggestions wrt reviews:')
    }
      })
  
  output$Instruction <- renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      paste("Note of Improvement: This is the increase amount of weighted ratings if score of this topic increases by 1.")
    }
  }) 

  output$myimage <- renderImage({
    list(src = "../figure/decision_tree.png",
         contentType = 'image/png',
         width = 500,
         height = 400)
  }, deleteFile = FALSE)
  
  output$attr1 <- renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      paste("The figure provides information about ‘GoodForKids’, ‘ByAppointmentOnly’ and ‘BusinessAcceptCreditCards’ on Yelp."
)
    }
  })
  output$attr3 <- renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      paste("It will better if street parking is provided." )
    }
  })
  output$attr4 <- renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      paste("Set places for kids playing!"
      )
    }
  })
  output$attr2 <- renderText({
    if ((input$id %in% business_scores$business_id) == FALSE){
      paste(" ")
    }else{
      paste("Provide appointment for premium customers for better service."
      )
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)



