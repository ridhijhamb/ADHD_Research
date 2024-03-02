library(shinythemes)
library(shiny)
library(survey)
library(stargazer)
library(dplyr)
library(sf)
library(tidyverse)
library(leaflet)
library(readr)
library(caret)
library(randomForest)



library(rsconnect)
#deployApp()

#-----------------------------------
# SERVER
#-----------------------------------

server<-function(input, output) {
  
  #Read cleaned data with selected features (221 out of 809 - Domain knowledge)
  data <- read.csv("NCHD_final.csv")
  
  # Highlight the problem - MAP % children with ADHD, ADD across the states - ADHD 
  df <- data %>% group_by( stusps)
  state_total<- df %>% count()%>% rename(total_in_state = n)
  
  df <- data %>% group_by( stusps, K2Q31A)
  total_ADD <- df %>% count()%>% rename(num_ADD = n)
  total_ADD1 <- total_ADD[total_ADD$K2Q31A==1, ]
  
  d1 <- inner_join(state_total,total_ADD1, by = c("stusps" = "stusps"))
  d1$stusps<- trimws(d1$stusps)
  d1$ratio_ADD<-round(d1$num_ADD*100/d1$total_in_state, 2)
  
  geo_data <- read_sf("cb_2016_us_state_500k")
  data_map <- left_join(geo_data,d1, by = c("STUSPS" = "stusps"))
  data_map$ratio_ADD <- factor(data_map$ratio_ADD)
  
  
  states_popup <- paste0("<strong>State: </strong>", 
                         data_map$NAME, 
                         "<br>Percentage Attention deficit children: ", 
                         data_map$ratio_ADD,"%")
  

  factpal <- colorFactor(c("#f0f9e8", 
                                    "#bae4bc",
                                    "#7bccc4",
                                    "#43a2ca"), data_map$ratio_ADD)
                                    
  
  map <- leaflet(data_map) %>%fitBounds(-124, 34, -62, 40)%>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .8,
                color = ~factpal(ratio_ADD),
                weight = 1, 
                popup = states_popup) %>%
    addLegend("topright", 
              colors =c("#f0f9e8", "#bae4bc","#7bccc4", "#43a2ca"),
              labels= c("Significantly Lower", "Lower","Higher","Significantly Higher"),  
              title= "Percentage ADD across states",
              opacity = 1) 
  output$map <- renderLeaflet(map)
  
  # Access to services across states
  df <- data %>% group_by( stusps, K4Q27)
  total_hc_no <- df %>% count()%>% rename(num_hc_no = n)
  total_hc_no1 <- total_hc_no[total_hc_no$K4Q27==1, ]
  
  data1 <- inner_join(state_total,total_hc_no1, by = c("stusps" = "stusps"))
  data1$hc_no<-round(data1$num_hc_no*100/data1$total_in_state, 2)
  
  df <- data %>% group_by( stusps, K3Q25)
  total_hc_exp<- df %>% count()%>% rename(num_hc_exp = n)
  total_hc_exp1<- total_hc_exp[total_hc_exp$K3Q25==1, ]
  
  data2 <- inner_join(data1,total_hc_exp1, by = c("stusps" = "stusps"))
  data2$hc_exp<-round(data2$num_hc_exp*100/data2$total_in_state, 2)
  
  df <- data %>% group_by( stusps, K11Q60)
  total_cash_help<- df %>% count()%>% rename(num_cash_help = n)
  total_cash_help1<- total_cash_help[total_cash_help$K11Q60==1, ]
  
  dt <- data[data$K11Q61==1 | data$K11Q62==1, ]
  df <- dt %>% group_by( stusps)
  total_food_ass<- df %>% count()%>% rename(num_food_ass = n)
  
  df <- data %>% group_by( stusps, S9Q34)
  total_wic<- df %>% count()%>% rename(num_wic = n)
  total_wic1<- total_wic[total_wic$S9Q34==1, ]
  
  dt <- data[data$K10Q11==1 | data$K10Q12==1| data$K10Q13==1|data$K10Q14==1, ]
  df <- dt %>% group_by( stusps)
  total_good_nghb<- df %>% count()%>% rename(num_good_nghb = n)
  
  dt <- data[data$K10Q20==1|data$K10Q22==1 | data$K10Q23==1, ]
  df <- dt %>% group_by( stusps)
  total_bad_nghb<- df %>% count()%>% rename(num_bad_nghb = n)
  
  dt <- data[data$K10Q40_R==1 | data$K10Q41_R==1, ]
  df <- dt %>% group_by( stusps)
  total_child_safe<- df %>% count()%>% rename(num_child_safe = n)
  
  df <- data %>% group_by( stusps, GOFORHELP)
  total_aware<- df %>% count()%>% rename(num_aware = n)
  total_aware1<- total_aware[total_aware$GOFORHELP==1, ]
  
  df <- data %>% group_by( stusps, K5Q32)
  total_comm_unsat<- df %>% count()%>% rename(num_comm_unsat = n)
  total_comm_unsat1<- total_comm_unsat[total_comm_unsat$K5Q32==2, ]
  
  df <- data %>% group_by( stusps, MENBEVCOV)
  total_insured<- df %>% count()%>% rename(num_insured = n)
  total_insured1<- total_insured[total_insured$MENBEVCOV==1, ]
  
  merged_df <- merge(data2, merge(total_cash_help1, merge(total_food_ass, merge(total_wic1, merge(total_bad_nghb, merge(total_good_nghb, 
                                                                                                                        merge(total_child_safe, merge(total_aware1, merge(total_comm_unsat1, total_insured1, by = "stusps"), by = "stusps"), by = "stusps"), by = "stusps"),
                                                                                                  by = "stusps"), by = "stusps"), by = "stusps"), by = "stusps"), by = "stusps")
  
  merged_df$stusps<- trimws(merged_df$stusps)
  merged_df$cash_help <-round(merged_df$num_cash_help*100/merged_df$total_in_state, 2)
  merged_df$food_ass <-round(merged_df$num_food_ass*100/merged_df$total_in_state, 2)
  merged_df$wic <-round(merged_df$num_wic*100/merged_df$total_in_state, 2)
  merged_df$good_nghb <-round(merged_df$num_good_nghb*100/merged_df$total_in_state, 2)
  merged_df$bad_nghb <-round(merged_df$num_bad_nghb*100/merged_df$total_in_state, 2)
  merged_df$child_safe  <-round(merged_df$num_child_safe*100/merged_df$total_in_state, 2)
  merged_df$aware  <-round(merged_df$num_aware*100/merged_df$total_in_state, 2)
  merged_df$comm_unsat  <-round(merged_df$num_comm_unsat*100/merged_df$total_in_state, 2)
  merged_df$insured <-round(merged_df$num_insured*100/merged_df$total_in_state, 2)
  
  
  data_map1 <- left_join(geo_data,merged_df, by = c("STUSPS" = "stusps"))
  data_map1$good_nghb <- factor(data_map1$good_nghb)
  
  states_popup1 <- paste0("<strong>State: </strong>", 
                          data_map1$NAME, 
                          "<br>Percentage children who did not have access to healthcare (physical, mental) when needed: ", 
                          data_map1$hc_no,"%",
                          "<br>Percentage parents who cannot afford good healthcare for their child: ", 
                          data_map1$hc_exp,"%",
                          "<br>Percentage children who received cash assistance from government in last 1 year: ", 
                          data_map1$cash_help,"%",
                          "<br>Percentage children who received food assistance from government in last 1 year: ", 
                          data_map1$food_ass,"%",
                          "<br>Percentage children who received WIC benefits in last 1 year: ", 
                          data_map1$wic,"%",
                          "<br>Percentage children who have access to a positive neighborhood env: ", 
                          data_map1$good_nghb,"%",
                          "<br>Percentage children who a growing up in a negative neighborhood env: ", 
                          data_map1$bad_nghb,"%",
                          "<br>Percentage children who are safe at home and school according to parents: ", 
                          data_map1$child_safe,"%",
                          "<br>Percentage parents aware about available resources and services for their child: ", 
                          data_map1$aware,"%",
                          "<br>Percentage parents who are unsatisfied with servives for their child - school, healthcare, child care etc.: ", 
                          data_map1$comm_unsat,"%",
                          "<br>Percentage children who are adequately insured and hence are comfortable seeking help: ", 
                          data_map1$insured,"%"
  )
  
  
  # coloring by factor level, from ACE variable
  factpal <- colorFactor(c("#F8DCA5", "#EDA570", "#D46B6D", "#BC494F"), data_map1$good_nghb)
  
  
  map2 <- leaflet(data_map1) %>%fitBounds(-124, 34, -62, 40)%>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .8,
                color = ~factpal(good_nghb),
                weight = 1, 
                popup = states_popup1) %>%
    addLegend("topright", 
              colors =c("#F8DCA5", "#EDA570", "#D46B6D", "#BC494F"),
              labels= c("Very few", "Few","Considerable","Significant"),  
              title= "Access to services and aids across states",
              opacity = 1) 
  
  
  filteredData <- reactive({
    if (input$ip1!= 'All')
    {
      data_map1 <- data_map1[data_map1$STUSPS==input$ip1, ]
    }
    else {
      data_map1
    }
  })
  
  observe({
    mapdata <- filteredData()
    if (nrow(mapdata) != 0) {
      leafletProxy("map2", data = mapdata) %>%
        clearShapes() %>% 
        clearControls()%>% 
        fitBounds(-124, 34, -62, 40) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = .8,
                    color = ~factpal(good_nghb),
                    weight = 1, 
                    popup = states_popup1) %>%
        addLegend("topright", 
                  colors =c("#F8DCA5", "#EDA570", "#D46B6D", "#BC494F"),
                  labels= c("Very few", "few","Considerable","Significant"),  
                  title= "Access to services and aids across states",
                  opacity = 1) 
      
    }
    
  })
  
  output$map2 <- renderLeaflet(map2)
  
  
  #Classification: Identifying important features
  
  data <- read.csv("dataset.csv")
  #Dropping cols that correlate with target var
  df_class <- data[, -c(which(names(data) %in% c('K2Q31D', 'ADDTREAT', 'HHID','stusps','stname')))]
  df_class  <- df_class [, c('K2Q33A',
                             'K7Q70_R',
                             'K2Q34A',
                             'REPEATED',
                             'K2Q36A',
                             'K4Q23',
                             'PHYSACTIV',
                             'MENBEVCOV',
                             'K2Q30A',
                             'K4Q22_R',
                             'HOURSLEEP',
                             'K4Q36',
                             'MEMORYCOND',
                             'SC_SEX',
                             'K6Q15', 'K2Q31A')]
  df_class$K2Q31A <- as.factor(df_class$K2Q31A)
  sample_df <- df_class[sample(nrow(df_class), 15000), ]
  
  trainIndex <- createDataPartition(sample_df$K2Q31A, p = 0.7, list = FALSE)
  train_data <- sample_df[trainIndex, ]
  test <- sample_df[-trainIndex, ]
  
  
  
  # Train a classification model using the random forest algorithm
  model <- randomForest(K2Q31A ~ ., data = train_data)
  
  
  # Make predictions on the testing data using the trained model
  preds <- predict(model, newdata = test)
  preds<- as.factor(preds)
  test$K2Q31A <- as.factor(test$K2Q31A)
  
  precision <- posPredValue(preds, test$K2Q31A)
  cm <- confusionMatrix(preds, test$K2Q31A)
  #cf <- classification(test$preds, test$K2Q31A)
  accuracy <- cm$overall[1]
  
  output$class_rep <- renderText({
    as.character(precision)
  })
  
  output$class_rep1 <- renderText({
    as.character(accuracy)
  })
  
  test1 <- data.frame(matrix(ncol = ncol(sample_df), nrow = 0))
  names(test1) <- names(sample_df)
  
  names(sample_df)

  test1 <- reactive({
    data.frame(
      K2Q33A=as.numeric(input$iv6) ,
      K7Q70_R=as.numeric(input$iv11),
      K2Q34A=as.numeric(input$iv2),
      REPEATED=as.numeric(input$iv14),
      K2Q36A=as.numeric(input$iv13),
      K4Q23=as.numeric(input$iv1),
      PHYSACTIV=as.numeric(input$iv12),
      MENBEVCOV=as.numeric(input$iv5),
      K2Q30A=as.numeric(input$iv3),
      K4Q22_R=as.numeric(input$iv7),
      HOURSLEEP=as.numeric(input$iv10),
      K4Q36=as.numeric(input$iv15),
      MEMORYCOND=as.numeric(input$iv4),
      SC_SEX=as.numeric(input$iv8),
      K6Q15=as.numeric(input$iv9), 
      K2Q31A=1
    )
    #c(as.numeric(input$iv1),as.numeric(input$iv2), as.numeric(input$iv3), as.numeric(input$iv4),as.numeric(input$iv5), as.numeric(input$iv6), 
                                #as.numeric(input$iv7),as.numeric(input$iv8), as.numeric(input$iv9), as.numeric(input$iv10), as.numeric(input$iv11), as.numeric(input$iv12), 
                               # as.numeric(input$iv13), as.numeric(input$iv14), as.numeric(input$iv15), 1)
  })
  
  observe({
    data_new <- test1()
    pred <- predict(model, newdata = data_new)
    pred_numeric <- as.numeric(as.character(pred))
    pred_numeric
    output$class_op <- renderText({
      if (pred_numeric==1)
      {
        "At risk"
      }
      else {
        "Not at risk"
      }
    })
    
    
    })
  
  
  #Plots
  output$plot1 <- renderPlot({
    df <- data %>% group_by(K2Q31A, K2Q33A)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$K2Q33A<- factor(total_ADD$K2Q33A)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = K2Q33A, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$K2Q33A))+
      ylim(0, 25000) +
      labs(x = "Anxiety", y = "Number of children", fill = "ADHD")
  })
  
  output$plot2 <- renderPlot({
    df <- data %>% group_by(K2Q31A, SC_SEX)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$SC_SEX <- factor(total_ADD$SC_SEX)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = SC_SEX, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$SC_SEX))+
      ylim(0, 15000) +
      labs(x = "Gender", y = "Number of children", fill = "ADHD")
  })
  
  output$plot3 <- renderPlot({
    df <- data %>% group_by(K2Q31A, K4Q23)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$K4Q23<- factor(total_ADD$K4Q23)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = K4Q23, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$K4Q23))+
      ylim(0, 25000) +
      labs(x = "On medication for emotions", y = "Number of children", fill = "ADHD")
  })
  
  output$plot4 <- renderPlot({
    df <- data %>% group_by(K2Q31A, K2Q34A)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$K2Q34A<- factor(total_ADD$K2Q34A)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)

    ggplot(total_ADD, aes(x = K2Q34A, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$K2Q34A))+
      ylim(0, 25000) +
      labs(x = "Behavioral Problems", y = "Number of children", fill = "ADHD")
  })
  output$plot5 <- renderPlot({
    df <- data %>% group_by(K2Q31A, MENBEVCOV)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$MENBEVCOV <- factor(total_ADD$MENBEVCOV)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
 
    ggplot(total_ADD, aes(x = MENBEVCOV, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$MENBEVCOV))+
      ylim(0, 25000) +
      labs(x = "Health Insurance - Cover Mental Behavioral Needs", y = "Number of children", fill = "ADHD")
  })
  output$plot6<- renderPlot({
    df <- data %>% group_by(K2Q31A, MEMORYCOND)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$MEMORYCOND <- factor(total_ADD$MEMORYCOND)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)

    ggplot(total_ADD, aes(x = MEMORYCOND, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$MEMORYCOND))+
      ylim(0, 15000) +
      labs(x = "Serious Difficulty Concentrating, Remembering, or Making Decisions", y = "Number of children", fill = "ADHD")
  })
  
  output$plot7<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE3)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE3<- factor(total_ADD$ACE3)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE3, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE3))+
      ylim(0, 15000) +
      labs(x = "Parent or Guardian Divorced", y = "Number of children", fill = "ADHD")
  })
  
  output$plot8<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE4)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE4<- factor(total_ADD$ACE4)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE4, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE4))+
      ylim(0, 25000) +
      labs(x = "Parent or Guardian Died", y = "Number of children", fill = "ADHD")
  })
  
  output$plot9<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE5)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE5<- factor(total_ADD$ACE5)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE5, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE5))+
      ylim(0, 25000) +
      labs(x = "Parent or Guardian Time in Jail", y = "Number of children", fill = "ADHD")
  })
  
  output$plot10<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE6)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE6<- factor(total_ADD$ACE6)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE6, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE6))+
      ylim(0, 25000) +
      labs(x = " Adults Slap, Hit, Kick, Punch Others", y = "Number of children", fill = "ADHD")
  })
  
  output$plot11<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE7)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE7<- factor(total_ADD$ACE7)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE7, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE7))+
      ylim(0, 25000) +
      labs(x = " Victim of Violence", y = "Number of children", fill = "ADHD")
  })
  output$plot12<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE8)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE8<- factor(total_ADD$ACE8)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE8, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE8))+
      ylim(0, 25000) +
      labs(x = " Lived with mentally ill", y = "Number of children", fill = "ADHD")
  })
  output$plot13<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE9)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE9<- factor(total_ADD$ACE9)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE9, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE9))+
      ylim(0, 25000) +
      labs(x = " Lived with Person with Alcohol/Drug Problem", y = "Number of children", fill = "ADHD")
  })
  output$plot14<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE10)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE10<- factor(total_ADD$ACE10)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE10, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE10))+
      ylim(0, 25000) +
      labs(x = "Treated Unfairly Because of Race", y = "Number of children", fill = "ADHD")
  })
  output$plot15<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE12)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE12<- factor(total_ADD$ACE12)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE12, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE12))+
      ylim(0, 15000) +
      labs(x = "Treated Unfairly Because of their Sexual Orientation or Gender Identity", y = "Number of children", fill = "ADHD")
  })
  output$plot16<- renderPlot({
    df <- data %>% group_by(K2Q31A, ACE1)
    total_ADD <- df %>% count()%>% rename(num_ADD = n)
    total_ADD$ACE1<- factor(total_ADD$ACE1)
    total_ADD$K2Q31A <- factor(total_ADD$K2Q31A)
    
    ggplot(total_ADD, aes(x = ACE1, y = num_ADD, fill=K2Q31A)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits=unique(total_ADD$ACE1))+
      ylim(0, 15000) +
      labs(x = "Hard to Cover Basics Like Food or Housing", y = "Number of children", fill = "ADHD")
  })

  
}


#-----------------------------------
# UI
#-----------------------------------
ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Development in children",
    #headerPanel("Monitoring early age development in children"),
    
    tabPanel(
      "Problem?",
      headerPanel("What is the problem?"),
      br(),
      h3("What impact does lack of opportunities and care (ACEs, Poverty etc.) have on a child’s overall health, eventually leading to serious conditions and abnormal behavior patterns?"),
      br(),
      h3('How does this relate to severe outcomes like unemployment, homelessness, and substance abuse in adulthood?'),
      br(),
      
      img(src = "cycle.png", width = "600px"),
      br(),
      br(),
      print('9.3% of children in America have ADHD
We focus on predicting the risk of ADD/ADHD in children based on their childhood experiences and overall health, hence this project aims at addressing half the problem cycle.
Outcome of this research would be multi fold
Predicting ADHD early on can help parents and key stakeholders take the required help timely
Understanding the relationship between ADHD and children’s health and experiences.
Understanding how existing policies are helping and how they can be improved in order to better support such individuals.
')
      ),
    
    tabPanel(
      "Scale of the problem",
      headerPanel("How significant is the problem?"),
      br(),
      h2(""),
      leafletOutput("map",
                    width = "100%",
                    height = "600px"),
      
    ),
    
    tabPanel(
      "Impact assessment",
      headerPanel("Impact of policies and services"),
      br(),
      h2(""),
      leafletOutput("map2",
                    width = "100%",
                    height = "600px"),
      
      absolutePanel(
        class = "panel panel-default",
        draggable = TRUE,
        top = 150,
        left = 50,
        right = "auto",
        bottom = "auto",
        width = 330,
        height = "auto",
        h2("Select a state"),
        selectInput("ip1",
                    "Conditions and servives:",
                    c("AL", "AK" ,"AZ", "AR", "CA" ,"CO", "CT" ,"DE" ,"DC" ,"GA", "HI" ,"ID" ,"IL" ,"IN" ,"IA" ,"KS" ,"MD" ,"MN" ,"MS" ,"MT", "NV",
                      "NJ", "NM" ,"ND" ,"OK", "PA" ,"SC" ,"SD", "UT" ,"VT","WV", "WY" ,"AS" ,"PR" ,"FL", "KY" ,"LA", "ME", "MA", "MI" ,"MO", "NE" ,
                      "NH" ,"NY" ,"NC" ,"OH" ,"OR", "RI" ,"TN" ,"TX", "VA", "WA" ,"WI" ,"GU", "MP", "VI","All"),
                    selected = "All")
      )
    ),
    tabPanel(
      "Visualizations",
      br(),
      h2(""),
      fluidRow(
        column(width = 4,
               
               plotOutput("plot1")
        ),
        column(width = 4,
               
               plotOutput("plot2")
        ),
        column(width = 4,
               
               plotOutput("plot3")
        )
        
        ),
      fluidRow(
        column(width = 4,
               
               plotOutput("plot4")
        ),
        column(width = 4,
               
               plotOutput("plot5")
        ),
        column(width = 4,
               
               plotOutput("plot6")
        )
        
      )),
    tabPanel(
      "ACE Visualizations",
      br(),
      h2(""),
      fluidRow(
        column(width = 4,
               
               plotOutput("plot7")
        ),
        column(width = 4,
               
               plotOutput("plot8")
        ),
        column(width = 4,
               
               plotOutput("plot9")
        )
        
      ),
      fluidRow(
        column(width = 4,
               
               plotOutput("plot10")
        ),
        column(width = 4,
               
               plotOutput("plot11")
        ),
        column(width = 4,
               
               plotOutput("plot12")
        )
        
      ),
      fluidRow(
        column(width = 4,
               
               plotOutput("plot13")
        ),
        column(width = 4,
               
               plotOutput("plot14")
        ),
        column(width = 4,
               
               plotOutput("plot15")
        )
        
      ),
      fluidRow(
        column(width = 4,
               
               plotOutput("plot16")
        ),
        
      )
      ),
    
    tabPanel(
      "ADHD/ADD Prediction",
      headerPanel("Top 15 most important features in predicting ADHD"),
      br(),
      h2(""),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          h2("Make your own predictions"),
          br(),
          
          selectInput(
            "iv1",
            label = "Taking medications for emotions",
            c("No"="2", 
              "Yes"="1"),
            selected = "2"),
          
          selectInput(
            "iv2",
            label = "Behavioral/Conduct Problems",
            c("No"="1", 
              "Currently has"="3", 
              "Had in the past"="2"),
            selected = "1"),
          
          selectInput(
            "iv3",
            label = "Learning Disability",
            c("No"="1", 
              "Currently has"="3", 
              "Had in the past"="2"),
            selected = "1"),
          
          selectInput(
            "iv4",
            label = "Serious Difficulty Concentrating, Remembering, or Making Decisions",
            c("No"="2", "Yes"="1"),
            selected = "2"),
          
          selectInput(
            "iv5",
            label = "Health Insurance covers Mental & Behavioral Needs",
            c("No"="3", 
              "Always"="1", 
              "Usually"="2"),
            selected = "3"),
          
          selectInput(
            "iv6",
            label = "Anxiety",
            c("No"="1", 
              "Currently has"="3", 
              "Had in the past"="2"),
            selected = "1"),
          
          selectInput(
            "iv7",
            label = "Mental Health Professional Treatment",
            c("Received or needed mental health care and did not have difficulty getting it"="1", 
              "Received or needed mental health care but it was somewhat difficult to get it" ="2",
              "Received or needed mental health care but it was very difficult to get it"="3",
              "It was not possible to obtain care"="4",
              "Did not need to see a mental health professional"="95"),
            selected = "1"),
          
          selectInput(
            "iv8",
            label = "Gender",
            c("Male"="1", "Female"="2", "Other"="99"),
            selected = "1"),
          
          selectInput(
            "iv9",
            label = "Ever received a Special Education Plan",
            c("Yes"="1", "No"="2"),
            selected = "1"),
          
          sliderInput("iv10", "Hours of sleep during a day",
                      min = 0, max = 10, value = 4),
          
          selectInput(
            "iv11",
            label = "Argues Too Much",
            c("Never"="4", 
              "Sometimes"="3",
              "Usually"="2",
              "Always"="1"
              ),
            selected = "4"),
          
          selectInput(
            "iv12",
            label = "Number of times a week the child indulges in physical activity",
            c("Never"="1", 
              "1-3"="2",
              "4-6"="3",
              "Daily"="4"
            ),
            selected = "1"),
          
          selectInput(
            "iv13",
            label = "Developmental Delay",
            c("No"="1", 
              "Currently has"="3", 
              "Had in the past"="2"
            ),
            selected = "1"),
          
          selectInput(
            "iv14",
            label = "Child Repeated Any Grades",
            c("Yes"="1", 
              "No"="2"
            ),
            selected = "1"),
          
          selectInput(
            "iv15",
            label = "Child ever received special services to meet child’s developmental needs such as speech, occupational or behavioral therapy",
            c("Yes"="1", 
              "No"="2"
            ),
            selected = "1")
        ),
        
        mainPanel(
          br(),

          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Classification Model",
              h3("Is the child with specified traits at risk?"),
              HTML('</br>'),
              div(h3(textOutput("class_op"))),
              
            ),
            tabPanel(
              "Model performance",
              h3("Precision"),
              HTML('</br>'),
              div(textOutput("class_rep")),
              h3("Accuracy"),
              HTML('</br>'),
              div(textOutput("class_rep1")),
              
              ),
            
            tabPanel(
              "Feature Importance",
              img(src = "feature_imp.png", width = "600px"),
              HTML('</br>'),
              HTML('</br>'),
              HTML('</br>'),
              img(src = "Key.png", width = "500px")
          )
        )
        
        )
    ))
   
    
    
    
    
         
  )
))


shinyApp(ui = ui, server = server)
#-----------------------------------

















