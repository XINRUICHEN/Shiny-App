library(shiny)
library(tidyverse)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(ggplot2)

# Load data
meat_data=read_csv("data/carcass_calculator_data.csv")
mur=read_csv("data/beef.csv")
calorie <- read_excel("data/Calorie.xlsx")
protein <- read_excel("data/upload.xlsx")

water_unit = 6.355078 #(million gallons)
land_unit = 77 #(acres)
CO2_unit = 102.959 #(thousand lbs)
agr<-c(protein$Food_type)

my_choices = c(meat_data$cut)

# Define server function
server <- function(input, output, session) {
   observe({
      updateSelectInput(
         session, 'type', choices=my_choices,
         selected = if (input$All) my_choices
      )})
   observe({
      updateSelectInput(
         session, 'type1', choices=my_choices,
         selected = if (input$ALL) my_choices
      )})
   observe({  updateSelectInput(
         session, 'agProduct', choices=agr,
         selected = if (input$all) agr
      )
   })

    # Subset data
    selected_type <- reactive({
      req(input$water_weight)
      req(input$land_weight)
      req(input$CO2_weight)
     })

   
   Score <- reactive({
      weight_meat <- if("All" %in% input$type) {
         meat_data
      } else {
         meat_data %>% filter(cut %in% input$type)
      }
      weight_meat %>%
         mutate(number_cows = ceiling(input$weight_meat/total_weight),
                water_usage = water_unit * number_cows,
                land_usage =  land_unit* number_cows ,
                CO2_emi = CO2_unit * number_cows )
     })
   Score2 <- reactive({
      weight_meat2 <- if("ALL" %in% input$type1) {
         meat_data
      } else {
         meat_data %>% filter(cut %in% input$type1)
      }
      weight_meat2 %>%
         mutate(number_cows = ceiling(input$weight_meat/total_weight),
                water_usage = water_unit * number_cows,
                land_usage =  land_unit* number_cows ,
                CO2_emi = CO2_unit * number_cows ) %>%
         mutate(EI = 100*(input$water_weight * (water_usage/(water_unit * ceiling(input$weight_meat/1.25)))
                          + input$land_weight * (land_usage/(land_unit*ceiling(input$weight_meat/1.25)))
                          + input$CO2_weight * (CO2_emi/ (CO2_unit*ceiling(input$weight_meat/1.25)))))
   })
   
 
   output$Table <- renderTable({
      Score2() %>%
         select("cut", "number_cows","water_usage","land_usage", "CO2_emi","EI") %>%
         rename("Cut"="cut", "Number of cows"="number_cows","Waster(Mill. gallons)"="water_usage","Land(acres)"="land_usage",
                "CO2(thous. lbs)"="CO2_emi","EI"="EI")
   })
  

   
   output$details <- renderPlotly({
      validate(
         need(nrow(Score()) > 0, 'No data exists, please select the cut')
      )
      plot_ly(Score(), 
              y = ~CO2_emi, 
              x = ~land_usage,
              type = 'scatter', 
              size = ~water_usage,
              sizes = c(10, 5000),
              color = ~cut,
              opacity = 0.5,
              text = ~paste('CUT:',cut,
                            '<br>CO2(thous. lbs):',CO2_emi,
                            '<br>Land(acres):', land_usage,
                            '<br>Water(Mill. gallons):', water_usage,
                            '<br>Number of cows:',number_cows)) %>%
         layout(
            title = 'Overall information for each cut',
            yaxis = list(title = "CO2emission (thous. lbs)",showgrid = FALSE),
            xaxis = list(title="Land (acres)",showgrid = FALSE),
            showlegend = FALSE,
            scene = list(
               xaxis = list(
                  spikecolor = '#a009b5',
                  spikesides = FALSE,
                  spikethickness = 6
               ),
               yaxis = list(
                  spikecolor = '#a009b5',
                  spikesides = FALSE,
                  spikethickness = 6        
               ),
               zaxis = list(
                  spikecolor = '#a009b5',
                  spikethickness = 6
               )
            )
         )
      
   })

   
   output$calcal <- renderPlotly({
      validate(
         need(nrow(calorie_data()) > 0, 'No data exists, please select the food type')
      )
      plot_ly(calorie_data() , 
              x = ~Wateruse, 
              y = ~CO2e,
              type = 'scatter', 
              size = ~Landuse,
              sizes = c(10, 5000),
              color = ~Food_type,
              opacity = 0.3,
              text = ~paste('Food tpye:',Food_type,
                            '<br>Land(acres):',Landuse,
                            '<br>Water(Mill. gallons):',Wateruse,
                            '<br>CO2(thous. lbs):', CO2e)) %>%
         layout(
            title = 'Overall information',
            xaxis = list(title="Water usage(Mill. gallons)",showgrid = FALSE),
            yaxis = list(title="CO2 emission(thous. lbs)",showgrid = FALSE),
            showlegend = FALSE,
            scene = list(
               xaxis = list(
                  spikecolor = '#a009b5',
                  spikesides = FALSE,
                  spikethickness = 6
               ),
               yaxis = list(
                  spikecolor = '#a009b5',
                  spikesides = FALSE,
                  spikethickness = 6        
               ),
               zaxis = list(
                  spikecolor = '#a009b5',
                  spikethickness = 6
               )
            )
         )
      
   })
   #Create histagram object the plotOutput function is expecting
    output$distplot <- renderPlotly({
       validate(
          need(nrow(Score2()) > 0, 'No data exists, please select the cut')
       )
     pp<- Score2() %>%
         ggplot(mapping = aes(y = EI,x=cut,fill=cut, text=paste("Cut:",cut,"<br>","EI:",EI))) +
         geom_bar(stat = "identity", alpha=0.7, color = "white")+
         labs(title = "Environmental Impact Score of Each Cut",
                x = NULL,
                y = "Environmental Impact Score") +
       theme_classic() +theme(legend.position = "none")+
       coord_flip()
      ggplotly(pp,tooltip = c("text"))
      })
   
    protein_data <- reactive({
       if("all" %in% input$agProduct) {
          protein
       } else {
          protein %>% filter(Food_type %in% input$agProduct)
       }
    })
    ## generate a bubble plot
    output$prepre <- renderPlotly({
       validate(
          need(nrow(protein_data()) > 0, 'No data exists, please select the food type')
       )
       plot_ly(protein_data() , 
               x = ~Wateruse, 
               y = ~CO2e,
               type = 'scatter', 
               size = ~Landuse,
               sizes = c(10, 5000),
               color = ~Food_type,
               opacity = 0.3,
               text = ~paste('Food tpye:',Food_type,
                             '<br>Land(acres):',Landuse, 
                             '<br>Water(Mill. gallons)',Wateruse,
                             '<br>CO2(thous. lbs)', CO2e)) %>%
          layout(
             title = 'Overall information',
             xaxis = list(title="Water usage(Mill. gallons)",showgrid = FALSE),
             yaxis = list(title="CO2 emission(thous. lbs)", showgrid = FALSE),
             showlegend = FALSE,
             scene = list(
                xaxis = list(
                   spikecolor = '#a009b5',
                   spikesides = FALSE,
                   spikethickness = 6
                ),
                yaxis = list(
                   spikecolor = '#a009b5',
                   spikesides = FALSE,
                   spikethickness = 6        
                ),
                zaxis = list(
                   spikecolor = '#a009b5',
                   spikethickness = 6
                )
             )
          )
       
    })
    

   # Create histagram object the plotOutput function is expecting
   output$pro_Land <- renderPlotly({
      validate(
         need(nrow(calorie_data()) > 0, 'No data exists, please select the food type')
      )
      p2 <- protein_data() %>%
         ggplot(aes(reorder(x = Food_type,Landuse),y = Landuse,text=paste("Food type:",Food_type,"<br>","Land usage:",Landuse))) +
         geom_bar(stat = "identity", fill = "deepskyblue3", color = "white") +
         labs(title = "Land Use \nPer Tonne Protein Consumed",
              x = NULL,
              y = "Total Land Use (acres)") +
         theme_classic() +
         coord_flip()
      ggplotly(p2,tooltip = c("text"))
   })
   
   output$pro_Water <- renderPlotly({
      validate(
         need(nrow(calorie_data()) > 0, 'No data exists, please select the food type')
      )
      p3<- protein_data() %>%
         ggplot(aes(reorder(x = Food_type,Wateruse),text=paste("Food type:",Food_type,"<br>","Water usage:",Wateruse)))+
         geom_bar(mapping = aes(y = Wateruse), stat = "identity",fill="deeppink", alpha=0.7, color = "white")+
         labs(  title = "Water Use \nPer Tonne Protein Consumed",
                x = NULL,
                y = "Total Water Consumption (Mill. gallons)")  +
         theme_classic() +
         coord_flip()
      ggplotly(p3,tooltip = c("text"))
   })
   
   output$pro_CO2 <- renderPlotly({
      validate(
         need(nrow(calorie_data()) > 0, 'No data exists, please select the food type')
      )
      p4 <-  protein_data() %>%
         ggplot(aes(reorder(x = Food_type,CO2e),text=paste("Food type:",Food_type,"<br>","CO2:",CO2e))) +
         geom_bar(mapping = aes(y = CO2e), stat = "identity", fill = "Seagreen3", alpha=0.7,color = "white")+
        
         labs(  title = "CO2 Emission \nPer Tonne Protein Consumed",
                x = NULL,
                y = "Total Carbon Dioxide Emission (CO2e)") +
         theme_classic() +
         coord_flip()
      ggplotly(p4,tooltip = c("text"))
   })
   
   calorie_data <- reactive({
      if("all" %in% input$agProduct) {
         calorie
      } else {
         calorie %>% filter(Food_type %in% input$agProduct)
      }
   })
   
   output$cal_Land <- renderPlotly({
      validate(
         need(nrow(calorie_data()) > 0, 'No data exists, please select the food type')
      )
      p5<- calorie_data() %>%
         ggplot(aes(reorder(x = Food_type,Landuse),text=paste("Food type:",Food_type,"<br>","Land usage:",Landuse))) +
         geom_bar(mapping = aes(y = Landuse), stat = "identity", fill = "sienna2", color = "white")+
         labs(  title = "Land Use \nPer Million Kilocalories Consumed ",
                x = NULL,
                y = "Total Land Use (acres)") +
         theme_classic() +
         coord_flip()
      ggplotly(p5,tooltip = c("text"))
   })
   
   output$cal_Water <- renderPlotly({
      validate(
         need(nrow(calorie_data()) > 0, 'No data exists, please select the food type')
      )
      p6<- calorie_data() %>%
         ggplot(aes(reorder(x = Food_type,Wateruse),text=paste("Food type:",Food_type,"<br>","Water usage:",Wateruse))) +
         geom_bar(mapping = aes(y = Wateruse), stat = "identity", fill = "#b2d7e9", color = "white")+
         labs(  title = "Water Consumption \nPer Million Kilocalories Consumed",
                x = NULL,
                y = "Total Water Consumption (Mill. gallons)")  +
         theme_classic() +
         coord_flip()
      ggplotly(p6,tooltip = c("text"))
   })
   
   output$cal_CO2 <- renderPlotly({
      validate(
         need(nrow(calorie_data()) > 0, 'No data exists, please select the food type')
      )
      p7 <- calorie_data() %>% 
         ggplot(aes(x=reorder(x = Food_type,CO2e),y = CO2e,
                    text = paste0('Food type: ', Food_type,
                                 '<br>CO2: ', CO2e)))+
         geom_bar( stat = "identity", fill = "orchid2", color = "white")+
         labs(  title = "CO2 Emission \nPer Million Kilocalories Consumed",
                x = NULL,
                y = "Total Carbon Dioxide Emission (CO2e)") +
         theme_classic() +
         coord_flip()
     ggplotly(p7,tooltip = "text")
   })
  
   
   }


