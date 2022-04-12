function(input, output, session){
  
  observeEvent(
    eventExpr = input$arr_hour_yn,
    handlerExpr = {
      if(input$arr_hour_yn == "N"){
        shinyjs::disable("arr_hour")
      }
      else{
        shinyjs::enable("arr_hour")
        }
    }
  )
  
  observeEvent(
    eventExpr = input$airline_yn,
    handlerExpr = {
      if(input$airline_yn == "N"){
        shinyjs::disable("airline")
      }
      else{
        shinyjs::enable("airline")
      }
    }
  )
  
  re_final_data <- eventReactive(
    input$submit,{
      temp_data <- data.frame(departure_datetime = as.Date(c(input$arr_date[1], input$arr_date[2])),
                              departure_city = as.factor(c(input$dep_city, input$dep_city)),
                              arrival_city = c(input$arr_city, input$arr_city),
                              route = as.factor(paste(input$dep_city, "-", input$arr_city)),
                              airline = c(input$airline,input$airline),
                              stops = c(0,0),
                              flight_duration = c(median(flfare_clean3$flight_duration), NA)
      ) %>% 
        pad(interval = "day") %>% 
        {
          if(input$airline_yn == "N")
            {
            select(., -airline) %>% 
            merge(., levels(flfare_clean3$airline)) %>% 
            rename(airline = y)
          }
          else{fill(., airline)}
        } %>% 
        fill(departure_city) %>% 
        fill(arrival_city) %>% 
        fill(route) %>% 
        fill(stops) %>% 
        fill(flight_duration) %>% 
        filter(trim(airline) != "") %>% 
        mutate(arrival_datetime = departure_datetime) %>% 
        mutate(dep_day = day(departure_datetime),
               dep_dow = wday(departure_datetime),
               dep_mon = month(departure_datetime),
               dep_hr = if(input$arr_hour_yn == "Y"){input$arr_hour} else {median(hour(departure_datetime))},
               dep_woy = week(departure_datetime),
               dep_qtr = quarter(departure_datetime),
               dep_yday = yday(departure_datetime),
               dep_qday = qday(departure_datetime),
               arr_day = day(arrival_datetime),
               arr_dow = wday(arrival_datetime),
               arr_mon = month(arrival_datetime),
               arr_hr = if(input$arr_hour_yn == "Y"){input$arr_hour} else {median(hour(departure_datetime))},
               arr_woy = week(arrival_datetime),
               arr_qtr = quarter(arrival_datetime),
               arr_yday = yday(arrival_datetime),
               arr_qday = qday(arrival_datetime))
      
      temp_predict <- predict(object = model_rf_datecomp_noroute,
                              newdata = temp_data %>% select(-c("route", "dep_qday", "arr_qday", "departure_datetime", "arrival_datetime")))
      
      temp_final_data <- temp_data %>%
        mutate(price = round(temp_predict,3)) %>% 
        select(departure_datetime, route, airline, price)
    })
  
  output$calculate_date_2 <- renderDataTable({
    validate(
      need(input$dep_city != input$arr_city, "")
    )
    
    temp_final_data <- re_final_data()
    DT::datatable(temp_final_data %>%
                    arrange(price) %>% 
                    head(10),
                  options = list(dom = 't'),
                  #caption = "Best 10 Prices",
                  style = "bootstrap"
    )
  })
  
  output$recommendation_title <- renderUI({
    re_final_data()
    HTML("<h5>Top 3 Recommendation</h5>
        ")  
  })
  
  output$mini_info <- renderUI({
    HTML("Quick Information :<br/>
         <ol><li>The prediction was made using Random Forest model, with 2 folds x 1 repetition.
         <li>Cross validation testing was made to train and evaluate the model with 80:20 proportion, respectively.
         <li>Mean Absolute Percentage Error (MAPE) using the evaluation/unseen data resulted in 12.93%, the best one comparatively.
         <li>\"Possible Flight Dates:\" input is defaulted to today's date, for simplicity and relevancy. 
         <li>\"Preferred Arrival Hour?\" input is defaulted to <b><u>No</u></b>, causing the \"Arrival Hour\" input to be disabled and we will pick the <b><u>median</u></b> as default.<br/>
         Of course, when picked as Yes, \"Arrival Hour\" the input is defaulted to 0, and can simply be filled with preferred hour.
         <li>\"Depart From\" and \"Arrive To\" inputs is defaulted to <b><u>BRE</u></b>, the first airport code in the data alphabetically.<br/>
         There will be a validation check to make sure that both of these inputs are different.
         <li>\"Preferred Airline?\" input is defaulted to <b><u>No</u></b>, causing the \"Airline Carrier\" input to be disabled and we will use every possible airlines in the database and show the best ones, as default.<br/>
         Of course, when picked as Yes, \"Airline Carrier\" can simply be filled with preferred airline carrier.
         <li>The outputs will be:<br/>
              <ol><li><b><u>Top 3 Recommendation:</u></b> showing the best (cheapest) 3 individual prices in Euro currency, with the departure dates and airline carriers.
                  <li><b><u>Best Airlines in Terms of Average Prices:</u></b> showing best 3 airlines, in terms of cheapest mean prices during the same period of time and route, for comparison.<br/>
                      It should be noted that if \"Preferred Airline?\" is chosen to Yes, this will only show the selected airlines. Otherwise, this will show a total of 3 airlines.
                  <li><b><u>Top 10 Prices</u></b>: showing the best (cheapest) up to 10 individual prices in Euro currency, with the departure dates and airline carriers, in format of table.
        ")  
  })
  
  output$recommendation1 <- renderInfoBox({
    
    validate(
      need(input$dep_city != input$arr_city, "Please select a different cities for departure and arrival.")
    )
    
    temp_final_data <- re_final_data()
    recommendation1 <-
      temp_final_data %>% 
      arrange(price) %>% 
      head(1)
    
    infoBox(
      title = HTML(paste("First Recommendation - Price of <b><i><u>EUR", as.character(round(recommendation1$price, 2)),"</b></i></u>")),
      subtitle = HTML(paste("Depart at <b><i><u>",format(recommendation1$departure_datetime, format="%B %d, %Y"),"</b></i></u><br/>Airline: <b><i><u>",recommendation1$airline,"</b></i></u>")),
      fill = TRUE,
      color = "green",
      icon = icon("plane"),
      width = 12
    )
  })
  
  output$recommendation2 <- renderInfoBox({
    
    validate(
      need(input$dep_city != input$arr_city, "")
    )
    
    temp_final_data <- re_final_data()
    recommendation2 <-
      temp_final_data %>% 
      arrange(price) %>% 
      head(2) %>% 
      tail(1)
    
    infoBox(
      title = HTML(paste("Second Recommendation - Price of <b><i><u>EUR", as.character(round(recommendation2$price, 2)),"</b></i></u>")),
      subtitle = HTML(paste("Depart at <b><i><u>",format(recommendation2$departure_datetime, format="%B %d, %Y"),"</b></i></u><br/>Airline: <b><i><u>",recommendation2$airline,"</b></i></u>")),
      fill = TRUE,
      color = "yellow",
      icon = icon("plane"),
      width = 12
    )
  })
  
  output$recommendation3 <- renderInfoBox({
    
    validate(
      need(input$dep_city != input$arr_city, "")
    )
    
    temp_final_data <- re_final_data()
    recommendation3 <-
      temp_final_data %>% 
      arrange(price) %>% 
      head(3) %>% 
      tail(1)
    
    infoBox(
      title = HTML(paste("Third Recommendation - Price of <b><i><u>EUR", as.character(round(recommendation3$price, 2)),"</b></i></u>")),
      subtitle = HTML(paste("Depart at <b><i><u>",format(recommendation3$departure_datetime, format="%B %d, %Y"),"</b></i></u><br/>Airline: <b><i><u>",recommendation3$airline,"</b></i></u>")),
      fill = TRUE,
      color = "red",
      icon = icon("plane"),
      width = 12
    )
  })
  
  output$table_title <- renderUI({
    re_final_data()
    HTML("<h5>Top 10 Prices</h5>
        ")  
  })
  
  output$calculate_date <- renderDataTable({
    temp_data <- data.frame(departure_datetime = as.Date(c(input$arr_date[1], input$arr_date[2])),
                            departure_city = as.factor(c(input$dep_city, input$dep_city)),
                            arrival_city = c(input$arr_city, input$arr_city),
                            route = as.factor(paste(input$dep_city, "-", input$arr_city)),
                            airline = c(input$airline, input$airline),
                            stops = c(0,0),
                            flight_duration = c(median(flfare_clean3$flight_duration), NA)
    ) %>% 
      pad(interval = "day") %>% 
      fill(departure_city) %>% 
      fill(arrival_city) %>% 
      fill(route) %>% 
      fill(airline) %>% 
      fill(stops) %>% 
      fill(flight_duration) %>% 
      mutate(arrival_datetime = departure_datetime) %>% 
      mutate(dep_day = day(departure_datetime),
             dep_dow = wday(departure_datetime),
             dep_mon = month(departure_datetime),
             dep_hr = hour(departure_datetime),
             dep_woy = week(departure_datetime),
             dep_qtr = quarter(departure_datetime),
             arr_day = day(arrival_datetime),
             arr_dow = wday(arrival_datetime),
             arr_mon = month(arrival_datetime),
             arr_hr = hour(arrival_datetime),
             arr_woy = week(arrival_datetime),
             arr_qtr = quarter(arrival_datetime))
     
      
    
    temp_predict <- predict(object = model_rf_datecomp,
                            newdata = temp_data %>% select(-c("departure_datetime", "arrival_datetime")))
    
    temp_data %>%
      mutate(price = temp_predict) %>% 
      select(departure_datetime, route, airline, price)
      
  })
  
  output$plot_title <- renderUI({
    re_final_data()
    HTML("<h5>Best Airlines in Terms of Average Prices</h5>
        ")  
  })
  
  output$prediction_plot <- renderPlotly({
    validate(
      need(input$dep_city != input$arr_city, "")
    )
    
    temp_final_data <- re_final_data()
    best_3_airline <-
      temp_final_data %>% 
      group_by(airline) %>% 
      summarize(price = mean(price)) %>% 
      arrange(price) %>% 
      head(3)
    
    plot <- temp_final_data %>% 
      filter(airline %in% best_3_airline$airline) %>%
      # mutate(label = glue("Price = EUR {round(price,2)}
      #                     Departure Date = {format(departure_datetime, format = \"%B %d, %Y\")}
      #                     Airline = {airline}")) %>% 
      ggplot(mapping = aes(x = departure_datetime, y = price)) + #, text = label
      geom_line(aes(color = airline))+
      geom_point(aes(color = airline))+
      theme_dark()+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA),
            title = element_text(colour = "#FFFFFF"),
            text = element_text(colour = "#FFFFFF"),
            axis.text = element_text(colour = "#BBBBBB"))+
      labs(#title = "Best 3 Airlines' Price in Comparison",
           x = "Departure Date",
           y = "Price (EUR)",
           color = "Airline Carrier")

    #ggplotly(plot, tooltip = "label")
    ggplotly(plot)
  })
  
  output$welcome <- renderImage({
    # Return a list containing the filename
    list(src = "./ticket.png", height = 600)
  }, deleteFile = FALSE)
  
  output$aboutme <- renderUI({
    HTML("<h4>Created by <b><u>Calvin</b></u><br/>
     Student of Algoritma Data Science School - Phoenix Night<br/>
     Part of Data Career Day Project<br/>
     <br/>
     As the first was sourced from a publicly available source of Royal Netherlands Academy of Arts and Sciences, I believe that the dataset is accountable as it was used for academical purposes and free to use under a Creative Commons Attribution 4.0 International license, that means that: \"You can share, copy and modify this dataset so long as you give appropriate credit, provide a link to the CC BY license, and indicate if changes were made, but you may not do so in a way that suggests the rights holder has endorsed you or your use of the dataset. Note that further permission may be required for any content within the dataset that is identified as belonging to a third party.\"<br/>
     <br/>
     Link to the preliminary analysis of the dataset, used as a proposal for this final project:  <a href=\"https://rpubs.com/ccalvintj/dcd_proposal_airline\">Airline Fare Prediction - DCD Proposal</a><br/>
     </h4>
     <h5>
     Contact Me:<br/>
     <a href='https://linkedin.com/in/ccalvintj'> LinkedIn </a><br/>
     <a href='https://github.com/ccalvintj'> GitHub </a>
     </h5>
     ")
  })
  
  output$background_1_1 <- renderUI({
    HTML("<h4>Background and Problem Research</h4>
         <br/>
         As a fan of The Amazing Race since a very young age, an American reality TV show about competition to become the fastest team to travel around the world, traveling to the most interesting places in the world has become a part of my lifelong goal, as I’m sure most other common people would also enjoy doing. For me, it’s about the interesting culture and culinary that makes me fall in love with doing so, despite some limitations of doing it more often, like the cost of transportations and accommodations as the biggest factor, as they are definitely not cheap, especially those flight trips, as also supported by <a href = \"test\">Sterling Price of ValuePenguin.com</a> that researched about average cost of a vacation, that we can see that the budget is mostly going to the transportation. Therefore, a good deal for flight fare has been a deciding factor for me and most people who also love to travel in the final decision of going through with spending our hard-earned money to travel to interesting places in the world.<br/>"
           )})
         
  output$bg1_image1 <- renderImage({list(src = "valuepenguin.jpg", height = 240)},
                                   deleteFile = FALSE)
  
  output$background_1_2 <- renderUI({
    HTML("<br/>As international travel came to quite a standstill since 2020 due to the COVID-19 pandemic, the travel and tourism industries, especially aviation industry, suffered what it describes as the “the worst year in history for air travel demand”. According to the International Air Transport Association (IATA), global passenger traffic as measured in revenue passenger kilometers declined by 65.9 percent compared to 2019, as international passenger demand dropped 75.6 percent and domestic demand fell 48.8 percent below 2019 levels.<br/>"
         )})
  
  output$bg1_image2 <- renderImage({list(src = "statista.jpeg", height = 350)},
                                   deleteFile = FALSE)
  
  output$background_1_3 <- renderUI({
    HTML("<br/>From <a href = \"https://www.statista.com/chart/24107/global-air-passenger-traffic/\">an article at statista.com, </a> we can see IATA’s prediction  once again claimed that air travel demand to reach just 50 percent of 2019 levels in 2021, even in its more optimistic scenario. If the new strains of the virus continue to take hold, things could be even worse and recovery could be limited to just 13 percent above 2020 levels, leaving the industry at 38 percent of 2019 passenger demand, as seen from above chart. Combined with rapidly changing local government’s regulations among other things, traveling as a hobby or even a business trip, has become the hardest activity to accomplish during these challenging times.<br/>
         <br/>
         Keeping positive thoughts that COVID-19 and its variants could soon be tackled, with the emerging vaccines and strict government regulations in place, I can only hope to be able to travel and see the world again one day. Since most people, like me, are not super rich, a budget planning for a duly required vacation would be important for us to be able to maximize our enjoyment of the vacation while balancing the feeling of impending doom of credit card bills and reduced savings after such a trip.<br/>
         <br/>
         Since we know that the transportation costs the most, if we are planning to visit places that are outside the land that we are currently in, it would be unavoidable to use an aviation service. As a frequent traveler and quite “stingy” person, won’t it be nice to be able to know when the cheapest flight to that destination vacation that we have in mind would become available, and when would the latest date be that can we hold off on buying such tickets just so we can hold off our expenses until it is really required? Sure, we can always look for our preferred travel-related services and applications like Travel_ka and Tik_t.com, that would be such a hassle. Since I have studied with Algoritma in the last few months about data science, can’t we somehow predict it with the help of machine learning?
        ")  
  })
  
  output$background_1_4 <- renderUI({
    HTML("<h4>Business Impact</h4>
         <br/>
         By having a flight dashboard to mainly help predicting the best range of date of reserving the cheapest ticket possible, it could help potential travelers, be it for leisure or business, to be able to optimize their traveling expenses, so that these people would be able to hold off expending budget until the time that it would be totally necessary and advantageous. Any third-party traveling services could also benefited from this as having the knowledge that their similar competitions could be expected to have the price pattern, and take advantage on being different, for example to plan some special price promotions during the time that most other services would be at their highest, unlikeable price, so that the last-minute potential customers could prefer their business instead.<br/>
         <br/>
         Flight-related businesses, like accommodation, culinary, and other tourism services would be indirectly impacted by the ease and effective way of travelers spending less money on flights, that they would be more inclined to spend money on other things during the vacation. As a country that had such an established tourism industry and experienced such blowback during the pandemic, Indonesia and most other interesting countries would be benefited in the general if the flight demand increased by the impact of ease of access to the most optimal date to buy airline tickets.<br/>
        ")  
  })
  
  output$background_1_5 <- renderUI({
    HTML("<h4>Target User</h4>
         <br/>
         Target user is more about the potential travelers that would be very likely to use flight services and optimize their expenditure, and also any third-party travel reservation services to be able to optimize their price algorithm to be more advantageous against their competitors.<br/>
         <br/>
         Other than those two that would be quite directly related, other travel and tourism industry businesses, such as hotels, culinary, etc. would also be indirectly targeted as it should be quite established already that they would be indirectly benefited from this.<br/>
         <hr/>
         <h4>Similar Interpretation</h4>
         As it would be likely for me to use Random Forest or some other machine learning methods for optimization and prediction purposes, this project could also be implemented to similar businesses that have some seasonality and fluctuating fares factored into the accuracy of the modeling, for examples (but not limited to): train fare prediction, rental house and accommodation prices prediction, etc. In other words, the fare vs time optimization case that would be presented in the project could be highly implemented across industries.<br/>
        ")  
  })
  
  output$background_2_1 <- renderUI({
    HTML("<h4> Original Dataset</h4>
         <br/>
         The main dataset that I am about to use was sourced from Koninklijke Nederlandse Akademie van Wetenschappen (KNAW) or simply Royal Netherlands Academy of Arts and Sciences.<br/>
         <br/>
         Link to the dataset: <a href=\"https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:158644\">F, Frederick (2020), “German Flight Ticket Prices”, Mendeley Data, V1, doi: 10.17632/gz75x2pzr7.1</a><br/>
         The description mentioned that the dataset was generated by web scraping and includes the ticket prices on 84 German connections <b>over a period of 6 months</b>. a total of 63,000 prices and connections are included in the dataset.<br/>
        <br/>")
  })

  output$background2_table <- renderDataTable({
    DT::datatable(flfare %>%
                    head(5),
                  options = list(dom = 't'),
                  #caption = "Best 10 Prices",
                  style = "bootstrap"
    )
  })
  
  output$background_2_2 <- renderUI({
    HTML("The dataset consists of 62627 rows x 10 columns, which are:<br/>
         <ul><li>`ï..departure_city`: character, 3-letter airport code followed by the airport name of the departure city.
         <li>`arrival_city`: character, 3-letter airport code followed by the airport name of the arrival city.
         <li>`scrape_date`: character / date, when the particular observation / row was scraped.
         <li>`departure_date`: character / date, when the particular flight departed.
         <li>`departure_date_distance`: character, how far are the `scrape_date` and `departure_date` are distanced.
         <li>`departure_time`: character / time, when the particular flight departed.
         <li>`arrival_time`: character / time, when the particular flight arrived.
         <li>`airline`: character, name of the airline carrier.
         <li>`stops`: character, showing how many stops did the flight before arriving.
         <li>`price..â...`: character / numeric, price of the particular flight in Euro.</ul>
         <hr/>
         ")
  })
  
  output$background_2_3 <- renderUI({
    HTML("<h4>Processed Dataset after Research Period</h4>
         <br/>
         After some researches and trying some machine learning models to achieve the target, some predictors are engineered to mainly help the time seasonality of the data and improve the accuracy of the model.<br/>
         ")
  })
  
  output$background2_table2 <- renderDataTable({
    DT::datatable(flfare_separatedatecomps %>%
                    select(-c("arr_day","arr_dow","arr_mon","arr_hr","arr_woy","arr_qtr")) %>% 
                    rename(dep_city = departure_city,
                           arr_city = arrival_city) %>% 
                    head(5),
                  options = list(dom = 't'),
                  #caption = "Best 10 Prices",
                  style = "bootstrap"
    )
  })
  
  output$background_2_4 <- renderUI({
    HTML("The dataset consists of 62594 rows x 13 columns, which are:<br/>
         <ul><li>`dep_city`: character/factor, 3-letter airport code followed by the airport name of the departure city.
         <li>`arr_city`: character/factor, 3-letter airport code followed by the airport name of the arrival city.
         <li>`airline`: character/factor, name of the airline carrier.
         <li>`stops`: numeric, showing how many stops did the flight before arriving.
         <li>`route` : character/factor, combined from departure_city and arrival_city.
         <li>`flight_duration`: numeric, duration of the specific flight.
         <li>`price`: numeric, price of the particular flight in Euro, <b>target variable of this prediction</b>.
         <li>`dep_day`: numeric, extracted day of month(1-31) of the departure_date.
         <li>`dep_dow`: numeric, extracted day of week(1-7) of the departure_date.
         <li>`dep_mon`: numeric, extracted month(1-12) of the departure_date.
         <li>`dep_hr`: numeric, extracted hour(0-23) of the departure_date.
         <li>`dep_woy`: numeric, extracted week of year of the departure_date.
         <li>`dep_qtr`: numeric, extracted quarter of year of the departure_date.
         </ul>
        <hr/>
         ")
  })
  
  output$background_4_1 <- renderUI({
    HTML("I was thinking of using Time-Series Analysis due to the possibility of the seasonality of the data would make a sense to the price pattern, but then again when checking the spread the of `departure_date`, seems that a lot of dates are not covered and have a lot of empty ones. Since a Time-Series would require a complete sequence of data, forcing to impute a lot of missing dates would not be a great course of action because our analysis would only based on the assumption of the imputation, not based on fact.<br/>
         <br/>
         Therefore, the best course I think of, would be to use Regression using Simple Linear Regression, Random Forest, and Neural Network. Or other algorithms that would be great that I found along the research, I might try them here.<br/>
         <br/>
         To evaluate them, I will be mostly using MAPE or Mean Absolute Percentage Error, since this metric would be easier to be explained or understood.
         <hr>
         <h5>Mean Absolute Percentage Error (MAPE)</h5>
         According to <a href=\"https://www.dataquest.io/blog/understanding-regression-error-metrics/\">dataquest.io</a>, MAPE is how far the model’s predictions are off from their corresponding outputs on average. Like Mean Absolute Error (MAE), MAPE also has a clear interpretation since percentages are easier for people to conceptualize. Both MAPE and MAE are robust to the effects of outliers thanks to the use of absolute value.<br/>
        ")  
  })
  
  output$background_4_img1 <- renderImage({
    # Return a list containing the filename
    list(src = "./mape1.jpg", height = 240)
  }, deleteFile = FALSE)
  
  output$background_4_2 <- renderUI({
    HTML("However for all of its advantages, we are more limited in using MAPE than we are MAE. Many of MAPE’s weaknesses actually stem from use division operation. Now that we have to scale everything by the actual value, MAPE is undefined for data points where the value is 0. Similarly, the MAPE can grow unexpectedly large if the actual values are exceptionally small themselves. Finally, the MAPE is biased towards predictions that are systematically less than the actual values themselves. That is to say, MAPE will be lower when the prediction is lower than the actual compared to a prediction that is higher by the same amount. The quick calculation below demonstrates this point.<br/>
        ")  
  })
  
  output$background_4_img2 <- renderImage({
    # Return a list containing the filename
    list(src = "./mape2.jpg", height = 240)
  }, deleteFile = FALSE)
  
  output$background_4_3 <- renderUI({
    HTML("We have a measure similar to MAPE in the form of the mean percentage error. While the absolute value in MAPE eliminates any negative values, the mean percentage error incorporates both positive and negative errors into its calculation.<br/>
        ")  
  })
  
  output$background_4_img3 <- renderImage({
    # Return a list containing the filename
    list(src = "./mape3.jpg", height = 240)
  }, deleteFile = FALSE)
  
  output$background_4_4 <- renderUI({
    HTML("<hr>
         <h5>Time Series & Seasonality Analysis</h5>
         <br/>
         ")  
  })
  
  output$bg_plot01 <- renderPlotly({
    flfare_viz1 <- flfare_clean3 %>% 
      mutate(departure_datetime = ymd(as.Date(departure_datetime))) %>% 
      group_by(departure_datetime) %>% 
      summarise(freq = n()) %>% 
      mutate(label = glue("Departure Date: {departure_datetime}
                          Frequency: {freq}")) %>% 
      ggplot(mapping = aes(x = departure_datetime, y = freq, text = label))+
      geom_point(mapping = aes(x = departure_datetime, y = freq, color = freq))+
      scale_color_gradient(low = "white", high = "#375a7f")+
      theme_dark()+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA),
            title = element_text(colour = "#FFFFFF"),
            text = element_text(colour = "#FFFFFF"),
            axis.text = element_text(colour = "#BBBBBB"))+
      labs(#title = "Best 3 Airlines' Price in Comparison",
        x = "Departure Datetime",
        y = "Frequency",
        color = "Frequency")
    
    ggplotly(flfare_viz1, tooltip = "label")
  })
  
  output$background_4_5 <- renderUI({
    HTML("As we see above, some dates appear to be missing. Therefore, we might not be able to consider Time-Series Analysis for our case here, unless we found a more complete data. Or maybe we need to aggregate it weekly, but we would need to explore the data a bit more.<br/>
         <hr>
         <h5>Multiple Regression</h5><br/>
         ")  
  })

  output$background_4_img4 <- renderImage({
    # Return a list containing the filename
    list(src = "./multiple_linreg.jpg", height = 240)
  }, deleteFile = FALSE)
           
  output$background_4_6 <- renderUI({
    HTML("    
         <ul><li>I've tried several iteration of linear regression using lm() and stepwise functions during the research phase of the project.
         <li>The best ones are the models using all of the predictors, and similarly in terms of R-squared (0.828) and Root Mean Squared Error (72.673) numbers, stepwise with only few predictors removed.
         <li>When evaluated against unseen 20% randomly chosen data, the Mean Absolute Percentage Error (MAPE) shows <b><u>35.82%</u></b></ul>
         <hr>
         <h5>Random Forest</h5><br/>
     ")  
  })    
  
  output$background_4_img5 <- renderImage({
    # Return a list containing the filename
    list(src = "./random_forest.jpg", height = 240)
  }, deleteFile = FALSE)
  
  output$background_4_7 <- renderUI({
    HTML("   
         <ul><li>I've tried several iteration of k-folds of 2 folds x 1 repetition and even to 5 folds x 3 repetitions.<br/>
         The 2x1 fold, averages in around 7 hours of training model time, while 5x3 averages in 40+ hours.<br/>
         Due to a very slight difference in MAPE value between the two (first tries are about 1 percent difference)
         <li>Some treatments of the data also was tried, including:
              <ul><li>Feature Engineering a new column \"Route\" made up of concatenated Departure and Arrival Cities, and removing the aforementioned Departure and Arrival Cities, in hope that some routes might show patterns and avoiding auto-correlations.
                  <li>Using \"Route\" and leaving Departure and Arrival Cities.
                  <li>Not suing \"Route\" and separating the date components (into day, month, year, day of week, day of year, and hour) in hope to catch the time-series seasonality.</ul>
         <li>The best ones are the models using all of the predictors, and similarly in terms of R-squared (0.828) and Root Mean Squared Error (72.673) numbers, stepwise with only few predictors removed.
         <li>When evaluated against unseen 20% randomly chosen data, the initial Mean Absolute Percentage Error (MAPE) shows <b><u>12.93%</u></b>
         <li>After several trial and errors, the best model is found when using 2x1 folds, not using \"Route\", and separating date components into different columns. The MAPE value shown is <b><u>12.93%</u></b>
         </ul>
         <h5>Neural Network and Deep Learning</h5><br/>
         ")  
})
         
  output$background_4_img6 <- renderImage({
    # Return a list containing the filename
    list(src = "./neural_network.jpg", height = 240)
  }, deleteFile = FALSE)
  
  output$background_4_8 <- renderUI({
    HTML("
         <ul><li>I've tried several iteration of different number of hidden layers, number of nodes, learning rates, optimizers, etc.
         <li>Different algorithms of Neural Network was also tried, including Convolutional Neural Network (CNN), using Dropout layer(s), and even XGBoost.
         <li>Some treatments of the data also was tried, including scaling each predictors using z-score (mean and standard deviation) standardization, and also strictly into range of 0-1
         <li>There is not really a good model after all of these iterations tried, with evaluation MAPE scores ranges around <b><u>50-90%</u></b> all the time.<br/>
         Since the Random Forest shows more promise, this algorithm was not pursued in the end.
         </ul>
        ")  
  })
  
  output$background_5 <- renderUI({
    HTML("Interactive webpage dashboard, which has the following features:<br/>
         <ul><li>Interactive webpage where we can input some parameters into, like: date picker to pick the range of desired departure date (required), drop-down choices input of arrival airport or city (required), drop-down choices input of departure airport or city (required), drop-down choices of possible airline carriers (optional), drop-down choices of possible airline classes (optional)<br/>
         When the required parameters are filled, the expected output would be at least a sentence of the best range of date possible for 1 top flight carrier with the best (cheapest) price possible. Optionally, it would be nice to have few other secondary options of airline carriers and/or date range as the recommendation.<br/>
         <li>Interactive informative dashboard concerning delay statistics of all flights arriving to the destination city, top overall flight carriers according to fare and on-time rate.<br/>
         Inputs will be optional, in which case by default it will show the overall result. When inputs are filled in, the information shown will be processed to be more specified.<br/>
         Inputs will include: date picker to pick the range of desired departure date, drop-down choices input of arrival airport or city, drop-down choices input of departure airport or city, drop-down choices of possible airline carriers, drop-down choices of possible airline classes.<br/>
         Desired outputs would be some sentences of recommendations like the best date range to travel in the destination places, and some plots, like line chart of forecasted lowest price according to a date range, bar plots for best airline carriers according to fare or on-time rate, scatter plots of different flight carriers according to their characteristics, etc.<br/>
         <br/>
        ")  
  })
  
  output$plotly01 <- renderPlotly({
    flfare_viz1 <- 
      ggplot(data = flfare_clean3 %>% filter(price >= input$plot1_price[1],
                                             price <= input$plot1_price[2],
                                         route %in% c(input$plot1_route, input$plot1_route2, input$plot1_route3)), 
             mapping = aes(x = route, y = price))+
      geom_boxplot(fill = "#375a7f", color = "#FFFFFF")+
      theme_dark()+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA),
            title = element_text(colour = "#FFFFFF"),
            text = element_text(colour = "#FFFFFF"),
            axis.text = element_text(colour = "#BBBBBB"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(title = "Route vs Price",
           x = "Available Routes",
           y = "Price in EUR")
    
    ggplotly(flfare_viz1)
  })
  
  output$plotly01_desc <- renderUI({
    HTML("As shown at the summary statistics and the boxplot above, the price range is quite far, from 26 EUR to the highest of 3088 EUR! Surely some of prices are outliers, or have some special cases that we haven't quite see, like distance, or ticket class that is unfortunately is not captured within our dataset.
         ")  
  })
  
  output$plot3_desc1 <- renderUI({
    HTML("There are just too many routes! I think we need to focus on only some of routes to see any patterns here, but 1 insight that I can take from here is especially from <b><u>`FRA - MUC`</u></b> route, having so many obvious and quite standing out outliers, compared to other routes.<br/>
         Seeing that our data might be affected by outliers, I will try to summarize some insights regarding price using median.
         ")  
  })
  
  output$plotly02 <- renderPlotly({
    flfare_viz1 <- 
    flfare_clean4 %>%
      filter(price >= input$plot2_price[1],
             price <= input$plot2_price[2],
             airline %in% c(input$plot2_airline, input$plot2_airline2)) %>% 
      group_by(airline) %>% 
      #summarise(med_price = median(price)) %>%
      summarise(med_price = case_when(input$plot2_metric == "Median" ~ median(price),
                                      input$plot2_metric == "Maximum" ~ max(price),
                                      input$plot2_metric == "Minimum" ~ min(price),
                                      TRUE ~ mean(price),)) %>% 
      mutate(label = glue("Airline: {airline}
                           Price: {med_price} EUR")) %>% 
      ggplot(mapping = aes(x = med_price, y = reorder(airline, med_price), text = label, fill = med_price))+
      geom_col()+#fill = "#375a7f", alpha = 0.7)+
      scale_fill_gradient(low = "#678aaf", high = "#072a4f")+
      theme_dark()+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA),
            title = element_text(colour = "#FFFFFF"),
            text = element_text(colour = "#FFFFFF"),
            axis.text = element_text(colour = "#BBBBBB"))+
      labs(title = "Airline vs Price",
           y = "Airlines",
           x = "Airplane Fare in EUR",
           fill = "Price (EUR)")
    
    ggplotly(flfare_viz1, tooltip = "label")
  })
  
  output$plotly02_desc<- renderUI({
    HTML("Airline `Mehrere Fluglinien` is clearly having the highest fare here, as it is the only one touching more than 800 EUR as even the median price. Airline branding could really play a part here in predicting the price.<br/>
         <br/>
         Meanwhile, `easyJet` and `Eurowings` are 2 airlines that are almost consistently are the cheapest out of the bunch. We can expect our predictions to be dominated by these airlines.
         ")  
  })
  
  output$plotly03 <- renderPlotly({
    flfare_viz1 <- 
      flfare_separatedatecomps %>%
      filter(price >= input$plot3_price[1],
             price <= input$plot3_price[2]) %>% 
      mutate(date_sum = case_when(input$plot3_season == "dow" ~ as.numeric(dep_dow),
                                  input$plot3_season == "dom" ~ as.numeric(dep_day),
                                  input$plot3_season == "hour" ~ as.numeric(dep_hr))) %>%
      group_by(date_sum) %>% 
      summarise(med_price = case_when(input$plot3_metric == "Median" ~ median(price),
                                      input$plot3_metric == "Maximum" ~ max(price),
                                      input$plot3_metric == "Minimum" ~ min(price),
                                      TRUE ~ mean(price),)) %>% 
      mutate(label = glue("Price: {round(med_price,2)} EUR")) %>% 
      ggplot(mapping = aes(x = as.factor(date_sum), y = med_price, text = label, fill = med_price))+
      geom_col()+
      scale_fill_gradient(low = "#678aaf", high = "#072a4f")+
      theme_dark()+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA),
            title = element_text(colour = "#FFFFFF"),
            text = element_text(colour = "#FFFFFF"),
            axis.text = element_text(colour = "#BBBBBB"))+
     
      labs(title = "Time Seasonality vs Price",
           y = "Price (EUR)",
           x = "Time Unit",
           fill = "Price (EUR)")
    
    if(input$plot3_season == "dow")
    {
      flfare_viz1 <- flfare_viz1 + scale_x_discrete(labels = c("1" = "Sun",
                                                               "2" = "Mon",
                                                               "3" = "Tue",
                                                               "4" = "Wed",
                                                               "5" = "Thu",
                                                               "6" = "Fri",
                                                               "7" = "Sat"))
    }
    
    
    ggplotly(flfare_viz1, tooltip = "label")
  })
  
  output$plotly03_desc<- renderUI({
    HTML("<ul>
         ")  
  })
  
#   output$infographic_plot06_02 <- renderPlotly({
#     flfare_viz1 <- 
#       flfare_clean3 %>%
#       filter(airline == "Lufthansa") %>% 
#       ggplot(mapping = aes(group = airline, y = price))+
#       geom_boxplot(fill = "#375a7f", color = "gray")+
#       theme_dark()+
#       theme(panel.background = element_rect(fill = "transparent", colour = NA),
#             plot.background = element_rect(fill = "transparent", colour = NA),
#             legend.background = element_rect(fill = "transparent", colour = NA),
#             title = element_text(colour = "#FFFFFF"),
#             text = element_text(colour = "#FFFFFF"),
#             axis.text = element_text(colour = "#BBBBBB"))+
#       labs(title = "Lufthansa Fares",
#            y = "Price in EUR")
#     
#     ggplotly(flfare_viz1)
#   })
#   
#   output$plot6_desc2 <- renderUI({
#     HTML("Most of the fares fall below 600 EUR. It should be noted that we might treat the fares above 600 as outliers, if we want to focus only on Lufthansa when it comes to the modelling.
#          ")  
#   })
  
}