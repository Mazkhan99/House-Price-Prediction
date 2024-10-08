#--------------------------------House Price Prediction--------------------------------
#----------------------------------------Project----------------------------------------

install.packages("shiny")
library(shiny)

ui <- fluidPage(
  
  
  headerPanel("HOUSE PRICE PREDICTION"),
  
  sidebarPanel(
    # Dropdown menu for selecting the area type
    selectInput("area" , "choose the area" , list("built - up area    1",
                                                  "super built - up area    4",
                                                  "Plot  Area     3",
                                                  "Carpet  Area     2")),
    # Text input--
    textInput("area_id","enter the area id which you can see in above part :- ",""),
    textInput("location_id","enter the location pin-code :- ",""),
    textInput("bhk","how many bhk flat you want :-",""),
    textInput("sqft","enter the total sqft :- ",""),
    textInput("bath","how many bath you want :- ",""),
    textInput("balcony","how many balcony you want :- ",""),
    actionButton('go',"Predict")
  ),
  # Main panel to display output
  mainPanel(
    sidebarPanel( width = 25,
                  
                  headerPanel("THE PRICE OF AN HOUSE IS:- "),  # Header for displaying prediction result
                  tags$style(HTML("#value { font-size: 24px; }")), #css style for the output
                  
                  textOutput("value")# Output for displaying the predicted house price
    )
  )
  
)



# Server logic for processing inputs and generating predictions
server <- function(input, output) {
  
  
  # Create a reactive object to store values
  data2 = reactiveValues()
  # Observe the 'go' button click event
  observeEvent(input$go,{
    
    data <- read.csv("C:\\Users\\lenovo\\Downloads\\new house data 2.csv")
    
    View(data)
    summary(data)
    str(data)
    
    is.factor(data$area_type)
    is.factor(data$location)
    
    data$area_type = as.factor(data$area_type)
    data$location = as.factor(data$location)
    # Display the structure of the factors
    str(data$area_type)
    str(data$location)
    str(data)
    # Select relevant columns for analysis
    Use_Data = data[,c("area_type","location","size","total_sqft","bath","balcony","price")]
    head(Use_Data)
    summary(Use_Data)
    
    na_clean_data = na.omit(Use_Data)
    summary(na_clean_data)
    str(na_clean_data)
    View(na_clean_data)
    # Convert area_type and location to numeric values for modeling
    area.type = sapply(na_clean_data$area_type, as.numeric)
    View(area.type)
    
    data.location = sapply(na_clean_data$location, as.numeric)
    View(data.location)
    
    second_final = cbind(na_clean_data, area_id = area.type)
    View(second_final)
    
    second_main_dataset = cbind(second_final, location_id = data.location)
    View(second_main_dataset)
    
    Main_data_set = second_main_dataset[,c("area_type","area_id","location","location_id","size","total_sqft","bath","balcony","price")]
    View(Main_data_set)
    # Create a dataset with the relevant features for prediction
    inputdata = Main_data_set[,c("area_id","location_id","size","total_sqft","bath","balcony","price")]
    
    View(inputdata)
    
    # Capture user inputs for prediction
    data2$myarea_id <- as.numeric(input$area_id)
    data2$myloaction_id <- as.numeric(input$location_id)
    data2$mybhk <- as.numeric(input$bhk)
    data2$mysqft <- as.numeric(input$sqft)
    data2$mybath <- as.numeric(input$bath)
    data2$mybalcony <- as.numeric(input$balcony)
    # Create a new data frame for prediction
    newPredict = data.frame(area_id = data2$myarea_id, location_id = data2$myloaction_id,
                            size = data2$mybhk, total_sqft = data2$mysqft,
                            bath = data2$mybath, balcony = data2$mybalcony)
    # Fit a linear model to the data
    model = lm(price ~ area_id+location_id+size+total_sqft+bath+balcony,
               data = inputdata, weights = 1/inputdata$price^1.9)
    # Generate predictions using the model
    data2$op = predict(model, newPredict)
  })
  # Render the predicted price as output
  output$value <- renderPrint({data2$op})
}

shinyApp(ui, server)
