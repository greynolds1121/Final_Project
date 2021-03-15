library(shiny)
library(tidyverse)
library(shinyWidgets)
library(lubridate)     
library(maps)         
library(ggmap)        
library(gplots)     
library(RColorBrewer)  
library(sf)           
library(ggthemes)     
library(readr)
library(rsconnect)
library(plotly)

# READ DATA --------------------------------------------------------------------------------

vaccines <- read_csv('immunizationspercentages.csv')

vaccines_copy <- read_csv('immunizationspercentages.csv')

states <- map_data("state")

mn_map <- subset(states, region %in% c("minnesota"))

mn_map_data <- subset(states, region == "minnesota")

counties <- map_data("county")

mn_county <- subset(counties, region == "minnesota")

vaccines <- vaccines %>% 
  mutate(location = str_replace(location, "St. Louis", "st louis"))

clean_vaccines2 <- vaccines %>% 
  filter(location != "Minnesota") %>%
  mutate(location = str_to_lower(location)) %>% 
  rename(subregion = location)

vaccines_and_map <- clean_vaccines2 %>% 
  left_join(mn_county, 
            by = c("subregion")) %>% 
  rename(`Percent Vaccinated` = percent)

# ----------------------------------------------------------------------------------------------------

ui <- navbarPage("MN Childhood Vaccination Rates",
                 tabPanel("Plotting Vaccine Rates", 
                          sidebarLayout(
                            sidebarPanel(
                              h4("MN Vaccine Dashboard"),
                              p("This interactive dashboard allows you to explore data on childhood vaccination rates across MN counties between 2015-2019. 
                                 Follow the instructions below to create a graph."),
                              
                              selectInput("vaccine_choice", 
                                          "Select Vaccine(s):",
                                          choices = vaccines$vaccine, 
                                          selected = "DTap", 
                                          multiple = TRUE),
                              
                              # County Choice Input
                              selectInput("county_name",
                                          "Select County of Interest:",
                                          choices = vaccines$location),
                              
                              # Time Frame Choice Input
                              sliderInput(inputId = "years",
                                          label = "Select Year Range:",
                                          min = 2015,
                                          max = 2019,
                                          value = c(2015, 2019),
                                          sep = ""),
                              # Submit Button
                              submitButton(text = "Create my plot!"),
                              HTML("<br>"),
                              HTML("<br>"),
                              HTML("<p> Data Source: Minnesota Department of Health. For more information, please visit their <a href='https://data.web.health.state.mn.us/immunization_basic'>website</a>.")
                              
                            ), 
                            
                            mainPanel(h4("Overview"),
                                      p("The MN Department of Health outlines a recommended schedule for childhood vaccinations known as the childhood immunization series. This dashboard interacts with data on the various vaccines listed in this immunization series. These data tell us the percentage of Minnesota children ages 24-35 months up-to-date on recommended childhood vaccines. While the intention of this dashboard was to allow users to explore this data in any capacity they see fit, this data (and this dashboard) can be an important tool in monitoring state and county-level immunization coverage as the state moves to achieve public health goals. Please follow the instructions on the side to enjoy this dashboard."),
                                      plotOutput(outputId = "vaccineplot"), 
                                      h4("Why Are Childhood Vaccines So Important?"),
                                      p("Preventing a disease is always better, and safer, than treating it after it occurs. Thanks to vaccines, diseases that used to be common around the world, and that killed millions of individuals, can now be prevented. Vaccinating children for a range of diseases is an important initiative that ensures the health of both the child and their community."),
                                      p("The percentage of children who have received recommended vaccinations is called the coverage rate. In order to protect the population from vaccine-preventable disease, the Office of Disease Prevention and Health Promotion calls for 90% coverage among young children for individual immunizations and 80% coverage for the complete childhood immunization series.")), 
                            
                          )
                 ),
                 
                 # Panel Break -------------------------------------------------------   
                 
                 tabPanel("Mapping Vaccine Rates",
                          sidebarLayout(
                            sidebarPanel(
                              
                              h4("MN Vaccine Dashboard"),
                              p("This interactive dashboard allows you to explore data on childhood vaccination rates across MN counties between 2015-2019. 
                                 Follow the instructions below to create a choropleth map of vaccine rates across MN."),
                              
                              # Vaccine Choice Input 
                              selectInput("vaccine_choice2", 
                                          "Select Vaccine:",
                                          choices = vaccines$vaccine, 
                                          selected = "DTap", 
                                          multiple = FALSE),
                              
                              # County Choice Input   
                              pickerInput("county_name2",
                                          "Select Counties:", 
                                          choices = c(Aitkin = "aitkin", Anoka = "anoka", Becker = "becker", Beltrami = "beltrami", Beton = "benton", 'Big Stone' = "big stone", "Blue Earth" = "blue earth", Brown = "brown", Carlton = "carlton", Carver = "carver", Cass = "cass", Chippewa = "chippewa", Chisago = "chisago", Clay = "clay", Clearwater = "clearwater", Cook = "cook", Cottonwood = "cottonwood", "Crow Wing" = "crow wing", Dakota = "dakota", Dodge = "dodge", Douglas = "douglas", Fairbault = "faribault", Fillmore = "fillmore", Freeborn = "freeborn", Goodhue = "goodhue", Grant = "grant", Hennepin = "hennepin", Houston = "houston", Hubbard = "hubbard", Isanti = "isanti", Itasca = "itasca", Jackson = "jackson", Kanabec = "kanabec", Kandiyohi = "kandiyohi", Kittson = "kittson", Koochiching = "koochiching", "Lac Qui Parle" = "lac qui parle", Lake =  "lake", "Lake of the Woods" = "lake of the woods", "Le Sueur" = "le sueur", Lincolon = "lincoln", Lyon = "lyon", McLeod = "mcleod", Mahnomen = "mahnomen", Marshall = "marshall", Martin = "martin", Meeker = "meeker", "Mille Lacs" = "mille lacs", Morrison = "morrison", Mower = "mower", Murray = "murray", Nicollet =  "nicollet", Nobles = "nobles", Norman = "norman", Olmsted = "olmsted", "Otter Tail" = "otter tail", Pennington = "pennington", Pine = "pine", Pipestone = "pipestone", Polk = "polk", Pope = "pope", Ramsey = "ramsey", "Red Lake" = "red lake", Redwood = "redwood", Renville = "renville", Rice = "rice", Rock = "rock", Roseau = "roseau", "St. Louis" = "st louis", Scott =  "scott", Sherburne = "sherburne", Sibley = "sibley", Stearns = "stearns", Steele = "steele", Stevens = "stevens", Swift = "swift", Todd = "todd", Traverse = "traverse", Wabasha =  "wabasha", Wadena = "wadena", Waseca = "waseca", Washington =  "washington", Watonwan = "watonwan", Wilkin = "wilkin", Winona = "winona", Wright = "wright", "Yellow Medicine" = "yellow medicine"), 
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              
                              # Time Frame Choice Input
                              selectInput(inputId = "years2",
                                          label = "Select Year:",
                                          choices = vaccines$year), 
                              
                              # Submit Button 
                              submitButton(text = "Create my map!"), 
                              HTML("<br>"),
                              HTML("<br>"),
                              HTML("<p> Data Source: Minnesota Department of Health. For more information, please visit their <a href='https://data.web.health.state.mn.us/immunization_basic'>website</a>.")
                              
                            ), 
                            mainPanel(h4("Comparing Vaccination Rates Across Counties"), 
                                      p("Mapping provides another useful visualization of this data. Particularly, choropleth maps can help identify populations at risk for vaccine-preventable disease and encourage public health actions and policies aimed at increasing immunization. Follow the instructions on the left to produce a map to compare vaccination rates across MN counties."),
                                      HTML("<br>"),
                                      plotlyOutput(outputId = "vaccinemap"),
                                      dataTableOutput(outputId = "vaccinetable"))
                          )
                 )
)


server <- function(input, output) {
  
  mapped_counties <- reactive({
    req(input$vaccine_choice2)
    req(input$years2)
    req(input$county_name2)
    filter(vaccines_and_map, 
           vaccine == input$vaccine_choice2,
           year == input$years2,
           subregion %in% c(input$county_name2)) 
    
  }) 
  
  tabled_counties <- reactive({
    filter(clean_vaccines2,
           vaccine == input$vaccine_choice2,
           year == input$years2,
           subregion %in% c(input$county_name2))
  })
  
  tabled_counties2 <- reactive({
    mutate(tabled_counties(),
           vaccine == input$vaccine_choice2,
           year == input$years2,
           subregion = str_to_title(c(input$county_name2)))
    
  })
  
  output$vaccineplot <- renderPlot({
    vaccines %>% 
      filter(vaccine %in% c(input$vaccine_choice), 
             location == input$county_name) %>% 
      ggplot() +
      geom_line(aes(x = year, y = percent, color = vaccine)) +
      geom_point(aes(x = year, y = percent, color = vaccine)) +
      geom_hline(yintercept = 80, color = "grey", alpha = .5, linetype = "dashed") +
      geom_hline(yintercept = 90, color = "grey", alpha = .5, linetype = "dashed") +
      #geom_text(aes(x = 2018, y = 80, label = "Individual Immunization Goal", vjust = -1)) +
      scale_x_continuous(limits = input$years) +
      theme_classic() +
      labs(x = "Years", 
           y = "Percent Vaccinated", 
           color = "Vaccine") +
      ggtitle(paste("Childhood Vaccination Rates", "in", input$county_name))
  })
  
  map1 <- reactive({
    mapped_counties() %>% 
      ggplot(data = vaccines_and_map, mapping = aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.3) +
      geom_polygon(color = "black", fill = "gray") +
      geom_polygon(data = mapped_counties(), aes(group = subregion, 
                                                 fill = `Percent Vaccinated`, 
                                                 text = paste(" County:", str_to_title(subregion), "<br>", "Percent Vaccinated:", `Percent Vaccinated`, "<br>", "Number of Children (24-35 Months):", population)), 
                   color = "black") +
      geom_polygon(color = "black", fill = NA) + 
      theme_bw() +
      scale_fill_viridis_c() +
      theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank() 
      ) +
      #labs(fill = "Percent of Children (24-35 Months) Vaccinated") 
      ggtitle(paste("MN County Childhood Vaccination Rates for", input$vaccine_choice2, "in", input$years2))
    
  })
  
  
  
  output$vaccinemap <- renderPlotly({
    
    ggplotly(map1(), 
             tooltip = c("text")) 
    
  })
  
  output$vaccinetable <- renderDataTable({
    tabled_counties2() %>% 
      select(year, subregion, vaccine, percent, population) %>% 
      rename(Year = year, 
             County = subregion, 
             Vaccine = vaccine, 
             "Percent Vaccinated" = percent,
             Population = population) %>% 
      arrange("Percent Vaccinated") 
    
  })
}

shinyApp(ui = ui, server = server)

