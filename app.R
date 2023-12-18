#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(flexdashboard)
library(shinydashboard)
library(tidyr)
library(ggplot2)
library(stringr)
library(plotly)
library(ggthemes)
library(tm)
library(readxl)
library(SnowballC)
library(devtools)
library(bitops)
library(reshape2)


#map data
indonesia <- read_xlsx("indo_map.xlsx")
#key demographics data
age <- read.csv("age_distribution_indonesia.csv")
imr <- read.csv("imr.csv")
#comparison data
compare <- read.table("compare.csv", header = TRUE, sep = ",", quote = "\"", fill = TRUE)



ui <- shinyUI(fluidPage(
  
  titlePanel(
    fluidRow(
      column(8, 
             h3("Indonesia and Its Vibrant"),
             h5("By Febriany Lete. All the codes can be approached from", 
                a(href="https://github.com/febylete/final-project_ma615", 
                  "my Github"))
      ))),
  
  navbarPage(
    title= "", 
    tabPanel("Introduction", 
             br(), 
             p("Indonesia, officially known as the Republic of Indonesia, 
             is a diverse and expansive country situated in Southeast Asia and Oceania, 
             spanning the regions between the Indian and Pacific oceans. 
             Comprising a vast archipelago, Indonesia boasts over 17,000 islands, 
             with notable ones including Sumatra, Java, Sulawesi, and portions of Borneo and New Guinea. 
             Recognized as the world's largest island country, Indonesia ranks as the 14th-largest nation globally, 
             covering an extensive area of 1,904,569 square kilometers (735,358 square miles).
             Home to a vibrant population exceeding 279 million people, Indonesia stands as the world's fourth-most populous country.
             The island of Java, renowned as the world's most populous island, accommodates more than half of the country's total population.
             Indonesia offers a plethora of captivating destinations to explore, boasting over tens of thousands of islands within its archipelago. 
             The country is renowned for its abundant and diverse natural resources, showcasing a wealth of unique flora and fauna that contributes to Indonesia's stunning natural landscapes.
             From majestic mountains to lush jungles and picturesque seas, each region in Indonesia holds its own distinct charm and cultural identity, 
             making every corner of the country a compelling destination throughout most of the year. 
             The natural beauty of Indonesia unveils a tapestry of wonders and mysteries that have endured the test of time, captivating the world's attention.
             In this report, I will highlight 3 natural wonders in Indonesia that have garnered international recognition, inviting travelers to witness the extraordinary beauty and richness of the country's landscapes."),
             br(),
             h4("3 Indonesia Natural Wonders Recognized by The World"),
             sidebarLayout(position = "right",
                          sidebarPanel(img(src = "borobudur-temple.jpg", height = 300, width = 500)),
                           mainPanel(p("- Borobudur is a Buddhist temple located in Magelang, Central Java, Indonesia. 
                  This stupa-shaped temple was founded by the adherents of Buddhism around the year 800 AD during the reign of Wangsa Syailendra. 
                  Borobudur is the largest Buddhist temple in the world and has been recognized as one of the seven wonders of the world and a UNESCO World Heritage Site."))
             ),#close sidebar
             sidebarLayout(position = "right",
                         sidebarPanel(img(src = "komodo-island.jpg", height=300, width=500)),
                           mainPanel(p("- Komodo is an island located in Komodo District, West Manggarai Regency, East Nusa Tenggara Province, Indonesia. Komodo Island is known as a native habitat of Komodo animals. This area is also part of Komodo Island National Park which is managed by the central government. This island is located in the east part of Sumbawa Island which is separated by the Sape Strait. The seabed water of Komodo is the best in the world. Komodo Island is also a UNESCO World Heritage Site."))
             ),#close sidebar
             sidebarLayout(position = "right",
                           sidebarPanel(img(src = "jayawijaya.jpg", height=300, width=500)),
                           mainPanel(
                             p("- Jayawijaya is a peak which is part of Sudirman Barisan mountains in Papua Province, Indonesia. This peak has an altitude of 4,884 meters above sea level and around the peak there is the Carstensz glacier, the only tropical glacier in Indonesia, which is likely to soon disappear due to global warming. Jayawijaya is the highest mountain to climb in Indonesia and one of the seven peaks of the world."))
             ) #close sidebar
    ), #close introduction tab
    
    
    tabPanel("General Description", 
             tabsetPanel(
               tabPanel("Maps",
                        br(),
                        p("Here is the map of Indonesia and its population. The map shows the location of Indonesia in the world and its state or province. Drag your cursor into the purple point, and it will show you the population of the state/province."),
                        hr(),
                        splitLayout(leafletOutput("indo_map"))
               ),#close maps
               
               
               tabPanel("Key Demographics",
                        br(),
                        p("Here is the key demographic information about Indonesia.There are population, age distribution, and health."),
                        p("1. Population"),
                        p("Indonesia's population distribution varies significantly across its islands and provinces. Java, the most populous island, hosts major urban centers, while Sumatra and Kalimantan also contribute significantly. Eastern regions like Papua and West Papua exhibit distinct demographics. Factors such as economic opportunities and historical settlement patterns influence these variations."),
                        p("2. Age Distribution"),
                        p("From the chart, we can see that Indonesian age distribution looks like expansive population pyramids which show larger numbers or percentages of the population in the younger age groups, usually with each age group smaller in size or proportion than the one born before it. These types of pyramids are usually found in populations with very large fertility rates and lower than average life expectancies."),
                        p("3. Health"),
                        p("This graph shows Infant Mortality Rate (IMR) in Indonesia by State. IMR is a crucial indicator used to assess the health of a population, particularly the well-being of infants within the first year of life. 
                          A low IMR generally indicates that a country has adequate and accessible healthcare services also reflects the overall environmental and living conditions.
                          If we look closely, eastern Indonesia on average has a higher IMR rate compared to western Indonesia. This shows the health conditions in the Indonesian archipelago.
                          Apart from that, it is clear that the IMR figure in Indonesia has also decreased drastically from 1971 to 2020"),
                        hr(),
                        sidebarLayout(
                          sidebarPanel(radioButtons(inputId = "timelineinput",
                                                    label="Select Category",
                                                    choices = c("Population", "Age Distribution", "Health"))),
                          mainPanel(
                            uiOutput("demographic_content")))
               ), # close Key Demographics tabPanel
               
               tabPanel("Comparison with Other State",
                        br(),
                        p("Here is a table show the comparison between Indonesia and Singapore based on population, economic, language, and so on."),
                        p("Indonesia and Singapore stand as two distinct Southeast Asian nations with contrasting demographic, economic, and cultural characteristics. 
                          Indonesia, the world's fourth most populous country, boasts a diverse cultural tapestry and a burgeoning economy that spans agriculture, manufacturing, and services. 
                          On the other hand, Singapore, a city-state with a smaller population, has evolved into a global financial hub, emphasizing high-tech industries and international trade. 
                          While Indonesia's richness lies in its natural resources and cultural diversity, Singapore's strength lies in its efficient infrastructure, economic stability, and innovation. 
                          Both nations contribute significantly to the region's dynamism, each offering unique contributions to the Southeast Asian landscape."),
                        tableOutput("comparison_table")
               ), #close comparison with Other State
               
               tabPanel("SWOT Analysis",
                        br(),
                        p("Here is the SWOT Analysis about Indonesia:"),
                        p("1. Strengths"),
                        p("Indonesia possesses several strengths that contribute to its prominence on the global stage. With a population exceeding 270 million, it is the fourth most populous nation globally, fostering a rich cultural tapestry. The country boasts a diverse and rapidly growing economy, being the largest in Southeast Asia and a member of the G20. Abundant natural resources, picturesque tourist destinations, a strategic geopolitical position, and political stability further enhance Indonesia's appeal. Cultural diversity, an emerging middle class, and a commitment to sustainable development are additional strengths that position Indonesia as a key player in the Southeast Asian region."),
                        p("2. Weaknesses"),
                        p("Indonesia faces several challenges that constitute its weaknesses. Infrastructure deficits, particularly in rural areas, hinder overall connectivity and economic development. Economic disparities between regions contribute to uneven distribution of resources and opportunities, posing social and developmental challenges. The nation is vulnerable to natural disasters, such as earthquakes and tsunamis, which can have devastating impacts on communities and infrastructure. Rampant deforestation and environmental degradation present environmental weaknesses, affecting ecosystems and biodiversity."),
                        p("3. Opportunities"),
                        p("Indonesia presents promising opportunities for growth and development. With its diverse economy, there is a chance to further diversify and tap into emerging industries such as technology and renewable energy, fostering innovation and economic resilience. The country's natural beauty and cultural richness provide a solid foundation for the thriving tourism sector, offering the opportunity to attract a larger number of international visitors. A youthful and expanding population contributes to a dynamic workforce that can drive economic growth and innovation. Additionally, sustainable development initiatives can position Indonesia as a leader in environmentally conscious practices, aligning with global trends toward a greener future."),
                        p("4. Threats"),
                        p("Indonesia faces various threats that pose challenges to its stability and development. The nation is highly susceptible to natural disasters, including earthquakes, tsunamis, and volcanic eruptions, which can result in significant human and economic losses. Global economic uncertainties and fluctuations can impact trade and investments, potentially disrupting economic growth. Political instability, though relatively subdued, remains a threat that could hinder long-term development efforts. Climate change poses a threat through rising sea levels and extreme weather events, impacting agriculture and vulnerable coastal communities."),
                        hr(),
               ) # close SWOT Analysis
             )), #close general description
    
    
    tabPanel("Conclusion",
             br(),
             p("Indonesia stands as a diverse and dynamic country with a rich tapestry of cultures, ethnicities, and landscapes. With a population exceeding 270 million, it is the fourth most populous nation globally. 
               The country's economic landscape is equally diverse, encompassing agriculture, manufacturing, and a burgeoning services sector. 
               Indonesia's natural beauty, from the pristine beaches of Bali to the cultural richness of cities like Yogyakarta, makes it a popular tourist destination. 
               However, the nation faces challenges such as regional disparities, environmental concerns, and healthcare access. 
               Indonesia's ongoing efforts to address these challenges, coupled with its economic growth and cultural vibrancy, position it as a significant player in the Southeast Asian region. 
               As the country continues to navigate the complexities of development, its journey reflects a blend of tradition and modernity, resilience, and a commitment to progress."),
             br(), 
             img(src = "rajaampat.jpg", width = "800px", height = "600px"),
             br(),
             h4("References"),
             p("https://bps.go.id"),
             p("https://sig.bps.go.id"),
             p("https://rpubs.com/Akshay91/401549"),
             p("https://gist.github.com/benmarwick/70f92dd61700abab1b590afa0040e3fa"),
             p("https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html"),
             p("https://tribratanews.polri.go.id/blog/none-22/berikut-7-keajaiban-dunia-di-indonesia-menarik-untuk-dikunjungi-51613"),
             p("https://chat.openai.com")
    ) #close navbarPage
  )))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$indo_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  
      setView(lng = 118.25, lat = 0.25, zoom = 4) %>%
      addCircleMarkers(data = indonesia,
                       lng = ~longitude,
                       lat = ~latitude,
                       color = "purple",
                       radius = 5,
                       opacity = 0.8) %>%
      addLabelOnlyMarkers(data = indonesia,
                          lng = ~longitude,
                          lat = ~latitude,
                          label = ~paste(woe_label, "<br>Population: ", population),
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"
                          )
      ) %>%
      addLegend("topright", colors = "purple", labels = "State")
  }) # close renderLeaflet
  
  
  
  output$demographic_content <- renderUI({
    category <- input$timelineinput
    
    if (category == "Population") {
      # Display an image for Population
      img(src = "pop_map.png", width = "100%", height = "auto")
    } else if (category == "Age Distribution") {
      # Include your code to generate the bar graph using plotly or other plotting libraries
      plot_ly(data = age, x = ~age_group_start, y = ~ratio, type = 'bar') %>%
        layout(title = "Age Distribution in Indonesia",
               xaxis = list(title = "Age Group"),
               yaxis = list(title = "Ratio"))
    } else if (category == "Health") {
      imr_long <- tidyr::gather(imr, key = "year", value = "population", -state)
      
      # Plotting time series by state and year with different colors
      output$linePlot <- renderPlot({
        ggplot(imr_long, aes(x = state, y = population, color = year, group = year)) +
          geom_line() +
          labs(x = "State", y = "Value", title = "Time Series Across States") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 8),
            legend.title = element_blank(),
            legend.position = "bottom"
          )})
    } else {
      # Default content if none of the categories match
      p("Select a category to view demographic information.")
    }
  }) #close demographic content
  
  
  output$comparison_table <- renderTable({
    tidy_compare <- compare %>%
      gather(key = "Category", value = "Value", -Country) %>%
      spread(key = "Country", value = "Value")
    
    tidy_compare$Category <- as.factor(tidy_compare$Category)
    return(tidy_compare)
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)
