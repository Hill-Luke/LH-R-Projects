## Creating a sample Shiny Dashboard for portfolio usage


#This dashboard uses data from the Atlas of Economic Complexity
#downloaded on 12/26/2018.

install.packages("shinydashboard")
install.packages("reshape")
install.packages("sqldf")
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(reshape)
library(ggplot2)
library(sqldf)
library(plotly)

#################################
#Loading and subsetting the data
#################################:

#Reading in the Internet Usage data
internetuse_all<-read_csv("/Users/lukeofthehill/Desktop/R/Data/Internet Usage/usage_all.csv")
groups<-read_csv("/Users/lukeofthehill/Desktop/R/Data/Internet Usage/groups.csv")

#Subsetting to 1990 through 2016
internetuse<-subset(internetuse_all, select=c('Country Name','Country Code','1990':'2016')) %>% data.frame()

#melting the data to format it in long format.
long_use<-melt(internetuse,id=c('Country.Name', 'Country.Code'), measured=c('X1990':'X2016'))
long_use$Year<-substr(long_use$variable, 2, 5) %>% as.integer()
long_use$pct_usage<-long_use$value
long_use$value<-NULL
groups$Country.Code<-groups$'Country Code'
groups$'Country Code'<-NULL
groups_sub<-subset(groups, select=c('Country.Code','Region', 'IncomeGroup'))
long_use<-subset(long_use, select=c('Country.Name','Country.Code','time','pct_usage')) %>% data.frame()
internet_reformat<-left_join(long_use,groups_sub, by='Country.Code', copy=FALSE)

world_internet_usage<-sqldf("select Region, AVG(pct_usage) as average_usage, Year 
                            from internet_reformat
                            group by Region, Year;")

world_internet_usage<-subset(world_internet_usage, is.na(Region)==FALSE & Year>=1990)


#Reading in the Tech Adoption in the US data
tech_adopt_USA<-read_csv("/Users/lukeofthehill/Desktop/R/Data/tech adoption/tech_adoption.csv")
tech_adopt_USA$adoptpct<-rename(tech_adopt_USA$`Technology Diffusion (Comin and Hobijn (2004) and others) (%)`, tech_adopt_USA$adoptpct)
#Reading in Mobile Money outside of the US.
mobile_money<-read_csv("/Users/lukeofthehill/Desktop/R/Data/tech adoption/mobile_money.csv")


#Reading in the Anxiety disorders in Men and Women in the US data
anxiety<-read_csv("/Users/lukeofthehill/Desktop/R/Data/anxiety.csv")
US_anxiety<-subset(anxiety, Entity=="United States" & Year>=1990)

#Reading in the GDP and Country Group Data
gdp_all_country<-read_csv("/Users/lukeofthehill/Desktop/R/Data/GDP per cap.csv")
country_group<-read_excel("/Users/lukeofthehill/Desktop/R/Data/WB_country_code.xlsx")

#Melting the GDP data
gdp_sub<-subset(gdp_all_country, select=c('Country Name','Country Code','1990':'2016')) %>% data.frame()
gdp_melt<-melt(gdp_sub,id=c('Country.Name', 'Country.Code'), measured=c("X1990":"X2016"))
gdp_melt$Year<-substr(gdp_melt$variable, 2, 5) %>% as.integer()

gdp_melt$'GDP Per Capita'<-as.integer(gdp_melt$value)
gdp_melt$variable<-NULL
gdp_melt$value<-NULL

#Joining with the country group data
gdp_full<-left_join(gdp_melt, select(country_group,"Country.Name", "Region", "Country.Code"), by=c("Country.Name", "Country.Code"))

#Joining the internet usage data
gdp_int<-left_join(gdp_full, select(internet_reformat, "Country.Code","pct_usage", "Year"), by=c("Country.Code","Year"))










###
#Subset this data to the tech you want to make it more legible.
table(tech_adopt_USA$Entity)
tech_sub<-subset(tech_adopt_USA, (Entity=="Social media usage"| Entity=="Smartphone usage"
                                  | Entity=="Tablet" | Entity=="Internet" | Entity=="Computer") & Year>=1990)

graph_anx<-left_join(tech_sub, US_anxiety, by="Year")

#Anxiety and Technology adoption



#tech adoption over time
tech_adopt_USA$Entity2<-as.factor(tech_adopt_USA$Entity)

tech_graph<-ggplot(tech_sub, aes(x=Year, y=adoptpct, color=Entity2))+
  geom_line(stat="identity", aes(fill=Entity2))

ggplotly(tech_graph)
names(mobile_money)



library(scales)
?percent()

world_internet_usage$'Mean Internet Usage (%)'<-round(world_internet_usage$average_usage/100,2)


world_intusage_plot<-ggplot(world_internet_usage, aes(x=Year, y=average_usage, color=Region))+
  geom_line(stat="identity")
ggplotly(world_intusage_plot)

table(tech_adopt_USA$Entity)
table(tech_sub$`Technology Diffusion (Comin and Hobijn (2004) and others) (%)`)

traditional_tech<-subset(tech_adopt_USA, Entity=="Automobile" | Entity=="Cable TV" |
                           Entity=="Electric power" | Entity=="Flush toilet" | Entity=="Household refridgerator")




#Chunk 1: Traditional Tech Adoption hasn't always been rapid.

Traditional_Adopt<-ggplot(traditional_tech, aes(x=Year, y=`Technology Diffusion (Comin and Hobijn (2004) and others) (%)`, color=Entity))+
  geom_line(stat="identity")
ggplotly(Traditional_Adopt)

#Internet technology adoption had a much quicker role in our lives than most other mainstream technology
Internet_Adopt<-ggplot(tech_sub, aes(x=Year, y=`Technology Diffusion (Comin and Hobijn (2004) and others) (%)`, color=Entity))+
  geom_line(stat="identity")
ggplotly(Internet_Adopt)

#Internet adoption still lags across the underdeveloped world.
world_intusage_plot<-ggplot(world_internet_usage, aes(x=Year, y=`Mean Internet Usage (%)`, color=Region))+
  geom_line(stat="identity") +
  ylab("Average Percent of Population using the Internet") +
  scale_y_continuous(limits=c(0,1), labels=scales::percent)
ggplotly(world_intusage_plot)




#Social Media usage does not appear to impact
table(tech_adopt_USA$Entity)
tech_sub<-subset(tech_adopt_USA, (Entity=="Social media usage"| Entity=="Smartphone usage"
                                  | Entity=="Tablet" | Entity=="Internet" | Entity=="Computer") & Year>=1990)

mental_health_us<-read.csv("/Users/lukeofthehill/Desktop/R/Data/mental_health.csv")
mental_health_us$Country.Name<-mental_health_us$Entity %>% as.character()
mental_health_us$Entity<-NULL

int_mental<-left_join(mental_health_us, select(long_use, c("Country.Name","pct_usage", "Year")), by=c("Country.Name","Year"))

graph_anx<-left_join(tech_sub, int_mental, by="Year")


graph_anx$`Anxiety disorders`<-graph_anx$`Anxiety.disorders....`
graph_anx$`Anxiety.disorders....`<-NULL
table(graph_anx$Entity.x)

graph_anx$Eating.disorders....
graph_anx$Diffusion<-graph_anx$`Technology Diffusion (Comin and Hobijn (2004) and others) (%)` %>% as.numeric()

table(graph_anx$Entity)
EatingDisorder_SM<- graph_anx %>% 
  filter(Entity=="Social media usage") %>%
  ggplot(aes(x=Year, y=`Eating.disorders....`, size=Diffusion))+
  geom_point(stat="identity") +  scale_colour_gradient(low = "steelblue", high = "yellow") +
  theme_bw()
#+ scale_fill_brewer(palette="Set1")
#scale_fill_manual(values=c("#48D1CC"))
plot(EatingDisorder_SM)

ggplotly(EatingDisorder_SM)

Depression_SM<- graph_anx %>% 
  filter(Entity=="Social media usage") %>%
  ggplot(aes(x=Year, y=`Depression....`, size=`Technology Diffusion (Comin and Hobijn (2004) and others) (%)`))+
  geom_point()
ggplotly(Depression_SM)








#################################
#Defining the dashboard structure.
#################################


sidebar<- dashboardSidebar(
  #Sidebar List of pages
  sidebarMenu(
    menuItem("Home", tabName = "home", icon=icon("dashboard")),
    menuItem("Technology", tabName = "Technology", icon=icon("dashboard")),
    menuItem("Mental Health", tabName = "MentalHealth", icon=icon("dashboard"))
  )
)
#Defining the body content for each page
dashbody<-dashboardBody(
  tabItems(
    #Defining what is going on each of the tabs
    #Home
    tabItem(
      tabName="home",
      #creating the content
      fluidRow(
        box("Welcome to my sample dashboard. This dashboard was created using R Shinydashboard. 
            Data were obtained from various public sources, all originally from Our World In Data.
            The specific data sources are listed in each tab. For more information, please visit the following URL:
            <https://ourworldindata.org>", width=12
        )
        )
        ),
    #Internet Usage Tab
    tabItem(tabName="Technology",
            #creating the content
            fluidRow(
              box(
                "This is the Technology Usage Tab", width=12
              ),
              box(
                plotlyOutput("world_intusage_plot"), width=12
              ),
              box(
                plotlyOutput("mobile_money_plot"), width=12
              ),
              box(
                plotlyOutput("gdp_world"), width=12, sliderInput(inputId = "year_gdp", "Year", min=1, max=27, value=1)
              )
            )
    ),
    tabItem(tabName = "MentalHealth",
            fluidRow(
              box("This is the Mental Health Tab", width=12)
            )
    )
        ))


ui <- dashboardPage(
  dashboardHeader(title="Example Dashboard"),
  sidebar,
  dashbody)

#################################
#Running the application.
#################################


server <- function(input, output) { 
  
  output$world_intusage_plot<-renderPlotly({  
    
    world_intusage_plot<-ggplot(world_internet_usage, aes(x=Year, y=`Mean Internet Usage (%)`, color=Region))+
      geom_line(stat="identity") +
      ylab("Average Percent of Population using the Internet") +
      scale_y_continuous(limits=c(0,1), labels=scales::percent)
  })
  
  output$mobile_money_plot<-renderPlotly({
    
    mobile_money$num_accounts<-as.integer(mobile_money$`Registered mobile money accounts (GSMA (2017)) (registered accounts)`)
    mobile_money_sub<-subset(mobile_money, Entity!="World" & Entity!="World (excluding Sub-Saharan Africa)" & Year >=2011)
    #Number of Mobile Money Accounts
    mobile_money_plot<-ggplot(mobile_money_sub, aes(x=Year, y=num_accounts, color=Entity)) +
      geom_area(stat="identity", aes(fill=Entity))+
      scale_y_continuous()
    #  ggplotly(mobile_money_plot)
  })
  output$gdp_world<-renderPlotly({
    slider<-sqldf("select distinct Year from gdp_int;")
    slider$slider<-c(1:27)
    GDP_year<-left_join(gdp_int,slider, by="Year") %>% data.frame()
    
    GDP_year$filter<-c(input$year_gdp)
    #GDP_year$filter<-c(7)
    
    
    #subsetting to the filter year
    graph_GDP<-subset(GDP_year, slider==GDP_year$filter) %>% data.frame()
    
    #Renaming vars
    graph_GDP$`GDP Per Capita`<-graph_GDP$GDP.Per.Capita
    graph_GDP$`Internet Usage (%)`<-graph_GDP$pct_usage/100
    
    GDP_Int_plot<-ggplot(graph_GDP, aes(x=`GDP Per Capita`, y=`Internet Usage (%)`, color=Region))+
      geom_point(stat="identity")+
      ylab("Percent of Population using the Internet")+
      scale_y_continuous(limits=c(0,1))
    ggplotly(GDP_Int_plot)
    
  })
  
}

shinyApp(ui, server)

#ggplotly(world_intusage_plot)



