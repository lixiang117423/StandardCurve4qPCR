#######################################
#################  ui  ################
#######################################

# load packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(shinyWidgets)
library(Cairo)
library(readxl)
library(ggpmisc)
library(ggprism)
library(export)
library(patchwork)

#########################################定义选卡###############################
if (TRUE) {
  sidebar = dashboardSidebar(width = 230,
                             sidebarMenu(
                               id = 'tabs',
                               color = 'olive',
                               menuItem('Welcome',
                                        tabName = 'welcome',
                                        icon = icon('bullhorn')),
                               menuItem('StandardCurve',
                                        tabName = 'main',
                                        icon = icon('quinscape')),
                               menuItem('Help',
                                        tabName = 'help',
                                        icon = icon('question-circle')
                                        
                               )))
  
}


body <- dashboardBody(
  tabItems(
    # 调试成功
    tabItem(tabName = 'welcome',
            source('./main/welcome_ui.R',
                   local = TRUE,
                   encoding = 'UTF-8')$value),
    tabItem(tabName = 'main',
            source('./main/qpcr_ui.R',
                   local = TRUE,
                   encoding = 'UTF-8')$value),
    tabItem(tabName = 'help',
            source('./main/help_ui.R',
                   local = TRUE,
                   encoding = 'UTF-8')$value)
    
    ))

# 组合
ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title = "StandardCurve4qPCR"),
  sidebar = sidebar,
  body
)