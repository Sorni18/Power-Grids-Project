library(readxl)
library(ggplot2)
library(dplyr)
library(networkD3)
library(shiny)

knitr::opts_chunk$set(echo = TRUE)

europa="europa.xlsx"
hojas=excel_sheets(europa)

hojas=setdiff(hojas, c("Contents", "footnotes and sources", "energy flows-energy balances", "energy products-energy balances"))


ui <- fluidPage(
  titlePanel("Selección de Año y País"),
  
  selectInput("year", "Selecciona un año:",
              choices = as.character(seq(1990, 2020)),
              selected = "1990"),
  
  selectInput("country", "Selecciona un país:",
              choices = hojas,
              selected = "ES"),
  
  verbatimTextOutput("resultados"),
  
  htmlOutput("html_output")
)

server <- function(input, output) {
  
  balance <- reactive({
    pais <- input$country
    año <- input$year
    
    excel=read_excel(europa, sheet=pais)
    
    excel=excel[-c(255:545), ]
    excel=excel[-c(1:10), ]
    excel=excel[, -c(1,2)]
    colnames(excel)=c("Balance", c(1990:2020))
    excel$`1990` <- as.numeric(excel$`1990`)
    
    excel <- excel[-c(1:85), ]
    excel <- excel[-c(26:95), ]
    excel <- excel[-c(29:37), ]
    excel <- excel[-27, ]
    excel <- excel[-c(29,31,33), ]
    excel <- excel[-c(32:51), ]
    
    
    excel <- excel[-c(3,4), ]
    excel <- excel[-c(11:20), ]
    excel <- excel[-7, ]
    
    excel[, c(2:32)]=lapply(excel[, c(2:32)], as.numeric)
    
    convertir_a_positivo <- function(fila) {
      return(ifelse(fila < 0, -1 * fila, fila))
    }
    
    excel[, c(2:32)]=lapply(excel[, c(2:32)], convertir_a_positivo)
    
    excel <- excel[, colSums(is.na(excel)) != nrow(excel)]
    
    agrupar <- excel[c(3,4,10,11,12), ]
    
    suma <- agrupar %>%
      summarise(across(where(is.numeric), sum))
    
    otros <- c("Others", as.numeric(suma))
    
    excel <- excel[-c(3,4,10,11,12),]
    
    excel <- rbind(excel, otros)
    
    excel <- excel[-38, ]
    
    excel <- excel[-3, ]
    
    excel <- excel[-c(14:26),]
    excel <- excel[-c(15:20),]
    
    
    pre_sankey <- data.frame(Balance = excel$Balance, 'Valor' = excel[, año])
    
    rownames(pre_sankey) <- pre_sankey$Balance
    pre_sankey <- subset(pre_sankey, select=-1)
    
    pre_sankey <- t(pre_sankey)
    
    node <- data.frame(
      name = colnames(pre_sankey),
      stringsAsFactors = FALSE
    )
    
    sankey <- data.frame(
      source = integer(),
      target = integer(),
      value = numeric(),
      stringsAsFactors = FALSE
    )
    
    columnas <- colnames(sankey)
    
    solid <- c(1, 0, pre_sankey[, "Solid fossil fuels"])
    sankey <- rbind(sankey, solid)
    colnames(sankey) <- columnas
    
    renov <- c(5, 0, pre_sankey[, "Renewables and biofuels"])
    sankey <- rbind(sankey, renov)
    
    nucl <- c(4, 0, pre_sankey[, "Nuclear"])
    sankey <- rbind(sankey, nucl)
    
    otros <- c(17, 0, pre_sankey[, "Others"])
    sankey <- rbind(sankey, otros)
    
    gas <- c(3, 0, pre_sankey[, "Natural gas"])
    sankey <- rbind(sankey, gas)
    
    oil <- c(2, 0, pre_sankey[, "Oil and petroleum products"])
    sankey <- rbind(sankey, oil)
    
    supply <- c(0, 9, pre_sankey[, "Available for final consumption"])
    sankey <- rbind(sankey, supply)
    
    supply_energy <- c(0, 7, pre_sankey[, "Energy Sector"])
    sankey <- rbind(sankey, supply_energy)
    
    supply_tlosses <- c(0, 6, pre_sankey[, "Transformation Losses"])
    sankey <- rbind(sankey, supply_tlosses)
    
    supply_dlosses <- c(0, 8, pre_sankey[, "Distribution losses"])
    sankey <- rbind(sankey, supply_dlosses)
    
    en_cons <- c(9, 11, pre_sankey[, "Final energy consumption"])
    sankey <- rbind(sankey, en_cons)
    
    non_en_cons <- c(9, 10, pre_sankey[, "Final non-energy consumption"])
    sankey <- rbind(sankey, non_en_cons)
    
    serv <- c(11, 15, pre_sankey[, "Services"])
    sankey <- rbind(sankey, serv)
    
    res <- c(11, 14, pre_sankey[, "Residential"])
    sankey <- rbind(sankey, res)
    
    agr <- c(11, 16, pre_sankey[, "Agriculture and Fishing"])
    sankey <- rbind(sankey, agr)
    
    ind <- c(11, 12, pre_sankey[, "Industry"])
    sankey <- rbind(sankey, ind)
    
    tra <- c(11, 13, pre_sankey[, "Transport"])
    sankey <- rbind(sankey, tra)
    
    sankey$source=as.integer(sankey$source)
    sankey$target=as.integer(sankey$target)
    sankey$value=as.numeric(sankey$value)
    
    list(nodes=node, links=sankey)
  })
  
  
  output$html_output <- renderUI({
    sankeyNetwork(Links = balance()$links, Nodes = balance()$nodes, Source = 'source',
                  Target = 'target', Value = 'value', NodeID = 'name',
                  units = 'Mtoe', fontSize = 12, nodeWidth = 30, width= 900, height=500)
    
  })
}

app <- shinyApp(ui = ui, server = server)
runApp(app, port=8888)