library(shiny)
library(e1071)

b = "buying"
m = "maint"
d = "doors"
p = "persons"
lb= "lug_boot"
s = "safety"

carros   = read.csv("car.data", sep = ',')

modelo   = naiveBayes(class ~., data = carros)

buying   = unique(carros$buying)
maint    = unique(carros$maint)
doors    = unique(carros$doors)
lug_boot = unique(carros$lug_boot)
persons  = unique(carros$persons)
safety   = unique(carros$safety)

ui <- fluidPage(
  
   titlePanel("Previsão de qualidade de veiculos"),
   fluidRow(
     column(4,selectInput(b,"Preço", choices = buying)),
     column(4,selectInput(m,"Manutenção", choices = maint)),
     column(4,selectInput(d,"Portas", choices = c(doors)))
   ),
   fluidRow(
     column(4,selectInput(p,"Capacidade de Passageiros", choices = persons)),
     column(4,selectInput(lb,"Porta malas", choices = lug_boot)),
     column(4,selectInput(s,"Segurança", choices = safety))
   ),
   fluidRow(
     column(12, actionButton("Processar", "Processar"), h1(textOutput("Resultado")))
   )
   
)


server <- function(input, output) {
   observeEvent(input$Processar, {
     novo_carro = data.frame("buying"=input$buying, "maint"=input$maint, "doors"=input$doors, "persons"=input$persons, "lug_boot"=input$lug_boot, "safety"=input$safety)
     predicao = predict(modelo, novo_carro)
     textoPred = as.character(predicao)
     texto = ""
     if(textoPred == "unacc"){
       texto = "Inaceitável"
     }else if(textoPred =="acc"){
       texto = "Aceitável"
     }else if(textoPred =="vgood"){
       texto = "Muito bom"
     }else if(textoPred =="acc"){
       texto = "Bom"
     }
     output$Resultado = renderText({texto})
   })
   
}

shinyApp(ui = ui, server = server)