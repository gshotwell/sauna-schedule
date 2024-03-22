
library(shiny)
library(bslib)
source("helpers.R")



# Define UI for application that draws a histogram
ui <- page_fillable(
  theme = bs_theme(bootswatch = "sketchy"),
  h1(paste0("The next Sauna will be Thursday, ", get_next_thursday(), " at 8:00 PM")),
  layout_columns(
    card(
      card_header("Who's coming?"), tableOutput("attendees")),
    card(
      card_header("Book your spot!"),
      uiOutput("sign_up")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  meta = get_metadata()
  attendees = reactiveVal(get_attendees())
  
  output$sign_up = renderUI({
    if(meta$sauna_on == 0){
      return(h1("Sauna is cancelled this week"))
    }
    if (nrow(attendees()) >= meta$max_seats) {
      return(h1("Sauna is all full"))
    }
    return(tagList(
      textInput("name", "Name"),
      actionButton("submit", "Submit")))
  })
  
  output$attendees = renderTable({
    attendees()
  })
  
  observeEvent(input$submit, {
    if(nchar(input$name) < 3) {
      return ()
    }
    shiny::showModal(
      modalDialog(
        title = "Confirmed!",
        "Can't wait to see you! Please bring a bathing suit and towel",
        easyClose = TRUE
      )
    )
    updateTextInput(inputId = "name", value = "")
    append_attendee(input$name)
    attendees(get_attendees())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
