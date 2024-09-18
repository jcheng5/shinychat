# shinychat

<!-- badges: start -->
[![R-CMD-check](https://github.com/jcheng5/shinychat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jcheng5/shinychat/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Chat UI component for [Shiny for R](https://shiny.posit.co/).

(For [Shiny for Python](https://shiny.posit.co/py/), see [ui.Chat](https://shiny.posit.co/py/components/display-messages/chat/).)

## Installation

You can install the development version of shinychat from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jcheng5/shinychat")
```

## Example

```r
library(shiny)
library(shinychat)

ui <- bslib::page_fluid(
  chat_ui("chat")
)

server <- function(input, output, session) {
  chat <- elmer::new_chat_openai(system_prompt = "You're a trickster who answers in riddles")
  
  observeEvent(input$chat_user_input, {
    stream <- chat$stream(input$chat_user_input)
    chat_append_stream("chat", stream)
  })
}

shinyApp(ui, server)
```
