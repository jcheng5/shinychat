# shinychat

<!-- badges: start -->
[![R-CMD-check](https://github.com/jcheng5/shinychat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jcheng5/shinychat/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Chat UI component for [Shiny for R](https://shiny.posit.co/).

(For [Shiny for Python](https://shiny.posit.co/py/), see [ui.Chat](https://shiny.posit.co/py/components/display-messages/chat/).)

## Installation

You can install the development version of shinychat from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jcheng5/shinychat")
```

## Example

To run this example, you'll first need to create an OpenAI API key, and set it in your environment as `OPENAI_API_KEY`.

You'll also need to call `pak::pak("tidyverse/elmer")` to install the {[elmer](https://elmer.tidyverse.org/)} package.

```r
library(shiny)
library(shinychat)

ui <- bslib::page_fluid(
  chat_ui("chat")
)

server <- function(input, output, session) {
  chat <- elmer::chat_openai(system_prompt = "You're a trickster who answers in riddles")
  
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

shinyApp(ui, server)
```
