# This is a stripped-down port of the ui.Chat feature in py-shiny. The main
# things it's missing are server-side state management, i.e. the py-shiny
# version will keep the list of messages for you, and will handle the
# trimming of the message history to fit within the context window; these
# are left for the caller to handle in the R version.

chat_deps <- function() {
  htmltools::htmlDependency(
    "shinychat",
    packageVersion("shinychat"),
    package = "shinychat",
    src = "chat",
    script = "chat.js",
    stylesheet = "chat.css"
  )
}

#' @export
chat_ui <- function(
    id,
    placeholder = "Enter a message...",
    width = "min(680px, 100%)",
    height = "auto",
    fill = TRUE,
    ...) {
  tag("shiny-chat-container", list(
    id = id,
    style = htmltools::css(
      width = width,
      height = height
    ),
    placeholder = placeholder,
    fill = fill,
    chat_deps(),
    ...
  ))
}

#' @export
chat_append_message <- function(id, msg, chunk = FALSE, operation = NULL, session = getDefaultReactiveDomain()) {
  if (identical(msg[["role"]], "system")) {
    return()
  }

  if (!isFALSE(chunk)) {
    msg_type <- "shiny-chat-append-message-chunk"
    if (chunk == "start") {
      chunk_type <- "message_start"
    } else if (chunk == "end") {
      chunk_type <- "message_end"
    } else if (isTRUE(chunk)) {
      chunk_type <- NULL
    } else {
      stop("Invalid chunk argument")
    }
  } else {
    msg_type <- "shiny-chat-append-message"
    chunk_type <- NULL
  }

  if (identical(class(msg[["content"]]), "character")) {
    content_type <- "markdown"
  } else {
    content_type <- "html"
  }

  msg <- list(
    content = msg[["content"]],
    role = msg[["role"]],
    content_type = content_type,
    chunk_type = chunk_type,
    operation = operation
  )

  session$sendCustomMessage("shinyChatMessage", list(
    id = id,
    handler = msg_type,
    obj = msg
  ))
}

#' @export
chat_append_stream <- function(id, stream, session = getDefaultReactiveDomain()) {
  chat_append_stream_impl(id, stream, session)
}

chat_append_stream_impl <- NULL
rlang::on_load(chat_append_stream_impl <- coro::async(function(id, stream, session = getDefaultReactiveDomain()) {
  chat_append_message(id, list(role = "assistant", content = ""), chunk = "start", session = session)
  tryCatch(
    {
      for (msg in stream) {
        if (promises::is.promising(msg)) {
          msg <- await(msg)
          if (coro::is_exhausted(msg)) {
            break
          }
        }
        chat_append_message(id, list(role = "assistant", content = msg), chunk = TRUE, operation = "append", session = session)
      }
      chat_append_message(id, list(role = "assistant", content = ""), chunk = "end", operation = "append", session = session)
    },
    error = function(err) {
      chat_append_message(id, list(role = "assistant", content = paste0("An error occurred: ", conditionMessage(err))), chunk = "end", operation = "append", session = session)
    }
  )
}))
