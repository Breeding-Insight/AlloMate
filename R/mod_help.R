#' Help page module
#'
#' @param id Module id
#'
#' @noRd
mod_help_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          style = "padding: 20px; background-color: #f8f9fa; border-radius: 8px; margin: 10px 0; max-height: 80vh; overflow-y: auto; position: relative;",
          shiny::div(
            style = "background-color: white; padding: 25px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            shiny::div(
              style = "text-align: center; margin-bottom: 25px; padding-bottom: 15px; border-bottom: 2px solid #007bff;",
              shiny::tags$h2("AlloMate Documentation", style = "color: #007bff; margin-bottom: 10px;"),
              shiny::tags$p("Complete user guide and technical documentation", style = "color: #666; font-size: 16px;")
            ),
            shiny::div(
              style = "line-height: 1.6; font-size: 14px;",
              shiny::htmlOutput(ns("help_content"))
            ),
            shiny::div(
              style = "text-align: center; margin-top: 30px; padding-top: 20px; border-top: 1px solid #dee2e6;",
              shiny::actionButton(
                ns("back_to_top"),
                "Back to Top",
                style = "background-color: #6c757d; color: white; border: none; padding: 8px 16px; border-radius: 5px;"
              )
            )
          )
        )
      )
    )
  )
}

#' @param id Module id
#' @param parent_session Parent (app) session (unused for now)
#' @noRd
mod_help_server <- function(id, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    markdown_to_html <- function(markdown_text) {
      lines <- strsplit(markdown_text, "\n")[[1]]
      html_lines <- character(length(lines))
      in_code_block <- FALSE
      in_list <- FALSE
      list_type <- ""

      for (i in seq_along(lines)) {
        line <- lines[i]

        if (grepl("^```", line)) {
          if (!in_code_block) {
            html_lines[i] <- "<pre><code class='language-markdown'>"
            in_code_block <- TRUE
          } else {
            html_lines[i] <- "</code></pre>"
            in_code_block <- FALSE
          }
          next
        }

        if (in_code_block) {
          html_lines[i] <- paste0("<span>", line, "</span>")
          next
        }

        if (grepl("^#+\\s", line)) {
          level <- nchar(gsub("^(#+).*", "\\1", line))
          content <- gsub("^#+\\s+", "", line)

          anchor <- tolower(gsub("[^a-zA-Z0-9\\s]", "", content))
          anchor <- gsub("\\s+", "-", anchor)

          html_lines[i] <- paste0("<h", level, " id='", anchor, "'>", content, "</h", level, ">")
          next
        }

        line <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", line)
        line <- gsub("\\*(.+?)\\*", "<em>\\1</em>", line)
        line <- gsub("`(.+?)`", "<code>\\1</code>", line)
        line <- gsub("\\[(.+?)\\]\\((.+?)\\)", "<a href='\\2' target='_blank'>\\1</a>", line)

        if (grepl("^[*-] ", line)) {
          content <- gsub("^[*-] ", "", line)
          if (!in_list) {
            html_lines[i] <- paste0("<ul><li>", content, "</li>")
            in_list <- TRUE
            list_type <- "ul"
          } else {
            html_lines[i] <- paste0("<li>", content, "</li>")
          }
          next
        }

        if (grepl("^\\d+\\. ", line)) {
          content <- gsub("^\\d+\\. ", "", line)
          if (!in_list) {
            html_lines[i] <- paste0("<ol><li>", content, "</li>")
            in_list <- TRUE
            list_type <- "ol"
          } else {
            html_lines[i] <- paste0("<li>", content, "</li>")
          }
          next
        }

        if (in_list && !grepl("^[*-] ", line) && !grepl("^\\d+\\. ", line) && nchar(trimws(line)) > 0) {
          html_lines[i - 1] <- paste0(html_lines[i - 1], paste0("</", list_type, ">"))
          in_list <- FALSE
          list_type <- ""
        }

        if (grepl("^---$", line)) {
          html_lines[i] <- "<hr>"
          next
        }

        if (nchar(trimws(line)) == 0) {
          html_lines[i] <- "</p><p>"
          next
        }

        html_lines[i] <- paste0(line, "<br>")
      }

      if (in_list && length(html_lines) > 0) {
        html_lines[length(html_lines)] <- paste0(html_lines[length(html_lines)], paste0("</", list_type, ">"))
      }

      html_content <- paste(html_lines, collapse = "\n")
      paste0("<p>", html_content, "</p>")
    }

    generate_toc <- function(markdown_text) {
      lines <- strsplit(markdown_text, "\n")[[1]]
      toc_items <- character()

      for (line in lines) {
        if (grepl("^#+\\s", line)) {
          level <- nchar(gsub("^(#+).*", "\\1", line))
          content <- gsub("^#+\\s+", "", line)

          anchor <- tolower(gsub("[^a-zA-Z0-9\\s]", "", content))
          anchor <- gsub("\\s+", "-", anchor)

          indent <- paste(rep("&nbsp;&nbsp;&nbsp;&nbsp;", max(level - 1, 0)), collapse = "")
          toc_items <- c(
            toc_items,
            paste0(
              "<div style='margin: 5px 0;'><a href='#",
              anchor,
              "' class='toc-link'>",
              indent,
              "• ",
              content,
              "</a></div>"
            )
          )
        }
      }

      if (length(toc_items) == 0) {
        return("<p>No headers found for table of contents.</p>")
      }

      paste(toc_items, collapse = "")
    }

    output$help_content <- shiny::renderUI({
      possible_paths <- c(
        "README.md",
        "../README.md",
        "../../README.md",
        app_sys("app/help/README.md")
      )

      readme_path <- NULL
      for (path in possible_paths) {
        if (file.exists(path)) {
          readme_path <- path
          break
        }
      }

      if (is.null(readme_path)) {
        return(shiny::HTML(
          "<h2>Help Documentation</h2><p>README.md file not found.</p>"
        ))
      }

      tryCatch({
        readme_text <- paste(readLines(readme_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        html_content <- markdown_to_html(readme_text)
        toc <- generate_toc(readme_text)

        full_content <- paste0(
          "<div class='toc-container'>",
          "<h3>Table of Contents</h3>",
          toc,
          "</div>",
          "<hr>",
          html_content
        )

        shiny::HTML(paste0("<div class='help-content'>", full_content, "</div>"))
      }, error = function(e) {
        shiny::HTML(paste0(
          "<h2>Help Documentation</h2>",
          "<p>Error reading README.md file: ", e$message, "</p>"
        ))
      })
    })

    shiny::observeEvent(input$back_to_top, {
      shinyjs::runjs("document.querySelector('.help-content')?.scrollTo({ top: 0, behavior: 'smooth' });")
    })
  })
}
