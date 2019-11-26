app_load_spinner <- function(csvFile) {
  message <- "Initializing R session. This process may take a while..."
  html <- ""
  html <- paste0(html, "<div style=\"position: absolute; left: 50%;\">")
  html <- paste0(html, "<div style=\"color: white; width:150px; margin-left:-75px;\">", message, "</div>")
  html <- paste0(html, "</div>")
  html <- paste0(html, "<div class=\"sk-fading-circle\">")
  html <- paste0(html, "<div class=\"sk-circle1 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle2 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle3 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle4 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle5 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle6 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle7 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle8 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle9 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle10 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle11 sk-circle\"></div>")
  html <- paste0(html, "<div class=\"sk-circle12 sk-circle\"></div>")
  html <- paste0(html, "</div>")
  return (html)
}