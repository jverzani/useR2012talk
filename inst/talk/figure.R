## code tomake a figure

setup_plot <- function() {
#  op <- par(no.readonly=TRUE)
#  on.exit(par(op))

  par(mai=c(0,0,1,0))
  plot.new()
  plot.window(xlim=c(0,10), ylim=c(0,6))
}


delta <- 0.15
light <- "gray85"
small <- 1.25 # misnamed

draw_webpage <- function(txt="Web Page",  col="black") {
  rect(1, 0, 3, 6, col="gray98")
  text(2,3, srt=90, labels=txt, col=col, cex=3)
}

draw_ajax <- function(txt="AJAX", sub="", col="black") {
  y <- 1
  arrows(3 + delta, y, 5-delta, y, col=col)
  text(4, y + delta, labels=txt, col=col)
  if(nchar(sub))
    text(4, y - delta, labels=sub, col=col, cex=small)
}

draw_JSON <- function(txt="JSON/XML", sub="", col="black") {
  y <- 2
  arrows(5 - delta, y, 3 + delta, y, col=col)
  text(4, y + delta, labels=txt, col=col)
  if(nchar(sub))
    text(4, y - delta, labels=sub, col=col, cex=small)
}

draw_JS <- function(txt="JavaScript", sub="", col="black") {
  y <- 3
  arrows(5 - delta, y, 3 + delta, y, col=col)
  text(4, y + delta, labels=txt, col=col)
  if(nchar(sub))
    text(4, y - delta, labels=sub, col=col, cex=small)

}

draw_css <- function(txt="CSS", sub="", col="black") {
  y <- 4
  arrows(5 - delta, y, 3 + delta, y, col=col)
  text(4, y + delta, labels=txt, col=col)
  if(nchar(sub))
    text(4, y - delta, labels=sub, col=col, cex=small)

}

draw_html <- function(txt="HTML", sub="", col="black") {
  y <- 5
  arrows(5 - delta, y, 3 + delta, y, col=col)
  text(4, y + delta, labels=txt, col=col)
  if(nchar(sub))
    text(4, y - delta, labels=sub, col=col, cex=small)
}

draw_server <- function(txt="Web server", col="black") {
  rect(5, 0, 7, 6, col="gray98")
  text(6, 3, srt=90, labels=txt, col=col, cex=3)
}

draw_R <- function(txt="R", text.in="", text.out="", col="black") {
  rect(8, 2, 10, 4, col="gray98")
  text(9, 3, labels=txt, col=col, cex=4)
  arrows(7 + delta, 3+delta, 8-delta, 3 + delta, col=col)
  arrows(8 - delta, 3-delta, 7+delta, 3 - delta, col=col)
  if(nchar(text.in)) {
    text(7.5, 4, srt=90, offset=0, labels=text.in)
  }
  if(nchar(text.out)) {
    text(7.5, 2, srt=270, offset=0, labels=text.out)
  }
  
}

draw_full <- function() {
  setup_plot()
  draw_webpage()
  draw_ajax()
  draw_JSON()
  draw_JS()
  draw_css()
  draw_html()
  draw_server()
  draw_R()
}

draw_rstudio <- function() {
  setup_plot()
  draw_webpage()
  draw_ajax(sub="Java/GWT")
  draw_JSON(sub="Java/GWT")
  draw_JS(sub="Java/GWT")
  draw_css()
  draw_html(col="gray90")
  draw_server()
  draw_R(text.in="C++", text.out="C++")
  title("RStudio")
}


draw_opencpu <- function() {
  setup_plot()
  draw_webpage()
  draw_ajax(sub="REST API")
  draw_JSON(sub="opencpu")
  draw_JS(col=light)
  draw_css(col=light)
  draw_html(col=light)
  draw_server("Apache")
  draw_R(text.in="python?")
  title("opencpu")
}



draw_live <- function() {
  setup_plot()
  draw_webpage()
  draw_ajax(sub="ExtJS")
  draw_JSON()
  draw_JS(sub="ExtJS")
  draw_css()
  draw_html(col=light)
  draw_server()
  draw_R()
  title("Live-R")
}


draw_gWidgets_local <- function() {
  setup_plot()
  draw_webpage()
  draw_ajax(sub="ExtJS")
  draw_JSON(sub="RJSONIO")
  draw_JS(sub="ExtJS")
  draw_css(sub="ExtJS")
  draw_html(col=light)
  draw_server(txt="Rook/Rhttpd")
  draw_R(text.out="gWidgetsWWW2")
  title("gWidgetsWWW2: local")

}


draw_gWidgets_remote <- function() {
  setup_plot()
  draw_webpage()
  draw_ajax(sub="ExtJS")
  draw_JSON(sub="RJSONIO")
  draw_JS(sub="ExtJS")
  draw_css(sub="ExtJS")
  draw_html(col=light)
  draw_server(txt="apache")
  draw_R(text.in="RServe/FastRWeb", text.out="gWidgetsWWW2")
  title("gWidgetsWWW2: Remote")

}



## Interactivity plot
setup_interactivity_plot <- function() {
#  op <- par(no.readonly=TRUE)
#  on.exit(par(op))

  par(mai=c(0,0,.5,0))
  plot.new()
  plot.window(xlim=c(1,7), ylim=c(0,6))
}


make_interactivity_plot <- function() {
  setup_interactivity_plot()
  draw_webpage("gWidgetsWWW2 app")
  draw_server()

  draw_line <- function(txt="", sub="", y, col="black", small=1.0, both=TRUE) {
    arrows(3 + delta, y, 5 - delta, y, col=col)
    if(both)
      arrows(5 - delta, y, 3 + delta, y, col=col)
    text(4, y + delta, labels=txt, col=col)
    if(nchar(sub))
      text(4, y - delta, labels=sub, col=col, cex=small)
  }
  
  draw_line("newSessionId", "JSON", y=5)
  draw_line("createGUI", "JavaScript", y=4)
  draw_line("runTransport", "", y=3, both=FALSE)
  draw_line("runHandler", "JavaScript", y=2)
  draw_line("runProxy", "JSON", y=1)
}
