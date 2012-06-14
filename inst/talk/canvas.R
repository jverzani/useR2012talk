graphics_example <- function(container) {
  g <- gvbox(cont=container, horizontal=FALSE)

  ghtml("
There are various ways to include graphics within a script:
<UL>
<li> - <code>gsvg</code> can display SVG files produced by the svg driver</li>
<li> - <code>gimage</code> can display graphics written by the jpeg driver, say</li>
<li> - this example uses <code>gcanvas</code> to display graphics written by the canvas driver (from the <code>canvas</canvas> package). This driver writes JavaScript to manipulate an HTML5 component on the page. The <code>gcanvas</code> object can listen for mouse clicks, as this example shows
</UL>

",
        container=g)

  ## globals
  x <- runif(3)
  y <- runif(3)
  width <- 400; height <- 300

  f <- tempfile()
  cnv <- gcanvas(f, width=width, height=height, cont=g)
  canvas(f, width=width, height=height)                  

  g1 <- ggroup(cont=g)
  tpl <- "Plotting %s points"
  lab <- glabel("", cont=g1)

  g2 <- ggroup(cont=g)                    # stop button stretch
  gbutton("reset", cont=g2, handler=function(h,...) {
    x <<- runif(3)
    y <<- runif(3)
    make_plot()
  })

  ## script globals
  setup_plot <- function() {
    xlim <- ylim <- c(0,1)
    plot.new()
    plot.window(xlim=xlim, ylim=ylim)
    axis(1); axis(2)
  }

  make_plot <- function() {
    canvas(f, width=width, height=height)
    setup_plot()
    points(x,y, pch=16, cex=2)
    abline(lm(y ~ x))
    dev.off()


    svalue(cnv) <- f
    svalue(lab) <- sprintf(tpl, length(x))
  }

  make_plot()

  addHandlerClicked(cnv, handler=function(h,...) {
    ## redo plot to get grconvertXY to work...
    canvas(f, width=width, height=height)
    setup_plot()
    new.x <- grconvertX(h$x, from="ndc", to="user")
    new.y <- grconvertX(h$y, from="ndc", to="user")
    dev.off()
    
    if(min(new.x, new.y) < 0 || max(new.x, new.y) > 1)
      return()                            # too big for [0,1]
    x <<- c(x, new.x); y <<- c(y, new.y)
    make_plot()
  })
  
}
