## manipulate port (not quite right though)

cnv_width <- 500
cnv_height <- 400


## simple maps. Used with `do.call` in `make_widget`.
picker <- function(...) 
  list(FUN="gcombobox", items=unlist(list(...)))

slider <- function(min, max, initial=min, step=NULL, ...)
  list(FUN="gslider",from=min, to=max, by=step, value=initial, width=200)

checkbox <- function(initial=FALSE, ...)
  list(FUN="gcheckbox", checked=initial)

button <- function(label)
  list(FUN="gbutton", text=label)

## main function
manipulate <- function(.expr, ..., container) {
  expr <- substitute(.expr)
  l <- list(...)
  tmp <- get_tempfile()
  
  pg <- gpanedgroup(container=container, default.size=300,
                    height=as.integer(.75*page_height))
  
  cnv <- gcanvas(tmp, container=pg, expand=TRUE,
                 width=cnv_width,
                 height=cnv_height
                 )
  g <- gvbox(container=pg)
  flyt <- gformlayout(container=g, align="right", label.width=50)

  update_expr <- function(...) {
    canvas(tmp,
           width=cnv_width,
           height=cnv_height
           )
    values <- svalue(flyt) 
    result <- withVisible(eval(expr, envir=values))
    dev.off()
    svalue(cnv) <- tmp
  }
  
  make_widget <- function(nm, lst) {
    ## common arguments for all widgets:
    lst$handler <- update_expr
    lst$label <- nm
    lst$container <- flyt
    do.call(lst$FUN, lst[-1])
  }

  widgets <- mapply(make_widget, names(l), l, SIMPLIFY=FALSE)
  addSpring(g)                          # nicer layout

  update_expr()                         # initial graphic
  invisible(widgets)
}
