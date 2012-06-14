gw_methods <- function(container) {
  ## our data
  require(MASS)
  mans <- c("Choose a manufacuturer", levels(Cars93$Manufacturer))
  get_makes <- function(val)
    with(Cars93, as.character(Model[Manufacturer == val]))

  ## layout
  g <- gvbox(container=container)        
  flyt <- gformlayout(cont=g)
  cb1 <- gcombobox(mans, label="Manufacturer",cont=flyt)
  cb2 <- gcombobox("", label="Make", expand=TRUE, cont=flyt)
  
  ## methods
  enabled(cb2) <- FALSE                   # not sensitive
  addHandlerChanged(cb1, handler=function(h,...) {
    ind <- svalue(h$obj, index=TRUE)      # index
    if(ind != 1) {
      val <- svalue(h$obj)                # value
      makes <- get_makes(val)
      enabled(cb2) <- TRUE                # make sensitive
      cb2[] <- c("Select a make", makes)  # set items
      svalue(cb2, index=TRUE) <- 1        # set by index
      focus(cb2) <- TRUE                  # keyboard focus
    } else {
      cb2[] <- ""                         # clear out items
      enabled(cb2) <- FALSE               # not sensitive
    }
  })

  addHandlerChanged(cb2, handler=function(h,...) {
    ind <- svalue(h$obj, index=TRUE)
    if(ind != 1) {
      gmessage(sprintf("Do something with: %s and %s.",
                     svalue(cb1),  svalue(cb2)), parent=container)
    }
  })
}
