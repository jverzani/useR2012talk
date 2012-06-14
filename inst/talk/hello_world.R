hello_world <- function(container) {

  ## Containers:
  ## container <- gwindow('Some title')
  box_container <- ggroup(container=container, horizontal=FALSE)

  ## Controls: a checkbox and a button
  cb <- gcheckbox('Show message', checked=TRUE, container=box_container)
  button <- gbutton('Click me for a message', container=box_container)

  ## Interactivity:
  addHandlerClicked(button, handler=function(h, ...) {
    if(svalue(cb))                                   ## method call
      gmessage('Hello world!', parent=box_container) ## a dialog
  })
}
