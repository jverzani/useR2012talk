hello_world <- function(container) {

  ## A parent container. Typically a gwindow instance.
  box_container <- ggroup(container=container, horizontal=FALSE)

  ## two controls: a label and button
  glabel('Hello world example', container=box_container)
  button <- gbutton('Click me for a message', container=box_container)

  ## add interactivity
  addHandlerClicked(button, handler=function(h, ...) {
    gmessage('Hello world!', parent=box_container)
  })
}
