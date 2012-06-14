gw_controls <- function(container) {
  vb <- gvbox(container=container)
  
  glabel("label", container=vb)
  ghtml("<b>html widget</b>", container=vb) ## *bold*
  gbutton("button", container=vb)
  gcheckbox("checkbox", checked=TRUE, container=vb)
  gradio(c("gradio:","select", "only", "one"),
         horizontal=TRUE, container=vb)
  ## to adjust spread of radio buttons:
  ##  bg <- ggroup(cont=vb)
  ##  gradio(items=c("do not", "expand"), horizontal=TRUE,
  ##         width= 2 * (20 + 8 * 6),   # set width
  ##         container=bg)
  gcheckboxgroup(c("select", "a", "choice"),
                 checked=c(TRUE, FALSE, TRUE),
                 horizontal=TRUE, container=vb)
  ##
  gslider(from=0, to=10, by=1, container=vb)
  gspinbutton(from=0, to=100, by=1, value=50, container=vb)
  ##
  gcombobox(c("combobox", "select", "one", "can", "edit", "too"), editable=TRUE,
            container=vb)
  gedit("", initial.msg="gedit", container=vb)
}
  
