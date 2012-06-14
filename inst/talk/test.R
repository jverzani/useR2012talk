w <- gwindow("test")
g <- gvbox(container=w)
gbutton("add another", container=g)


flyt <- gformlayout(container=g)
gradio(1:3, horizontal=TRUE, label="radio", container=flyt)
gcheckboxgroup(letters[1:3], horizontal=TRUE, label="cbg", container=flyt)
gedit("", initial="some default", label="edit", container=flyt)
gcombobox(state.name, label="combo", container=flyt)
gcheckbox("some label", label="nbsp;", container=flyt)
