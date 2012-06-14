show_containers <- function(container) {
  pg_height <- 400
  
  vert_box <- gvbox(container=container)
  nb <- gnotebook(container=vert_box)

  ## horizontal box container
  ## need to force height here, else vbox does not grab any space
  vb <- gvbox(container=nb,  label="ggroup", expand=TRUE, height=pg_height)
  hbox <- ggroup(cont=vb)
  gbutton("horizontal", container=hbox, expand=1)
  gbutton("box", container=hbox, expand=2)
  gbutton("container", container=hbox, expand=3)
  gseparator(cont=vb)
  vbox <- gvbox(cont=vb, expand=TRUE)
  gbutton("vertical", container=vbox, expand=1)
  gbutton("box", container=vbox, expand=2)
  gbutton("container", container=vbox, expand=3)
  

  
  ## framed box container
  f <- gframe("Framed component", container=nb, label="gframe", height=pg_height)
  glabel("inside frame", container=f)

  ## expandable/collapsible box container
  g1 <- gvbox(container=nb, label="gexpandgroup")
  a1 <- gexpandgroup("click to expand", container=g1); visible(a1) <- FALSE
  glabel("See also gborderlayout", container=a1)
  a2 <- gexpandgroup("Expanded", container=g1); visible(a2) <- TRUE
  glabel("Some label in a2", container=a2)

  ## grid layout
  tbl <- glayout(container=nb, label="glayout", height=pg_height)
  tbl[1,1] <- glabel("label at 1,1", container=tbl)
  tbl[2,2] <- gbutton("button at 2,2", container=tbl)
  tbl[3,3] <- gedit("", initial="edit area at 3,3", container=tbl)
  visible(tbl) <- TRUE                  # needed here
  svalue(nb) <- 1

  ## paned container with sash to allocate space
  pg <- gpanedgroup(container=nb, label="gpanedgroup", height=pg_height)
  glabel("left", container=pg)
  glabel("right", container=pg)

  ## gformlayout, pass in label= as with gnotebook
  flyt <- gformlayout(container=nb, label="gformlayout")
  gedit("", initial="numeric", label="x", container=flyt)
  gcombobox(c("two.sided", "left", "right"),
            label="alternative", container=flyt)
  gedit("", initial="mu for null hypothesis",
        label="mu", container=flyt)
  gslider(from=100*.5, to=100*1.0,      # integers only!
          by=100*.01, value=100*0.95,
          coerce.with=function(x) x/100,# compensate
          tpl="{0}%",                   # in percents
          width=250,                    # needs width
          label="conf.level", container=flyt)

  ## gborderlayout and gnotebook don't nest nicely -- bug
  

  svalue(nb) <- 1                       # first page

  ## nested group to keep button from stretching orthogonal to packing
  bg <- ggroup(container=vert_box); addSpring(bg);
  gbutton("source", container=bg, handler=function(...) {
    f <- "containers.R"
    subwindow <- gwindow("Source", parent=w,
                         width=600, height=400)
    vb <- gvbox(container=subwindow)
    gcodemirror(paste(readLines(f), collapse="\n"),
                container=vb)
    gseparator(container=vb)
          bg <- ggroup(container=vb)
    addSpring(bg)
    gbutton("dismiss", container=bg, handler=function(h,...) {
      dispose(subwindow)
    })
    
  })
  
}
 
  
