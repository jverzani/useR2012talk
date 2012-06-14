## Presentation for gWidgetsWWW2
## source's in several files along the way
##

scale <-1 #0.75
page_height <- as.integer(600*scale)

fig_height <- as.integer(500*scale)
fig_width <- as.integer(750*scale)

subw_height <- as.integer(500*scale)
subw_width <- as.integer(700*scale)

def_sidewidth <- as.integer(300*scale)


require(markdown)

## insert this code for making the figure
source("figure.R")

## map pages
## use structure of pages list below
map_page <- function(i, lst) {
  b <- gbutton(lst$name, container=button_gp, handler=function(h,...) {
    set_page(h$action)
  }, action=i)


  
  content <- lst$content

  if(is.character(content)) {
    ## markup code
    if(lst$type == "markdown")
      ghtml(markdownToHTML(text=content), container=sw, expand=TRUE)
    else if(lst$type == "pre")
      ghtml(sprintf("<pre>%s</pre>", content), container=sw, expand=TRUE)
    else if(lst$type == "code")
      ghtml(sprintf("<code><pre>%s</pre></code>", content), container=sw, expand=TRUE)
    else if(lst$type == "html")
      ghtml(content, container=sw, expand=TRUE)
    
  } else if(is.function(content)) {
    content(sw)
  }
  the_page <<- i
  b
}

## Basic layout has button prev, next controls
w <- gwindow("useR!2012 Presentation on gWidgetsWWW2")
sb <- gstatusbar("Powered by gWidgetsWWW2, Rook and FastRWeb", container=w)
glabel(" ", cont=sb, expand=TRUE)
dog_food <- gbutton("Source...", container=sb, handler=function(h,...) {
  f <- "pres.R"
  subwindow <- gwindow("Source", parent=w,
                       width=subw_width,
                       height=subw_height)
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


pg <- gpanedgroup(cont=w, default.size=160)

g <- ggroup(cont=pg, horizontal=FALSE)

bg <- ggroup(cont=g)
img_file <- get_tempfile(ext=".png")
file.copy("user-r-med.png", img_file)
ghtml(sprintf("<img src='%s' height=24></img>", get_tempfile_url(img_file)), cont=bg)

addSpring(bg)
page_title <- ghtml("<h3></h3>", container=bg)
addSpring(bg)


prev_action <- gaction("<", key.accel="LEFT", parent=w, handler=function(h,...) {
  ind <- max(1, the_page - 1)
  set_page(ind)
})
prev_btn <- gbutton(action=prev_action, container=bg)

next_action <- gaction(">", key.accel="RIGHT", parent=w, handler=function(h,...) {
  ind <- min(length(sw), the_page + 1)
  set_page(ind)
})
next_btn <- gbutton(action=next_action, container=bg)
## helper to set the page and update title
set_page <- function(i) {
  svalue(sw) <- i

  ## bold, unbold
  new <- buttons[[i]]; old <- buttons[[the_page]]
  svalue(new) <- sprintf("<b>%s</b>", svalue(new))
  svalue(old) <- gsub("<[^>]*>","", svalue(old))

  
  the_page <<- i
  txt <- pages[[i]]$title
  if(is.null(txt))
    txt <- pages[[i]]$name
  svalue(page_title) <- sprintf("<h3>%s</h3>", txt)
}



## Main container
sw <- gstackwidget(cont=g, expand=TRUE, fill=TRUE)

## gropu to hold buttons
button_gp <- gvbox(cont=pg, use.scrollwindow=TRUE)


## Some helper functions
           
some_fig <- function(container, .FUN) {
  g <- ggroup(cont=container, height=fig_height)
  cnv <- gsvg(container=g, width=fig_width, height=fig_height) # JV Modify to suit
  f <- get_tempfile(ext=".svg")
  svg(filename=f,
      width=as.integer(fig_width/72),
      height=as.integer(fig_height/72)
      )
  .FUN()
  dev.off()
  svalue(cnv) <- f
}


##################################################
## make the pages

## globals
the_page <- 0
pages <- list()
add_page <- function(l) pages[[length(pages) + 1]] <<- l

### An intro page

add_page(list(name="Intro",
              title="What is gWidgetsWWW2?",
              content=function(container) {
                intro <- "

Creating interactive web pages within R: gWidgetsWWW2
-----------------------------------------------------------------------------

*John Verzani*

CUNY/College of Staten Island

<br/>

```

The gWidgets package implements an easy-to-learn interface for programming
graphical user interfaces within R. It aims to support tcltk, RGtk2 and qtbase.

The gWidgetsWWW2 package implements most of the gWidgets interface,
allowing users to quickly make interactive web pages within R, using
web technologies, in lieu of a graphical toolkit.


```
"
  
about_gwidgets <- "

<br/>

- - `gWidgets`: package defining API, implemented in `gWidgetstcltk`, `gWidgetsRGtk2`, `gWidgetsQt`

<br/>

- - `gWidgetsWWW`: in a standalone manner, implements most of `gWidgets`

<br/>

- - `gWidgetsWWW2`: rewrite of `gWidgetsWWW` using reference classes, `Rook`, `FastRWeb`, `devtools, `github`, ...

<br/>

- - `gWidgets2`: rewrite of `gWidgets` using reference classes, implemented in `gWidgets2tcltk`, `gWidgets2RGtk2`, `gWidgets2Qt`
"

                
                vb <- gvbox(container=container)
                ghtml(markdownToHTML(text=intro), container=vb)
                bg <- ggroup(cont=vb)
                addSpring(bg)
                gbutton("About gWidgets", cont=bg, handler=function(h,...) {
                  w1 <- gwindow("About gWidgets", parent=w,
                                width=subw_width, height=subw_height)
                  g <- gvbox(container=w1, height=subw_height-50)
                  ghtml(markdownToHTML(text=about_gwidgets), container=g)
                  bg <- ggroup(cont=g)
                  gseparator(cont=g)
                  addSpring(bg)
                  gbutton("dismiss", container=bg, handler=function(h,...) {
                    dispose(w1)
                  })
                })
                addSpring(bg)


              }))



##################################################
### The web stack and implementations
add_page(list(name="Web technologies",
              type="markdown",
              content="

Creating a simple web page requires nothing more than

<br/>

```
- basic knowledge of HTML, or some way to generate HTML (e.g., markdown)
- a means to display that page (some web server)
- The new RPubs service (rpubs.com) makes this drop-dead easy.
```

<br />

Web pages are enhanced with CSS (MathJAX in RPubs) and in many cases
simple JavaScript-based eye-candy (e.g, the popular `Bootstrap`
templates)

<br/>

Creating *interactive* web pages is more complicated:

<br/>

```
- can be done with CGI programming and HTML forms

- more common tasks are decoupled and handled separately:

* JavaScript is used to make a page dynamic,
* AJAX technologies are used to communicate with a server,
* JSON or XML to pass data back to a web page.
```

"
              ))


add_page(list(name="Web stack",
              title="Web technologies",
              content=function(container) some_fig(container, .FUN=draw_full))
         )


add_page(list(name="Web technologies for R",
              type="markdown",
              content="
R users have a wide variety of packages to choose from for web development and interaction:

<br/>

**Authoring:**


      R2HTML, hwriter, xtable, *markdown*, *bespoke*, concerto, googleVis, ...

**Templating:**

      brew, whisker, ...

**Data:**

      rjson, RJSONIO, XML, RSQLite, DBI, ...


**HTTP:**

      httr, RCurl, ROAuth, ...

**Server:**

      *Rook*, rApache, RServe, *FastRWeb*, *OpenCPU*, *R + node.js*,  ...

**Services:**


       twitteR, imguR, RGoogleMaps, *ggmap*, OpenStreetMap, *FlickR*, ...


"
              ))



add_page(list(name="RStudio",
              title="RStudio, www.rstudio.org",
              content=function(container) {
                f <- get_tempfile(ext=".png")
                file.copy("rstudio.png", f)
                gimage(f, container=container)
              }))

add_page(list(name="RStudio (stack)",
              title="RStudio's use of web technologies",
              content=function(container) some_fig(container, .FUN=draw_rstudio))
         )

add_page(list(name="Live-r",
              title="Live-R, http://live-analytics.com/",
              content=function(container) {
                f <- get_tempfile(ext=".png")
                file.copy("live-r.png", f)
                gimage(f, container=container)
              }))


add_page(list(name="Live-r (stack)",
              title="Live-R stack",
              content=function(container) some_fig(container, .FUN=draw_live))
         )



add_page(list(name="opencpu",
              title="Opencpu.org",
              content=function(container) some_fig(container, .FUN=draw_opencpu))
         )


add_page(list(name="Why gWidgetsWWW2?",
              title="Why gWidgetsWWW2?",
              type="markdown",
              content=function(container) {
                txt <- "
To be clear -- creating interactive web pages is best left to the industry standards.


<br />

The OpenCPU project is a good example. A typical set of tools might be to use:

* * Bootstrap for CSS
* * jquery for interactivity
* * jqueryUI for on-screen widgets
* * &#36;.ajax for AJAX calls back into opencpu
* * opencpu to integrate in R

<br />

```
     The `gWidgetsWWW2` package isn't going to make a faster,
     or more stable, or more scalable interface than the above.
```

However, there are some reasons using the package might be of interest:

* * used locally, it provides an easy-to-install, easy-to-program-for GUI interface from R. (Compare to tcltk.)
* * like `gWidgets`, programming simple to moderate sized GUIs is
    fairly simple
* * within limitations, web pages can be served remotely for wider distribution



"
                ghtml(markdownToHTML(text=txt), container=container)
              }
                ))


add_page(list(name="Local",
              title="Using gWidgetsWWW2 locally",
              content=function(container) some_fig(container, .FUN=draw_gWidgets_local))
         )

add_page(list(name="Remote",
              title="Remotely serving pages with gWidgetsWWW2 and FastRWeb",              
              content=function(container) some_fig(container, .FUN=draw_gWidgets_remote))
         )

##################################################


add_page(list(name="Hello World!",
              title="A simple 'Hello World!' example",
               content=function(container) {
                 f <- "hello_world.R"
                 source(f, local=TRUE)
                 pg <- gpanedgroup(cont=container,
                                   default.size=def_sidewidth,
                                   collapsible=TRUE)

                 lg <- gframe("Source", horizontal=FALSE,
                              container=pg, use.scrollwindow=TRUE)
                 txt <- sprintf("<code><pre>%s</pre></code>",
                                paste(readLines(f), collapse="\n"))
                 ghtml(txt, container=lg)
                 
                 container <- gvbox(container=pg)
                 hello_world(container)

                ## manually adjust sizing

               }
          ))


## container
source("containers.R", local=TRUE)
add_page(list(name="Containers",
              title="Basic containers available in gWidgetsWWW2",
              content=show_containers
              ))

## controls
source("controls.R", local=TRUE)
add_page(list(name="Controls",
              title="Basic controls available in gWidgetsWWW2",
              content=function(container) {
                pg <- gpanedgroup(cont=container,
                                  height=page_height,
                                  collapsible=TRUE,
                                  default.size=def_sidewidth)

                lg <- gframe("Source", horizontal=FALSE,
                             container=pg, use.scrollwindow=TRUE)
                txt <- sprintf("<code><pre>%s</pre></code>",
                               paste(readLines("controls.R"), collapse="\n"))
                ghtml(txt, container=lg)

                container <- gvbox(container=pg)
                gw_controls(container)

                ## manually adjust sizing
                
              }))



## Tables
add_page(list(name="Tables",
              title="Tables",
              content=function(container) {
                f <- system.file("apps", "gw_browse.R", package="gWidgetsWWW2")

                vb <- gvbox(container=container)

                ghtml("
<p>
Tables in <em>gWidgetsWWW2</em> are easily displayed using <code>gtable</code>
and <code>gdf</code>.
</p>
<br />

<p>
The package comes with a simple workspace browser, which utilizes <code>gtable</code> to display a listing of current items.
</p>
</br>

<p>
The basic usage involves a call like:
</p>
</br>

<pre>
<code>
gtable(list_objects())
</code>
</pre>

where <code>list_objects()</code> returns a data frame of the the current workspace items and a description of each.
",
                      container=vb)

                gseparator(container=vb)
                bg <- ggroup(cont=vb); addSpring(bg)
                gbutton("load_app", container=bg, handler=function(...)
                        load_app(f)
                        )
                ghtml("<a href='http://23.21.60.54/custom/gw_browse' target='_blank'>load remote</a>", container=bg)
                gbutton("source", container=bg, handler=function(...) {
                  subwindow <- gwindow("Source", parent=w,
                                       width=subw_width, height=subw_height)
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
              ))

## Graphics
source("canvas.R", local=TRUE)

add_page(list(name="Graphics",
              title="Inserting graphics",
              content=function(container) {
                g <- gvbox(container=container)
                bg <- ggroup(cont=g); addSpring(bg)
                gbutton("Source", container=bg, handler=function(...) {
                  f <- "canvas.R"
                   subwindow <- gwindow("Source", parent=w,
                                       width=subw_width, height=subw_height)
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
                graphics_example(g)

              }
              ))

## Manipulate example
source("manipulate.R", local=TRUE)
add_page(list(name="Manipulate",
              title="A simple port of the manipulate package",
              content=function(container) {
                g <- gvbox(container=container)
                bg <- ggroup(cont=g)
                addSpring(bg)
                gbutton("View source", cont=bg, handler=function(...) {
                  f <- "manipulate.R"
                  subwindow <- gwindow("Manipulate source", parent=w,
                                       width=subw_width,
                                       height=subw_height)
                  vb <- gvbox(container=subwindow)
                  gcodemirror(paste(readLines(f), collapse="\n"),
                              container=vb, height=subw_height-50)
                  gseparator(container=vb)
                  bg <- ggroup(container=vb)
                  addSpring(bg)
                  gbutton("dismiss", container=bg, handler=function(h,...) {
                    dispose(subwindow)
                  })
                })

                manipulate(## expression
                           plot(cars, xlim = c(x.min, x.max), type = type, 
                                axes = axes, ann = label),
                           ## controls
                           x.min = slider(0,15),
                           x.max = slider(15,30, initial = 25),
                           type=picker("p", "l", "b", "c", "o", "h", "s"),
#                           type = picker(points="p", lines="l", both="b", "lines only"="c","overplotted"= "o", "hist like"="h", "steps"="s"),
                           axes = checkbox(TRUE, "Draw axes"),
                           label = checkbox(FALSE, "Draw labels"),
                           
                             container=g
                           )
              }
           ))




## Methods
source("methods.R", local=TRUE)
add_page(list(name="Methods",
              title="gWidgetsWWW2 methods",
              content=function(container) {
                f <- "methods.R"
                vb <- gvbox(container=container)
                glabel("A few basic methods: svalue, [<-, enabled, focus, ...", container=vb)
                pg <- gpanedgroup(cont=vb, default.size=def_sidewidth,
                                  collapsible=TRUE)
                
                lg <- gvbox(container=pg, spacing=0, use.scrollwindow=TRUE)
                txt <- sprintf("<code><pre>%s</pre></code>",
                               paste(readLines(f), collapse="\n"))
                ghtml(txt, container=lg)

                container <- gvbox(container=pg)
                gw_methods(container)

              }
              ))


## Interactivity
add_page(list(name="Interactivity",
              title="Adding event handlers",
              content=function(container) {
                pg <- gpanedgroup(cont=container, height=fig_height,
                                  default.size=def_sidewidth, collapsible=TRUE)
                vb <- gvbox(container=pg, height=fig_height)
                tmp <- get_tempfile(ext=".svg")
                svg(tmp)
                make_interactivity_plot()
                dev.off()
                cnv <- gsvg(tmp, container=vb, width=fig_width, height=fig_height)
                vb <- gvbox(container=pg)
                txt <- "
<p>
Interactivity is added to a widget through the
<code>addHandlerXXX</code> methods. The <code>XXX</code> refers to the
event that signals the callback, e.g.: <code>addHandlerClicked</code> and
<code>addHandlerChanged</code>.
</p>
<br/>
<p>
The callback is processed by <code>R</code>,
meaning each takes a trip back to the server.
</p>
<br/><br/>
<p>
The figure shows various 'hits' on the server to make a page. Each
requires a (potentially costly) lookup in the evironment associated to the page.
</p>
"
                ghtml(txt, container=vb)
              }
              ))



## Deploying an app
add_page(list(name="Deploying",
              title="Deploying a gWidgetsWWW2 app",
              content=function(container) {
                vb <- gvbox(container=container, height=fig_height, use.scrollwindow=TRUE)
                txt <- "
Deploying an app
------------------------

Installation (currently) is done with <code>devtools</code>:

```{r}
> devtools:::install_github('gWidgetsWWW2', 'jverzani')
```

<br/>

First one must save the `gWidgetsWWW2` commands into a script file, say `web_script.R`.

Then serving the script depends on whether the page is served locally or through the internet.

<br/>
"
                ghtml(markdownToHTML(text=txt), container=vb)
                local_pg <- gexpandgroup("Serving the script locally", container=vb)
                visible(local_pg) <- FALSE
                txt <- "
<br/>

```
## Call `load_app` with the script name:
> require(gWidgetsWWW2)
> load_app('web_script.R')
## this opens the url http://127.0.0.1:9000/custom/web_script
```

<br/>
"
                ghtml(markdownToHTML(text=txt), container=local_pg)

                internet_pg <- gexpandgroup("Serving the script remotely",
                                            container=vb)
                visible(internet_pg) <- FALSE
                txt <- "
There are two ways: reverse proxying the `Rook` service or connect R to a web server. The former is faster the latter *in theory* should scale better.

<br />

**Using a reverse proxy**

<br/>

Though one can have Rook listen to external IPs, it is likely best to just use a web server to reverse proxy, say `apache` or `nginx`. For `apache` one can add something like the following to the configuration files:

```
ProxyPass  /custom http://127.0.0.1:9000/custom
ProxyPassReverse /custom http://127.0.0.1:9000/custom
```

<br />

**Using FastRWeb**

<br/>

One can also use `FastRWeb` to connect an R process to a web server. There are installation instructions. Basically the process then is:

```
* place script into the `/var/FastRWeb/gw_app` directory

* url is based on the script name: `http://ip.address/cgi-bin/R/app?app=web_script`
```

This method only works well with the simplest of scripts.

"
              ghtml(markdownToHTML(text=txt), container=internet_pg)
              }
))

add_page(list(name="Issues",
              title="Issues with the package",
              type="markdown",
              contents="
Issues with the package:

<br/>

* - with remote use and FastRWeb, storing of the entire enviroment is
    *slow* and space inefficient. This really needs to be cleared up
* - before the package is useful that way.  Browser variances are
* - *still* noticeable (HTML5 needed) debugging is difficult:

<br />

```
- can `source` code in from console to identify syntax errors
- can use *JavaScript* console to check on script errors (when a handler runs)
- evaluation within environment can be funny (packages restored, but environments such as reference classes are tricky
```                                                

"
              ))

add_page(list(name="A 'real' example",
              title="Collaborating on a google map",
              contents=function(container) {
                vb <- gvbox(container=container)
                ghtml("
A few of us are trying to create a KML file (for Google Earth) showing
trails, hikes and points of interest in a local 2000-acre 'reservation'.
<br/>
Each of us gets his or her GPS coordinates in various ways, and no one
has software to piece this altogether. A simple-minded idea is
<br/>
<p> a)
creata a simple website for entering in the data and
<p> b) letting R
write out a KML file.

<br />
Though the usefulness is still to be proven, here
is a somewhat functional demo.
<br/>
<a href=http://23.21.60.54/custom/trails target='_blank'>http://23.21.60.54/custom/trails</a>
<br />
",
                      container=vb)
                bg <- ggroup(cont=vb)
                gbutton("local", container=bg, handler=function(h,...) {
                  load_app("trails.R")
                })

              }))


##################################################
## Let 'er rip
buttons <- mapply(map_page, seq_along(pages), pages)
set_page(1)

