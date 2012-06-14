##' a framework where we have
##' * a list storing our data with methods
##' - read from file
##' - write to file
##' - delete component
##' - add component
##' - edit component
##' - make_kml
##' * for each component we have
##' - an editor for editing current, and adding new of that type
##' - a view (map view)


## some place to store our widgets
e <- new.env()
## Our objects stored in an environment as well
o <- new.env()


##' DataList stuff

dbug <- function(msg, ...) {
  return()                              # quiet
  message(msg)
  print(list(...))
}


newList <- function(fname) {
  l <- list();
  class(l) <- c("DataList", class(l))

  attr(l, "filename") <- fname
  l
}

##' @examples
##' x <- newList("/tmp/file.R")
##' x <- from_file(x)
from_file <- function(x) {
  f <- attr(x, "filename")
  if(file.exists(f))
    dget(f)
  else
    x
}


to_file <- function(x) dput(x, attr(x, "filename"))

##' @return modified object
remove_component <- function(x, comp) {
  dbug("remove componet", comp=comp)
  
  x[comp$name] <- NULL
  x
}

##' add component with optional class
##'
##' @param comp component to add
##' @param klass what component class (used to dispatch as.knl, open.ineditor)
##' @return modified object
add_component <- function(x, comp,
                                  klass=paste(class(x)[1], "Component", sep="")) {
  if(!is.null(klass))
    class(comp) <- c(klass, class(comp))

  dbug("add componet", comp=comp)
  
  x[[comp$name]] <- comp
  x
}


##' @return NULL
edit_component <- function(x, comp) {
  open_in_editor(comp)
}


##################################################
## Component subclasses
## we have three Trails, Hikes, Points of Interest.

open_in_editor <- function(x) {
  ## dispatch on class
  ## S3 methods may not be found within call
  cls <- class(x)[1]
  if(cls == "TrailListComponent") {
    svalue(e$trails$name) <- x$name
    svalue(e$trails$color) <- x$color
    svalue(e$trails$txt) <- x$txt
  } else if(cls == "HikeListComponent") {
    svalue(e$hikes$name) <- x$name
    svalue(e$hikes$desciption) <- x$description
    svalue(e$hikes$txt) <- x$txt
  } else if(cls == "POIListComponent") {
    svalue(e$pois$name) <- x$name
    svalue(e$pois$point) <- x$point
    svalue(e$pois$description) <- x$description
  }
}

#####

as_kml <- function(x, fname) {
  cls <- class(x)[1]
  if(cls == "TrailListComponent") {
    ## A TrailListComponent has components
    ## name, color, txt
    tpl <-"
<Placemark>
      <name>{{name}}</name>
        <description>{{description}}</description>
          <LineStyle><color>88aa0000</color><width>8</width></LineStyle>
      <LineString>
        <extrude>0</extrude><altitudeMode>clampToGround</altitudeMode>
          <coordinates>
            {{txt}}
    </coordinates>
      </LineString>
        </Placemark>
"
    ## How to color the trail
    out <- whisker.render(tpl, x)
  } else if(cls == "HikeListComponent") {
    tpl <-"
  <Placemark>
      <name>{{name}}</name>
      <description>{{description}}</description>
      <LineString>
        <coordinates>
          {{txt}}
        </coordinates>
      </LineString>
  </Placemark>
"

  out <- whisker.render(tpl, x)
  } else if(cls == "POIListComponent") {
    tpl <- "
 <Placemark>
    <name>{{name}}</name>
    <description><h4>{{type}}</h4>{{description}}</description>
    <Point>
      <coordinates>{{lat}},{{lng}},0</coordinates>
    </Point>
  </Placemark>
"

  tmp <- strsplit(x$point, ",")[[1]]
  x$lat <- tmp[1]; x$lng <- tmp[2]

  
  out <- whisker.render(tpl, x)
  }

  ## process out
  if(!missing(fname))
    cat(out, file=fname, append=TRUE)
  else
    out
}



write_kml <- function(fname) {
  kml_head <- "<?xml version='1.0' encoding='UTF-8'?>
<kml xmlns='http://www.opengis.net/kml/2.2'>
  <Document>
    <name>Paths</name>
"
  kml_foot <- "
  </Document>
</kml>
"
  cat(kml_head, file=fname)

  for(nm in ls(o)) {
    obj <- o[[nm]]
    sapply(obj, as_kml, fname=fname)
  }

  cat(kml_foot, append=TRUE, file=fname)
}
    

#####
graphic_view <- function(x) UseMethod("graphic_view")
graphic_view <- function(x) {
  cls = class(x)[1]

  if(cls == "TrailListComponent") {
    f <- tempfile()
    txt <- gsub("\\s+", "\n", x$txt)
    cat(txt, file=f)
    tr <- read.csv(f, header=FALSE)
  tr <- tr[, c(2,1)]
    
    stroke <- list(color=x$color,
                   opacity=1,
                   weight=2)
    ggooglemaps_polyline(tr, stroke, gmap)
  } else if(cls == "HikeListComponent") {
    f <- tempfile()
    cat(x$txt, file=f)
    tr <- read.csv(f, header=FALSE)
    tr <- tr[, c(2,1)]
    
    stroke <- list(color=x$color,
                   opacity=1,
                 weight=2)
    ggooglemaps_polyline(tr, stroke, gmap)
  } else if(clas == "POIListComponent") {
    content <- "<h3>{{name}}</h3>{{description}}"
    position <- rev(strsplit(x$point, ",")[[1]][1:2])
    
    ggooglemaps_infowindow(position, whisker.render(content, x), gmap)
  }
}
  
  

##################################################
## some subclass
##' Main interface requires methods for
##' as_kml: convert component to kml
##' open_in_editor: map component to view for editing
##' graphic_view: show component in view for editing
##' These are defined prior
##' 
##' A trail list subclass
newTrailList <- function(fname) {
  l <- newList(fname)
  class(l) <- c("TrailList", class(l))
  l
}

##################################################
## hikes
newHikeList <- function(fname) {
  l <- newList(fname)
  class(l) <- c("HikeList", class(l))
  l
}

##################################################
## Points of interest
newPOIList <- function(fname) {
  l <- newList(fname)
  class(l) <- c("POIList", class(l))
  l
}



#################################################
## Data
kml_file <- "./map.kml"              # should be downloadable or on dropbox

## -74.29418563842773, 40.74465591168391
reservation <- c(40.74465591168391, -74.29418563842773)
zoom_level <- 14
maptype <- "satellite"
trail_colors <- c("#FF00FF", "#00FFFF", "#FFFF00")

e$trails <- list()
e$hikes <- list()
e$pois <- list()

o$trails <- newTrailList("./.trails.R")
o$trails <- from_file(o$trails)

o$hikes <- newHikeList("./.hikes.R")
o$hikes <- from_file(o$hikes)

o$pois <- newPOIList("./.pois.R")
o$pois <- from_file(o$pois)

get_items <- function() {
  l <- list()
  l$Trail <- unlist(sapply(o$trails, function(i) i$name))
  l$Hike <- unlist(sapply(o$hikes, function(i) i$name))
  l$POI <- unlist(sapply(o$pois, function(i) i$name))



  if(length(l)) {
    d <- stack(l)
    names(d) <- c("Name", "Type")
    d
  } else {
    data.frame(Name="none", Type="yet",
               stringsAsFactors=FALSE)
  }
}

##' return item or parent name of item
get_item <- function(ind, parent=FALSE) {
  items <- item_tbl[]
  lookup <- c("Trail"="trails", "Hike"="hikes", "POI"="pois")
  p_name <- as.character(items[ind,2])

  dbug("get_item", ind=ind, items=items, p_name = p_name, lookup=lookup[p_name], obj =  ls(o), ret=o[[lookup[p_name]]], out=as.character(items[ind,1]))

  if(parent)
    lookup[p_name]
  else
    o[[lookup[p_name]]][[as.character(items[ind,1])]]
}

update_gui <- function() {
  items <- get_items()
  item_tbl[] <- items
}




## Our GUI
w <- gwindow("Edit me")
border_gp <- gborderlayout(container=w,
                           title=list(
                             west="Created items",
                             center="South Mountain Reservation",
                             East="Edit an item"
                             ),
                           collapsible=list(west=TRUE))

left_gp <- gvbox(cont=border_gp, where="west")
notebook_gp <- gnotebook(cont=border_gp, where="east")
gmap <- ggooglemaps(center=reservation,
                   zoom=zoom_level,
                   maptype=maptype,
                   container=border_gp, where="center")


border_gp$set_panel_size("west", 250)
border_gp$set_panel_size("east", 200)

graphic_handler <- function(h,...) {
  ## call show graphic
  x_name <- h$action$x
  x <- o[[x_name]]
  widgets <- e[[x_name]]                   # e$trails, say
  l <- sapply(widgets, svalue, simplify=FALSE)              # values

  
  kls <- paste(class(x)[1], "Component", sep="")
  class(l) <- c(kls, class(l))

  graphic_view(l)
}
  
add_handler <- function(h,...) {
  ## call show graphic
  x_name <- h$action$x
  obj <- o[[x_name]]
  widgets <- e[[x_name]]
  l <- sapply(widgets, svalue, simplify=FALSE)              # values

  ## check if possible
  do_it <- function(...) {
    o[[x_name]] <- add_component(obj, l)
    update_gui()
  }
  if(length(Filter(function(y) y$name == l$name, obj))) {
    gconfirm(sprintf("overwrite object?", l$name), parent=w, handler=do_it)
  } else {
    do_it()
  }
}
  

##
## The left side
items <- get_items()
item_tbl <- gtable(items, container=left_gp)
bg <- ggroup(cont=left_gp)
kml_button <- gbutton("write KML", cont=bg, handler=function(...) {
  message("make KML")
  write_kml(kml_file)
  
  w1 <- gwindow("A KML file", parent=w)
  g <- gvbox(container=w1)
  glabel("Copy and paste or get from dropbox", cont=g)
  gtext(paste(readLines(kml_file), collapse="\n"), container=g)
})
addSpring(bg)
save_button <- gbutton("save", cont=bg, handler=function(h,...) {
  sapply(o, to_file)

})

rm_button <- gbutton("remove", cont=bg, handler=function(h,...) {
  message("get item, remove")
  ind <- svalue(item_tbl, index=TRUE)
  item <- get_item(ind)
  x_name <- get_item(ind, parent=TRUE)
  o[[x_name]] <- remove_component(o[[x_name]], item)

  update_gui()
  
})
edit_handler <- function(h,...) {
  message("edit item")
  ind <- svalue(item_tbl, index=TRUE)
  item <- get_item(ind)
  p_name <- get_item(ind, parent=TRUE)  # e.g. "trails"
  widgets <- e[[p_name]]


  ## transport item to widgets
  f <- function(nm) {
    svalue(widgets[[nm]]) <- item[[nm]]
  }
  dbug("edit handler", item=item)
  sapply(names(item), f)

  ## set tab
  svalue(notebook_gp) <- c("trails"=1, "hikes"=2, "pois"=3)[p_name]
  
}
edit_button <- gbutton("edit", cont=bg, handler=edit_handler)
addHandlerDoubleClick(item_tbl, handler=edit_handler)

## add trail stuff
trail_gp <- gvbox(container=notebook_gp, use.scrollwindow=TRUE, label="Trails")
bg <- ggroup(container=trail_gp)
gbutton("view path", container=bg, handler=graphic_handler,
        action=list(x="trails"))

gbutton("add path", container=bg, handler=add_handler,
        action=list(x="trails"))

e$trails$name <- gedit("", initial.msg="Name of the trail",
                    container=gframe("Trail name", container=trail_gp))

e$trails$color <- gcombobox(trail_colors,
                         container=gframe("Trail color", container=trail_gp))

e$trails$txt <- gtext("", expand=TRUE, fill=TRUE,
                      container=gframe("Path (csv, Lng, Lat, [Alt])", container=trail_gp))

e$trails$description <- gtext("", expand=TRUE, fill=TRUE,
                              container=gframe("Description", container=trail_gp))




## add hike stuff
hike_gp <- gvbox(container=notebook_gp, use.scrollwindow=TRUE, label="Hikes")
bg <- ggroup(container=hike_gp)
gbutton("view hike", container=bg, handler=graphic_handler,
        action=list(x="hikes"))

gbutton("add hike", container=bg, handler=add_handler,
        action=list(x="hikes"))

e$hikes$name <- gedit("", initial.msg="Name of the hike",
                    container=gframe("Trail name", container=hike_gp))



e$hikes$txt <- gtext("", expand=TRUE, fill=TRUE,
                      container=gframe("Path (csv, Lng, Lat, [Alt])", container=hike_gp))

e$hikes$description <- gtext("", expand=TRUE, fill=TRUE,
                             container=gframe("Description", container=hike_gp))



## add pois stuff
pois_gp <- gvbox(container=notebook_gp, label="POIs")
bg <- ggroup(container=pois_gp)
gbutton("view point", container=bg, handler=graphic_handler,
        action=list(x="pois"))

gbutton("add point", container=bg, handler=add_handler,
        action=list(x="pois"))


e$pois$name <- gedit("", initial.msg="Name",
                    container=gframe("Item name", container=pois_gp))

e$pois$type <-gcombobox(c("Parking", "Historical", "Viewport"),
                    container=gframe("Type", container=pois_gp))

e$pois$point <- gedit("", initial.msg="longitude, latitude",
                      container=gframe("Where", container=pois_gp))
e$pois$description <- gtext("", expand=TRUE, fill=TRUE,
                      container=gframe("Description", container=pois_gp))


## first one
svalue(notebook_gp) <- 1
