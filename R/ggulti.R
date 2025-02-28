library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(gganimate)
library(gifski)

#' Generate a pitch map as a ggplot object. The 'type' parameter controls the section of pitch.
#' Default size is set to 100 x 37, with endzones 18 deep.
#'
#' @param type Style of pitch: "full" (default), "half_attack", "half_defend", "blank", "void"
#' @param colour Colour of pitch outline : default = "dimgrey"
#' @param fill Fill colour of central zone  : default = "white"
#' @param endzone_fill Fill colour of endzone : default = #AA4422
#' @param linewidth Width of lines : default = 0.5
#' @param alpha Fill opacity of central zone : default = 1
#' @param endzone_alpha Fill opacity of endzone : default = 0.2
#' @param linetype Linetype of lines : default = solid
#' @param xmin Start x coordinate : default = 0
#' @param xmax End x coordinate : default = 37
#' @param ymin Start y coordinate : default = 0
#' @param ymax End y coordinate : default = 100
#' @param zone_depth Depth of endzones : default = 18
#' @return A ggplot object
#' @examples
#' ggpitch()
#' ggpitch("half_attack",endzone_fill="blue")
ggpitch <- function (type = "full", colour = "dimgray", fill = "white", endzone_fill = "#D55E00",
                     linewidth = 0.5, alpha = 1, endzone_alpha=0.2, linetype = "solid",
                     xmin = 0, xmax = 37, ymin = 0, ymax = 100, zone_depth = 18)
{
  p = ggplot()
  if(type=="full"){
    p = p +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=fill,colour=colour,linewidth=linewidth,alpha=alpha,linetype=linetype) +
      geom_rect(aes(xmin=0,xmax=37,ymin=0,ymax=18),fill=endzone_fill,colour=colour,linewidth=linewidth,alpha=endzone_alpha,linetype=linetype) +
      geom_rect(aes(xmin=0,xmax=37,ymin=82,ymax=100),fill=endzone_fill,colour=colour,linewidth=linewidth,alpha=endzone_alpha,linetype=linetype)
  }
  if(type=="half_attack"){
    p = p +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymax/2,ymax=ymax),fill=fill,colour=colour,linewidth=linewidth,alpha=alpha,linetype=linetype) +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymax-zone_depth,ymax=ymax),fill=endzone_fill,colour=colour,linewidth=linewidth,alpha=endzone_alpha,linetype=linetype)+
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymax/2,ymax=ymax/2),fill=fill,colour=fill,alpha=alpha)
  }
  if(type=="half_defend"){
    p = p +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax/2),fill=fill,colour=colour,linewidth=linewidth,alpha=alpha,linetype=linetype) +
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymin+zone_depth),fill=endzone_fill,colour=colour,linewidth=linewidth,alpha=endzone_alpha,linetype=linetype)+
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymax/2,ymax=ymax/2),fill=fill,colour=fill,alpha=alpha)
  }
  if(type=="blank"){
   p = p +
     geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=fill,alpha=alpha)
  }
  if(type=="void"){
    p
  }
  p + theme_minimal()
}

#' Helper function to print a list of 7 players in horizontal stack formation
#'
#' @return A tibble
#' @examples
#' hstack
hstack <- function(){
  players <- list(
    player("H1", show=T, x=c(8), y=c(22), alpha = 0.8, frame = 1),
    player("H2", show=T, x=c(17), y=c(22), alpha = 0.8, frame = 1),
    player("H3", show=T, x=c(25), y=c(22), alpha = 0.8, frame = 1),
    player("C1", show=T, x=c(7), y=c(40), alpha = 0.8, frame = 1),
    player("C2", show=T, x=c(15), y=c(40), alpha = 0.8, frame = 1),
    player("C3", show=T, x=c(22), y=c(40), alpha = 0.8, frame = 1),
    player("C4", show=T, x=c(29), y=c(40), alpha = 0.8, frame = 1)
  )
  bind_rows(players)
}

#' Create a pitch_object dataframe. Objects are represented with geom_point in the plot_play() function.
#' Objects are usually players on the field.
#'
#' @param label Name for the object (required)
#' @param show Show the label in plots : default = F
#' @param object Name for the type of object : "Offense" (default), "Defense", "Coach", "Cone", "Disc" are recognised but can be anything.
#' @param x Vector of x-coords covered by the object (required)
#' @param y Vector of y-coords covered by the object (required)
#' @param frame Vector of frames to show the object in : default = 1
#' @param alpha Colour opacity of the object : default = 0.5
#' @param label_pos Position of label relative to object : "down" (default), "up", "left", "right"
#' @return A data.frame
#' @examples
#' pitch_object("H1", T, x = c(15,15,5), y = c(25,35,37), frame = 1:3)
#' pitch_object("D1", F, object = "Defense", x = c(13,13,8), y = c(25,35,37), frame = 1:3)
#' pitch_object("q1", F, object = "Cone", x = c(13), y = c(26), frame = 1:3)
pitch_object <- function(label = "", show = F, object = "Offense", x, y, frame = 1, alpha = 0.5, label_pos = "down"){
  lshow = ""
  if(show == T){
    lshow = label
  }
  data.frame(label = label, show = lshow, object = object, x = x, y = y, frame = frame, alpha = alpha, label_pos = label_pos)
}

#' Convenience function to create a Defense pitch_object dataframe which tracks another objects movements.
#'
#' @param player Name of the object_dataframe to track (required)
#' @param label Name for the object (required)
#' @param show Show the label in plots : default = Same as player object, T, F
#' @param object Name for the type of object : "Defense" (default), "Offense", "Coach", "Cone", "Disc" are recognised but can be anything.
#' @param xjust Vector of adjustments to player x-coords to offset defenders position : default = -1
#' @param yjust Vector of adjustments to player y-coords to offset defenders position : default = -1
#' @param frame Vector of frames to show the object in : default = same as player object
#' @param alpha Colour opacity of the object : default = same as player object
#' @param label_pos Position of label relative to object : default = same as player object, "down"  "up", "left", "right"
#' @return A data.frame
#' @examples
#' defender(player_list[[1]], "D1", x = c(-1,-1,5), y = c(2,2,5))
defender <-function(player,label = "", show = NA, object = "Defense", xjust = -1, yjust = -1, frame = NA, alpha = NA, label_pos = NA){
  if(is.na(frame)){
    frame = player$frame
  }
  if(is.na(alpha)){
    alpha = player$alpha
  }
  if(is.na(label_pos)){
    label_pos = player$label_pos
  }
  if(is.na(show)){
    show = player$show
  }
  lshow = ""
  if(show == T){
    lshow = label
  }
  data.frame(label = label, show = lshow, object = object, x = player$x + xjust, y = player$y + yjust, frame = frame, alpha = alpha, label_pos = label_pos)
}

#' Create a pitch_arrow dataframe. Arrows are represented with geom_segment or geom_curve in the plot_play() function.
#' Arrows are usually player movements (cuts) or throws on the field.
#'
#' @param label Name for the arrow (required)
#' @param show Show the label in plots : default = F
#' @param object Attribute an arrow to an object type to match colours : "Offense" (default), "Defense", "Coach", "Cone", "Disc" are recognised but can be anything.
#' @param type What the arrow represents : "Cut" (default), "Throw", "Label" are recognised but can be anything
#' @param x x-coord for start of arrow (required)
#' @param y y-coord for start of arrow (required)
#' @param xend x-coord for end of arrow (required)
#' @param yend y-coord for end of arrow (required)
#' @param frame Vector of frames to show the arrow in : default = 1
#' @param alpha Colour opacity of the arrow : default = 1
#' @param arrow_shape Shape of the arrow : "straight" (default), "bhrc", "fhrc", "bhio", "fhio" (give curved throwing shapes)
#' @param label_pos Position of label relative to start or end of arrow : "start_down" (default), "start_up", "end_down", "end_up"
#' @return A data.frame
#' @examples
#' pitch_arrow("T1", type = "Throw", x = 5, y = 37, xend = 5, yend = 90, frame = 3)
#' pitch_arrow("C1", x = 5, y = 37, xend = 5, yend = 90, frame = 3)
pitch_arrow <- function(label = "", show = F, object = "Offense", type = "Cut", x, y, xend, yend, frame = 1, alpha = 1, arrow_shape = "straight", label_pos = "start_down"){
  lshow = ""
  if(show == T){
    lshow = label
  }
  data.frame(label = label, show = lshow, object = object, type = type, x = x, y = y, xend = xend, yend = yend, frame = frame, alpha = alpha, arrow_shape = arrow_shape, label_pos = label_pos)
}

#' Convenience function to create pitch_arrows which trace an objects movement path on the pitch e.g. a players cut. You can choose
#' to only trace specific object types e.g offense players only.
#'
#' @param object_list A list of object data.frames e.g. generated by pitch_object (required)
#' @param objects A vector of the object types to trace : default = "Offense"
#' @return A data.frame
#' @examples
#' object_paths(player_list)
#' object_paths(player_list, c("Offense","Defense"))
object_paths <- function(object_list, objects = c("Offense")){
  arrows <- object_list |> map(~if(!is.null(.x)){
                      .x |>
                       mutate(xend = x, yend = y,
                              x = lag(x), y = lag(y),
                              type = "Cut", arrow_shape = "straight", show="", label_pos="start_down",
                              label = paste0(label,row_number()-1),
                              frame = frame - 1 ) |>
                       filter(!is.na(x), !(x == xend & y == yend)) |>
                       select(label,show,object,type,x,y,xend,yend,frame,alpha,arrow_shape,label_pos)
  })

  bind_rows(arrows) |> filter(object %in% objects)
}

#' Convenience function to create a pitch_arrow dataframe representing a throw between two players in an object list.
#'
#' @param object_list List of pitch_objects (required)
#' @param label Name for the arrow (required)
#' @param show Show the label in plots : default = F
#' @param from Label of the throwing object
#' @param to Label of the receiving object or "space" if into empty space (provide space_x and space_y)
#' @param frame Vector of frames to show the arrow in : default = 1
#' @param throw_frame The frame representing the throw and receive positions of the to and from objects
#' @param alpha Colour opacity of the arrow : default = 1
#' @param arrow_shape Shape of the arrow : "straight" (default), "bhrc", "fhrc", "bhio", "fhio" (give curved throwing shapes)
#' @param space_x x-coord of a throw into space (must specify to = "space")
#' @param space_y y-coord of a throw into space (must specify to = "space")
#' @param label_pos Position of label relative to start or end of arrow : "start_down" (default), "start_up", "end_down", "end_up"
#' @return A data.frame
#' @examples
#' throw(player_list,"T1", from = "H1", to = "C1", frame = 1:3, throw_frame = 3)
#' throw(player_list,"T1", from = "H1", to = "space", space_x = 5, space_y = 90, arrow_shape = "bhrc", frame = 2:3, throw_frame = 3)
throw <- function(object_list,label,show=F,from,to,frame=1,throw_frame,alpha=1,arrow_shape="straight",space_x=NA,space_y=NA,label_pos="start_down"){
  lshow = ""
  if(show == T){
    lshow = label
  }

  p <- player_list |> bind_rows()

  object = p |> filter(label == from, frame == throw_frame) |> pull(object)
  x = p |> filter(label == from, frame == throw_frame) |> pull(x)
  y = p |> filter(label == from, frame == throw_frame) |> pull(y)

  ## Throw to a defined space
  if(to == "space"){
    if(is.na(space_x)){
      xend = x
    }
    else{
      xend = space_x
    }
    if(is.na(space_y)){
      yend = y
    }
    else{
      yend = space_y
    }
  }
  else{
    xend = p |> filter(label == to, frame == throw_frame) |> pull(x)
    yend = p |> filter(label == to, frame == throw_frame) |> pull(y)
  }

  data.frame(label = label, show = lshow, object = object, type = "Throw", x = x, y = y, xend = xend, yend = yend, frame = frame, alpha = alpha, arrow_shape = arrow_shape, label_pos = label_pos)
}

#' Convenience function to create a pitch_object which follows the path of an arrow. E.g. a disc following a throw.
#'
#' @param throw A pitch_arrow dataframe (required)
#' @param label Name for the object (required)
#' @param show  Show the label in plots : default = F
#' @param xjust 2 value vector showing the x-coord offset for the thrower and receiver : default = c(-1,-1)
#' @param yjust 2 value vector showing the y-coord offset for the thrower and receiver : default = c(1,1)
#' @param frame Vector of frames to show the object in : default = 1
#' @param release_frame The frame where the disc should be released from the thrower : default = 2
#' @param alpha Colour opacity of the object : default = 1
#' @param label_pos Position of label relative to start or end of object : "down" (default), "up", "left", "right"
#' @return A data.frame
#' @examples
#' disc(throw_list[[1]],"D1",frame = 1:3, release_frame = 2)
disc <- function(throw,label,show=F,xjust=c(-1,-1),yjust=c(1,1),frame=1,release_frame=2,alpha=1,label_pos="down"){
    lshow = ""
    if(!show == F){
      lshow = "label"
    }

    x = vector()
    y = vector()
    for(i in frame){
      if(i < release_frame){
        x = append(x,c(throw[1,]$x + xjust[1]))
        y = append(y,c(throw[1,]$y + yjust[1]))
      }
      else{
        x = append(x,c(throw[1,]$xend + xjust[2]))
        y = append(y,c(throw[1,]$yend + yjust[2]))
      }
    }
    data.frame(label = label,show = lshow,object="Disc",x = x,y = y,frame = frame,alpha = alpha,label_pos = label_pos)
}

#' Helper function used by plot_play() function. Takes a dataframe of objects/arrows and fills so that each object/arrow appears in all frames
#'
#' @param df Dataframe of objects/arrows
#' @param frames Total number of frames to fill
#' @return A data.frame
#' @examples
#' all_frames(throws,3)
all_frames <- function(df,frames){
  df |> group_by(label) |>
    complete(frame = full_seq(1:frames,1)) |>
    fill(-frame,.direction="updown")
}

#' Export an object or arrow list as a tsv file arranged by frames. This is useful for making manual edits.
#'
#' @param list Either a list of objects or arrows
#' @param filename Name of the file to save : default = "frames.tsv"
#' @return A data.frame
#' @examples
#' export_frames(object_list)
export_frames <- function(list,filename="frames.tsv"){
  df <- list |> bind_rows() |> arrange(frame,label)
  write_tsv(df,filename)
}

# Hardcoded plotting variables
arrowtypes = c("Cut" = "solid", "Throw" = "dashed", "Label" = "dotted")
obj_cols =c("Disc" = "grey","Offense" = "#009E73","Defense" = "#0072B2", "Coach" = "#f8766d", "Cone" = "darkorange")
obj_shapes = c("Disc"=19,"Cone"=17)
obj_sizes = c("Disc"=7,"Cone"=4)
ggulti_plot_values = list(
  arrowtypes = arrowtypes,
  obj_cols = obj_cols,
  obj_shapes = obj_shapes,
  obj_sizes = obj_sizes
)

#' Plotting function to create final graphic with a pitch, objects and arrows.
#'
#' @param pitch A pitch ggplot object create by ggpitch() : default = ggpitch()
#' @param arrow_list List of pitch_arrow dataframes
#' @param object_list List of pitch_object dataframes
#' @param static_frame Which frame to show in a static plot : default = 1
#' @param animate Output a gif instead of an image : F (default), T
#' @param animate_res Manually set animation resolution : default = 80
#' @param shadow Add a shadow to objects in animation : T, F (default)
#' @param transition_length Transition time (s) for animations : default = 1
#' @param state_length State time (s) for animations : default = 0.5
#' @param keep_arrows Show arrows in all frames and only apply frames to objects : F (default), T
#' @param show_all Show all frames together in one image : F (default), T
#' @param default_obj_size Default point size for objects : default = 8
#' @param default_obj_shape Default shape for objects : default = 19
#' @param default_obj_col Default colour for objects : default = #009E73
#' @param obj_exclude Exclude objects from guide : default = c("Cone")
#' @param pv Replace default plotting values for objects
#' @return A ggplot object
#' @examples
#' plot_play(pitch,arrow_list,object_list,static_frame=2)
plot_play <- function(pitch=ggpitch(),arrow_list=NULL,object_list=NULL,static_frame=1,animate=F,animation_res=150,animation_width=8,animation_height=8,shadow=F,transition_length=1,state_length=0.5,keep_arrows=F,show_all=F,default_obj_size=8,default_obj_shape=19,default_obj_col="#009E73",obj_exclude=c("Cone"),pv=ggulti_plot_values){

  arrows = bind_rows(arrow_list)
  objects = bind_rows(object_list)

  p = pitch
  if(nrow(arrows) > 0){
    if((animate==F & show_all==F) & keep_arrows==F){
      arrows = arrows |> filter(frame==static_frame)
    }
    if(keep_arrows==T){
      frames = max(arrows$frame,objects$frame)
      arrows = all_frames(arrows,frames)
    }
    if(arrows |> filter(arrow_shape %in% c("fhrc","bhio")) |> nrow() > 0){
      p = p + geom_curve(data=arrows |> filter(arrow_shape %in% c("fhrc","bhio")),
               aes(x = x, y = y, xend = xend, yend = yend,
                   colour=object, linetype=type, group = label),
               curvature = 0.2,
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed"))
    }
    if(arrows |> filter(arrow_shape %in% c("bhrc","fhio")) |> nrow() > 0){
      p = p + geom_curve(data=arrows |> filter(arrow_shape %in% c("bhrc","fhio")),
               aes(x = x, y = y, xend = xend, yend = yend,
                   colour=object, linetype=type, group = label),
               curvature = -0.2,
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed"))
    }
    if(arrows |> filter(arrow_shape == "straight") |> nrow() > 0){
      p = p + geom_segment(data=arrows |> filter(arrow_shape == "straight"),
                 aes(x = x, y = y, xend = xend, yend = yend,
                     colour=object, linetype=type, group = label),
                 arrow = arrow(length = unit(0.25, "cm"),
                               type = "closed"))
    }
    p = p + scale_linetype_manual(values=pv$arrowtypes)

    ## Add labels
    for(pos in c("start_up","start_down","end_up","end_down")){
      vjust = case_when(pos %in% c("start_up","end_up") ~ -0.5, pos %in% c("start_down","end_down") ~ 1.5, .default = NA)

      if(arrows |> filter(!show == "" & label_pos==pos) |> nrow() > 0){
        if(pos %in% c("start_up","start_down")){
          p = p + geom_label(data=arrows |> filter(!show == "" & label_pos==pos), aes(x=x,y=y,colour=object,alpha=alpha,label=show,group=label),vjust=vjust,show.legend = FALSE)
        }
        else{
          p = p + geom_label(data=arrows |> filter(!show == "" & label_pos==pos), aes(x=xend,y=yend,colour=object,alpha=alpha,label=show,group=label),vjust=vjust,show.legend = FALSE)
        }
      }
    }
  }

  if(nrow(objects) > 0){
    if((animate==F & show_all==F)){
      objects = objects |> filter(frame==static_frame)
    }
    breaks = objects |> filter(!object %in% obj_exclude) |> pull(object) |> unique()
    p = p + geom_point(data=objects,aes(x=x,y=y,colour=object,alpha=alpha,group=label,shape=object,size=object)) +
      scale_size_manual(values=pv$obj_sizes,na.value = default_obj_size,breaks = breaks) +
      scale_colour_manual(values=pv$obj_cols,na.value = default_obj_col,breaks = breaks) +
      scale_shape_manual(values=pv$obj_shapes,na.value = default_obj_shape,breaks = breaks)
    ## add labels
    for(pos in c("up","down","right","left")){
        vjust = case_when(pos == "up" ~ -2, pos == "down" ~ 3, .default = NA)
        hjust = case_when(pos == "right" ~ -1, pos == "left" ~ 2, .default = NA)
        if(objects |> filter(!show == "" & label_pos==pos) |> nrow() > 0){
          p = p + geom_text(data=objects |> filter(!show == "" & label_pos==pos), aes(x=x,y=y,colour=object,alpha=alpha,label=show,group=label),vjust=vjust,hjust=hjust,show.legend = FALSE)
        }
    }
  }
  p = p +
    labs(x=NULL,y=NULL,colour="",linetype="")+
    scale_alpha_identity() +
    guides(alpha="none",shape="none",size="none",colour = guide_legend(override.aes = list(size=8))) +
    theme_minimal()
  if(animate == T){
    ga = p + transition_states(states = factor(frame,levels=1:max(frame)),
                               transition_length = transition_length,
                               state_length = state_length,
                               wrap=T)

    if(shadow == T){
      ga = ga + shadow_mark(size = 0, colour = 'grey') + enter_fade()
    }
    animate(ga, renderer = gifski_renderer(),res = animation_res,width = animation_width,height = animation_height)
  }
  else{
    p
  }
}


