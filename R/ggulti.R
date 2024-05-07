library(ggsoccer)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(gganimate)
library(gifski)

ggpitch <- function (colour = "dimgray", fill = "white", type = "full", endzone_fill = "blue",
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
  p
}

player <- function(label = "", show = F, object = "Offense", x, y, frame = 1, alpha = 0.5, label_pos = "down"){
  lshow = ""
  if(show == T){
    lshow = label
  }
  data.frame(label = label, show = lshow, object = object, x = x, y = y, frame = frame, alpha = alpha, label_pos = label_pos)
}

defender <-function(player,label = "", show = NA, object = "Defense", xjust, yjust, frame = NA, alpha = NA, label_pos = NA){
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

pitch_arrow <- function(label = "", show = F, object = "Offense", type = "Cut", x, y, xend, yend, frame = 1, alpha = 1, arrow_shape = "straight", label_pos = "start_down"){
  lshow = ""
  if(show == T){
    lshow = label
  }
  data.frame(label = label, show = lshow, object = object, type = type, x = x, y = y, xend = xend, yend = yend, frame = frame, alpha = alpha, arrow_shape = arrow_shape, label_pos = label_pos)
}

player_paths <- function(player_list, objects = c("Offense")){
  arrows <- player_list |> map(~if(!is.null(.x)){
                      .x |>
                       mutate(xend = x, yend = y,
                              x = lag(x), y = lag(y),
                              type = "Cut", arrow_shape = "straight", show="", label_pos="start_down",
                              label = paste0(label,row_number()-1),
                              frame = frame -1) |>
                       filter(!is.na(x), !(x == xend & y == yend)) |>
                       select(label,show,object,type,x,y,xend,yend,frame,alpha,arrow_shape,label_pos)
  })

  bind_rows(arrows) |> filter(object %in% objects)
}

throw <- function(player_list,label,show=F,from,to,frame=1,throw_frame,alpha=1,arrow_shape="straight",space_x=NA,space_y=NA,label_pos="start_down"){
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

disc <- function(throws,xjust,yjust,alpha=1,start_frame=1){
  dp <- 1:length(throws) |> map(function(i){
    throw = throws[[i]]
    xstart = throw$x + xjust[i]
    ystart = throw$y + yjust[i]
    xend = throw$xend + xjust[i]
    yend = throw$yend + yjust[i]
    frame = c(throw$frame,throw$frame+1)
    data.frame(label = paste0("D_",throw$label),show = "",object = "Disc",x = c(xstart,xend),
               y = c(ystart,yend),frame = frame, alpha = alpha, label_pos = "down")
  })
  bind_rows(dp) |> complete(frame=full_seq(start_frame:max(frame),1)) |>
    fill(-frame,.direction = "up")
}

arrowtypes = c("Cut" = "solid", "Throw" = "dashed", "Label" = "dotted")
obj_cols =c("Disc" = "grey","Offense" = "#018571", "Defense" = "#f8766d", "Coach" = "#0571b0")
obj_shape = c(1)

plot_play <- function(pitch,arrow_list=NULL,object_list=NULL,static_frame=1,animate=F){
  arrows = bind_rows(arrow_list)
  objects = bind_rows(object_list)

  p = pitch
  if(nrow(arrows) > 0){
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
    p = p + scale_linetype_manual(values=arrowtypes)

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
    if(animate==F){
      objects = objects |> filter(frame==static_frame)
    }
    p = p + geom_point(data=objects,aes(x=x,y=y,colour=object,alpha=alpha,group=label),size=8) +
      scale_colour_manual(values=obj_cols)
    ## add labels
    for(pos in c("up","down","right","left")){
        vjust = case_when(pos == "up" ~ -2, pos == "down" ~ 3, .default = NA)
        hjust = case_when(pos == "right" ~ -2, pos == "left" ~ 2, .default = NA)
        if(objects |> filter(!show == "" & label_pos==pos) |> nrow() > 0){
          p = p + geom_text(data=objects |> filter(!show == "" & label_pos==pos), aes(x=x,y=y,colour=object,alpha=alpha,label=show,group=label),vjust=vjust,hjust=hjust,show.legend = FALSE)
        }
    }
  }
  p +
    labs(x=NULL,y=NULL,colour="Object",linetype="Arrow")+
    scale_alpha_identity() +
    guides(alpha="none") +
    theme_minimal()
}
plot_play(pitch, arrow_list, object_list,static_frame = 2)

players <- list(
  player("A", show=T, x=c(20,20,10),y=c(30,75,50), frame = 1:3, label_pos = "right"),
  player("J-DOG", show=T, x=c(10), y=c(20), alpha = 1, frame = 1:3),
  player("D1",object="Defense",x=c(12), y=c(22), frame = 1:3)
)

defenders <- list(
  defender(players[[1]],"DA",show=T,xjust=c(-3,-3,3),yjust=c(0,0,5),label_pos = "left")
)
player_list <- c(players,defenders)

throws <- list(
  throw(player_list,"T1",F,"J-DOG","A",frame=2,throw_frame = 3),
  throw(player_list,"T2",F,"A","space",frame=3,throw_frame = 3,space_y = 90,arrow_shape = "bhrc")
)

arrow_list = list(
  player_paths(player_list),
  throws,
  pitch_arrow("Roll Curve", object = "Offense", show = T, x = 5, y = 70, xend = 8, yend =  75, type = "Label", frame = 3,label_pos = "start_down")
)

disc_list = list(
  disc(throws,c(-1,-1),c(1,1),start_frame = 1)
)
object_list <- c(player_list,disc_list)

pitch = ggpitch(type="full",endzone_fill = "#AA4422")

plot_play(pitch, arrow_list, object_list,static_frame = 3)
gp = plot_play(pitch, arrow_list = arrow_list, object_list,animate = T,static_frame = 2)

ga=gp +
    transition_states(states = factor(frame,levels=1:4),transition_length = 1,state_length = 1,wrap=F)

animate(ga, renderer = gifski_renderer())

