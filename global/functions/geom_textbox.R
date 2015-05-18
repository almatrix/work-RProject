# function to add background to text 
# extension to ggplot2 geom_text
library(proto)

btextGrob <- function (label,x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
                       just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, 
                       default.units = "npc", name = NULL, gp = gpar(), vp = NULL, expand_w, expand_h, box_gp = gpar()) {
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(label = label, x = x, y = y, just = just, hjust = hjust, 
         vjust = vjust, rot = rot, check.overlap = check.overlap, 
         name = name, gp = gp, vp = vp, cl = "text")
    tg <- textGrob(label = label, x = x, y = y, just = just, hjust = hjust, 
                   vjust = vjust, rot = rot, check.overlap = check.overlap)
    w <- unit(rep(1, length(label)), "strwidth", as.list(label))
    h <- unit(rep(1, length(label)), "strheight", as.list(label))
    rg <- rectGrob(x=x, y=y,  just = just, hjust = hjust, 
                   vjust = vjust, width=expand_w*w, height=expand_h*h,
                   gp=box_gp)
    
    gTree(children=gList(rg, tg), vp=vp, gp=gp, name=name)
}

GeomTextbox <- proto(ggplot2:::GeomText, {
    objname <- "textbox"
    
    draw <- function(., data, scales, coordinates, ..., parse = FALSE, na.rm = FALSE,
                     expand_w = 1.2, expand_h = 2, bgcol = "grey50", bgfill = "white", bgalpha = 1) {
        data <- remove_missing(data, na.rm, 
                               c("x", "y", "label"), name = "geom_textbox")
        lab <- data$label
        if (parse) {
            lab <- parse(text = lab)
        }
        
        with(coord_transform(coordinates, data, scales),
             btextGrob(lab, x, y, default.units="native", 
                       hjust=hjust, vjust=vjust, rot=angle, 
                       gp = gpar(col = alpha(colour, alpha), fontsize = size * .pt,
                                 fontfamily = family, fontface = fontface, lineheight = lineheight),
                       box_gp = gpar(fill = bgfill, alpha = bgalpha, col = bgcol),
                       expand_w = expand_w, expand_h = expand_h)
        )
    }
    
})

geom_textbox <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                          parse = FALSE,  ...) { 
    GeomTextbox$new(mapping = mapping, data = data, stat = stat,position = position, 
                    parse = parse, ...)
}
