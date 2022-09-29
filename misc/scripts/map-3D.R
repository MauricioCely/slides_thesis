library(rayshader) # Create Maps and Visualize Data in 2D and 3D, CRAN v0.24.10
library(raster) # Geographic Data Analysis and Modeling, CRAN v3.6-3
library(rgl) # 3D Visualization Using OpenGL, [github::dmurdoch/rgl] v0.110.2

watercolor = "#2a89b3"
maxcolor = "#e6dbc8"
mincolor = "#b6bba5"
contour_color = "#7d4911"

lavarone_zoom <- readRDS(here("images", "Animation_3D", "DEM_Lavarone.RDS"))

extent_zoomed <- extent(lavarone_zoom)

lavarone_zoom_mat = raster_to_matrix(lavarone_zoom, verbose = FALSE)
lavarone_zoom_mat = rayshader::resize_matrix(lavarone_zoom_mat, scale = 0.5)

base_map <- readRDS(here("images", "Animation_3D", "base_map.RDS"))


plot_3d(base_map, lavarone_zoom_mat,
        zscale = 5*2, fov = 0, theta = 180, zoom = 0.65, phi = 0, windowsize = c(1080, 720),
        shadowcolor="#40310a", watercolor="#233aa1", shadow = F, solid = T, triangulate = F)

### Add Tower

lat = 5092131
long = 676671
e = extent_zoomed
heightmap = lavarone_zoom_mat
x = (long-e@xmin)/(e@xmax - e@xmin) * nrow(heightmap)
y = ncol(heightmap) - (lat-e@ymin)/(e@ymax - e@ymin) * ncol(heightmap)
zscale = 5*2
z = 1300
z=z/zscale

x = x - nrow(heightmap)/2
y = y - ncol(heightmap)/2

readRDS(here("images", "Animation_3D", "tower_obj.rds") ) %>%
  rgl::rotate3d(-pi/2, 1, 0, 0) %>%
  rgl::scale3d(0.4/2,0.4/2,0.4/2) %>%
  rgl::translate3d(x, z, y) %>%
  rgl::shade3d()

### Add verticals axis

z_ticks  = seq(0, 2500, length.out = 6)
z_ticks = z_ticks /zscale


rgl::axes3d(edges = "y--" , at = z_ticks,  labels = paste(seq(0, 2500, length.out = 6), "m"))
rgl::axes3d(edges = "z+-" , at = seq(-395.95, 395.95, length.out = 5),
            labels = paste(seq(0, 16, length.out = 5), "km"))
rgl::axes3d(edges = "x--", at = seq(-439.55, 439.55, length.out = 7),
            labels = paste(seq(18, 0, length.out = 7), "km"))


# render_label(heightmap = lavarone_zoom_mat, lat = lat, long = long, text = "Tower",
#              linecolor = "darkred", textcolor = "darkred",
#              altitude = 300, zscale = 5*2, offset = 400, dashed = T, linewidth = 2,
#              extent = extent_zoomed, clear_previous = T)

### Scalebar

# render_scalebar(scale_length = c(3/4, 1)-0.1, clear_scalebar = F,
#                 limits = c(0, 2, 4),
#                 label_unit = "km",
#                 y = 500/3,
#                 offset = -100,
#                 radius = 5,
#                 text_x_offset = 50,
#                 text_y_offset = 100,
#                 text_switch_side = TRUE,
#                 position = "E")

### Compass

render_compass(x = 600/2, y = 50, z = -665/2, 
               scale_distance = 1.2, compass_radius=40,
               color_n = "red", color_arrow = "black",
               color_background = "white", color_bevel = "black",
               clear_compass = F)
