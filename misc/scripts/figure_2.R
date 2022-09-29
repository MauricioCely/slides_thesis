library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.2
library(patchwork) # The Composer of Plots, CRAN v1.1.2
library(ggiraph) # Make 'ggplot2' Graphics Interactive, CRAN v0.8.3


# Figure 2 ----------------------------------------------------------------

### Parameters

p = 1/60
m = c(0, 1.4/sqrt(3), 1.5)
q = m * p

### Correlation 
theorical_correlation <- 
function(f, p, q){
  
  y = 2 *p * ( (1 /(p^2 +  (q + 2*pi * f)^2 ) ) + (1 / (p^2 + (q - 2*pi * f)^2 ) )  )
  
  return(y)
  
}

### Time scale peak

Tpeak <- function(p, q) {
    x <- 1/(2*pi) * sqrt( 2 * sqrt(q^2 * (p^2 + q ^2) ) - p^2 - q^2  )
    return(x)
  }

Tpeak <- 
Tpeak(p = p, q = q) %>% 
  `names<-`(c("m=0", "m=1.4/sqrt(3)", "m=1.5")) %>% 
  enframe() %>% 
  rename(case = name)


f = seq(1E-4, 1E-1, length.out = 400)

theorical_data <-
map_dfc(.x = q, 
    .f = ~ theorical_correlation(f = f, p = p, q = .x) ) %>% 
  mutate(f = f) %>% 
  relocate(f) %>% 
  `colnames<-`(c("freq", "m=0", "m=1.4/sqrt(3)", "m=1.5")) %>% 
  pivot_longer(cols = -freq, names_to = "case") 


p1 <- theorical_data %>% 
  ggplot(aes(x = freq, y = value, color = case)) +
  geom_line_interactive(aes(tooltip = case, data_id = case)) +
  geom_vline_interactive(data = Tpeak, linetype = 2, show.legend = F, 
                         aes(xintercept = value, color = case,
                             tooltip = case, data_id = case)) +
  scale_y_log10(labels = function(x) parse(text = paste0("10^~", log10(x) ) ),
                breaks = 10^(-5:5),
                minor_breaks = as.numeric(1:10 %o% 10 ^ (-4:4))) +
  scale_x_log10(labels = function(x) parse(text = paste0("10^~", log10(x) ) ),
                breaks = 10^(-5:5),
                minor_breaks = as.numeric(1:10 %o% 10 ^ (-4:4))) +
  scale_color_manual(values = pals::tol(3), 
                     breaks = c("m=0", "m=1.4/sqrt(3)", "m=1.5"),
                     labels = parse(text = c("m[X] < frac(1, sqrt(3))", "list(frac(1, sqrt(3)) <= phantom(x)*m[X]) <~1", "m[x] >= ~ 1")),
                     guide = guide_legend(title = NULL, override.aes = list(shape = 15, size = 3))) +
  labs(#title = "Vertical wind",,
    # caption = bquote(bold("Note:")~"Figure represents the mean values of the normalized spectra"),
    x = bquote(f (Hz)),
    y = bquote(F[w](f)~CoF[uw](f))) +
  theme_bw(base_family = 'Arial', base_size = 12) +
  theme(aspect.ratio = 1,
        legend.position = "top"
  )

p2 <- theorical_data %>% 
  ggplot(aes(x = freq, y = freq*value, color = case)) +
  geom_line_interactive(aes(tooltip = case, data_id = case)) +
  scale_y_log10(labels = function(x) parse(text = paste0("10^~", log10(x) ) ),
                breaks = 10^(-5:5),
                minor_breaks = as.numeric(1:10 %o% 10 ^ (-4:4))) +
  scale_x_log10(labels = function(x) parse(text = paste0("10^~", log10(x) ) ),
                breaks = 10^(-5:5),
                minor_breaks = as.numeric(1:10 %o% 10 ^ (-4:4))) +
  scale_color_manual(values = pals::tol(3), 
                     breaks = c("m=0", "m=1.4/sqrt(3)", "m=1.5"),
                     labels = parse(text = c("m[X] < frac(1, sqrt(3))", "list(frac(1, sqrt(3)) <= phantom(x)*m[X]) <~1", "m[x] >= ~ 1")),
                     guide = guide_legend(title = NULL, override.aes = list(shape = 15, size = 3))) +
  labs(
    x = bquote(f (Hz)),
    y = bquote(fF[w](f)~fCoF[uw](f))) +
  theme_bw(base_family = 'Arial', base_size = 12) +
  theme(aspect.ratio = 1,
        legend.position = "top"
  )



girafe(ggobj = p1+p2+plot_layout(guides = "collect")&
         theme(legend.position='top'),
       options = list(
         opts_hover_inv(css = "opacity:0.1;"),
         opts_hover(css = "stroke-width:2;")), 
       width_svg = 8, 
       height_svg = 6)

       
