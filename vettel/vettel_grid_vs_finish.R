# Grid vs position

# Load libraries
library(dplyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
library(RColorBrewer)
library(colorspace)

# Load data
vet <- read.csv("vet.csv")

# Font
#library(extrafont)
#font_import(paths = "/Users/aphelion/Library/Fonts") # Run only once!
extrafont::loadfonts(device = "postscript") # works! load every time if it doesn't work
import_titillium_web()

# Manipulate data
vet <- vet %>% 
  filter(!positionText %in% c("W", "D")) %>%
  filter(grid != 0) %>% # Filter pit lane starts
  mutate(dnf = ifelse(positionText == "R", TRUE, FALSE),
         position_diff = grid - positionOrder,
         position_gained = ifelse(positionOrder < grid, "Gained",
                                  ifelse(positionOrder == grid, "-", "Lost")),
         name_constructor = factor(name_constructor,
                                   levels = c("BMW Sauber",
                                              "Toro Rosso",
                                              "Red Bull",
                                              "Ferrari",
                                              "Aston Martin")))


# Colors
cols <- brewer.pal(5, name = "RdBu")
#cols_viridis <- viridis_pal()(5)
# BMW Sauber is missing
team_colors <- c("black", "#0000FF", "#1E5BC6", "#ED1C24", "#2D826D")

# Plot
p <- ggplot(vet, aes(positionOrder, grid, fill = position_diff, shape = dnf)) +
  geom_abline(
    slope = 1,
    linetype = "dashed",
    alpha = 0.8,
    color = "darkgrey"
  ) +
  # Invisible layer for ggmarginal histogram
  geom_point(
    aes(color = name_constructor),
    alpha = 0
  ) + 
  geom_point(
    data = filter(vet, dnf == TRUE),
    position = position_jitter(width = 0.1, height = 0.1, seed = 71),
    alpha = 0.9,
    show.legend = TRUE,
    size = 2
    ) +
  geom_point(
    data = filter(vet, dnf == FALSE),
    position = position_jitter(width = 0.1, height = 0.1, seed = 71),
    alpha = 0.9,
    size = 2
    ) +
  geom_rug(
    aes(color = name_constructor),
    position = "jitter",
    alpha = 0.3
    ) +
  scale_x_continuous(breaks = seq(1, 26, 2)) +
  scale_y_continuous(breaks = seq(1, 26, 2)) +
  scale_shape_manual(values = c(21, 19)) +
  scale_color_manual(values = team_colors) +
  scale_fill_gradientn(colours = cols,
                        values = scales::rescale(c(-20, -5, 0, 5, 20))) +
  #colorspace solution
  # colorspace::scale_fill_binned_diverging(
  #   palette = "Blue-Red",
  #   n_interp = 3,
  #   mid = 0,
  #   rev = TRUE
  # ) +
  
  guides(color = guide_legend(override.aes = list(shape = c(NA, NA, NA, NA, NA),
                                                  alpha = 1))) +
  labs(
    title = "Sebastian Vettel's Results",
    subtitle = "Grid start vs Finished position",
    x = "Finished position",
    y = "Grid position",
    caption = "Data: Ergast API",
    shape = "DNF",
    fill = "Gained/Lost \npositions",
    color = "Team"
  ) +
  expand_limits(x = -0.001, y = -0.001) +
  theme_ipsum_tw() +
  theme(
    plot.title = element_text(family = "Titillium Web", face = "bold"),
    aspect.ratio = 1,
    legend.position = "left",
    plot.title.position =  "panel",
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
    )

p

p1 <- ggMarginal(
  p,
  type = "histogram",
  #position = "identity",
  fill = "honeydew3",
  # groupColour = TRUE,
  # groupFill = TRUE,
  binwidth = 1,
  center = 0)
p1

# Consider using ggside package? https://cran.r-project.org/web/packages/ggside/vignettes/ggside_basic_usage.html


ggsave("images/vettel_scatter.png", p1, width = 30, height = 30, dpi = 300, scaling = 5)
