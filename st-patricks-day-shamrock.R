# St. Patricks Day Shamrock Plot
# By: Joe Domaleski
# Date: 3/17/2025
#
# inspired by a 4-leaf clover concept from Natalie Patten

library(ggplot2)

# Step 1: Generate a Single Petal
# --------------------------------
# Define a function that returns a petal based on a rose curve.
# By using theta in the interval [0, π/2], we trace out only one petal.
petal <- function(theta = seq(0, pi/2, length.out = 1000)) {
  r <- sin(2 * theta) + (1/4) * sin(6 * theta)
  data.frame(x = r * cos(theta), y = r * sin(theta))
}

# Step 2: Define a Rotation Function
# ------------------------------------
# This helper function rotates a set of (x, y) points by a given angle (in degrees)
# using the standard 2D rotation transformation.
rotate <- function(df, deg) {
  a <- deg * pi / 180
  data.frame(
    x = df$x * cos(a) - df$y * sin(a),
    y = df$x * sin(a) + df$y * cos(a)
  )
}

# Step 3: Create and Adjust the Base Petal
# ------------------------------------------
# Pre-rotate the petal by -45° so that its tip aligns horizontally (pointing right).
# Then widen the petal by scaling its y-coordinate (which is perpendicular to the tip)
# by a factor of 1.2. This makes the petal fatter rather than taller.
base_leaf <- rotate(petal(), -45)
base_leaf$y <- base_leaf$y * 1.2

# Step 4: Generate the Three Leaves
# -----------------------------------
# Create three leaves by rotating the base petal:
# - The right leaf (tip at 0°) remains as is.
# - The top leaf is rotated by 90° (tip at 90°).
# - The left leaf is rotated by 180° (tip at 180°).
leaf_right <- rotate(base_leaf, 0)
leaf_top   <- rotate(base_leaf, 90)
leaf_left  <- rotate(base_leaf, 180)

# Combine these three leaves, labeling each for clarity.
leaves <- rbind(
  data.frame(x = leaf_right$x, y = leaf_right$y, part = "right"),
  data.frame(x = leaf_top$x, y = leaf_top$y, part = "top"),
  data.frame(x = leaf_left$x, y = leaf_left$y, part = "left")
)

# Step 5: Create a Slightly Curved Stem
# --------------------------------------
# Define a stem that starts at the center (0,0) and extends downward.
# A small sinusoidal adjustment to the x-coordinate gives it a gentle curve.
stem <- data.frame(
  x = seq(0, 0.05, length.out = 100) - 0.025 * sin(seq(0, pi, length.out = 100)),
  y = seq(0, -1.2, length.out = 100)
)

# Step 6: Plot the Three-Leaf Shamrock with the Curved Stem
# -----------------------------------------------------------
# The leaves are drawn as filled polygons, and the stem is drawn as a line.
# A fixed coordinate ratio and a void theme create a clean, centered display.
ggplot() +
  geom_polygon(data = leaves, aes(x, y, group = part), fill = "darkgreen", color = "black") +
  geom_line(data = stem, aes(x, y), color = "black", size = 1) +
  coord_fixed() +
  theme_void() +
  ggtitle("Happy St. Patrick's Day") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

