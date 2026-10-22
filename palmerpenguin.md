# Palmer Penguins Data Visualization

This project explores the `palmerpenguins` dataset in R using `ggplot2` to visualize relationships between penguin measurements and species.

## Project Overview

The script demonstrates:
- loading and inspecting the dataset
- cleaning and organizing data
- summarizing metrics by species and island
- filtering penguins by species
- creating scatterplots and annotations with `ggplot2`

## Requirements

Install the following R packages:

```r
install.packages("ggplot2")
library(ggplot2)

install.packages("palmerpenguins")
library(palmerpenguins)
```

## Dataset

This project uses the Palmer Penguins dataset, which contains measurements for penguins including:
- species
- island
- bill length
- bill depth
- flipper length
- body mass
- sex

## Example Workflow

### View the dataset

```r
glimpse(penguins)
View(penguins)
```

### Clean and rename columns

```r
penguins %>%
  rename(island_new = island)
```

### Summaries by island

```r
penguins %>%
  group_by(island) %>%
  drop_na() %>%
  summarize(mean_bill_length_mm = mean(bill_length_mm))
```

### Filter by species

```r
penguins %>%
  filter(species == "Adelie")
```

### Create a visualization

```r
Penguins_plot <- ggplot(data = penguins) +
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = species)) +
  geom_smooth(mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  labs(
    title = "Palmer Penguins: Body Mass vs. Flipper Length",
    caption = "Data collected by Dr. Kristen Gorman"
  ) +
  annotate("text", x = 195, y = 6200, label = "The Gentoo are the Largest")

plot(Penguins_plot)
```

## Outputs

The script creates a scatter plot showing the relationship between:
- flipper length (x-axis)
- body mass (y-axis)
- species grouped by color and shape

It also includes a trend line and annotation highlighting the Gentoo penguins as the largest group.

## Notes

- Use `drop_na()` to remove missing values when summarizing numeric variables.
- `ggplot2` is the main visualization library used in this project.
- The dataset can be extended with additional plots and more advanced statistical analysis.

## Future Ideas

- Compare bill length across species
- Create boxplots by island and species
- Add faceting for clearer comparisons
- Build a dashboard or presentation-ready report

## License

This is a sample project for learning purposes.
