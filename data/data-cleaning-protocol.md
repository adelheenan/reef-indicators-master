# reef-indicators
## data cleaning protocol

*Describing protocol for the biomass-based estimates. Same applies for abundance, mean length, and mean mass*

**Step 1 - islands and areas**

- Reef habitat: forereef
- Years: 2010-2016
- Islands: group Alamagan, Sarigan and Guguan
- Islands: remove South Bank, Timor Leste, Gardner, Necker, Nihoa


**Step 2 - fish sizes and species**

- Remove fish ≥ 50cm from SPC, and remove fish < 50cm from TOW.
- Recruits: remove small fish from each species (as proportion of Lmax).
- Remove large tuna species (species code = EUAF)


**Step 3 - mean estimates and standardising for area sampled**

*For SPC data*

- At each depth zone, for each island, estimate the mean biomass for each species and each size class (irrespective of species)
- Weight biomass estimate according to the reef area of each depth zone
- Sum weighted biomass estimates across depth zones to get island-means

*For TOW data*

- Estimate mean biomass for each species and each size class (irrespective of species) across all tows at each island

**Step 4 - reduce outliers**

- For each species/size class/functional group biomass estimate, reduce any outlier to 95% percentile 

#### Categories required for indicators

For each dataset, at each island, we want the mean estimate of each of these categories (weighted for SPC):

1. Species biomass
2. Species abundance
3. Mean length
4. Mean mass
5. Biomass of fishes > 1kg
6. Log10 mass classes (in grams: 10-100, 100-1000, 1000-10,000, 10,000-100,000)

Navigating around the analysis ready data:

All the tow_ files are lists, with one data frame the mean, the other as the standard error. 

Species biomass and mean mass (labelled total fish biomass): _1_bio
Species abund: _2_abund
Mean length: _3_mean length
Biomass of large fishes: _4_big_fish
Biomass per mass class: _5_mass_size

All biomass estimates are in g m-2




