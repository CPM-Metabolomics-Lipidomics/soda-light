## Preprocessing

Before the data is visualized several data preprocessing steps are applied. 

* Blank filtering
* Normalization

### Blank filtering

The blank filtering is applied to determine which lipid species are kept for visualization. This is done in two steps. 

1) A lipid species is flagged for removal if in more than 20% of the samples, its signal is less than two times the average of the blank signal.
2) A lipid species flagged for removal can be introduced back if in one of the groups in 80% or more of the samples, belonging to a specific group, its signal is more than two times the average of the blank signal.

#### Groups

***Explain what a group is.***

### Normalization

There are two types of normalization applied.

* Total area normalization
* Total class normalization

#### Total signal normalization

Total signal normalization is applied on the lipid species and the lipid classes. Here each lipid species (or lipid class) is normalized against the total sum of the lipid species (or lipid class).

#### Total class normalization

Total class normalization is only applied on the lipid species. Here each lipid species is normalized against the total sum of the lipid species of a lipid class.

## Data tables

There are four data tables available for visualization.

* Lipid species, normalized to total amount of lipids.
* Lipid species, normalized to total amount of lipids per lipid class.
* Lipid class, absolute values. ***Is this correct?***
* Lipid class, normalized to total amount.