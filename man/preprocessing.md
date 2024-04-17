## Preprocessing

Before the data is visualized several data preprocessing steps are applied. 

* Blank filtering
* Normalization

### Blank filtering

The blank filtering is applied to determine which lipid species are kept for visualization. This is done in two steps. 

1) A lipid species is flagged for removal if in more than 20% of the samples, its signal is less than two times the average of the blank signal.
2) A lipid species flagged for removal can be introduced back if in one of the groups in 60% or more of the samples, belonging to a specific group, its signal is more than two times the average of the blank signal.

#### Groups

A dataset can have multiple groups, e.g. genotype, treatmen/diagnosis. For the blank filtering a new group is created which is a combination of the following groups:

* sample type
* genotype
* treatment / diagnosis
* parental cell line / brain region
* culture conditions

### Normalization

There are two types of normalization applied.

* Total area normalization
* Total class normalization

#### Total signal normalization

Total signal normalization is applied on the lipid species and the lipid classes. Here each lipid species (or lipid class) is normalized against the total sum of the lipid species (or lipid class).

#### Total class normalization

Total class normalization is only applied on the lipid species. Here each lipid species is normalized against the total sum of the lipid species of a lipid class.

## Data tables

There are three data tables available for visualization.

* Lipid species, normalized to total amount of lipids.
* Lipid species, normalized to total amount of lipids per lipid class.
* Lipid class, normalized to total amount.

<style>
.col-center {
  text-align: center;
}
</style>

**Table:** Overview of which data table is available for which visualization.

<table style="width:100%;">
<tr>
  <th>Visualization</th>
  <th class="col-center">Lipid species<br>(normalized to total amount of lipids)</th>
  <th class="col-center">Lipid species<br>(normalized to total amount of lipids per lipid class)</th>
  <th class="col-center">Lipid class<br>(normalized to total amount)</th>
</tr>
<tr>
  <td>Class distribution</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
  <td class="col-center" style="color: green;">&#10004;</td>
</tr>
<tr>
  <td>Class comparison</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
  <td class="col-center" style="color: green;">&#10004;</td>
</tr>
<tr>
  <td>Volcano plot</td>
  <td class="col-center" style="color: green;">&#10004;</td>
  <td class="col-center" style="color: green;">&#10004;</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
</tr>
<tr>
  <td>Heatmap (z-scores)</td>
  <td class="col-center" style="color: green;">&#10004;</td>
  <td class="col-center" style="color: green;">&#10004;</td>
  <td class="col-center" style="color: green;">&#10004;</td>
</tr>
<tr>
  <td>PCA (mean centered and uv scaled)</td>
  <td class="col-center" style="color: green;">&#10004;</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
</tr>
<tr>
  <td>Fatty acid analysis</td>
  <td class="col-center" style="color: green;">&#10004;</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
</tr>
<tr>
  <td>Fatty acid composition analysis</td>
  <td class="col-center" style="color: green;">&#10004;</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
  <td class="col-center" style="font-size: 75%">&#10060;</td>
</tr>
</table>

<!---
&#10004; = bold check
&#10060; = red cross
--->