# Community Temperature Index (CTI) for intertidal macroalgae in NW Spain.

## Introduction

Ocean warming is expected to drive quick shifts in marine species distributions modifying coastal communities. Previous research has shown how the southern Bay of Biscay is particularly affected and the canopy-forming subtidal and intertidal macroalgal assemblages are expected to be replaced by turf-forming Mediterranean-like communities with warmer affinities by the end of the century. The effect of temperature on macroalgal communities could be assesed using thermal metrics that can target temperature effects alone. An example is the Community Temperature Index (CTI) which is an abundance-weighted mean of the species’ optimal temperature. It has been previously used to understand the thermal preferences of different organisms such us birds, fish or invertebrates.

## Directory structure

```
{{directory_name}}/
├── analysis
├── data/
│   ├── derived_data/
│   └── raw_data/
├── docs/
├── product/
└── scripts/
```

* **analysis** - Markdown or Jupyter notebooks
* **data** - Raw and derived data
* **docs** - Rendered reports
* **product** - Output product files
* **scripts** - Reusable code

## Data series

The biological data series used in this product have been downoladed from EMODnet Biology, OBIS and GBIF. 
The environmental data series used in this product have been downloaded from Bio-Oracle as worldwide extension is required. 
The scripts used to obtain this data are included here 
    - Biological: '1_download_BR.R' 
    - Environmental: '2_download_EV.R'

## Data product

This product provides the Community Temperature Index (CTI) of macroalgae intertidal communities from NW Spain. The CTI has been calculated for 18 sites along a longitudinal thermal gradient in NW Spain, two intertidal levels (Low and Mid) and two different years (2011 and 2017). Results are stored in a csv file with the coordinates of each site ("decimalLongitude" and "decimalLatitude" columns), their codes ("Site" and "Intertidal_level" columns), the sampling year ("Year" column) and the mean CTI ("CTI column"). 

## More information:

### Code and methodology

CTI is the weighted mean of species’ thermal preferences calculated for a community. The temperature preference of species could be represented by their species temperature index (STI), which is the 50th percentile of the existing temperatures within their distribution ranges. Here, the thermal worldwide distribution ranges of each species included in the dataset of intertidal macroalgae from NW Spain (dataset ID: 5973) were modelled using MAXENT with default settings (Code in '4_models.R'). Occurrences to train models were gathered from the data series mentioned above and processed to reduce sampling bias, missidentifcations, errors and duplications. The codes used for that purpose are in '1_download_BR.R' and '3_nearest_cell.R'. After running the models, STIs were estimated from the resulting binary maps (Code in '4_models.R'). Finally, CTIs were calculated combining observation data from the intertidal dataset and the calculated/modelled STIs, as explained in the following script: '5_cti.R'.

### Citation and download link

This product should be cited as:

Ramos, E., Sainz-Villegas, S., de la Hoz, C.F., Puente, A., Juanes, J.A. (2025) Community Temperature Index (CTI) for intertidal macroalgae in NW Spain.Integrated data products created under the European Marine Observation Data Network (EMODnet) Biology project CINEA/EMFAF/2022/3.5.2/SI2.895681, funded by the European Union under Regulation (EU) No 508/2014 of the European Parliament and of the Council of 15 May 2014 on the European Maritime and Fisheries Fund.

Available for download in:

Data: https://erddap.emodnet.eu/erddap/files/biology_8780_20a9_a982_ec71/macroalgae_thermaldistribution_order.nc (global output)

Metadata record: https://emodnet.ec.europa.eu/geonetwork/srv/eng/catalog.search#/metadata/6d617269-6e65-696e-666f-000000008772 and https://emodnet.ec.europa.eu/geonetwork/srv/eng/catalog.search#/metadata/6d617269-6e65-696e-666f-000000008780

EMODnet Viewer: https://emodnet.ec.europa.eu/geoviewer/?layers=6d617269-6e65-696e-666f-000000008772:1:1&basemap=ebwbl&active=14163&bounds=-7473683.368577629,3082466.814999999,10427724.524577629,11459766.29&filters=&projection=EPSG:3857 and https://emodnet.ec.europa.eu/geoviewer/?layers=14161@6d617269-6e65-696e-666f-000000008780:1:1,12985@6d617269-6e65-696e-666f-000000008780:1:1&basemap=ebwbl&active=14161&bounds=-7473683.368577629,3082466.814999999,10427724.524577629,11459766.29&filters=&projection=EPSG:3857


### Authors

Ramos, E., Sainz-Villegas, S., de la Hoz, C.F., Puente, A., Juanes, J.A.
