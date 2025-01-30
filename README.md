# Fiordland_acoustic_monitoring

Deployment_map.R creates an [interactive Leaflet map](https://github.com/leahcrowe-otago/Fiordland_acoustic_monitoring/deployments/github-pages) of the most recent deployments available through github-pages

# Workflow:

1.  ./scripts/FAM.R: stands for Fiordland Acoustic Monitoring. This is where all acoustic detection data from all sources and deployments are collated. The data used for modeling is produced here.
    -   calls ./scripts/listening.R which calculates the active listening periods for each site
2.  ./scripts/occupancy.R: this is the code to compile and run the occupancy model. The results from the model are organized and produced in this code. Some site summaries are also executed here related to performance metrics. Outputs includes:
    -   data for Table 1, Table 2, Table S3, Table S5
3.  ./scripts/effort & daylight_figures.R: figures are created here related to survey effort and diel patterns including:
    -   Fig. 3, Fig. S3, Fig. S5
4.  Tables and Figures.qmd
    -   ./scripts/deployment_map_ms.R
    -   ./scripts/sightings_venn.R
5.  Suppmat.qmd
    - Fig. S1 is created in DOCoppsig.R
    - Fig. S4 is created in dist.R

Acoustic detection data are available at [![DOI](https://img.shields.io/badge/Mendeley-9D1620.svg?style=for-the-badge&logo=Mendeley&logoColor=white)](https://data.mendeley.com/datasets/hvb2gxv2zx/1) DOI:
[10.17632/hvb2gxv2zx.1](https://data.mendeley.com/datasets/hvb2gxv2zx/1)
