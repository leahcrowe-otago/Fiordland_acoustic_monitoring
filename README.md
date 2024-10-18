# Fiordland_acoustic_monitoring

# Workflow:

1.  ./scripts/FAM.R: stands for Fiordland Acoustic Monitoring. This is where all acoustic detection data from all sources and deployments are collated. The data used for modeling is produced here.
2.  ./scripts/occupancy.R: this is the code to compile and run the occupancy model. The results from the model are organized and produced in this code. Some site summaries are also executed here related to performance metrics. Output includes:
    -   data for Table 1, Table 2, Table S3, Table S5
3.  ./scripts/effort & daylight_figures.R: figures are created here related to survey effort and diel patterns including:
    -   Fig. 3, Fig. S3, Fig. S5, Fig. S6
4.  Tables and Figures.qmd
    -   ./scripts/deployment_map_ms.R
    -   ./scripts/sightings_venn.R
5.  Suppmat.qmd
