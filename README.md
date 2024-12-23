# er-forest-structure

## Replication code for 'Abiotic influences on continuous conifer forest structure across a subalpine watershed' (Worsham et al., 2024).

This repository contains code used for data processing, analysis, and inference in 'Abiotic influences on continuous conifer forest structure across a subalpine watershed' (Worsham et al., 2024). In the paper, we quantified the relative influence of climate, topographic, edaphic, and geologic factors on conifer stand structure and composition, and their functional relationships, in Colorado's East River watershed and adjacent drainages. We used waveform LiDAR data to derive spatially continuous stand structure metrics within this upper montane–subalpine domain. We fused these with a species-level classification map to estimate tree species abundance. We applied generalized additive and generalized boosted models to evaluate the covariability of structural and compositional metrics with abiotic variables.

The repository is complete in the sense that it includes all operations used in the analysis and reported in the paper. However, it does not include initial exploratory efforts or analytical dead-ends---approaches that failed or that we ultimately abandoned in favor of more reliable or efficient methods. Many of the operations, particularly those that process waveform LiDAR, require parallel implementation on large-scale, high-performance computing infrastructure because of data burden (1E+12 bytes) or algorithmic complexity. Operations for directly reproducing publication tables and figures will run on a personal computer. 


## Attribution

When using these models and/or the underlying data, please cite the associated publication and the archive version of this repository:

```
Worsham H M ; Wainwright H M; Powell T; Falco N; Kueppers L (2024): 'Abiotic influences on continuous conifer forest structure across a subalpine watershed' *Preprint*. DOI: TK

Worsham H M ; Wainwright H M; Powell T; Falco N; Kueppers L (2024): Code archive for 'Abiotic influences on continuous conifer forest structure across a subalpine watershed'. Code repository. DOI: TK

```

## Requirements

The bulk of the analysis runs in the R statistical computing environment and is configured for Apple Mac and Linux operating systems. R session specifications are in `./config/r_session_info`. The header of every R script in the repository contains a call to load a YAML configuration file, `./config/config.yml`. This file contains a list of packages and a list of local and external paths for ingesting data, models, and other requisite information and is required for any subsequent code in the script to run. On execution, R scripts will automatically try to install the required packages and their dependencies (if not already present) and load them in the global environment. 

## Replicating tables and figures

To replicate all tables and figures, open a `bash` shell. From the repository root, navigate to './inst/notebooks/tables_figures/' and run: 

```sh
chmod u+x code/replicate_all.sh
./replicate_all.sh
```

## Repository structure

The repository is organized much like a standard R package stored on the Comprehensive R Archive Network, following the 'Research Compendium' framework described in Marwick et al. (2018). In this framework, compendium components are separated into distinct classes, and a clear separation is maintained between data, analytical functions, analytical scripts implementing those functions, and output. The file structure in the root directory of this repository enforces this separation. A summary follows below, with a full directory tree and file-level metadata below that. 

- **config:** stores configuration files for environment setup and external data source management
- **data:** stores raw, intermediate, and final processed data output; note: because of the compute scale of this analysis, most input and output data are not stored in this repository but rather in external cloud storage buckets, from which they are ingested into memory on the fly
- **inst:** stores scripts for processing data, running analyses, and generating manuscript elements, including tables and figures
	- **ms:** stores source files for the publication manuscript; LaTEx source files for styles, references, and author metadata; and subdirectories containing final files for submitted tables and figures
		-**figures:** stores final output files for submitted tables and figures
		-**tables:** stores final output files for submitted tables and figures
	- **notebooks:** stores scripts used for data processing and analysis
		- **composition**: stores an R markdown file containing the full analytical workflow for subsetting individual tree crown (ITC) objects to top-of-canopy, assigning species to ITC objects, and computing performance statistics
		- **LiDAR**: stores multiple scripts for cleaning and processing waveform LiDAR data, discretizing waveforms to point cloud, training and testing individual tree crown detection (ITD) algorithms, implementing an optimal ITD algorithm for the full domain, estimating allometric equations, predicting species, and producing gridded estimates of forest structure
		- **misc**: stores multiple scripts for miscellaneous operations, including computing statistics describing the study domain and submitting data to a permanent ESS-DIVE repository
		- **regressions:** stores scripts for building model frames and estimating inferential models on explanatory and response variables
		- **state_factors:** contains scripts for processing and cleaning data used for explanatory features in inferential modeling
		- **tables_figures:** contains scripts for generating final tables and figures for the publication manuscript and supplementary information
	- **shell_scripts:** contains bash scripts for parallelizing analytical scripts on multiple nodes on a high-performance computing cluster and for data ingress/egress using rclone
- **man:** stores reference manual (man) pages describing the analytical functions in `./R`
- **models:** stores inferential model objects, typically an intermediate or final output of one or more analytical scripts
- **R:** stores R files containing generalized functions sourced in analytical scripts

### Directory tree

<details>
<summary>Click to unpack the full directory tree.</summary>

```shell
.
├── DESCRIPTION
├── LICENSE
├── NAMESPACE
├── R
│   ├── bipart.match.R
│   ├── bipart.match.plot.R
│   ├── bipart.match2.R
│   ├── bipart.match3.R
│   ├── build.models.R
│   ├── find.incompletes.R
│   ├── helpers.R
│   ├── layerstacking_funs.R
│   ├── li2012_funs.R
│   ├── lmfauto_funs.R
│   ├── lmffw_funs.R
│   ├── lmfvw_funs.R
│   ├── multichm_funs.R
│   ├── plot.gams.R
│   ├── points_to_raster.R
│   ├── ptrees_funs.R
│   ├── species.mapping.R
│   ├── stratified.R
│   └── watershed_funs.R
├── README.md
├── config
│   ├── config.yml
│   ├── pyenv-virtualenv_setup.sh
│   ├── pyenv_setup.sh
│   └── r_session_info
├── data
│   ├── intermediate
│   │   ├── explainer_names_table.csv
│   │   ├── gam_selection.md
│   │   └── ls_debugging
│   │       ├── ls+lmffill_filter80.png
│   │       ├── ls+lmffill_nofilter.png
│   │       ├── ls_complete_npoints.png
│   │       ├── ls_incomp2_npoints.png
│   │       ├── ls_incomplete_binary.png
│   │       ├── ls_incomplete_clusters_all.png
│   │       ├── ls_incomplete_clusters_gt100k.png
│   │       ├── ls_incomplete_clusters_gt150k.png
│   │       ├── ls_incomplete_clusters_gt50k.png
│   │       ├── ls_incomplete_clusters_gt80k.png
│   │       ├── ls_incomplete_npoints.png
│   │       ├── ls_ntrees_50m+remain_postmerge.png
│   │       └── ls_ntrees_50m+remain_premerge.png
│   ├── processed
│   └── raw
│       ├── all_variables.csv
│       ├── field_methods.csv
│       ├── gam_specs.csv
│       ├── itd_algorithms.csv
│       ├── layerstacking_params.csv
│       ├── match_criteria.csv
│       └── variable_definitions.csv
├── er-forest-structure.Rproj
├── inst
│   ├── dev
│   │   ├── 00_chunk_waveforms.py
│   │   ├── 00_debug_chunk.ipynb
│   │   ├── 00_debug_decomp.R
│   │   ├── 00_debug_deconvolution.R
│   │   ├── 00_debug_processwf.R
│   │   ├── 01_batch_process_all_waveforms.R
│   │   ├── 01_process_waveforms.R
│   │   ├── 02_compare_mylas_neonlas.R
│   │   ├── 02_itc_segmentation.R
│   │   ├── 04.00_gam.R
│   │   ├── 05.00_gbm.R
│   │   ├── 05.00_itc_traintest.R
│   │   ├── 05.00_itc_traintest.Rmd
│   │   ├── 05.00_itc_traintest_v2.R
│   │   ├── 05.13_check_detected_trees.R
│   │   ├── 06.00_predict_dbh.R
│   │   ├── 09.01_accuracy_assessment.R
│   │   ├── Canopy_Segmentation_Test_2020-06-17.R
│   │   ├── chunk_waveforms.py
│   │   ├── deconvolution.c
│   │   ├── pyenvi_explore.ipynb
│   │   ├── pyenvi_test.png
│   │   ├── pyenvi_test.py
│   │   ├── pylidar_explore.ipynb
│   │   ├── sfa_neonaop_drive_curl.py
│   │   ├── spectrum_deconvolution.py
│   │   ├── waveformlidar_vignette1.R
│   │   └── we.limitation.R
│   ├── ms
│   │   ├── ER_Forest_Structure_Paper_BetterBib.bib
│   │   ├── EastRiver_Forest_Structure_Paper_05.Rmd
│   │   ├── EastRiver_Forest_Structure_Paper_05.docx
│   │   ├── EastRiver_Forest_Structure_Paper_References.Rmd
│   │   ├── EastRiver_Forest_Structure_Paper_SI.Rmd
│   │   ├── EastRiver_Forest_Structure_Paper_SI.docx
│   │   ├── EastRiver_Forest_Structure_Paper_TODO.md
│   │   ├── EastRiver_Forest_Structure_Paper_TablesFigures.md
│   │   ├── Fig_Interp.xlsx
│   │   ├── RSE_Submission_Guidelines.md
│   │   ├── author-info-blocks.lua
│   │   ├── dropped_sentences.txt
│   │   ├── figures
│   │   │   ├── Fig1.pdf
│   │   │   ├── Fig1.png
│   │   │   ├── Fig10.pdf
│   │   │   ├── Fig10.png
│   │   │   ├── Fig2.pdf
│   │   │   ├── Fig2.png
│   │   │   ├── Fig3.pdf
│   │   │   ├── Fig3.png
│   │   │   ├── Fig4.pdf
│   │   │   ├── Fig4.png
│   │   │   ├── Fig5.pdf
│   │   │   ├── Fig5.png
│   │   │   ├── Fig6.pdf
│   │   │   ├── Fig6.png
│   │   │   ├── Fig7.pdf
│   │   │   ├── Fig7.png
│   │   │   ├── Fig8.pdf
│   │   │   ├── Fig8.png
│   │   │   ├── Fig9.pdf
│   │   │   ├── Fig9.png
│   │   │   ├── FigS1.pdf
│   │   │   ├── FigS1.png
│   │   │   ├── FigS2.pdf
│   │   │   ├── FigS2.png
│   │   │   ├── FigS3.pdf
│   │   │   ├── FigS3.png
│   │   │   ├── FigS3A.pdf
│   │   │   ├── FigS3A.png
│   │   │   ├── FigS4.pdf
│   │   │   ├── FigS4A.png
│   │   │   ├── FigS4B.png
│   │   │   ├── FigS4C.png
│   │   │   ├── FigS4D.png
│   │   │   ├── FigS4E.png
│   │   │   ├── FigS4F.png
│   │   │   ├── FigS4G.png
│   │   │   └── FigS4H.png
│   │   ├── outline
│   │   │   ├── ER_Forest_Structure_Paper_BrainDump.md
│   │   │   ├── ER_Forest_Structure_Paper_KeySources.md
│   │   │   ├── ER_Forest_Structure_Paper_Story_Summary.md
│   │   │   └── EastRiver_Forest_Structure_Paper_IntroNotes.md
│   │   ├── remote-sensing-of-environment.csl
│   │   ├── scholarly-metadata.lua
│   │   ├── styles.docx
│   │   ├── tables
│   │   │   ├── tbl1.svg
│   │   │   ├── tbl2.svg
│   │   │   ├── tbl3.svg
│   │   │   ├── tbls1.svg
│   │   │   ├── tbls2.svg
│   │   │   ├── tbls3.svg
│   │   │   ├── tbls4.svg
│   │   │   ├── tbls5.svg
│   │   │   ├── tbls6.svg
│   │   │   ├── tbls7.svg
│   │   │   └── tbls8.svg
│   │   ├── v01
│   │   │   ├── EastRiver_Forest_Structure_Outline_01.html
│   │   │   ├── EastRiver_Forest_Structure_Outline_01.md
│   │   │   ├── EastRiver_Forest_Structure_Paper_01.Rmd
│   │   │   ├── EastRiver_Forest_Structure_Paper_01.docx
│   │   │   ├── EastRiver_Forest_Structure_Paper_01.log
│   │   │   ├── EastRiver_Forest_Structure_Paper_01.pdf
│   │   │   ├── EastRiver_Forest_Structure_Paper_01.tex
│   │   │   └── Figures
│   │   │       ├── 90p_height.png
│   │   │       ├── A_elevation.png
│   │   │       ├── Fig2.png
│   │   │       ├── Fig3.png
│   │   │       ├── Fig4.png
│   │   │       ├── Fig8.png
│   │   │       ├── Fig9.png
│   │   │       ├── ba_contour.png
│   │   │       ├── basal_area.png
│   │   │       ├── dens_contour.png
│   │   │       ├── density.png
│   │   │       ├── diam_distro.png
│   │   │       ├── er_location.png
│   │   │       ├── height_contour.png
│   │   │       ├── height_distro_spp.png
│   │   │       ├── noisy_waveform.png
│   │   │       ├── qmd.png
│   │   │       ├── qmd_contour.png
│   │   │       ├── qmd_cut.png
│   │   │       ├── responses.png
│   │   │       ├── snow_soil_data.pptx
│   │   │       ├── structure_stacks.pptx
│   │   │       └── studydomain.png
│   │   ├── v02
│   │   │   ├── EastRiver_Forest_Structure_Paper_02.Rmd
│   │   │   ├── EastRiver_Forest_Structure_Paper_02.docx
│   │   │   ├── EastRiver_Forest_Structure_Paper_02.log
│   │   │   ├── EastRiver_Forest_Structure_Paper_02.tex
│   │   │   └── Figures
│   │   │       ├── Fig1.png
│   │   │       ├── Fig5.png
│   │   │       ├── Fig6.pdf
│   │   │       ├── Fig6.png
│   │   │       ├── Fig7.png
│   │   │       ├── Screen Shot 2023-10-25 at 12.03.25 PM.png
│   │   │       ├── gbm_importance_01.png
│   │   │       ├── gbm_importance_02.png
│   │   │       ├── gbm_importance_03.png
│   │   │       ├── gbm_importance_04.png
│   │   │       ├── ls_perform.png
│   │   │       ├── match_agreement.png
│   │   │       ├── max_height_comp.png
│   │   │       ├── performance_comp.png
│   │   │       ├── relative_influence.png
│   │   │       ├── sp_QMD.png
│   │   │       ├── sp_ba.png
│   │   │       ├── sp_density.png
│   │   │       ├── sp_height.png
│   │   │       └── sp_heightskew.png
│   │   ├── v03
│   │   │   ├── EastRiver_Forest_Structure_Paper_03.Rmd
│   │   │   └── EastRiver_Forest_Structure_Paper_03.docx
│   │   └── v04
│   │       ├── EastRiver_Forest_Structure_Paper_04.Rmd
│   │       └── EastRiver_Forest_Structure_Paper_04.docx
│   ├── notebooks
│   │   ├── LiDAR
│   │   │   ├── 00_chunkwf.py
│   │   │   ├── 00_count_wf.R
│   │   │   ├── 00_find_flightpaths_by_plot.R
│   │   │   ├── 00_make_poly_for_missing_flightpaths.R
│   │   │   ├── 00_subset_flightpaths_to_forest.R
│   │   │   ├── 00_synthetic_return.R
│   │   │   ├── 00_unzipwf.py
│   │   │   ├── 00_waveformlidar_explore.R
│   │   │   ├── 01.00_process_all_waveforms.R
│   │   │   ├── 01.01_process_waveforms_atplots.R
│   │   │   ├── 01.02_checklogs.R
│   │   │   ├── 01.03_cleanlogs.R
│   │   │   ├── 02.00_regrid_points.R
│   │   │   ├── 02.01_hyperpointcloud.R
│   │   │   ├── 02.02_points_to_las.R
│   │   │   ├── 02.03_regrid_LAScatalog.R
│   │   │   ├── 02.04_downsample_las.R
│   │   │   ├── 02.05.01_regridclip_neon_LAScatalog.R
│   │   │   ├── 02.05_regrid_neon_LAScatalog.R
│   │   │   ├── 02.06_regrid_points_checksums.R
│   │   │   ├── 02.07_pointcount_heatmap.R
│   │   │   ├── 03.00_normalize_points.R
│   │   │   ├── 03.01_filter_abg_points.R
│   │   │   ├── 03.01_normalize_points_missing_fp.R
│   │   │   ├── 04.00_decimate_points.R
│   │   │   ├── 05.00_itc_traintest_loadup.R
│   │   │   ├── 05.01_itc_li.R
│   │   │   ├── 05.02_itc_pt.R
│   │   │   ├── 05.03_itc_ls.R
│   │   │   ├── 05.04_itc_mc.R
│   │   │   ├── 05.05_itc_lmf_fw.R
│   │   │   ├── 05.06_itc_lmf_vw.R
│   │   │   ├── 05.07_itc_lmfauto.R
│   │   │   ├── 05.08_itc_ws.R
│   │   │   ├── 05.09_itc_model_selection.R
│   │   │   ├── 05.10_run_optimal_itd.R
│   │   │   ├── 05.11_optimal_itd_performance.R
│   │   │   ├── 06.00_detect_trees_full_watershed.R
│   │   │   ├── 06.01_detect_trees_missing_fp.R
│   │   │   ├── 06.02_check_detected_trees.R
│   │   │   ├── 07.00_predict_dbh.R
│   │   │   ├── 07.01_mask_trees.R
│   │   │   ├── 07.02_make_chm.R
│   │   │   ├── 07.03_predict_species.R
│   │   │   ├── 08.00_make_rasters.R
│   │   │   ├── 09.01_detection_sumstats.R
│   │   │   ├── requirements.txt
│   │   │   └── waveform
│   │   │       ├── __init__.py
│   │   │       └── chunk_waveforms.py
│   │   ├── composition
│   │   │   └── ER_Forest_Structure_Species_Mapping.Rmd
│   │   ├── misc
│   │   │   ├── er_climate_means.R
│   │   │   ├── er_wilderness_area.R
│   │   │   ├── ess-dive_submit_pubdata.ipynb
│   │   │   └── ess-dive_submit_wfdata.ipynb
│   │   ├── regressions
│   │   │   ├── 00.00_plot_rasters.R
│   │   │   ├── 01.00_stats_ingest_data.R
│   │   │   ├── 02.00_corrmat.R
│   │   │   ├── 03.00_ols.R
│   │   │   ├── 04.00_gam.R
│   │   │   ├── 04.01_gam_reports.R
│   │   │   ├── 05.00_gbm.R
│   │   │   └── 05.01_gbm_reports.R
│   │   ├── state_factors
│   │   │   ├── aso_swe_processing.R
│   │   │   ├── geology_processing.R
│   │   │   ├── polaris_processing.R
│   │   │   └── ssurgo_ingest.R
│   │   └── tables_figures
│   │       ├── fig10_geology.R
│   │       ├── fig1_domain.R
│   │       ├── fig2_crownmap.R
│   │       ├── fig3_itd_performance.R
│   │       ├── fig4_height_distribution.R
│   │       ├── fig5_structure_metrics.R
│   │       ├── fig6_structure_histograms.R
│   │       ├── fig7_gbm.R
│   │       ├── fig8_gam_partial_effects.R
│   │       ├── fig9_gam_interactions.R
│   │       ├── replicate_all.sh
│   │       ├── si_figs.R
│   │       ├── si_tables.R
│   │       ├── table1.R
│   │       ├── table2.R
│   │       └── table3.R
│   └── shell_scripts
│       ├── batchcluster_init.sh
│       ├── batchcluster_init_li.sh
│       ├── batchcluster_init_lmfauto.sh
│       ├── batchcluster_init_lmffw.sh
│       ├── batchcluster_init_lmfvw.sh
│       ├── batchcluster_init_ls.sh
│       ├── batchcluster_init_makehpc.sh
│       ├── batchcluster_init_mc.sh
│       ├── batchcluster_init_pt.sh
│       ├── batchcluster_init_ws.sh
│       ├── chunk_waveforms.sh
│       ├── findtrees_init.sh
│       ├── ipcluster_init.sh
│       ├── make_findtrees.sh
│       ├── multibatch_findtrees_init.sh
│       ├── multibatch_processwf_init.sh
│       ├── multibatch_traintest_init.sh
│       ├── processwfpll.sh
│       ├── rclone_checksums2drive.sh
│       ├── rclone_copy_drive.sh
│       ├── rclone_fullwf2drive.sh
│       ├── rclone_fullwf2gcp.sh
│       ├── rclone_gridreturns2drive.sh
│       ├── rclone_hpc2drive.sh
│       ├── rclone_imgs2drive.sh
│       ├── rclone_itcresults2drive.sh
│       ├── rclone_las2drive.sh
│       ├── rclone_lasabg2drive.sh
│       ├── rclone_lasdecim2drive.sh
│       ├── rclone_lasdownsampled2drive.sh
│       ├── rclone_lasnorm2drive.sh
│       ├── rclone_lasregrid2drive.sh
│       ├── rclone_logs2drive.sh
│       ├── rclone_missingfp2drive.sh
│       ├── rclone_neonlas2drive.sh
│       ├── rclone_returns2drive.sh
│       ├── rclone_trees2drive.sh
│       ├── srun_init.sh
│       ├── targz_bigdir.sh
│       └── unzip_wf.sh
├── man
└── models
    ├── abla_density_gam.rda
    ├── abla_density_gbm.rda
    ├── ba_gam.rda
    ├── ba_gbm.rda
    ├── density_gam.rda
    ├── density_gbm.rda
    ├── diam_gam.rda
    ├── diam_gbm.rda
    ├── height_95p_gam.rda
    ├── height_95p_gbm.rda
    ├── height_skew_gam.rda
    ├── height_skew_gbm.rda
    ├── pico_density_gam.rda
    ├── pico_density_gbm.rda
    ├── pien_density_gam.rda
    └── pien_density_gbm.rda
```  
</details>

### File-level documentation

| Directory            | File                                                               | Description                                                                                                                                                                                                                        | Input Directory                                          | Input Filepath                                               | Output Filepath                                              | Output Filetype | Output Description                                                                                                                                               | Output File Count | Output Directory Size |     |
| -------------------- | ------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ | --------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------- | --------------------- | --- |
| config               | config.yml                                                         | YAML configuration file storing a list of packages and a list of local and external paths for ingesting data, models, and other requisite information                                                                              | NA                                                       | NA                                                           | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    | NA  |
|                      | r_session_info                                                     | Text file storing R session information for the original development enviroment for this repository                                                                                                                                | NA                                                       | NA                                                           | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    | NA  |
| data/intermediate    | explainer_names_table.csv                                          | Comma-separated values file storing variable names in multiple formats for tables and plotting                                                                                                                                     | NA                                                       | NA                                                           | NA                                                           | NA              | NA                                                                                                                                                               |                   |                       |     |
| data/processed       | NA                                                                 | NA                                                                                                                                                                                                                                 | NA                                                       | NA                                                           | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    | NA  |
| data/raw             | NA                                                                 | NA                                                                                                                                                                                                                                 | NA                                                       | NA                                                           | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    | NA  |
| inst/notebooks/LiDAR | 00_chunkwf.py                                                      | Splits large waveform binary files into chunks of n waveforms each                                                                                                                                                                 | waveform_binary                                          |                                                              | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY                             | directory       | directory containing ENVI binary waveform files subset to chunks of 1e5 waveforms                                                                                | 14067             | 3.67E+02              |     |
|                      | 00_chunkwf.py                                                      | Splits large waveform binary files into chunks of n waveforms each                                                                                                                                                                 | waveform_binary                                          |                                                              | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY                             | directory       | directory containing ENVI binary waveform files subset to chunks of 1e6 waveforms                                                                                | 1448              | NA                    |     |
|                      | 01.00_process_all_waveforms.R                                      | Deconvolves and decomposes waveforms within a buffer of x meters around each plot; writes geolocated returns as csv                                                                                                                | waveform_binary_chunks                                   | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY                             | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY_returnpoints                | csv             | geolocated returns for a given flightpath in csv format)                                                                                                         | 14061             | 3.18E+02              |     |
|                      | 01.01_process_waveforms_at_plots.R                                 | Deconvolves and decomposes all waveforms; writes geolocated returns as csv                                                                                                                                                         | waveform_binary_chunks                                   | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY                             | AA-BBBN_2018_CRBU_1_2018MMDDFN_FLXXX-YYY_returnpoints        | csv             | geolocated returns within plot boundaries in csv format)                                                                                                         | 118               | 1.03E-01              |     |
|                      | 02.00_regrid_points.R                                              | DEPRECATED. Regrids geolocated points into equal-area grid cells of size mXn meters; writes waveforms corresponding to each grid cell to a csv; deprecated because the readLASCatalog function in lidR creates a smoother workflow | geolocated_returns                                       | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY_returnpoints                | ER_gridded_returns_NNN                                       | csv             | geolocated returns within a given grid cell                                                                                                                      | 555               | NA                    |     |
|                      | 02.01_hyperpointcloud.R                                            | Creates a hyperpointcloud (Zhou 2019) from ungridded, often overlapping geolocated returns; writes a hyperpointcloud for each flightpath as csv                                                                                    | geolocated_returns, waveform_binary_chunks               | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY_returnpoints                | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY_hpc                         | csv             | hyperpointcloud for a given flightpath                                                                                                                           | 14061             | NA                    |     |
|                      | 02.02_points_to_las.R                                              | Converts csv files with geolocated returns to a LAS point cloud using rlas                                                                                                                                                         | hyperpointcloud                                          | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY_hpc                         | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY_hpc                         | las             | hyper-dense las derived from hyperpointcloud per flightpath                                                                                                      | NA                | NA                    |     |
|                      | 02.03_regrid_LAScatalog                                            | Regrids overlapping las flies into uniform grid of dimension mXn (500x500); writes compressed .laz file per new grid cell                                                                                                          | las_ungridded                                            | 2018_CRBU_1_2018MMDDFN_FLXXX-YYY_hpc                         | las_regridded_XXXXXX_YYYYYYY                                 | laz             | hyper-dense laz regridded from ungridded flightpaths                                                                                                             | 1632              | 1.01E+03              |     |
|                      | 02.04_downsample_las.R                                             | Downsamples hyper-dense las file by preserving all peaks and sampling 1/n of the remaining points                                                                                                                                  | las_regridded2                                           | las_regridded_XXXXXX_YYYYYYY                                 | las_downsampled_XXXXXX_YYYYYYY                               | laz             | less-dense laz downsampled from hyper-dense laz per grid cell                                                                                                    | 1627              | 1.77E+02              |     |
|                      | 02.05_regrid_neon_LAScatalog.R                                     | DEPRECATED. Regrids original NEON point cloud to match resolution of regridded hyperpointcloud-derived LAS                                                                                                                         | neon_las                                                 | NEON_D13_CRBU_DP1_L0XY-Z_2018MMDDFN_unclassified_point_cloud | neon_las_regridded_XXXXXX_YYYYYYY                            | laz             | regridded laz from moderate-density neon point cloud                                                                                                             | NA                | 2.53E+01              |     |
|                      | 02.05.01_regridclip_neon_LAScatalog                                | DEPRECATED Regrids original NEON point cloud for missing flightpaths to match resolution of regridded hyperpointcloud-derived LAS                                                                                                  | neon_las                                                 | NEON_D13_CRBU_DP1_L0XY-Z_2018MMDDFN_unclassified_point_cloud | neon_gaps_las_regridded_XXXXXX_YYYYYYY                       | laz             | regridded laz from moderate-density neon point cloud to fill gaps in full waveform                                                                               | NA                | NA                    |     |
|                      | 02.06_regrid_points_checksums                                      | DEPRECATED. Checks data loss after running 02.00_regrid_points.R on geolocated_returns                                                                                                                                             | geolocated_returns; gridded_returns                      | NA                                                           | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    |     |
|                      | 02.07_pointcount_heatmap.R                                         | DEPRECATED. Plots heatmap of point counts per file in gridded_returns. Deprecated in favor of native lidR lasCatalog plotting.                                                                                                     | gridded returns                                          | NA                                                           | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    |     |
|                      | 03.00_normalize_points                                             | Performs ground normalization on LAS catalog                                                                                                                                                                                       | las_downsampled                                          | las_downsampled_XXXXXX_YYYYYYY                               | las_norm_XXXXXX_YYYYYYY                                      | laz             | normalized laz from downsampled laz                                                                                                                              | 1615              | 1.77E+02              |     |
|                      | 03.01_filter_abg_points                                            | Filters above-ground points (Z>0)                                                                                                                                                                                                  | las_normalized                                           | las_norm_XXXXXX_YYYYYYY                                      | las_abg_XXXXXX_YYYYYY                                        | laz             | Z>0 filtered laz from normalized laz                                                                                                                             | 1612              | 5.23E+01              |     |
|                      | 04.00_decimate_points                                              | Decimates las catalog to homogenize sample density across watershed                                                                                                                                                                | las_normalized                                           | las_norm_XXXXXX_YYYYYYY                                      | las_decimated_XXXXXX_YYYYYYY                                 | laz             | decimated laz from normalized laz                                                                                                                                | 1617              | 4.14E+01              |     |
|                      | Multiple scripts                                                   | Multiple scripts write to log files                                                                                                                                                                                                | Multiple input directories                               | Multiple input filepaths                                     | Multiple output filepaths                                    | txt             | data processing logs                                                                                                                                             | 15                | 3.20E-02              |     |
|                      | 05.00_traintest_loadup                                             | Initializes data for ITC training and testing, sourced in 05.01:05.08                                                                                                                                                              | las_decimated; Drive:EastRiver_Census1_Data_Collated.csv | las_decimated_XXXXXX_YYYYYY;                                 | NA; held in memory                                           | NA              | in-memory data for ITC detection optimization                                                                                                                    | NA                | NA                    |     |
|                      | 05.01_itc_li.R                                                     | Runs optimization procedure on ITC detection using Li 2012 algorithm                                                                                                                                                               | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | li_itd_results.csv                                           | csv; png        | csv of traintest results; pngs of agreement figures                                                                                                              | 1; 114688         | 4.88E-4; 6.7E0        |     |
|                      | 05.02_itc_pt.R                                                     | Runs optimization procedure on ITC detection using PTrees algorithm                                                                                                                                                                | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | pt_itd_results.csv                                           | csv; png        | csv of traintest results; pngs of agreement figures                                                                                                              | 1; many           | 4.88E-4; 6.7E0        |     |
|                      | 05.03_itc_ls.R                                                     | Runs optimization procedure on ITC detection using LayerStacking algorithm                                                                                                                                                         | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | ls_itd_results.csv                                           | csv; png        | csv of traintest results; pngs of agreement figures                                                                                                              | 1; many           | 4.88E-4; 6.7E0        |     |
|                      | 05.04_itc_mc.R                                                     | Runs optimization procedure on ITC detection using MultiCHM algorithm                                                                                                                                                              | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | mc_itd_results.csv                                           | csv; png        | csv of traintest results; pngs of agreement figures                                                                                                              | 1; many           | 4.88E-4; 6.7E0        |     |
|                      | 05.05_itc_lmf_fw.R                                                 | Runs optimization procedure on ITC detection using LMF-fixed window algorithm                                                                                                                                                      | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | lmffw_itd_results.csv                                        | csv; png        | csv of traintest results; pngs of agreement figures                                                                                                              | 1; many           | 4.88E-4; 6.7E0        |     |
|                      | 05.06_itc_lmf_vw.R                                                 | Runs optimization procedure on ITC detection using LMF-variable window algorithm                                                                                                                                                   | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | lmfvw_itd_results.csv                                        | csv; png        | csv of traintest results; pngs of agreement figures                                                                                                              | 1; many           | 4.88E-4; 6.7E0        |     |
|                      | 05.07_itc_lmfauto.R                                                | Runs optimization procedure on ITC detection using LMFAuto algorithm                                                                                                                                                               | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | lmfauto_itd_results.csv                                      | csv; png        | csv of traintest results; pngs of agreement figures                                                                                                              | 1; many           | 4.88E-4; 6.7E0        |     |
|                      | 05.08_itc_ws.R                                                     | Runs optimization procedure on ITC detection using watershed segmentation algorithm                                                                                                                                                | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | ws_itd_results.csv                                           | csv; png        | csv of traintest results; pngs of agreement figures                                                                                                              | 1; many           | 4.88E-4; 6.7E0        |     |
|                      | 05.09_itc_model_selection.R                                        | Parses optimization output to identify model and parameter set with least loss or highest f                                                                                                                                        | itd_results                                              | [model]_itd_results.csv                                      | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    |     |
|                      | 05.10_run_optimal_itd.R                                            | Runs ITC detection using optimal algorithm and parameters determined in 05.10 for reporting and figures                                                                                                                            | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | AA-BBBN_opt_trees.shp                                        | shp             | shapefiles of trees detected through optimal ITC procedure by training site                                                                                      | 68                | 3.28E-04              |     |
|                      | 05.10_run_optimal_itd.R                                            | Runs ITC detection using optimal algorithm and parameters determined in 05.10 for reporting and figures                                                                                                                            | NA; sources in-memory data from 05.00_traintest_loadup   | NA                                                           | opt_matches.csv                                              | csv             | csv of matched trees; includes observations all field-observed and lidar-detected trees with associated match data for each source, which yields some duplicates | 1                 | 3.28E-04              |     |
|                      | 05.11_optimal_itd_performance                                      | Generates performance statistics and figures for reporting                                                                                                                                                                         | itd_results; itd_results/opt_trees                       | opt_matches.csv; AA-BBBN_opt_trees.shp                       | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    |     |
|                      | 05.11_optimal_itd_performance                                      | Generates performance statistics and figures for reporting; generates CHM for example site                                                                                                                                         | las_decimated                                            | las_decimated_XXXXXX_YYYYYY;                                 | uc2_chm.tif                                                  | tif             | tif of CHM for example site (CC-UC2)                                                                                                                             | NA                | 3.00E-03              |     |
|                      | 06.00_detect_trees_full_watershed.R                                | Detects trees across the full watershed using optimal ITC algorithm and parameters                                                                                                                                                 | las_decimated                                            | las_decimated_XXXXXX_YYYYYY;                                 | trees_XXXXXX_YYYYYY                                          | shp             | shapefiles of trees detected through optimal ITC procedure per unique 50m grid cell identifier                                                                   | NA                | 7.00E-01              |     |
|                      | 06.01_detect_trees_missing_fp.R                                    | DEPRECATED. Not using missing filepath data                                                                                                                                                                                        | NA                                                       | NA                                                           | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    |     |
|                      | dev_code/05.13_check_detected_trees.R                              | Checks detected trees across full watershed to find incomplete or missing output; resamples to 100m grid                                                                                                                           | trees_ls_50m                                             | trees_XXXXXX_YYYYYY                                          | trees_XXXXXX_YYYYYY                                          | shp             | shapefiles of trees detected through optimal ITC procedure per unique 100m grid cell identifier                                                                  | NA                | 7.35E-01              |     |
|                      | 06.02_check_detected_trees.R                                       | Checks detected trees across full watershed to find incomplete or missing output                                                                                                                                                   | trees_ls_100m                                            | trees_XXXXXX_YYYYYY                                          | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    |     |
|                      | 07.00_predict_dbh.R                                                | Estimates DBH of detected tree objects and exports .shp to .csv                                                                                                                                                                    | trees_ls_100m                                            | trees_XXXXXX_YYYYYY                                          | trees_XXXXXX_YYYYYY                                          | csv             | csvs of trees detected through optimal ITC procedure with DBH and BA estimates by unique 100m grid cell identifier                                               | 37408             | 2.90E+00              |     |
|                      | 07.01_mask_rasters_conifer.R                                       | OUTPUT DEPRECATED. NOW MASK TO 5M. Masks detected tree objects to conifers and non-developed areas and writes a big CSV of all trees for rasterizing                                                                               | tifs; trees_ls_100m_csv                                  | trees_XXXXXX_YYYYYY                                          | trees_masked_100m.csv                                        | csv             | csv of detected trees masked to conifers and non-developed areas                                                                                                 | 1                 | 4.10E+00              |     |
|                      | 07.01_mask_rasters_conifer.R                                       | Masks detected tree objects to conifer and non-developed areas and writes a big CSV of all trees for rasterizing                                                                                                                   | tifs; trees_ls_100m_csv                                  | trees_XXXXXX_YYYYYY                                          | trees_masked_5m.csv                                          | csv             | csv of detected trees masked to conifers and non-developed areas                                                                                                 | 1                 | 4.10E+00              |     |
|                      | 07.01_mask_rasters_conifer.R                                       | OUTPUT DEPRECATED. NOW MASK CONIFERS AND OTHER AREAS TO 5M. Masks detected tree objects to conifers and writes a big CSV of all trees for rasterizing                                                                              | tifs; trees_ls_100m_csv                                  | trees_XXXXXX_YYYYYY                                          | trees_conif_100m.csv                                         | csv             | csv of detected trees masked to conifers                                                                                                                         | 1                 | 4.10E+00              |     |
|                      | 07.02_make_CHM.R                                                   | Creates CHM from las catalog                                                                                                                                                                                                       | las_decimated                                            | las_decimated_XXXXXX_YYYYYY;                                 | chm_smooth; chm_smooth_masked.tif                            | tif             | tif of chm                                                                                                                                                       | NA                | 7.60E+00              |     |
|                      | 07.03_predict_species.R                                            | Extracts species from Falco et al. classification raster and assigns to tree objects >= 90th pctl height                                                                                                                           | trees_ls_100m_csv; Drive:species_classification          | trees_XXXXXX_YYYYYY                                          | trees_XXXXXX_YYYYYY_species.csv                              | csv             | csvs of 90th pctl height trees detected through optimal ITC procedure with species classification, DBH and BA estimates by unique 100m grid cell identifier      | 21818             | 2.26E-01              |     |
|                      | 07.03_predict_species.R                                            | OUTPUT DEPRECATED. NOW MASK TO 5M. Extracts species from Falco et al. classification raster and assigns to tree objects >= 90th pctl height                                                                                        | trees_ls_100m_csv; Drive:species_classification          | NEON_ER_4ws_species_map.tif                                  | trees_species.csv                                            | csv             | csv of detected trees >90th pctl height with species                                                                                                             | 1                 | 4.10E+00              |     |
|                      | 07.03_predict_species.R                                            | Extracts species from Falco et al. classification raster and assigns to tree objects >= 90th pctl height                                                                                                                           | trees_ls_100m_csv; Drive:species_classification          | NEON_ER_4ws_species_map.tif                                  | trees_species_masked_5m.csv                                  | csv             | csv of detected trees >90th pctl height, masked to conifers and non-developed areas, with species                                                                | 1                 | 4.10E+00              |     |
|                      | 08.00_make rasters.R                                               | Creates png and tif files of gridded forest structure metrics                                                                                                                                                                      | trees_ls_100m_csv                                        | trees_XXXXXX_YYYYYY                                          | [parameter]                                                  | tif             | tif of gridded forest structure metrics                                                                                                                          | 21                | 1.50E-03              |     |
|                      | 08.00_make rasters.R                                               | Creates png and tif files of gridded forest structure metrics                                                                                                                                                                      | trees_ls_100m_csv                                        | trees_XXXXXX_YYYYYY                                          | [parameter]                                                  | tif             | tif of gridded forest structure metrics                                                                                                                          | 21                | 1.50E-03              |     |
|                      | 09.01_detection_sumstats.R                                         | Generates summary statistics and figures from field inventory and ITC detection for reporting on tree detection performance                                                                                                        | ~                                                        | trees_masked_100m.csv                                        | NA                                                           | NA              | NA                                                                                                                                                               | NA                | NA                    |     |
|                      | checksums                                                          | Multiple scripts output checksum data                                                                                                                                                                                              | Multiple input directories                               | Multiple input filepaths                                     | Multiple output filepaths                                    | txt             | delimited checksum tables                                                                                                                                        | 31                | 1.55E-04              |     |
|                      | NA                                                                 | DEPRECATED? Not sure where this came from                                                                                                                                                                                          |                                                          |                                                              |                                                              |                 |                                                                                                                                                                  | 3                 | 1.20E-04              |     |
|                      | NA                                                                 | DEPRECATED? Not sure where this came from                                                                                                                                                                                          |                                                          |                                                              |                                                              |                 |                                                                                                                                                                  | 4                 | 9.30E-05              |     |
|                      | NA                                                                 | Not sure where this came from but necessary for filling missing flightpath                                                                                                                                                         |                                                          |                                                              |                                                              |                 |                                                                                                                                                                  | 5                 | 2.05E-06              |     |
|                      | NA                                                                 |                                                                                                                                                                                                                                    |                                                          |                                                              |                                                              |                 |                                                                                                                                                                  |                   | 4.50E-08              |     |
|                      | NA                                                                 |                                                                                                                                                                                                                                    |                                                          |                                                              |                                                              |                 |                                                                                                                                                                  |                   | 4.50E-08              |     |
|                      | NA                                                                 |                                                                                                                                                                                                                                    |                                                          |                                                              |                                                              |                 |                                                                                                                                                                  |                   | 4.50E-08              |     |
|                      | 04.00_gam                                                          | Fits GAMs to structure metrics and explanatory data                                                                                                                                                                                | tifs                                                     | AAA.tif                                                      | AAA_gam_performance.txt                                      | txt             | Performance metrics and coefficient tables for GAMs                                                                                                              | NA                | 8.51E-05              |     |
|                      | 05.00_gbm                                                          | Fits GBMs to structure metrics and explanatory data                                                                                                                                                                                | tifs                                                     | AAA.tif                                                      | AAA_gbm_performance.txt                                      | txt             | Performance metrics and tables for GBMs                                                                                                                          | NA                | 4.39E-05              |     |
|                      | NA                                                                 |                                                                                                                                                                                                                                    |                                                          |                                                              |                                                              |                 |                                                                                                                                                                  |                   | 1.33E+01              |     |
|                      | NA                                                                 |                                                                                                                                                                                                                                    |                                                          |                                                              |                                                              |                 |                                                                                                                                                                  |                   | 1.10E+00              |     |
|                      | NA                                                                 | DEPRECATED. Not used downstream                                                                                                                                                                                                    | NA                                                       | NA                                                           | NA                                                           | NA              | NA                                                                                                                                                               | NA                | 6.60E-02              |     |
|                      | 04.00_gam                                                          | Fits GAMs to structure metrics and explanatory data                                                                                                                                                                                | tifs                                                     | AAA.tif                                                      | AAA.Rda                                                      | rda             | RDA model objects for GAMs                                                                                                                                       | 8                 | 2.45E-01              |     |
|                      | 05.00_gbm                                                          | Fits GBMs to structure metrics and explanatory data                                                                                                                                                                                | tifs                                                     | AAA.tif                                                      | BBB.Rda                                                      | rda             | RDA model objects for GAMs                                                                                                                                       | 8                 | 8.60E-01              |     |
|                      | 05.00_gbm                                                          | Fits GBMs to structure metrics and explanatory data                                                                                                                                                                                | tifs                                                     | AAA.tif                                                      | gbm_relative_influence.csv                                   | csv             | ranked relative influence of each variable in GBMs                                                                                                               | 1                 | 5.00E-06              |     |
|                      | NA                                                                 | NA                                                                                                                                                                                                                                 | NA                                                       | NA                                                           | EastRiver_Census1_Data_Collated.csv                          | csv             | collated census 1 field inventory data                                                                                                                           | 1                 | 2.00E-03              |     |
|                      | NA                                                                 | NA                                                                                                                                                                                                                                 | NA                                                       | NA                                                           | aop_naip_ortho_3m.tif                                        | tif             | NAIP orthoimagery intersecting AOP boundary                                                                                                                      | 1                 | 3.28E-01              |     |
|                      | NA                                                                 | NA                                                                                                                                                                                                                                 | NA                                                       | NA                                                           | EastRiver_AOP_2018_Boundary.tar.gz                           | shp             | boundary of AOP domain                                                                                                                                           | 1                 | 1.14E-04              |     |
|                      | NA                                                                 | NA                                                                                                                                                                                                                                 | NA                                                       | NA                                                           | Kueppers_EastRiver_AllPlots_Centroid_2023_WGS84UTM13N.tar.gz | shp             | centroids of plot locations                                                                                                                                      | 1                 | 5.10E-06              |     |
|                      | NA                                                                 | NA                                                                                                                                                                                                                                 | NA                                                       | NA                                                           | Kueppers_EastRiver_AllPlots_Polygon_2023_WGS84UTM13N.tar.gz  | shp             | polygons of plot boundaries                                                                                                                                      | 1                 | 6.30E-06              |     |
|                      | NA                                                                 | NA                                                                                                                                                                                                                                 | NA                                                       | NA                                                           | all_variables_unscaled.csv                                   | csv             | csv dataframe of all explanatory and response variables in raw unscaled values                                                                                   | 1                 | 8.00E-03              |     |
|                      | NA                                                                 |                                                                                                                                                                                                                                    |                                                          |                                                              | tblA.csv                                                     | csv             | csv of qualitative data for publication tables                                                                                                                   | 1                 | 4.30E-07              |     |
|                      | inst/notebooks/composition/ER_Forest_Structure_Species_Mapping.Rmd | Tests several approaches to classifying top-of-canopy trees                                                                                                                                                                        |                                                          |                                                              |                                                              |                 |                                                                                                                                                                  | 1                 |                       |     |

## References

Marwick B; Boettiger C; Mullen L (2018): Packaging Data Analytical Work Reproducibly Using R (and Friends). The American Statistician, 72(1), 80–88. https://doi.org/10.1080/00031305.2017.1375986
