# dbs_dataMANA
Central hub of data management for deep brain stimulation (DBS) data of Parkinson's disease (PD) patients I work with at First Faculty of Medicine, Charles University, Prague.

The goal is to take-in all kind of disparate data sets (primarily from the iTEMPO REDCap database though) and spit out item-level as well as sum-score data of response variables being investigated (so far this includes (MDS-)UPDRS III motor scale, DRS-2 dementia screeing,  BDI-II depressive symptoms self-report, STAIX anxiety symptoms self-report, and FAQ and PDAQ instrumental activities of daily living self-report).

After extracting and wrangling the data, they are distributed for analysis to following repositories:

  - [connREPLI](https://github.com/josefmana/dbs_connREPLI.git) for a study of generalising published results of the predictive accuracy of MRI connectivity with stimulated part of subthalamic nucleus (STN) in DBS treated PD
  - [iadLONG](https://github.com/josefmana/dbs_iadLONG.git) for a study of longitudinal change in instrumental activities of daily living in STN DBS treated PD
  - [neuroFILA](https://github.com/josefmana/dbs_neuroFILA.git) for a study linking neurofilamenta to cognitive symptoms in PD (the repository is Private so far)
  - [demCR1T](https://github.com/josefmana/pd_demCR1T.git) for a study evaluating validity of criteria for PD dementia (the repository is Private so far)
