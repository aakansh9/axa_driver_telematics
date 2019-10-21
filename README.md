axa_driver_telematics
===============================
*ML compeitition at [Kaggle](https://www.kaggle.com/c/axa-driver-telematics-analysis/overview)*

---
title: "Kaggle AXA Driver Telematics Challenge"
author: "Aakansh Gupta"
date: "March 18, 2015"
---

This repo contains submission code and ideas for the [Driver Telematics Challenge](https://www.kaggle.com/c/axa-driver-telematics-analysis) hosted by [Kaggle](https://www.kaggle.com) and [AXA](http://www.axa.com/en/).

### Hardware and OS Used
MacBook Pro (Mid 2014)
Processor 2.6 GHz Intel Core i5
Memory 16 GB 1600 MHz DDR3
Cores 2
OSX 10.10.2

### Software
Rstudio Version 0.98.1091 – © 2009-2014 RStudio, Inc.

### File Structure
1. functions/ contains all the functions necessary.
  + **to featurize**
      + Driver.Attr.Behav.R
      + Driver.Attr.Topo.R
      + Driver.Discretize.R
      + Driver.Feat.R
      + Trip.Feat.R
      + Smooth.Kalman.R
  + **to model**
      + Nclass.R
      + Pclass.R
      + Ensemble.Avg.R
      + Short.Trips.R
      + Load.Featurefiles.R
  + **to visualize**
      + Visualize.Attr.R
  + **Others**
      + Jumps.Find.R
      + Subm.Order.R
     
2. submissions/ contains all the submissions of the author in the contest.

3. clean.R is used to process and smooth raw data.

4. featurize.R is used to create features from the smoothed/unsmoothed data. 2 kinds of features were created a) from smoothed data b) from unsmoothed data.

5. model.R contains code to train the GBM model and make the submission file.

6. matching.R contains code to find matched trips from each driver.

7. localbenchmark.R contains an implementation of local evaluation of the AUC score.

### Summary

3 models (all gbm) were used to make an ensemble. The first one was ~110 features on unsmoothed data (gave an AUC of ~0.89)(submissions/sub-7). The second one was same features + 700 more but on smoothed out data (gave AUC of ~0.86)(sub-)(submissions/sub-9). The third one was slightly different 641 features on smoothed out data (gave ~0.87)(submissions/sub-14). The ensemble gives ~0.91. In all cases there is no trip matching used. 

For each driver all its 200 trips are considered positive and 10 trips chosen from 50 different drivers (total 500) considered negative. The model is then validates on the same 200 trips used for training and the scores obtained are used as the outlier scores.

Submissions can be boosted by ensembling ( here just using average) nad boosting the probability of short trips (<100m) by say 0.1. The AUC is observed to increase by ~0.0006

In each case the gbm is 1000 trees and 3 level deep with 0.006 as shrinkage.

For localbenchmark, training set for a driver is the matched trips( 'sure bets') and some fake implanted trips as 1 and 500 other trips as 0. This is validated on the implanted trips and matched trips and the local AUC calcualted. 

### Features

+ The features for the first model are described by:
smooth=F
X1-40, X251-290, Y1-11, ttime
(complete definition in Info1362.txt)

+ The features for the second model are described by:
smooth=T
c( paste('X', c(41:75, 81:100, 121:130, 161:170, 201:210, 300:340, 351:380, 401:425, 450:480, 500:525, 554:570, 600:620, 655:670, 700:720, 750:770, 841:875, 891:925, 941:975, 991:1025, 1041:1080, 1091:1120, 1141:1180, 1191:1275, 1291:1320), sep=''), paste('Y', c(1:10), sep=''),'ttime','tdist')
(complete definition in Info1352.txt)

+ The features for the third model are described by:
smooth=T
all 641 features
(complete definition in Info641.txt)
