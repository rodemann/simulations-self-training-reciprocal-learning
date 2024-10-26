




### Introduction, TOC
This repository contains code to reproduce experiments in Appendix A.1.2 "Illustrating Experiments" of our paper "Reciprocal Learning" (NeurIPS 2024)

* [R](R) contains implementation of BPLS with PPP and alternative PLS methods to benchmark against
* [benchmarking](benchmarking) provides files for experiments, in order to reproduce results, see setup below
* [data](data) contains real-world data (banknote and breast cancer data) used in experiments
* experimental results and visualization thereof will be saved in [plots](plots) and [results](results) 


### Tested with

- R 4.2.0
- R 4.1.6
- R 4.0.3

on
- Linux Ubuntu 20.04
- Linux Debian 10
- Windows 11 Pro Build 22H2 


### Setup

First and foremost, please install all dependencies by sourcing [this file](_setup_session.R).

The implementations of self-training with different selection criteria as detailed in the paper are in folder "R":

* [Supervised Baseline](R/standard_supervised.R)
* [Probability Score](R/standard_self_training_conf.R)
* [Predictive Variance](R/standard_self_training.R)
* [PPP (Bayes-optimal)](R/diff_marg_likelihood_pred_ext.R)
* [**Regularized** PPP (Bayes-optimal)](R/diff_marg_likelihood_pred_reg.R)
* [Likelihood (max-max)](R/diff_marg_likelihood_pred.R)
* [Utilities for PPP](R/utils_diff_marg_likelihood.R)


In order to reproduce the papers' key results (and visualizations thereof) further download these scripts and save in respective folder:

* in folder analysis/
    * [analysis and visualization](analyze/analyze.R) 
* in folder benchmarks/
    * [global setup of experiments](benchmarks/run_benchmarks_simulated_data_p=60.R)
* in folder benchmarks/experiments/
    * [experiments with likelihood (max-max)](benchmarks/experiments/benchmark-dml-pred.R)
    * [experiments with PPP (bayes-opt)](benchmarks/experiments/benchmark-dml-pred-ext.R)
    * [experiments with supervised baseline](benchmarks/experiments/_benchmark-standard-supervised.R)
    * [experiments with predictive variance](benchmarks/experiments/_benchmark-standard-self-training.R)
    * [experiments with probability score](benchmarks/experiments/_benchmark-standard-self-training_conf.R)


Eventually, download [benchmarks/experiments_simulated_data.R](benchmarks/experiments_simulated_data.R) and run from benchmarks/ (estimated runtime: 30 CPU hours)

Important: Create empty folders [results](results) and [plots](plots) where experimental results will be stored automatically. In addition, you can access them as object after completion of the experiments.


### Further experiments

Additional experimental setups can now easily be created by modifying [benchmarks/experiments_simulated_data.R](benchmarks/experiments_simulated_data.R)


### Data

Find data and files to read in data in folder [data](data). 
