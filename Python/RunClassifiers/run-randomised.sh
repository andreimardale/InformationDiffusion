#!/bin/bash

source ~/.bashrc ; conda activate vocationcompass
jupyter nbconvert --to script Randomised_dev.ipynb

## execute the script for each datasets, corresponding to each of the feature sets.
#python Randomised_dev.py ../uniq_data.csv rand_optimised_all.db
python Randomised_dev.py ../uniq_data_big5.csv rand_optimised_big5.db
python Randomised_dev.py ../uniq_data_values.csv rand_optimised_values.db
