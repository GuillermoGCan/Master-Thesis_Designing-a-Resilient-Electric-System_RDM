#!/bin/bash

for((i = 0; i <= 3; i++))
do 
    python run_LEAP_model.py $i
done