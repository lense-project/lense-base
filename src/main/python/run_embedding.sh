#!/bin/sh

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/lib64:/usr/local/lib
python src/main/python/embedding.py $1
