#!/bin/bash
rm -rf ./dist

python setup.py bdist_wheel
python setup.py sdist