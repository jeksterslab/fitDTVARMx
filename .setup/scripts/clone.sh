#!/bin/bash

git clone git@github.com:jeksterslab/fitDTVARMx.git
rm -rf "$PWD.git"
mv fitDTVARMx/.git "$PWD"
rm -rf fitDTVARMx
