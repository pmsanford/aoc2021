#!/bin/bash
ormolu --mode inplace $(git ls-files '*.hs')
