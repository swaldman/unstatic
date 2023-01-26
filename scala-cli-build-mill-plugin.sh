#!/bin/bash

UNSTATIC_VERSION="0.0.1-SNAPSHOT"
UNTEMPLATE_VERSION="0.0.2"
MILL_VERSION="0.10.11"

scala-cli \
    publish \
    local \
    -Ytasty-reader \
    --version "0.0.1-SNAPSHOT" \
    --dep com.lihaoyi::mill-main:$MILL_VERSION \
    --dep com.lihaoyi::mill-scalalib:$MILL_VERSION \
    --dep com.mchange::untemplate-mill:$UNTEMPLATE_VERSION \
    --dep com.mchange:unstatic-ztapir_3:$UNSTATIC_VERSION \
    ./unstatic/mill/src




