#!/bin/bash

clear

# Clean before
find . -name 'root.ca.key' -delete
find . -name 'root.ca.crt' -delete

# Create Root CA

## Create Root Key

openssl \
    genrsa \
    -out \
    root.ca.key \
    2048
#    -des3 \ If you want a non password protected key just remove the -des3

## Create and self-sign the Root Certificate

openssl \
    req \
    -x509 \
    -new \
    -nodes \
    -key root.ca.key \
    -sha256 \
    -days 1024 \
    -out root.ca.crt \
    -config example.conf

## Reference
# - https://gist.github.com/fntlnz/cf14feb5a46b2eda428e000157447309
