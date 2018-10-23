#!/bin/bash

clear

# Clean before
find . -name 'localhost.key' -delete
find . -name 'localhost.csr' -delete
find . -name 'localhost.crt' -delete

# Create a certificate (Done for each server)

## Create the certificate key

openssl \
    genrsa \
    -out localhost.key \
    2048

## Create the signing request

openssl \
    req \
    -new \
    -key localhost.key \
    -out localhost.csr \
    -config example.conf

## Generate the certificate using the mydomain csr and key along with the CA
## Root key

openssl \
    x509 \
    -req \
    -in localhost.csr \
    -CA root.ca.crt \
    -CAkey root.ca.key \
    -CAcreateserial \
    -out localhost.crt \
    -days 500 \
    -sha256

## References
# - https://gist.github.com/fntlnz/cf14feb5a46b2eda428e000157447309
