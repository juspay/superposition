#!/bin/sh

# CONSTANTS
region="ap-south-1"
alias aws="aws --endpoint-url=http://localhost:4566 --region=${region}"

# ****** KMS *******

secret_key_id=`aws kms create-key | jq -r .KeyMetadata.KeyId`

kms_encrypt(){
  echo $(aws kms encrypt --key-id $secret_key_id --plaintext $2 | jq -r .CiphertextBlob)
}

kms_encrypt DB_PASSWORD docker
