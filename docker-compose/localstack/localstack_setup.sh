#!/bin/sh

mkdir /alloo
# CONSTANTS

region="ap-south-1"
alias aws="aws --endpoint-url=http://localhost:4566 --region=${region}"

# ****** KMS *******

echo "#!/bin/sh" > /etc/localstack/export_cyphers.sh
secret_key_id=`aws kms create-key | jq -r .KeyMetadata.KeyId`

kms_encrypt(){
  cypher=`aws kms encrypt --key-id $secret_key_id --plaintext $2 | jq -r .CiphertextBlob`
  echo "export $1=$cypher" >> /etc/localstack/export_cyphers.sh
}

kms_encrypt DB_PASSWORD docker
