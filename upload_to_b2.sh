#!/bin/bash
set -euo

AUTH_REL=$(curl --retry 5 -s https://api.backblazeb2.com/b2api/v2/b2_authorize_account -u "$ACCOUNT_ID:$APPLICATION_KEY")

ACCOUNT_AUTHORIZATION_TOKEN=$( echo "$AUTH_REL" | grep -Po '(?<="authorizationToken": ")[^"]*')
API_URL=$( echo "$AUTH_REL" | grep -Po '(?<="apiUrl": ")[^"]*')


UPLOAD_REL=$(curl --retry 5 -s \
    -H 'Authorization: '"$ACCOUNT_AUTHORIZATION_TOKEN"'' \
    -d '{"bucketId": "'"$BUCKET_ID"'"}' \
    "$API_URL"/b2api/v2/b2_get_upload_url)
    

UPLOAD_AUTHORIZATION_TOKEN=$( echo "$UPLOAD_REL" | grep -Po '(?<="authorizationToken": ")[^"]*' )
UPLOAD_URL=$( echo "$UPLOAD_REL" | grep -Po '(?<="uploadUrl": ")[^"]*' )
FILE_TO_UPLOAD="$1"
SHA1_OF_FILE=$(openssl dgst -sha1 "$FILE_TO_UPLOAD" | awk '{print $2;}')

curl --retry 5 -s \
    -H "Authorization: $UPLOAD_AUTHORIZATION_TOKEN" \
    -H "X-Bz-File-Name: $FILE_TO_UPLOAD" \
    -H "Content-Type: =$(file --mime-type -b "$FILE_TO_UPLOAD")" \
    -H "X-Bz-Content-Sha1: $SHA1_OF_FILE" \
    -H "X-Bz-Info-Author: unknown" \
    --data-binary "@$FILE_TO_UPLOAD" \
    "$UPLOAD_URL" > /dev/null 2>&1
