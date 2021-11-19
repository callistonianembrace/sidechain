 #! /bin/sh


FOLDER=4

SIDECLI=$(esy x which sidecli)
sidecli () {
  eval $SIDECLI '"$@"'
}

ADDRESS=$(sidecli self "$FOLDER" | grep "address:" | awk '{ print $2 }')

# esy x deku-node "$FOLDER" 
sidecli add-trusted-validator 0 ${ADDRESS}
sidecli propose-new-validator  0 ${ADDRESS}
NONCE=$(curl --data '{ "uri": "http://localhost:4444" }' http://localhost:4440/request-nonce | jq -r ".nonce")
curl --data "{ \"uri\": \"http://localhost:4444\", \"nonce\": \"$NONCE\" }" http://localhost:4440/register-uri 
esy x deku-node "$FOLDER"
