 #! /bin/sh

FOLDER=4

SIDECLI=$(esy x which sidecli)
sidecli () {
  eval $SIDECLI '"$@"'
}

ADDRESS=$(sidecli self "$FOLDER" | grep "address:" | awk '{ print $2 }')
CURRENT_BLOCK_PRODUCER=2

sidecli add-trusted-validator 0 ${ADDRESS}
sidecli add-trusted-validator 1 ${ADDRESS}
sidecli add-trusted-validator 2 ${ADDRESS}
sidecli propose-new-validator ${CURRENT_BLOCK_PRODUCER} ${ADDRESS}
read -p "Proceed with removal?" foo
# sidecli register-uri 0 $(jq -r .secret 4/identity.json) $(jq -r .uri 4/identity.json)  
sidecli remove-trusted-validator 0 ${ADDRESS}
sidecli remove-trusted-validator 1 ${ADDRESS}
sidecli remove-trusted-validator 2 ${ADDRESS}
sidecli propose-validator-removal ${CURRENT_BLOCK_PRODUCER} ${ADDRESS}
# esy x deku-node "$FOLDER"
