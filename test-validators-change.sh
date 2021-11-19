 #! /bin/sh

FOLDER=4

SIDECLI=$(esy x which sidecli)
sidecli () {
  eval $SIDECLI '"$@"'
}

ADDRESS=$(sidecli self "$FOLDER" | grep "address:" | awk '{ print $2 }')
CURRENT_BLOCK_PRODUCER=1

read -p "Proceed with addition tests?" foo
sidecli add-trusted-validator 0 ${ADDRESS}
sidecli add-trusted-validator 1 ${ADDRESS}
sidecli add-trusted-validator 2 ${ADDRESS}
sidecli propose-new-validator ${CURRENT_BLOCK_PRODUCER} ${ADDRESS}
sidecli register-uri 0 $(jq -r .secret 4/identity.json) $(jq -r .uri 4/identity.json)  
esy x deku-node "$FOLDER"

# read -p "Proceed with removal tests?" foo
# sidecli remove-trusted-validator 0 ${ADDRESS}
# sidecli remove-trusted-validator 1 ${ADDRESS}
# sidecli remove-trusted-validator 2 ${ADDRESS}
# sidecli propose-validator-removal ${CURRENT_BLOCK_PRODUCER} ${ADDRESS}
