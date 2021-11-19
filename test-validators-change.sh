 #! /bin/sh


FOLDER=4

SIDECLI=$(esy x which sidecli)
sidecli () {
  eval $SIDECLI '"$@"'
}

ADDRESS=$(sidecli self "$FOLDER" | grep "address:" | awk '{ print $2 }')

sidecli add-trusted-validator 0 ${ADDRESS}
sidecli add-trusted-validator 1 ${ADDRESS}
sidecli add-trusted-validator 2 ${ADDRESS}
sidecli propose-new-validator  0 ${ADDRESS}
sidecli register-uri 0 $(jq -r .secret 4/identity.json) uri 
esy x deku-node "$FOLDER"
