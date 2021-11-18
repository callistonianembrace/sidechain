 #! /bin/sh


FOLDER=4

SIDECLI=$(esy x which sidecli)
sidecli () {
  eval $SIDECLI '"$@"'
}

ADDRESS=$(sidecli self "$FOLDER" | grep "address:" | awk '{ print $2 }')

esy x deku-node "$FOLDER" 
# sidecli add-trusted-validator 0 ${ADDRESS}
# sidecli propose-new-validator  0 ${ADDRESS}
# esy x deku-node "$FOLDER"
