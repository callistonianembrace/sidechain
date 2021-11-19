#! /bin/sh

# This script needs tezos-* and ligo available on $PATH
# You'll need
# $ tezos-client --endpoint https://testnet-tezos.giganode.io activate account alice with ~/Downloads/tz1TnaohYE6nRm2qmov9pVBdjubRTBCwB6Nz.json
# to activate the wallet from the test faucet

TMP=$(mktemp -d)
export PATH=~/bin:$PATH;
PATH_TESTNET_WALLET="$HOME/Downloads/tz1TnaohYE6nRm2qmov9pVBdjubRTBCwB6Nz.json"
echo "Creating workspace " $TMP
ligo compile-contract ~/development/marigold/sidechain/tezos_interop/consensus.mligo main > $TMP/consensus.tz

set -e 
data_directory="data"

rm -rf data/
git checkout data/

SIDECLI=$(esy x which sidecli)
sidecli () {
  eval $SIDECLI '"$@"'
}

VALIDATORS=(0 1 2)
echo "Creating validator identities."
for i in ${VALIDATORS[@]}; do
  FOLDER="$data_directory/$i"
  mkdir -p $FOLDER
  echo [] > "$FOLDER/trusted-validator-membership-change.json"
  sidecli setup-identity $FOLDER --uri "http://localhost:444$i"
  KEY=$(sidecli self $FOLDER | grep "address:" | awk '{ print $2 }')
  URI=$(sidecli self $FOLDER | grep "uri:" | awk '{ print $2 }')
  VALIDATORS[$i]="$i;$KEY;$URI"
done


echo "Creating a new unauthenticated validator"
i=4
  FOLDER="$data_directory/$i"
  mkdir -p $FOLDER
  echo [] > "$FOLDER/trusted-validator-membership-change.json"
  sidecli setup-identity $FOLDER --uri "http://localhost:444$i"
  KEY=$(sidecli self $FOLDER | grep "address:" | awk '{ print $2 }')
  URI=$(sidecli self $FOLDER | grep "uri:" | awk '{ print $2 }')
# To register the validators, run consensus.mligo with the list of
# validators. To do this quickly, open the LIGO IDE with the url
# provided and paste the following storage as inputs to the contract.
echo_initial_contract_storage () {
  VALIDATORS=("$@")
  cat <<EOF
{
  root_hash = {
    current_block_hash = 0x;
    current_block_height = 0;
    current_state_hash = 0x;
    current_handles_hash = 0x;
    current_validators = [
EOF
  ## this iteration is done here just to ensure the indentation
  for VALIDATOR in ${VALIDATORS[@]}; do
    KEY=$(echo $VALIDATOR | awk -F';' '{ print $2 }')
    echo "      (\"$KEY\": key_hash);"
  done
cat <<EOF
    ];
  };
  vault = {
    known_handles_hash = (Big_map.empty : vault_known_handles_hash_set);
    used_handles = (Big_map.empty : vault_used_handle_set);
    vault = (Big_map.empty : vault);
  }
}
EOF
}

validators_json () {
  ## most of the noise in this function is because of indentation
  echo "["
for VALIDATOR in "${VALIDATORS[@]}"; do 
  i=$(echo $VALIDATOR | awk -F';' '{ print $1 }')
  KEY=$(echo $VALIDATOR | awk -F';' '{ print $2 }')
  URI=$(echo $VALIDATOR | awk -F';' '{ print $3 }')
  if [ $i != 0 ]; then
    printf ",
"
  fi
cat <<EOF
  {
    "address": "$KEY",
    "uri": "$URI"
EOF
  printf "  }"
done
  echo ""
  echo "]"
}


INITIAL_STORAGE=$(echo_initial_contract_storage "${VALIDATORS[@]}")
echo "Initial storage"
echo $INITIAL_STORAGE
INITIAL_STORAGE_MICHELSON=$(ligo compile-storage "/home/manas/development/marigold/sidechain/tezos_interop/consensus.mligo" "main" "$INITIAL_STORAGE")

export TEZOS_RPC_NODE="https://granadanet.smartpy.io/"
export TEZOS_SECRET=$(jq -r .[0].value ~/.tezos-client/secret_keys) # Not $(jq -r .secret $PATH_TESTNET_WALLET). Because it needs to be imported and activated
export TEZOS_CONSENSUS_ADDRESS=$(tezos-client \
	     --endpoint "$TEZOS_RPC_NODE" \
	     originate contract consensus transferring 1 from alice \
             running "$TMP/consensus.tz" \
	     --init "$INITIAL_STORAGE_MICHELSON" \
             --burn-cap 2 \
	     --force 2>&1 | grep -A1 Originated | grep -v Originated | xargs)

echo "${TEZOS_SECRET#unencrypted:}"
for VALIDATOR in ${VALIDATORS[@]}; do
  i=$(echo $VALIDATOR | awk -F';' '{ print $1 }')
  FOLDER="$data_directory/$i"
  validators_json > "$FOLDER/validators.json"
  sidecli setup-tezos "$FOLDER" \
    --tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
    --tezos_rpc_node="$TEZOS_RPC_NODE" \
    --unsafe_tezos_required_confirmations 1 \
    --tezos_secret="${TEZOS_SECRET#unencrypted:}"
done
FOLDER=data/4
cp data/0/validators.json $FOLDER/
sidecli setup-tezos "$FOLDER" \
	--tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
	--tezos_rpc_node="$TEZOS_RPC_NODE" \
	--unsafe_tezos_required_confirmations 1 \
	--tezos_secret="${TEZOS_SECRET#unencrypted:}"

echo "Cleaning temporary workspace"
rm -rf $TMP
tmuxinator
