#!/usr/bin/env bash
# Purpose: Create (idempotently) an Azure Storage Queue, Private Endpoint, and Private DNS zone linking to a VNet/subnet.
# Usage:
#   ./01-create-queue-and-pe.sh \
#     -g <resource-group> \
#     -s <storage-account-name> \
#     -q <queue-name> \
#     -v <vnet-resource-id> \
#     -n <subnet-resource-id> \
#     -l <location>
# Optional:
#     --pe-name <private-endpoint-name> (default: pe-${storage-account}-${queue})
#     --dns-zone-name <privatelink.file.core.windows.net> (for file) NOTE: queue uses privatelink.queue.core.windows.net
# Requirements: az CLI logged in, user has rights on RG & VNet/subnet.
set -euo pipefail

usage(){ grep '^#' "$0" | sed -e 's/^# \{0,1\}//'; }

if [[ $# -eq 0 ]]; then usage; exit 1; fi

PE_NAME=""
DNS_ZONE_NAME="privatelink.queue.core.windows.net"
while [[ $# -gt 0 ]]; do
  case $1 in
    -g|--resource-group) RG=$2; shift 2;;
    -s|--storage-account) SA=$2; shift 2;;
    -q|--queue-name) QNAME=$2; shift 2;;
    -v|--vnet-id) VNET_ID=$2; shift 2;;
    -n|--subnet-id) SUBNET_ID=$2; shift 2;;
    -l|--location) LOCATION=$2; shift 2;;
    --pe-name) PE_NAME=$2; shift 2;;
    --dns-zone-name) DNS_ZONE_NAME=$2; shift 2;;
    -h|--help) usage; exit 0;;
    *) echo "Unknown arg: $1"; usage; exit 1;;
  esac
done

: "${RG:?required}"
: "${SA:?required}"
: "${QNAME:?required}"
: "${VNET_ID:?required}"
: "${SUBNET_ID:?required}"
: "${LOCATION:?required}"

if [[ -z "$PE_NAME" ]]; then PE_NAME="pe-${SA}-${QNAME}"; fi

echo "[INFO] Ensuring storage queue exists"
az storage queue create --name "$QNAME" --account-name "$SA" >/dev/null

# Get storage account id & key
SA_ID=$(az storage account show -n "$SA" -g "$RG" --query id -o tsv)
KEY=$(az storage account keys list -n "$SA" -g "$RG" --query '[0].value' -o tsv)

# Private DNS Zone (Queue): privatelink.queue.core.windows.net
if ! az network private-dns zone show -g "$RG" -n "$DNS_ZONE_NAME" >/dev/null 2>&1; then
  az network private-dns zone create -g "$RG" -n "$DNS_ZONE_NAME" >/dev/null
fi
# Link VNet to zone
LINK_NAME="${SA}-queue-link"
if ! az network private-dns link vnet show -g "$RG" -z "$DNS_ZONE_NAME" -n "$LINK_NAME" >/dev/null 2>&1; then
  az network private-dns link vnet create -g "$RG" -n "$LINK_NAME" -z "$DNS_ZONE_NAME" -v "$VNET_ID" --registration-enabled false >/dev/null
fi

# Private Endpoint
if ! az network private-endpoint show -g "$RG" -n "$PE_NAME" >/dev/null 2>&1; then
  az network private-endpoint create -g "$RG" -n "$PE_NAME" \
    --vnet-id "$VNET_ID" --subnet "$SUBNET_ID" \
    --private-connection-resource-id "$SA_ID" \
    --group-id queue --connection-name "${PE_NAME}-conn" --location "$LOCATION"
fi

# DNS Zone A record creation (auto with PE for storage; but ensure link exists)
echo "[INFO] Private endpoint and DNS zone configured. Exporting connection string."
CONN_STR=$(az storage account show-connection-string -n "$SA" -g "$RG" --query connectionString -o tsv)
cat > queue-connection.env <<EOF
STORAGE_QUEUE_CONNECTION_STRING=${CONN_STR}
QUEUE_NAME=${QNAME}
STORAGE_ACCOUNT_NAME=${SA}
EOF

echo "[INFO] Done. queue-connection.env created."
