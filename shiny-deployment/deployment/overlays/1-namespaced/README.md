COMMANDS TO DEPLOY:

1. Run `cp .env.example .env`

2. Fill in `.env` with values
2a. ```set -a
     source .env
     set +a```
3. Deploy

```bash
kustomize build . | envsubst | kubectl apply -f - --server-side
```
