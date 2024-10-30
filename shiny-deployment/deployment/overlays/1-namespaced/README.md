COMMANDS TO DEPLOY:

1. Run `cp .env.example .env`

2. Fill in `.env` with values

3. Deploy

```bash
kustomize build . | envsubst | kubectl apply -f - --server-side
```