# Automated Production Deployment Plan for Superposition

## Current State Assessment

### What's Already Working Well

| Capability | Status | Notes |
|---|---|---|
| CI checks on PR (fmt, clippy, tests) | Done | `ci_check_pr.yaml` - runs on every PR |
| Conventional commits enforcement | Done | cocogitto via `ci_check_pr.yaml` |
| Semantic versioning | Done | cocogitto auto-bumps based on commit types |
| Multi-arch Docker image builds | Done | amd64 + arm64, pushed to `ghcr.io/juspay/superposition` |
| Multi-platform native binaries | Done | Linux, macOS (x86+arm64), Windows |
| Package publishing (npm, PyPI, crates.io, Maven) | Done | Automated after binary generation |
| Helm chart with production config | Done | Supports HPA, Istio, OIDC, TLS |
| Monitoring stack (Prometheus + Grafana) | Done | In `/grafana/`, with custom exporters |
| Health check endpoints | Done | `/health` with liveness + readiness probes |

### What's Missing

| Capability | Status |
|---|---|
| Release workflow trigger is manual (`workflow_dispatch`) | Gap |
| GitHub Release is created as `draft: true` | Gap |
| No Helm chart published to an OCI/chart registry | Gap |
| No automated deployment to any K8s environment | Gap |
| No staging environment or smoke tests | Gap |
| No production deployment with approval gates | Gap |
| No automated rollback on failure | Gap |
| No deployment notifications (Slack, etc.) | Gap |
| No post-deployment health validation | Gap |
| No database migration automation in CD pipeline | Gap |

---

## Recommended Plan (Phased)

### Phase 1: Complete the Release Pipeline

**Goal**: Make the existing release flow end-to-end without manual intervention (except an approval gate).

#### 1.1 Auto-trigger releases on merge to `main`

Currently the release workflow requires someone to click "Run workflow" in the GitHub UI. Change it so that merging to `main` automatically starts a release.

**Option A (recommended)**: Trigger on push to `main` with a path filter so that doc-only changes don't trigger releases:

```yaml
on:
  push:
    branches: [main]
    paths-ignore:
      - 'docs/**'
      - '*.md'
      - 'grafana/**'
```

**Option B**: Keep `workflow_dispatch` but add a merge-triggered workflow that calls it via `workflow_call`.

#### 1.2 Publish the GitHub Release (not as draft)

Change `draft: true` to `draft: false` in the `release` job, or add a follow-up step that publishes the draft after all package jobs succeed.

#### 1.3 Publish Helm chart to an OCI registry

Add a job to the release workflow that packages and pushes the Helm chart:

```yaml
publish-helm-chart:
  needs: [tag-release]
  steps:
    - helm package helm/ --version ${{ needs.tag-release.outputs.version }}
    - helm push superposition-*.tgz oci://ghcr.io/juspay/charts
```

This enables `helm install` directly from the registry in later phases.

---

### Phase 2: Staging Environment + Smoke Tests

**Goal**: Every release gets deployed to a staging environment and validated before production.

#### 2.1 Create a staging environment in GitHub

Define a `staging` GitHub Environment with no required reviewers (auto-approve):

```yaml
deploy-staging:
  needs: [tag-release, create-manifest]
  environment:
    name: staging
    url: https://staging.superposition.example.com
```

#### 2.2 Deploy to staging via Helm

Add a job that deploys to the staging K8s cluster:

```yaml
steps:
  - uses: azure/setup-kubectl@v3
  - uses: azure/setup-helm@v3
  - run: |
      helm upgrade --install superposition ./helm \
        --namespace superposition-staging \
        --set image.tag=${{ needs.tag-release.outputs.version }} \
        --values helm/values-staging.yaml \
        --wait --timeout 5m
```

This requires:
- A `values-staging.yaml` with staging-specific config (DB connection, auth disabled or test OIDC, etc.)
- K8s credentials stored as GitHub secrets (or use OIDC workload identity)

#### 2.3 Run smoke tests against staging

Add integration/smoke tests that validate the deployed service:

```yaml
smoke-tests:
  needs: [deploy-staging]
  steps:
    - run: |
        # Health check
        curl -sf https://staging.superposition.example.com/health

        # API smoke tests (create tenant, create context, etc.)
        ./scripts/smoke-test.sh staging
```

Create `scripts/smoke-test.sh` with basic API validation:
- Health endpoint returns 200
- Can create and retrieve a test tenant
- Can create and evaluate a context
- Can run an experiment

#### 2.4 Run database migrations automatically

Add Diesel migrations as a pre-deploy step:

```yaml
steps:
  - run: |
      diesel migration run --database-url=$STAGING_DB_URL
```

Or use a Kubernetes Job / init container in the Helm chart to run migrations before the app starts.

---

### Phase 3: Production Deployment with Approval Gates

**Goal**: Production deployments happen automatically after staging validation, gated by a manual approval.

#### 3.1 Create a production environment with required reviewers

```yaml
deploy-production:
  needs: [smoke-tests]
  environment:
    name: production
    url: https://superposition.example.com
```

Configure the `production` environment in GitHub Settings to require approval from designated reviewers. This provides the human-in-the-loop gate.

#### 3.2 Deploy to production via Helm

```yaml
steps:
  - run: |
      helm upgrade --install superposition \
        oci://ghcr.io/juspay/charts/superposition \
        --version ${{ needs.tag-release.outputs.version }} \
        --namespace superposition \
        --values helm/values-production.yaml \
        --wait --timeout 10m
```

Create `values-production.yaml` with production config:
- OIDC authentication enabled
- HPA enabled (min 2 replicas)
- Resource limits tuned for production
- TLS/Ingress configured
- Real AWS credentials via K8s secrets

#### 3.3 Post-deployment health validation

```yaml
post-deploy-check:
  needs: [deploy-production]
  steps:
    - run: |
        # Wait for rollout
        kubectl rollout status deployment/superposition -n superposition --timeout=300s

        # Verify health
        for i in $(seq 1 10); do
          curl -sf https://superposition.example.com/health && break
          sleep 5
        done
```

#### 3.4 Automated rollback on failure

```yaml
steps:
  - run: |
      helm upgrade ... --wait --timeout 10m || {
        echo "Deployment failed, rolling back..."
        helm rollback superposition -n superposition
        exit 1
      }
```

---

### Phase 4: Observability + Notifications

**Goal**: Full visibility into deployments with alerting.

#### 4.1 Deployment notifications

Add Slack/Discord/email notifications at key points:

```yaml
- uses: slackapi/slack-github-action@v1
  with:
    payload: |
      {
        "text": "Superposition ${{ needs.tag-release.outputs.version }} deployed to production"
      }
```

Notify on:
- Release started
- Staging deployment succeeded/failed
- Production approval requested
- Production deployment succeeded/failed
- Rollback triggered

#### 4.2 Deployment annotations in Grafana

Push deployment events to Grafana so they appear on dashboards:

```yaml
- run: |
    curl -X POST $GRAFANA_URL/api/annotations \
      -H "Authorization: Bearer $GRAFANA_API_KEY" \
      -d '{"text":"Deploy v$VERSION","tags":["deploy"]}'
```

#### 4.3 Alerting rules

Add Prometheus alerting rules for post-deployment monitoring:
- Error rate spike after deploy
- Latency P99 regression
- Pod restart loops
- Health check failures

---

### Phase 5: Advanced Deployment Strategies (Future)

#### 5.1 Canary deployments

Use Istio (already supported in the Helm chart) for traffic splitting:
- Deploy new version alongside old
- Route 5% -> 10% -> 25% -> 50% -> 100% of traffic
- Auto-rollback if error rate exceeds threshold

#### 5.2 GitOps with ArgoCD/Flux

Instead of Helm commands in CI, adopt GitOps:
- CI pushes updated Helm values to a config repo
- ArgoCD watches the config repo and syncs to the cluster
- Provides audit trail, drift detection, and declarative state

#### 5.3 Multi-cluster / multi-region

- Deploy to multiple K8s clusters for HA
- Use DNS-based failover or global load balancing

---

## Implementation Priority & Effort Estimate

| Phase | Items | Effort | Impact |
|---|---|---|---|
| **Phase 1** | Auto-trigger, publish release, publish Helm chart | Small | Removes manual release steps |
| **Phase 2** | Staging env, smoke tests, migration automation | Medium | Catches bugs before production |
| **Phase 3** | Production deploy, approval gates, rollback | Medium | Enables automated prod deploys |
| **Phase 4** | Notifications, Grafana annotations, alerts | Small | Visibility and confidence |
| **Phase 5** | Canary, GitOps, multi-cluster | Large | Advanced reliability |

**Recommended starting point**: Phase 1 is low-effort, high-value. Phases 2-3 together form the core automated deployment pipeline. Phase 4 adds confidence. Phase 5 is for scale.

---

## Prerequisites

Before implementing, the team needs to decide on:

1. **Where is production hosted?** (EKS, GKE, self-hosted K8s, etc.)
2. **How are K8s credentials managed?** (GitHub OIDC → cloud IAM, kubeconfig secret, etc.)
3. **What's the database strategy?** (Managed RDS/CloudSQL, self-hosted, etc.)
4. **Who approves production deploys?** (Team leads, on-call, etc.)
5. **What notification channels to use?** (Slack, Discord, email, etc.)
6. **Is GitOps (ArgoCD/Flux) preferred over direct Helm deploys from CI?**
