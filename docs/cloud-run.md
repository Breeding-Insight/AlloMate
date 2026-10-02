# Running AlloMate on Google Cloud Run

AlloMate runs on Cloud Run as a scale-to-zero service behind ORCID sign-in, with a
static landing page on GitHub Pages. The desktop app (`AlloMate::run_app()`) and the
shinyapps.io deployment are unchanged: everything below is switched on only by the
`cloudrun` config profile (`R_CONFIG_ACTIVE=cloudrun`, set in the `Dockerfile`).

## What the `cloudrun` profile changes

| Area | Behaviour | Code |
|---|---|---|
| Sign-in | ORCID sign-in; only ORCID iDs with `users/{orcid_id}.is_active: true` in Firestore get in. Signed-out visitors to `/` go to the landing page. | `R/cloud_auth.R`, `R/cloud_routes.R` |
| Routes | `/health` (204, no sign-in), `/robots.txt` (disallow all), `/auth/login`, `/auth/callback`, `/auth/logout` | `R/cloud_routes.R` |
| Idle sessions | Warning after 8 minutes without activity, disconnect after 10 | `R/cloud_session.R`, `inst/app/www/cloud.js` |
| Uploads | Limit raised from Shiny's 5 MB to 30 MiB; uploaded files deleted when the session ends | `R/run_app.R`, `R/cloud_session.R` |
| Navbar | Signed-in name and a Sign out link | `R/cloud_session.R` |

Settings live in the `cloudrun` section of `inst/golem-config.yml`.

How sign-in works: `/auth/login` clears any existing AlloMate session, sets a short-lived
`state` cookie and redirects to ORCID. `/auth/callback` checks `state`, exchanges the
code for the person's ORCID iD and reads `users/{orcid_id}` from Firestore through the
REST API, using a token from the Cloud Run metadata server (no key file). An approved
person gets a signed, HttpOnly session cookie (HMAC-SHA256 with `SECRET_KEY`, 12 hours).
The Shiny connection checks the same cookie again, so the app never starts without a
valid sign-in.

## Known limits

- **Uploads and downloads over 32 MiB fail.** Cloud Run limits HTTP/1 requests and
  non-streamed responses to 32 MiB. This mainly affects relationship-matrix CSVs for
  more than about 4,000 individuals.
- **Connections last at most 60 minutes** (the service request timeout). The disconnect
  message explains this; reloading starts a new session.
- **Nothing is saved between sessions.** People download results before they leave.

## 1. Firestore

Create a Firestore Standard database in Native mode, in the same project as HapApp:

```text
database ID: allomate-db
location: us-west1
security rules: deny all reads and writes from browsers and mobile clients
```

AlloMate only reads Firestore, from the R server, through Google IAM. Seed each allowed
person as `users/{orcid_id}`:

```json
{
  "orcid_id": "0000-0000-0000-0000",
  "display_name": "Example User",
  "is_active": true
}
```

Set `is_active` to `false` to remove access; it takes effect at the person's next sign-in.

## 2. Service identity

Create a service account such as `allomate-cloud-run` and grant it:

- `roles/datastore.viewer` on the project (read-only Firestore access)
- `roles/secretmanager.secretAccessor` on each AlloMate secret

Attach it as the Cloud Run service identity. Do not create a JSON key.

## 3. ORCID

ORCID allows one set of public API credentials per ORCID account, so AlloMate shares the
ORCID application already registered for HapApp. In that account's ORCID Developer Tools:

- **Add a redirect URI** for AlloMate, keeping HapApp's:

  ```text
  https://allomate-510101458052.us-west1.run.app/auth/callback
  ```

  That is the service URL if the service is named `allomate` in HapApp's project
  (project number 510101458052) and region; adjust if either differs.
- **Name the application for both apps**, e.g. "Breeding Insight apps", with application URL
  `https://breedinginsight.org/open-source-software-solutions/`. People see the name and URL
  on ORCID's sign-in screen. Changing them does not change the client ID or secret, so
  HapApp keeps working.

Copy the shared client ID and secret into AlloMate's own secrets (step 4), so AlloMate can
move to its own ORCID application later without touching HapApp. Access stays separate:
AlloMate checks its own Firestore approved list after sign-in.

For testing against ORCID's sandbox, set `ORCID_BASE_URL=https://sandbox.orcid.org`.

## 4. Secrets and environment

Secret Manager secrets, exposed as environment variables:

| Variable | Secret |
|---|---|
| `SECRET_KEY` | A random value of at least 32 characters, e.g. `openssl rand -hex 32` |
| `ORCID_CLIENT_ID` | From the shared ORCID application (same value as HapApp's) |
| `ORCID_CLIENT_SECRET` | From the shared ORCID application (same value as HapApp's) |

Plain environment variables:

| Variable | Value |
|---|---|
| `FIRESTORE_DATABASE` | `allomate-db` |
| `GOOGLE_CLOUD_PROJECT` | Project ID (optional; read from the metadata server otherwise) |
| `ALLOMATE_LANDING_URL` | `https://breeding-insight.github.io/AlloMate/` (when empty, `/` goes straight to ORCID sign-in) |
| `ALLOMATE_PUBLIC_URL` | Optional; defaults to the Host and X-Forwarded-Proto headers Cloud Run sends |

The app checks these at startup and stops with one message listing anything missing.

## 5. Deploy

From the repository root (Cloud Build builds the `Dockerfile`):

```bash
gcloud run deploy allomate \
  --source . \
  --region=us-west1 \
  --allow-unauthenticated \
  --service-account=allomate-cloud-run@PROJECT_ID.iam.gserviceaccount.com \
  --port=8080 \
  --cpu=1 \
  --memory=4Gi \
  --cpu-throttling \
  --cpu-boost \
  --min-instances=0 \
  --max-instances=5 \
  --concurrency=4 \
  --timeout=3600 \
  --session-affinity \
  --execution-environment=gen2 \
  --set-env-vars=FIRESTORE_DATABASE=allomate-db,ALLOMATE_LANDING_URL=https://breeding-insight.github.io/AlloMate/ \
  --set-secrets=SECRET_KEY=allomate-secret-key:latest,ORCID_CLIENT_ID=allomate-orcid-client-id:latest,ORCID_CLIENT_SECRET=allomate-orcid-client-secret:latest
```

Why these settings:

- **Request-based billing (`--cpu-throttling`).** Nothing runs between requests, and an
  instance is billed only while a request (including an open Shiny connection) is active.
- **1 vCPU / 4 GiB.** Analyses are single-threaded and take seconds. Memory grows with the
  square of pedigree size: a 3,107-row pedigree peaked at about 600 MB and a
  20,000-row pedigree at about 4.9 GB on a Mac.
- **Concurrency 4, session affinity.** Each Shiny session stays on one instance, and
  one R process serves several sessions while analyses are short.
- **Timeout 3600 s.** The longest a Shiny connection can stay open.

Cost at list prices is about $0.12 per hour while anyone is connected, and nothing while
the service is scaled to zero.

## 6. Landing page

`landing/` is published to GitHub Pages by `.github/workflows/pages.yml`. In the
repository, set **Settings → Pages → Source** to **GitHub Actions**. Its layout and styles
are copied from HapApp's landing page; see `docs/landing-pages.md` for the shared pattern.
If the service URL changes, update both sign-in links and the `wake.js` script tag in
`landing/index.html`. Remove `cloud_run` from the workflow's branches once this is on `main`.

## Running the Cloud Run build locally

```bash
docker build --platform linux/amd64 -t allomate-cloudrun .
```

```bash
docker run --rm -p 8080:8080 -e ALLOMATE_AUTH_BYPASS=true allomate-cloudrun
```

`ALLOMATE_AUTH_BYPASS=true` skips ORCID for local testing only; it is ignored on Cloud
Run (whenever `K_SERVICE` is set). To try the cloud profile without Docker:

```r
Sys.setenv(R_CONFIG_ACTIVE = "cloudrun", ALLOMATE_AUTH_BYPASS = "true")
pkgload::load_all()
shiny::runApp(run_app())
```
