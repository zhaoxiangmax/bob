# Ecommerce Dashboard — Deploy Guide

## Option A: Manual deploy from R
```r
install.packages("rsconnect")

rsconnect::setAccountInfo(
  name   = "<your-shinyapps-username>",
  token  = "<your-token>",
  secret = "<your-secret>"
)
rsconnect::deployApp(".")
```
Get your token/secret: https://www.shinyapps.io/admin/#/tokens → **Add Token** → **Show**.

---

## Option B: Push-to-deploy via GitHub Actions (recommended)

Every push to `main` automatically deploys to shinyapps.io. All free.

### 1. Create a GitHub repo
```bash
git init
git add .
git commit -m "Initial commit"
git remote add origin https://github.com/<you>/<repo>.git
git push -u origin main
```

### 2. Add your shinyapps.io credentials as GitHub Secrets
Go to your repo → **Settings** → **Secrets and variables** → **Actions** → **New repository secret**.

Add these three secrets:

| Secret name          | Value                                      |
|----------------------|--------------------------------------------|
| `SHINYAPPS_NAME`     | Your shinyapps.io username                 |
| `SHINYAPPS_TOKEN`    | Token from shinyapps.io → Tokens → Show    |
| `SHINYAPPS_SECRET`   | Secret from shinyapps.io → Tokens → Show   |

### 3. Push to deploy
```bash
git add .
git commit -m "Update dashboard"
git push
```
GitHub Actions picks it up, installs R packages, and deploys. Takes ~3–5 min on first run
(package install is cached after that).

Watch the run: repo → **Actions** tab.

### Live URL
`https://<your-shinyapps-username>.shinyapps.io/ecommerce_dashboard/`

---

## Notes
- Free GitHub: 2,000 Actions minutes/month (each deploy ~3–5 min = ~400–600 free deploys).
- Free shinyapps.io: 5 apps, 25 active hours/month. App sleeps after 15 min idle.
- The app queries the DB directly — no data is stored in the repo.
