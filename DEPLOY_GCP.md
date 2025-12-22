# Deploying Easyeyes-Analyzer to Google Cloud Platform (GCP)

This guide walks you through deploying your Shiny application from shinyapps.io to Google Cloud Platform using Cloud Run.

## 📋 Prerequisites

### 1. Install Google Cloud SDK
```bash
# For macOS (using Homebrew)
brew install google-cloud-sdk

# For other systems, download from:
# https://cloud.google.com/sdk/docs/install
```

### 2. Install Docker
```bash
# For macOS (using Homebrew)
brew install docker

# Or download Docker Desktop from:
# https://www.docker.com/products/docker-desktop
```

### 3. Create GCP Project
1. Go to [Google Cloud Console](https://console.cloud.google.com/)
2. Create a new project or select an existing one
3. Note your **PROJECT_ID** (you'll need this later)
4. Enable billing for your project

## 🚀 Quick Deployment

### Method 1: Automated Script (Recommended)

1. **Edit the deployment script:**
   ```bash
   # Open deploy-to-gcp.sh and replace YOUR_PROJECT_ID with your actual GCP project ID
   nano deploy-to-gcp.sh
   ```
   Update the variables near the top of the script:
   ```bash
   PROJECT_ID="your-actual-project-id"  # Replace with your GCP project ID
   REGION="us-east1"                    # Use US East Coast (South Carolina)
   # In the Cloud Run deploy command, set:
   # --memory 16Gi                     # Increase memory to 16 GiB
   # --cpu 2                           # (optional) adjust CPU if needed
   ```

2. **Authenticate with GCP:**
   ```bash
   gcloud auth login
   gcloud auth application-default login
   ```

3. **Run the deployment:**
   ```bash
   ./deploy-to-gcp.sh
   ```

### Method 2: Manual Step-by-Step

1. **Set up GCP project:**
   ```bash
   gcloud config set project YOUR_PROJECT_ID
   ```

2. **Enable required APIs:**
   ```bash
   gcloud services enable cloudbuild.googleapis.com
   gcloud services enable run.googleapis.com
   gcloud services enable containerregistry.googleapis.com
   ```

3. **Build and deploy:**
   ```bash
   # Build the Docker image using Cloud Build
   gcloud builds submit --tag gcr.io/YOUR_PROJECT_ID/easyeyes-analyzer .

   # Deploy to Cloud Run
   gcloud run deploy easyeyes-analyzer \
     --image gcr.io/YOUR_PROJECT_ID/easyeyes-analyzer \
     --platform managed \
     --region us-east1 \
     --allow-unauthenticated \
     --memory 16Gi \
     --cpu 2 \
     --timeout 3600 \
     --max-instances 10 \
     --port 3838
   ```

## 📁 Deployment Files

This repository includes the following deployment files:

- **`Dockerfile`**: Container configuration for the Shiny app
- **`.dockerignore`**: Files to exclude from Docker build
- **`deploy-to-gcp.sh`**: Automated deployment script
- **`cloudbuild.yaml`**: Cloud Build configuration for CI/CD

## ⚙️ Configuration Options

### Resource Allocation
You can modify the following in `deploy-to-gcp.sh`:

```bash
# Memory options: 1Gi, 2Gi, 4Gi, 8Gi, 16Gi
--memory 16Gi

# CPU options: 1, 2, 4
--cpu 2

# Timeout: maximum request duration (seconds)
--timeout 3600

# Scaling: maximum number of instances
--max-instances 10
```

### Regions
Available regions for Cloud Run:
- `us-central1` (Iowa)
- `us-east1` (South Carolina)
- `us-west1` (Oregon)
- `europe-west1` (Belgium)
- `asia-east1` (Taiwan)

### Authentication
- **Public access**: `--allow-unauthenticated` (current setting)
- **Private access**: Remove the `--allow-unauthenticated` flag

## 💰 Cost Estimation

Google Cloud Run pricing (as of 2024):
- **CPU**: $0.00002400 per vCPU-second
- **Memory**: $0.00000250 per GiB-second
- **Requests**: $0.40 per million requests

**Free tier includes:**
- 2 million requests per month
- 400,000 GiB-seconds per month
- 200,000 vCPU-seconds per month

**Estimated monthly cost for moderate usage:**
- ~1000 daily users: $15-30/month
- ~100 daily users: $3-8/month
- Light usage: Often covered by free tier

## 🔄 Updating Your Application

To deploy updates:

1. **Make your code changes**
2. **Commit changes to git** (optional but recommended)
3. **Run deployment script again:**
   ```bash
   ./deploy-to-gcp.sh
   ```

Cloud Build will automatically create a new version and deploy it.

## 🌐 Custom Domain Setup

### 1. Map your domain to Cloud Run:
```bash
gcloud run domain-mappings create \
  --service easyeyes-analyzer \
  --domain your-domain.com \
  --region us-east1
```

### 2. Configure DNS:
Add the DNS records shown in the Cloud Console to your domain provider.

### 3. SSL Certificate:
Google automatically provisions SSL certificates for custom domains.

## 📊 Monitoring and Logging

### View logs:
```bash
gcloud logging read "resource.type=cloud_run_revision AND resource.labels.service_name=easyeyes-analyzer AND resource.labels.location=us-east1" --limit 50
```

### Monitor in Console:
1. Go to [Cloud Run Console](https://console.cloud.google.com/run)
2. Click on your service
3. View metrics, logs, and performance data

## 🔧 Troubleshooting

### Common Issues:

1. **Build timeout:**
   - Increase timeout in `cloudbuild.yaml`
   - Use a more powerful machine type

2. **Memory issues:**
   - Increase memory allocation: `--memory 4Gi`
   - Optimize your R code for memory usage

3. **Package installation fails:**
   - Check system dependencies in `Dockerfile`
   - Verify `renv.lock` is up to date

4. **Authentication errors:**
   ```bash
   gcloud auth login
   gcloud auth application-default login
   ```

5. **Permission errors:**
   ```bash
   # Grant necessary IAM roles
   gcloud projects add-iam-policy-binding YOUR_PROJECT_ID \
     --member="user:your-email@gmail.com" \
     --role="roles/run.admin"
   ```

### Debug commands:
```bash
# Test Docker image locally
docker build -t easyeyes-test .
docker run -p 3838:3838 easyeyes-test

# Check Cloud Run service status
gcloud run services describe easyeyes-analyzer --region us-east1

# View recent deployments
gcloud run revisions list --service easyeyes-analyzer --region us-east1
```

## 🔒 Security Considerations

### 1. Private Access:
Remove `--allow-unauthenticated` and set up IAM authentication:
```bash
gcloud run services add-iam-policy-binding easyeyes-analyzer \
  --member="user:specific-user@gmail.com" \
  --role="roles/run.invoker" \
  --region us-east1
```

### 2. Environment Variables:
For sensitive configuration:
```bash
gcloud run services update easyeyes-analyzer \
  --set-env-vars="API_KEY=your-secret-key" \
  --region us-east1
```

### 3. VPC Integration:
For database connections or private resources:
```bash
gcloud run services update easyeyes-analyzer \
  --vpc-connector=your-vpc-connector \
  --region us-east1
```

## 📚 Additional Resources

- [Cloud Run Documentation](https://cloud.google.com/run/docs)
- [Dockerfile Best Practices](https://docs.docker.com/develop/dev-best-practices/)
- [R Docker Images (Rocker)](https://rocker-project.org/)
- [Google Cloud Pricing Calculator](https://cloud.google.com/products/calculator)

## 🆘 Support

If you encounter issues:
1. Check the troubleshooting section above
2. Review Cloud Run logs in the GCP Console
3. Test the Docker image locally first
4. Consult the [Cloud Run documentation](https://cloud.google.com/run/docs)

---

**Last Updated**: December 2024
**Tested with**: R 4.3+, Shiny 1.7+, Google Cloud SDK 450+
