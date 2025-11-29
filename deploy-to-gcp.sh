#!/bin/bash

# Deploy Shiny App to Google Cloud Platform
# Make sure to replace YOUR_PROJECT_ID with your actual GCP project ID

set -e

# Configuration
PROJECT_ID="praxis-zoo-380019"  # Replace with your GCP project ID
IMAGE_NAME="easyeyes-analyzer"
REGION="us-central1"
SERVICE_NAME="easyeyes-analyzer"

echo "🚀 Starting deployment to Google Cloud Platform..."

# Check if gcloud is installed
if ! command -v gcloud &> /dev/null; then
    echo "❌ gcloud CLI is not installed. Please install it first:"
    echo "https://cloud.google.com/sdk/docs/install"
    exit 1
fi

# Check if user is authenticated
if ! gcloud auth list --filter=status:ACTIVE --format="value(account)" | grep -q .; then
    echo "❌ You are not authenticated with gcloud. Please run:"
    echo "gcloud auth login"
    exit 1
fi

# Set the project
echo "📋 Setting GCP project to: $PROJECT_ID"
gcloud config set project $PROJECT_ID

# Enable required APIs
echo "🔧 Enabling required APIs..."
gcloud services enable cloudbuild.googleapis.com
gcloud services enable run.googleapis.com
gcloud services enable containerregistry.googleapis.com

# Build the Docker image using Cloud Build
echo "🏗️  Building Docker image using Cloud Build..."
gcloud builds submit --tag gcr.io/$PROJECT_ID/$IMAGE_NAME .

# Deploy to Cloud Run
echo "🚀 Deploying to Cloud Run..."
gcloud run deploy $SERVICE_NAME \
    --image gcr.io/$PROJECT_ID/$IMAGE_NAME \
    --platform managed \
    --region $REGION \
    --allow-unauthenticated \
    --memory 2Gi \
    --cpu 2 \
    --timeout 3600 \
    --max-instances 10 \
    --port 3838

echo "✅ Deployment complete!"
echo "🌐 Your app should be available at:"
gcloud run services describe $SERVICE_NAME --region $REGION --format 'value(status.url)'

echo ""
echo "📝 Next steps:"
echo "1. Test your application at the URL above"
echo "2. Configure a custom domain if needed"
echo "3. Set up monitoring and logging"
echo "4. Configure authentication if required"
