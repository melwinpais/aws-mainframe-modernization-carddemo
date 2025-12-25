# CardDemo Modernization - Deployment Guide

This guide provides comprehensive instructions for deploying the modernized CardDemo application in various environments.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Environment Variables](#environment-variables)
3. [Database Initialization](#database-initialization)
4. [Deployment Steps](#deployment-steps)
5. [Production Deployment](#production-deployment)
6. [Verification](#verification)
7. [Troubleshooting](#troubleshooting)

## Prerequisites

### Required Software

- **Docker**: Version 20.10 or higher
- **Docker Compose**: Version 2.0 or higher
- **Git**: For cloning the repository

### System Requirements

**Minimum**:
- CPU: 2 cores
- RAM: 4 GB
- Disk: 10 GB free space

**Recommended**:
- CPU: 4 cores
- RAM: 8 GB
- Disk: 20 GB free space

### Network Requirements

The following ports must be available:
- **80**: Frontend (HTTP)
- **443**: Frontend (HTTPS, production only)
- **8080**: Backend API
- **5432**: PostgreSQL database

## Environment Variables

### Required Variables

Create a `.env` file in the `app/transform/` directory with the following variables:

```bash
# JWT Configuration (REQUIRED)
JWT_SECRET=your-secure-secret-key-here-minimum-32-characters

# Database Configuration
DATABASE_URL=jdbc:postgresql://postgres:5432/carddemo
DATABASE_USERNAME=carddemo_user
DATABASE_PASSWORD=carddemo_password_change_in_production

# Backend Configuration
SERVER_PORT=8080
SPRING_PROFILES_ACTIVE=prod

# Frontend Configuration
VITE_API_BASE_URL=http://localhost:8080/api

# CORS Configuration
CORS_ALLOWED_ORIGINS=http://localhost,http://localhost:80

# Logging Configuration
LOG_LEVEL=INFO
```

### Generating Secure JWT Secret

For production, generate a secure JWT secret:

```bash
# Using OpenSSL (recommended)
openssl rand -base64 32

# Using Python
python3 -c "import secrets; print(secrets.token_urlsafe(32))"

# Using Node.js
node -e "console.log(require('crypto').randomBytes(32).toString('base64'))"
```

### Environment-Specific Configuration

#### Development Environment

```bash
JWT_SECRET=dev-secret-key-not-for-production
DATABASE_PASSWORD=dev_password
SPRING_PROFILES_ACTIVE=dev
LOG_LEVEL=DEBUG
CORS_ALLOWED_ORIGINS=http://localhost:5173,http://localhost:3000
```

#### Staging Environment

```bash
JWT_SECRET=<generated-secure-key>
DATABASE_PASSWORD=<strong-password>
SPRING_PROFILES_ACTIVE=staging
LOG_LEVEL=INFO
CORS_ALLOWED_ORIGINS=https://staging.yourdomain.com
```

#### Production Environment

```bash
JWT_SECRET=<generated-secure-key>
DATABASE_PASSWORD=<strong-password>
SPRING_PROFILES_ACTIVE=prod
LOG_LEVEL=WARN
CORS_ALLOWED_ORIGINS=https://yourdomain.com
```

## Database Initialization

### Automatic Initialization (Recommended)

The database is automatically initialized when the backend service starts for the first time using Flyway migrations.

**Migration Files Location**: `backend/src/main/resources/db/migration/`

**Initial Schema**: `V1__initial_schema.sql`

### Manual Initialization

If you need to manually initialize the database:

1. **Start only the database**:
   ```bash
   docker-compose up -d postgres
   ```

2. **Wait for database to be ready**:
   ```bash
   docker-compose exec postgres pg_isready -U carddemo_user -d carddemo
   ```

3. **Run migration script manually**:
   ```bash
   docker-compose exec postgres psql -U carddemo_user -d carddemo -f /docker-entrypoint-initdb.d/init-db.sql
   ```

### Database Schema Verification

Verify the database schema was created successfully:

```bash
# Connect to database
docker-compose exec postgres psql -U carddemo_user -d carddemo

# List all tables
\dt

# Expected tables:
# - users
# - accounts
# - customers
# - cards
# - transactions
# - card_xref
# - flyway_schema_history

# Exit psql
\q
```

### Database Backup and Restore

**Backup**:
```bash
docker-compose exec postgres pg_dump -U carddemo_user carddemo > backup_$(date +%Y%m%d_%H%M%S).sql
```

**Restore**:
```bash
cat backup_file.sql | docker-compose exec -T postgres psql -U carddemo_user -d carddemo
```

## Deployment Steps

### Step 1: Clone Repository

```bash
git clone <repository-url>
cd app/transform
```

### Step 2: Configure Environment

```bash
# Copy example environment file
cp .env.example .env

# Edit .env file with your configuration
nano .env  # or use your preferred editor

# IMPORTANT: Set JWT_SECRET to a secure value
```

### Step 3: Build Docker Images

```bash
# Build all images
docker-compose build

# Or build individually
docker-compose build postgres
docker-compose build backend
docker-compose build frontend
```

**Build Output Verification**:
- Backend build should complete without errors
- Frontend build should complete without errors
- All dependencies should be downloaded successfully

### Step 4: Start Services

```bash
# Start all services in detached mode
docker-compose up -d

# View startup logs
docker-compose logs -f
```

**Startup Order**:
1. PostgreSQL database starts first
2. Backend waits for database to be ready
3. Backend runs Flyway migrations
4. Frontend starts and connects to backend

### Step 5: Verify Deployment

See [Verification](#verification) section below.

### Step 6: Load Sample Data (Optional)

See Task 24.3 for sample data loading instructions.

## Production Deployment

### Security Hardening

1. **Use Strong Secrets**:
   ```bash
   # Generate secure JWT secret
   JWT_SECRET=$(openssl rand -base64 32)
   
   # Generate secure database password
   DATABASE_PASSWORD=$(openssl rand -base64 24)
   ```

2. **Enable HTTPS**:
   - Configure SSL/TLS certificates
   - Update nginx configuration in `frontend/nginx.conf`
   - Redirect HTTP to HTTPS

3. **Restrict CORS Origins**:
   ```bash
   CORS_ALLOWED_ORIGINS=https://yourdomain.com
   ```

4. **Disable Debug Logging**:
   ```bash
   LOG_LEVEL=WARN
   SPRING_PROFILES_ACTIVE=prod
   ```

5. **Use Docker Secrets** (Docker Swarm):
   ```yaml
   secrets:
     jwt_secret:
       external: true
     db_password:
       external: true
   ```

### Resource Limits

Add resource limits to `docker-compose.yml`:

```yaml
services:
  backend:
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 2G
        reservations:
          cpus: '1'
          memory: 1G
  
  frontend:
    deploy:
      resources:
        limits:
          cpus: '1'
          memory: 512M
        reservations:
          cpus: '0.5'
          memory: 256M
  
  postgres:
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 2G
        reservations:
          cpus: '1'
          memory: 1G
```

### Health Checks

Configure health checks in `docker-compose.yml`:

```yaml
services:
  backend:
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/api/actuator/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 60s
  
  frontend:
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost/"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 30s
  
  postgres:
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U carddemo_user -d carddemo"]
      interval: 10s
      timeout: 5s
      retries: 5
```

### Monitoring and Logging

1. **Centralized Logging**:
   - Configure log aggregation (ELK, Splunk, CloudWatch)
   - Set up log rotation
   - Monitor error rates

2. **Application Monitoring**:
   - Enable Spring Boot Actuator endpoints
   - Configure Prometheus metrics
   - Set up Grafana dashboards

3. **Database Monitoring**:
   - Monitor connection pool usage
   - Track query performance
   - Set up automated backups

### Backup Strategy

1. **Database Backups**:
   ```bash
   # Daily backup script
   #!/bin/bash
   BACKUP_DIR=/backups
   DATE=$(date +%Y%m%d_%H%M%S)
   docker-compose exec -T postgres pg_dump -U carddemo_user carddemo | gzip > $BACKUP_DIR/carddemo_$DATE.sql.gz
   
   # Keep last 30 days
   find $BACKUP_DIR -name "carddemo_*.sql.gz" -mtime +30 -delete
   ```

2. **Schedule with Cron**:
   ```bash
   # Add to crontab
   0 2 * * * /path/to/backup-script.sh
   ```

### High Availability

For production high availability:

1. **Database Replication**:
   - Set up PostgreSQL streaming replication
   - Configure read replicas
   - Implement automatic failover

2. **Load Balancing**:
   - Deploy multiple backend instances
   - Use nginx or HAProxy for load balancing
   - Configure session affinity if needed

3. **Container Orchestration**:
   - Use Kubernetes for orchestration
   - Configure auto-scaling
   - Implement rolling updates

## Verification

### 1. Check Service Status

```bash
# Check all services are running
docker-compose ps

# Expected output:
# NAME                STATUS              PORTS
# postgres            Up (healthy)        5432/tcp
# backend             Up (healthy)        8080/tcp
# frontend            Up (healthy)        80/tcp
```

### 2. Verify Database

```bash
# Check database is ready
docker-compose exec postgres pg_isready -U carddemo_user -d carddemo

# Connect to database
docker-compose exec postgres psql -U carddemo_user -d carddemo

# Verify tables exist
\dt

# Check Flyway migration history
SELECT * FROM flyway_schema_history;

# Exit
\q
```

### 3. Verify Backend API

```bash
# Health check
curl http://localhost:8080/api/actuator/health

# Expected response:
# {"status":"UP"}

# Test authentication endpoint
curl -X POST http://localhost:8080/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"userId":"testuser","password":"testpass"}'

# Should return 401 or valid response if test user exists
```

### 4. Verify Frontend

```bash
# Check frontend is accessible
curl http://localhost/

# Should return HTML content

# Check frontend can reach backend
curl http://localhost/api/actuator/health

# Should proxy to backend and return health status
```

### 5. End-to-End Test

1. **Open browser**: Navigate to `http://localhost`
2. **Login page**: Should display login form
3. **Test login**: Try logging in (will fail without sample data)
4. **Check console**: No JavaScript errors should appear

### 6. Check Logs

```bash
# View all logs
docker-compose logs

# View specific service logs
docker-compose logs backend
docker-compose logs frontend
docker-compose logs postgres

# Follow logs in real-time
docker-compose logs -f backend
```

## Troubleshooting

### Services Won't Start

**Problem**: Docker Compose fails to start services

**Solutions**:
1. Check Docker is running:
   ```bash
   docker info
   ```

2. Check for port conflicts:
   ```bash
   # macOS/Linux
   lsof -i :80
   lsof -i :8080
   lsof -i :5432
   
   # Windows
   netstat -ano | findstr :80
   netstat -ano | findstr :8080
   netstat -ano | findstr :5432
   ```

3. Check Docker Compose configuration:
   ```bash
   docker-compose config
   ```

4. View detailed logs:
   ```bash
   docker-compose up
   ```

### Database Connection Errors

**Problem**: Backend cannot connect to database

**Solutions**:
1. Verify database is running:
   ```bash
   docker-compose ps postgres
   ```

2. Check database logs:
   ```bash
   docker-compose logs postgres
   ```

3. Verify connection parameters:
   ```bash
   docker-compose exec backend env | grep DATABASE
   ```

4. Test connection manually:
   ```bash
   docker-compose exec postgres psql -U carddemo_user -d carddemo
   ```

5. Check network connectivity:
   ```bash
   docker-compose exec backend ping postgres
   ```

### Backend Won't Start

**Problem**: Backend service fails to start

**Solutions**:
1. Check backend logs:
   ```bash
   docker-compose logs backend
   ```

2. Verify Java version:
   ```bash
   docker-compose exec backend java -version
   # Should be JDK 17
   ```

3. Check Flyway migrations:
   ```bash
   docker-compose logs backend | grep Flyway
   ```

4. Verify environment variables:
   ```bash
   docker-compose exec backend env
   ```

5. Check for missing JWT_SECRET:
   ```bash
   grep JWT_SECRET .env
   ```

### Frontend Issues

**Problem**: Frontend not accessible or shows errors

**Solutions**:
1. Check frontend logs:
   ```bash
   docker-compose logs frontend
   ```

2. Verify nginx configuration:
   ```bash
   docker-compose exec frontend nginx -t
   ```

3. Check backend connectivity:
   ```bash
   docker-compose exec frontend curl http://backend:8080/api/actuator/health
   ```

4. Verify environment variables:
   ```bash
   docker-compose exec frontend env | grep VITE
   ```

### Migration Errors

**Problem**: Flyway migration fails

**Solutions**:
1. Check migration logs:
   ```bash
   docker-compose logs backend | grep Flyway
   ```

2. Verify migration files:
   ```bash
   ls -la backend/src/main/resources/db/migration/
   ```

3. Check Flyway schema history:
   ```bash
   docker-compose exec postgres psql -U carddemo_user -d carddemo -c "SELECT * FROM flyway_schema_history;"
   ```

4. Reset database (CAUTION: destroys all data):
   ```bash
   docker-compose down -v
   docker-compose up -d
   ```

### Performance Issues

**Problem**: Application is slow or unresponsive

**Solutions**:
1. Check resource usage:
   ```bash
   docker stats
   ```

2. Check database connections:
   ```bash
   docker-compose exec postgres psql -U carddemo_user -d carddemo -c "SELECT count(*) FROM pg_stat_activity;"
   ```

3. Review slow queries:
   ```bash
   docker-compose exec postgres psql -U carddemo_user -d carddemo -c "SELECT query, calls, total_time FROM pg_stat_statements ORDER BY total_time DESC LIMIT 10;"
   ```

4. Increase resource limits in docker-compose.yml

### CORS Errors

**Problem**: Frontend shows CORS errors in browser console

**Solutions**:
1. Verify CORS configuration:
   ```bash
   grep CORS_ALLOWED_ORIGINS .env
   ```

2. Check backend CORS settings:
   ```bash
   docker-compose logs backend | grep CORS
   ```

3. Update CORS_ALLOWED_ORIGINS to include frontend URL:
   ```bash
   CORS_ALLOWED_ORIGINS=http://localhost,http://localhost:80
   ```

4. Restart backend:
   ```bash
   docker-compose restart backend
   ```

## Rollback Procedure

If deployment fails, follow these steps to rollback:

1. **Stop current deployment**:
   ```bash
   docker-compose down
   ```

2. **Restore database backup**:
   ```bash
   cat backup_file.sql | docker-compose exec -T postgres psql -U carddemo_user -d carddemo
   ```

3. **Checkout previous version**:
   ```bash
   git checkout <previous-tag>
   ```

4. **Rebuild and restart**:
   ```bash
   docker-compose build
   docker-compose up -d
   ```

5. **Verify rollback**:
   ```bash
   docker-compose ps
   curl http://localhost:8080/api/actuator/health
   ```

## Support and Maintenance

### Regular Maintenance Tasks

1. **Weekly**:
   - Review application logs
   - Check disk space usage
   - Monitor error rates

2. **Monthly**:
   - Update dependencies
   - Review security patches
   - Test backup restoration

3. **Quarterly**:
   - Performance tuning
   - Capacity planning
   - Security audit

### Getting Help

For issues not covered in this guide:

1. Check application logs
2. Review Docker Compose logs
3. Consult the main README.md
4. Check component-specific READMEs:
   - backend/README.md
   - frontend/README.md
   - database/README.md

## Appendix

### Useful Commands

```bash
# View all containers
docker-compose ps

# View logs
docker-compose logs -f [service]

# Restart service
docker-compose restart [service]

# Rebuild service
docker-compose up -d --build [service]

# Execute command in container
docker-compose exec [service] [command]

# View resource usage
docker stats

# Clean up unused resources
docker system prune -a

# Export logs
docker-compose logs > deployment_logs.txt
```

### Environment File Template

See `.env.example` for a complete template with all available configuration options.

### Migration from Legacy System

For migrating data from the original COBOL/CICS/VSAM system, refer to the data migration utilities in Task 12 of the implementation plan.
