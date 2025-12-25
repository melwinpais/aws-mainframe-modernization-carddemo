# CardDemo Modernization

This directory contains the modernized CardDemo application using modern technologies.

## Architecture

- **Backend**: Java Spring Boot 3 with JDK 17
- **Frontend**: Vue.js 3 with Vite
- **Database**: PostgreSQL 16.9
- **Containerization**: Docker

## Directory Structure

```
app/transform/
├── backend/           # Spring Boot backend service
├── frontend/          # Vue.js frontend application
├── database/          # Database scripts and configuration
├── docker-compose.yml # Docker orchestration
└── README.md         # This file
```

## Quick Start with Docker

### Prerequisites

- Docker 20.10+
- Docker Compose 2.0+

### Start All Services

1. **Clone the repository and navigate to this directory**:
   ```bash
   cd app/transform
   ```

2. **Create environment file**:
   ```bash
   cp .env.example .env
   # Edit .env and set JWT_SECRET to a secure value
   ```

3. **Start all services**:
   ```bash
   docker-compose up -d
   ```

4. **Check service health**:
   ```bash
   docker-compose ps
   ```

5. **Access the application**:
   - Frontend: http://localhost
   - Backend API: http://localhost:8080/api
   - Database: localhost:5432

### Stop All Services

```bash
docker-compose down
```

To also remove volumes (database data):
```bash
docker-compose down -v
```

## Development Setup

### Backend Development

See [backend/README.md](backend/README.md) for detailed backend setup instructions.

Quick start:
```bash
cd backend
mvn spring-boot:run
```

### Frontend Development

See [frontend/README.md](frontend/README.md) for detailed frontend setup instructions.

Quick start:
```bash
cd frontend
npm install
npm run dev
```

### Database Setup

See [database/README.md](database/README.md) for detailed database setup instructions.

Quick start with Docker:
```bash
cd database
docker-compose up -d
```

## Building Docker Images

### Build All Images

```bash
docker-compose build
```

### Build Individual Images

Backend:
```bash
docker build -t carddemo-backend:latest ./backend
```

Frontend:
```bash
docker build -t carddemo-frontend:latest ./frontend
```

## Environment Variables

Key environment variables (see `.env.example`):

- `JWT_SECRET`: Secret key for JWT token generation (required for production)
- `DATABASE_URL`: PostgreSQL connection URL
- `DATABASE_USERNAME`: Database username
- `DATABASE_PASSWORD`: Database password
- `SERVER_PORT`: Backend server port (default: 8080)
- `CORS_ALLOWED_ORIGINS`: Allowed CORS origins

## Monitoring and Logs

### View Logs

All services:
```bash
docker-compose logs -f
```

Specific service:
```bash
docker-compose logs -f backend
docker-compose logs -f frontend
docker-compose logs -f postgres
```

### Health Checks

Backend health:
```bash
curl http://localhost:8080/api/actuator/health
```

Frontend health:
```bash
curl http://localhost/
```

Database health:
```bash
docker-compose exec postgres pg_isready -U carddemo_user -d carddemo
```

## Troubleshooting

### Services Won't Start

1. Check Docker is running:
   ```bash
   docker info
   ```

2. Check for port conflicts:
   ```bash
   lsof -i :80    # Frontend
   lsof -i :8080  # Backend
   lsof -i :5432  # Database
   ```

3. Check service logs:
   ```bash
   docker-compose logs
   ```

### Database Connection Issues

1. Verify database is healthy:
   ```bash
   docker-compose ps postgres
   ```

2. Check database logs:
   ```bash
   docker-compose logs postgres
   ```

3. Test connection:
   ```bash
   docker-compose exec postgres psql -U carddemo_user -d carddemo
   ```

### Backend Won't Start

1. Check if database is ready:
   ```bash
   docker-compose ps postgres
   ```

2. Verify environment variables:
   ```bash
   docker-compose config
   ```

3. Check backend logs:
   ```bash
   docker-compose logs backend
   ```

### Frontend Issues

1. Check if backend is healthy:
   ```bash
   curl http://localhost:8080/api/actuator/health
   ```

2. Check frontend logs:
   ```bash
   docker-compose logs frontend
   ```

3. Verify nginx configuration:
   ```bash
   docker-compose exec frontend nginx -t
   ```

## Production Deployment

For detailed production deployment instructions, see [DEPLOYMENT.md](DEPLOYMENT.md).

Quick checklist:

1. **Set secure JWT secret**:
   ```bash
   export JWT_SECRET=$(openssl rand -base64 32)
   ```

2. **Use strong database password**:
   Update `DATABASE_PASSWORD` in `.env`

3. **Enable HTTPS**:
   Configure SSL/TLS certificates in nginx

4. **Set appropriate CORS origins**:
   Update `CORS_ALLOWED_ORIGINS` to match your domain

5. **Configure resource limits**:
   Add resource limits to docker-compose.yml

6. **Set up monitoring**:
   Integrate with monitoring tools (Prometheus, Grafana, etc.)

7. **Configure backups**:
   Set up automated database backups

For comprehensive deployment documentation including environment variables, database initialization, troubleshooting, and production best practices, refer to [DEPLOYMENT.md](DEPLOYMENT.md).

## Testing

### Run Backend Tests

```bash
cd backend
mvn test
```

### Run Frontend Tests

```bash
cd frontend
npm test
```

## Migration from Legacy System

The modernized system maintains compatibility with the original COBOL/CICS/VSAM CardDemo application. Data migration utilities will be provided in future tasks.

## License

Copyright AWS - CardDemo Modernization Project
