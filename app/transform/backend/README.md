# CardDemo Backend Service

Modern Java Spring Boot 3 backend service for the CardDemo credit card management application.

## Technology Stack

- **Java**: JDK 17
- **Framework**: Spring Boot 3.2.1
- **Database**: PostgreSQL 16.9
- **Build Tool**: Maven
- **Migration**: Flyway

## Prerequisites

- JDK 17 or higher
- Maven 3.8+
- PostgreSQL 16.9
- Docker (optional, for containerized deployment)

## Getting Started

### Local Development

1. **Start PostgreSQL database**:
   ```bash
   # Using Docker
   docker run -d \
     --name carddemo-postgres \
     -e POSTGRES_DB=carddemo \
     -e POSTGRES_USER=carddemo_user \
     -e POSTGRES_PASSWORD=carddemo_pass \
     -p 5432:5432 \
     postgres:16.9
   ```

2. **Build the application**:
   ```bash
   mvn clean install
   ```

3. **Run the application**:
   ```bash
   mvn spring-boot:run
   ```

   Or with a specific profile:
   ```bash
   mvn spring-boot:run -Dspring-boot.run.profiles=dev
   ```

4. **Access the API**:
   - Base URL: `http://localhost:8080/api`
   - Health check: `http://localhost:8080/api/actuator/health`

### Running Tests

```bash
mvn test
```

## Configuration

### Environment Variables

For production deployment, set the following environment variables:

- `SPRING_PROFILE`: Active profile (dev/prod)
- `DATABASE_URL`: PostgreSQL connection URL
- `DATABASE_USERNAME`: Database username
- `DATABASE_PASSWORD`: Database password
- `JWT_SECRET`: Secret key for JWT token generation
- `JWT_EXPIRATION`: Token expiration time in milliseconds
- `SERVER_PORT`: Server port (default: 8080)
- `CORS_ALLOWED_ORIGINS`: Comma-separated list of allowed origins

### Profiles

- **dev**: Development profile with debug logging and local database
- **prod**: Production profile with optimized settings
- **test**: Test profile using H2 in-memory database

## Project Structure

```
src/
├── main/
│   ├── java/com/carddemo/
│   │   ├── config/          # Configuration classes
│   │   ├── controller/      # REST controllers
│   │   ├── dto/             # Data Transfer Objects
│   │   ├── entity/          # JPA entities
│   │   ├── exception/       # Exception handlers
│   │   ├── repository/      # JPA repositories
│   │   ├── security/        # Security configuration
│   │   └── service/         # Business logic services
│   └── resources/
│       ├── db/migration/    # Flyway migration scripts
│       └── application.yml  # Application configuration
└── test/
    └── java/com/carddemo/   # Test classes
```

## API Documentation

API endpoints will be documented using OpenAPI/Swagger (to be added in future tasks).

## Database Migrations

Database schema changes are managed using Flyway. Migration scripts are located in `src/main/resources/db/migration/`.

Migration naming convention: `V{version}__{description}.sql`

Example: `V1__initial_schema.sql`

## Security

- JWT-based authentication
- BCrypt password hashing
- CORS configuration for frontend integration
- Role-based access control (Admin/User)

## License

Copyright AWS - CardDemo Modernization Project
