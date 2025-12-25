# CardDemo Database Setup

This directory contains database initialization and migration scripts for PostgreSQL 16.9.

## Quick Start

### Using Docker

The easiest way to set up PostgreSQL is using Docker:

```bash
docker run -d \
  --name carddemo-postgres \
  -e POSTGRES_DB=carddemo \
  -e POSTGRES_USER=carddemo_user \
  -e POSTGRES_PASSWORD=carddemo_pass \
  -p 5432:5432 \
  postgres:16.9
```

### Manual Installation

If you have PostgreSQL 16.9 installed locally:

1. **Create the database and user**:
   ```bash
   psql -U postgres -f init-db.sql
   ```

2. **Verify connection**:
   ```bash
   psql -U carddemo_user -d carddemo -h localhost
   ```

## Database Configuration

### Connection Details

- **Host**: localhost (or postgres for Docker)
- **Port**: 5432
- **Database**: carddemo
- **Username**: carddemo_user
- **Password**: carddemo_pass

### Connection String

```
jdbc:postgresql://localhost:5432/carddemo
```

## Schema Management

Database schema is managed using Flyway migrations located in:
```
app/transform/backend/src/main/resources/db/migration/
```

Migrations are automatically applied when the Spring Boot application starts.

### Migration Naming Convention

```
V{version}__{description}.sql
```

Examples:
- `V1__initial_schema.sql`
- `V2__add_indexes.sql`
- `V3__add_audit_columns.sql`

## Connection Pooling

The application uses HikariCP for connection pooling with the following configuration:

**Development**:
- Maximum pool size: 10
- Minimum idle: 5

**Production**:
- Maximum pool size: 20
- Minimum idle: 10

## Backup and Restore

### Backup

```bash
pg_dump -U carddemo_user -d carddemo -F c -f carddemo_backup.dump
```

### Restore

```bash
pg_restore -U carddemo_user -d carddemo -c carddemo_backup.dump
```

## Monitoring

### Check Active Connections

```sql
SELECT count(*) FROM pg_stat_activity WHERE datname = 'carddemo';
```

### Check Database Size

```sql
SELECT pg_size_pretty(pg_database_size('carddemo'));
```

### Check Table Sizes

```sql
SELECT 
  schemaname,
  tablename,
  pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size
FROM pg_tables
WHERE schemaname = 'public'
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC;
```

## Troubleshooting

### Connection Refused

If you get "connection refused" errors:

1. Check PostgreSQL is running:
   ```bash
   docker ps  # for Docker
   # or
   sudo systemctl status postgresql  # for system installation
   ```

2. Verify port 5432 is accessible:
   ```bash
   telnet localhost 5432
   ```

3. Check PostgreSQL logs:
   ```bash
   docker logs carddemo-postgres  # for Docker
   # or
   sudo tail -f /var/log/postgresql/postgresql-16-main.log  # for system installation
   ```

### Authentication Failed

If you get authentication errors:

1. Verify credentials in `application.yml`
2. Check user exists:
   ```sql
   SELECT usename FROM pg_user WHERE usename = 'carddemo_user';
   ```

3. Reset password if needed:
   ```sql
   ALTER USER carddemo_user WITH PASSWORD 'new_password';
   ```

## Security Notes

**Important**: The default credentials are for development only. For production:

1. Use strong passwords
2. Store credentials in environment variables or secrets management
3. Enable SSL/TLS connections
4. Restrict network access using firewall rules
5. Regular security updates for PostgreSQL

## Performance Tuning

For production deployments, consider tuning these PostgreSQL parameters:

```
shared_buffers = 256MB
effective_cache_size = 1GB
maintenance_work_mem = 64MB
checkpoint_completion_target = 0.9
wal_buffers = 16MB
default_statistics_target = 100
random_page_cost = 1.1
effective_io_concurrency = 200
work_mem = 4MB
min_wal_size = 1GB
max_wal_size = 4GB
```

Add these to `postgresql.conf` and restart PostgreSQL.
