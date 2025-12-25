#!/bin/bash

# Load Sample Data Script for CardDemo Application
# This script loads sample data into the PostgreSQL database

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DATABASE_DIR="$PROJECT_ROOT/database"
SAMPLE_DATA_FILE="$DATABASE_DIR/sample-data.sql"

# Database connection parameters
DB_HOST="${DB_HOST:-localhost}"
DB_PORT="${DB_PORT:-5432}"
DB_NAME="${DB_NAME:-carddemo}"
DB_USER="${DB_USER:-carddemo_user}"
DB_PASSWORD="${DB_PASSWORD:-carddemo_pass}"

# Docker container name (if using Docker)
DOCKER_CONTAINER="${DOCKER_CONTAINER:-carddemo-postgres}"

echo -e "${YELLOW}CardDemo Sample Data Loader${NC}"
echo "================================"
echo ""

# Check if sample data file exists
if [ ! -f "$SAMPLE_DATA_FILE" ]; then
    echo -e "${RED}Error: Sample data file not found at $SAMPLE_DATA_FILE${NC}"
    exit 1
fi

echo -e "${GREEN}Found sample data file: $SAMPLE_DATA_FILE${NC}"
echo ""

# Determine connection method
if docker ps --format '{{.Names}}' | grep -q "^${DOCKER_CONTAINER}$"; then
    echo -e "${YELLOW}Using Docker container: $DOCKER_CONTAINER${NC}"
    echo ""
    
    # Load data using Docker exec
    echo -e "${YELLOW}Loading sample data...${NC}"
    docker exec -i "$DOCKER_CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" < "$SAMPLE_DATA_FILE"
    
    if [ $? -eq 0 ]; then
        echo ""
        echo -e "${GREEN}✓ Sample data loaded successfully!${NC}"
        echo ""
        
        # Display summary
        echo -e "${YELLOW}Data Summary:${NC}"
        docker exec "$DOCKER_CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" -c "
            SELECT 'Users' as table_name, COUNT(*) as count FROM users
            UNION ALL
            SELECT 'Accounts', COUNT(*) FROM accounts
            UNION ALL
            SELECT 'Customers', COUNT(*) FROM customers
            UNION ALL
            SELECT 'Cards', COUNT(*) FROM cards
            UNION ALL
            SELECT 'Card XRef', COUNT(*) FROM card_xref
            UNION ALL
            SELECT 'Transactions', COUNT(*) FROM transactions;
        "
    else
        echo -e "${RED}✗ Failed to load sample data${NC}"
        exit 1
    fi
else
    echo -e "${YELLOW}Using direct PostgreSQL connection${NC}"
    echo "Host: $DB_HOST:$DB_PORT"
    echo "Database: $DB_NAME"
    echo "User: $DB_USER"
    echo ""
    
    # Check if psql is available
    if ! command -v psql &> /dev/null; then
        echo -e "${RED}Error: psql command not found. Please install PostgreSQL client tools.${NC}"
        exit 1
    fi
    
    # Load data using psql
    echo -e "${YELLOW}Loading sample data...${NC}"
    PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" < "$SAMPLE_DATA_FILE"
    
    if [ $? -eq 0 ]; then
        echo ""
        echo -e "${GREEN}✓ Sample data loaded successfully!${NC}"
        echo ""
        
        # Display summary
        echo -e "${YELLOW}Data Summary:${NC}"
        PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -c "
            SELECT 'Users' as table_name, COUNT(*) as count FROM users
            UNION ALL
            SELECT 'Accounts', COUNT(*) FROM accounts
            UNION ALL
            SELECT 'Customers', COUNT(*) FROM customers
            UNION ALL
            SELECT 'Cards', COUNT(*) FROM cards
            UNION ALL
            SELECT 'Card XRef', COUNT(*) FROM card_xref
            UNION ALL
            SELECT 'Transactions', COUNT(*) FROM transactions;
        "
    else
        echo -e "${RED}✗ Failed to load sample data${NC}"
        exit 1
    fi
fi

echo ""
echo -e "${GREEN}Sample data loading complete!${NC}"
echo ""
echo -e "${YELLOW}Test Credentials:${NC}"
echo "  Admin User:"
echo "    Username: U0001"
echo "    Password: password"
echo ""
echo "  Regular Users:"
echo "    Username: U0002, U0003, U0004"
echo "    Password: password"
echo ""
