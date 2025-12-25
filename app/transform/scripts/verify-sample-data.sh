#!/bin/bash

# Verify Sample Data Script for CardDemo Application
# This script verifies the integrity of loaded sample data

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Docker container name
DOCKER_CONTAINER="${DOCKER_CONTAINER:-carddemo-postgres}"
DB_USER="${DB_USER:-carddemo_user}"
DB_NAME="${DB_NAME:-carddemo}"

echo -e "${YELLOW}CardDemo Sample Data Verification${NC}"
echo "===================================="
echo ""

# Check if Docker container is running
if ! docker ps --format '{{.Names}}' | grep -q "^${DOCKER_CONTAINER}$"; then
    echo -e "${RED}Error: Docker container $DOCKER_CONTAINER is not running${NC}"
    exit 1
fi

echo -e "${GREEN}✓ Database container is running${NC}"
echo ""

# Function to run SQL query
run_query() {
    docker exec "$DOCKER_CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" -t -c "$1"
}

# Function to check count
check_count() {
    local table=$1
    local expected=$2
    local actual=$(run_query "SELECT COUNT(*) FROM $table;" | tr -d ' ')
    
    if [ "$actual" -eq "$expected" ]; then
        echo -e "${GREEN}✓ $table: $actual records (expected $expected)${NC}"
        return 0
    else
        echo -e "${RED}✗ $table: $actual records (expected $expected)${NC}"
        return 1
    fi
}

# Verify record counts
echo -e "${BLUE}Verifying Record Counts:${NC}"
check_count "users" 4
check_count "customers" 4
check_count "accounts" 4
check_count "cards" 5
check_count "card_xref" 5
check_count "transactions" 8
echo ""

# Verify referential integrity
echo -e "${BLUE}Verifying Referential Integrity:${NC}"

# Check accounts have valid customers
orphaned_accounts=$(run_query "SELECT COUNT(*) FROM accounts a WHERE NOT EXISTS (SELECT 1 FROM customers c WHERE c.customer_id = a.customer_id);" | tr -d ' ')
if [ "$orphaned_accounts" -eq "0" ]; then
    echo -e "${GREEN}✓ All accounts have valid customers${NC}"
else
    echo -e "${RED}✗ Found $orphaned_accounts orphaned accounts${NC}"
fi

# Check cards have valid accounts
orphaned_cards=$(run_query "SELECT COUNT(*) FROM cards c WHERE NOT EXISTS (SELECT 1 FROM accounts a WHERE a.account_id = c.account_id);" | tr -d ' ')
if [ "$orphaned_cards" -eq "0" ]; then
    echo -e "${GREEN}✓ All cards have valid accounts${NC}"
else
    echo -e "${RED}✗ Found $orphaned_cards orphaned cards${NC}"
fi

# Check cards have valid customers
orphaned_card_customers=$(run_query "SELECT COUNT(*) FROM cards c WHERE NOT EXISTS (SELECT 1 FROM customers cu WHERE cu.customer_id = c.customer_id);" | tr -d ' ')
if [ "$orphaned_card_customers" -eq "0" ]; then
    echo -e "${GREEN}✓ All cards have valid customers${NC}"
else
    echo -e "${RED}✗ Found $orphaned_card_customers cards with invalid customers${NC}"
fi

# Check transactions have valid accounts
orphaned_txn_accounts=$(run_query "SELECT COUNT(*) FROM transactions t WHERE NOT EXISTS (SELECT 1 FROM accounts a WHERE a.account_id = t.account_id);" | tr -d ' ')
if [ "$orphaned_txn_accounts" -eq "0" ]; then
    echo -e "${GREEN}✓ All transactions have valid accounts${NC}"
else
    echo -e "${RED}✗ Found $orphaned_txn_accounts transactions with invalid accounts${NC}"
fi

# Check transactions have valid cards
orphaned_txn_cards=$(run_query "SELECT COUNT(*) FROM transactions t WHERE NOT EXISTS (SELECT 1 FROM cards c WHERE c.card_number = t.card_number);" | tr -d ' ')
if [ "$orphaned_txn_cards" -eq "0" ]; then
    echo -e "${GREEN}✓ All transactions have valid cards${NC}"
else
    echo -e "${RED}✗ Found $orphaned_txn_cards transactions with invalid cards${NC}"
fi

# Check card_xref integrity
orphaned_xref=$(run_query "SELECT COUNT(*) FROM card_xref cx WHERE NOT EXISTS (SELECT 1 FROM cards c WHERE c.card_number = cx.card_number) OR NOT EXISTS (SELECT 1 FROM accounts a WHERE a.account_id = cx.account_id) OR NOT EXISTS (SELECT 1 FROM customers cu WHERE cu.customer_id = cx.customer_id);" | tr -d ' ')
if [ "$orphaned_xref" -eq "0" ]; then
    echo -e "${GREEN}✓ All card_xref entries are valid${NC}"
else
    echo -e "${RED}✗ Found $orphaned_xref invalid card_xref entries${NC}"
fi

echo ""

# Verify data quality
echo -e "${BLUE}Verifying Data Quality:${NC}"

# Check for users with valid user types
invalid_user_types=$(run_query "SELECT COUNT(*) FROM users WHERE user_type NOT IN ('A', 'U');" | tr -d ' ')
if [ "$invalid_user_types" -eq "0" ]; then
    echo -e "${GREEN}✓ All users have valid user types (A or U)${NC}"
else
    echo -e "${RED}✗ Found $invalid_user_types users with invalid user types${NC}"
fi

# Check for accounts with valid status
invalid_account_status=$(run_query "SELECT COUNT(*) FROM accounts WHERE active_status NOT IN ('Y', 'N');" | tr -d ' ')
if [ "$invalid_account_status" -eq "0" ]; then
    echo -e "${GREEN}✓ All accounts have valid status (Y or N)${NC}"
else
    echo -e "${RED}✗ Found $invalid_account_status accounts with invalid status${NC}"
fi

# Check for cards with valid status
invalid_card_status=$(run_query "SELECT COUNT(*) FROM cards WHERE card_status NOT IN ('ACTIVE', 'INACTIVE', 'BLOCKED', 'EXPIRED');" | tr -d ' ')
if [ "$invalid_card_status" -eq "0" ]; then
    echo -e "${GREEN}✓ All cards have valid status${NC}"
else
    echo -e "${RED}✗ Found $invalid_card_status cards with invalid status${NC}"
fi

# Check for negative credit limits
negative_limits=$(run_query "SELECT COUNT(*) FROM accounts WHERE credit_limit < 0;" | tr -d ' ')
if [ "$negative_limits" -eq "0" ]; then
    echo -e "${GREEN}✓ All accounts have non-negative credit limits${NC}"
else
    echo -e "${RED}✗ Found $negative_limits accounts with negative credit limits${NC}"
fi

# Check for valid card numbers (16 digits)
invalid_card_numbers=$(run_query "SELECT COUNT(*) FROM cards WHERE LENGTH(card_number::text) != 16;" | tr -d ' ')
if [ "$invalid_card_numbers" -eq "0" ]; then
    echo -e "${GREEN}✓ All card numbers are 16 digits${NC}"
else
    echo -e "${RED}✗ Found $invalid_card_numbers cards with invalid card numbers${NC}"
fi

echo ""

# Display sample data summary
echo -e "${BLUE}Sample Data Summary:${NC}"
echo ""

echo -e "${YELLOW}Users:${NC}"
docker exec "$DOCKER_CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" -c "SELECT user_id, first_name, last_name, user_type FROM users ORDER BY user_id;"
echo ""

echo -e "${YELLOW}Customers with Accounts:${NC}"
docker exec "$DOCKER_CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" -c "SELECT c.customer_id, c.first_name, c.last_name, COUNT(a.account_id) as num_accounts, COUNT(ca.card_number) as num_cards FROM customers c LEFT JOIN accounts a ON c.customer_id = a.customer_id LEFT JOIN cards ca ON c.customer_id = ca.customer_id GROUP BY c.customer_id, c.first_name, c.last_name ORDER BY c.customer_id;"
echo ""

echo -e "${YELLOW}Account Balances:${NC}"
docker exec "$DOCKER_CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" -c "SELECT a.account_id, c.first_name || ' ' || c.last_name as customer_name, a.current_balance, a.credit_limit, a.active_status FROM accounts a JOIN customers c ON a.customer_id = c.customer_id ORDER BY a.account_id;"
echo ""

echo -e "${YELLOW}Recent Transactions:${NC}"
docker exec "$DOCKER_CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" -c "SELECT t.transaction_id, t.account_id, t.transaction_type, t.amount, t.transaction_date, t.merchant_name FROM transactions t ORDER BY t.transaction_date DESC, t.transaction_id DESC LIMIT 5;"
echo ""

echo -e "${GREEN}Sample data verification complete!${NC}"
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
