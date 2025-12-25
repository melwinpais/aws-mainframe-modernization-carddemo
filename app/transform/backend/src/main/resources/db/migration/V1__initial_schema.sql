-- CardDemo Initial Schema Migration
-- Version: 1.0
-- Description: Creates all tables for the modernized CardDemo application
-- Requirements: 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8, 11.9

-- ============================================================================
-- Table: users
-- Purpose: Store user authentication and authorization information
-- Requirement: 11.1
-- ============================================================================
CREATE TABLE users (
    user_id VARCHAR(8) PRIMARY KEY,
    first_name VARCHAR(20) NOT NULL,
    last_name VARCHAR(20) NOT NULL,
    password VARCHAR(255) NOT NULL,  -- Hashed with bcrypt
    user_type CHAR(1) NOT NULL CHECK (user_type IN ('A', 'U')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Index for filtering users by type
CREATE INDEX idx_users_type ON users(user_type);

COMMENT ON TABLE users IS 'User authentication and authorization records';
COMMENT ON COLUMN users.user_id IS 'Unique user identifier, 8 characters, uppercase';
COMMENT ON COLUMN users.user_type IS 'User type: A=Admin, U=Regular User';
COMMENT ON COLUMN users.password IS 'Bcrypt hashed password';

-- ============================================================================
-- Table: customers
-- Purpose: Store customer personal information
-- Requirement: 11.3
-- ============================================================================
CREATE TABLE customers (
    customer_id NUMERIC(9) PRIMARY KEY,
    first_name VARCHAR(25) NOT NULL,
    middle_name VARCHAR(25),
    last_name VARCHAR(25) NOT NULL,
    address_line_1 VARCHAR(50),
    address_line_2 VARCHAR(50),
    address_line_3 VARCHAR(50),
    state_code CHAR(2),
    country_code CHAR(3),
    zip_code VARCHAR(10),
    phone_number_1 VARCHAR(15),
    phone_number_2 VARCHAR(15),
    ssn NUMERIC(9),
    government_issued_id VARCHAR(20),
    date_of_birth VARCHAR(10),
    eft_account_id VARCHAR(10),
    primary_cardholder_ind CHAR(1) CHECK (primary_cardholder_ind IN ('Y', 'N')),
    fico_credit_score NUMERIC(3),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for common customer queries
CREATE INDEX idx_customers_name ON customers(last_name, first_name);
CREATE INDEX idx_customers_ssn ON customers(ssn);

COMMENT ON TABLE customers IS 'Customer personal information and demographics';
COMMENT ON COLUMN customers.customer_id IS '9-digit unique customer identifier';
COMMENT ON COLUMN customers.ssn IS 'Social Security Number, 9 digits';
COMMENT ON COLUMN customers.primary_cardholder_ind IS 'Primary cardholder indicator: Y=Yes, N=No';
COMMENT ON COLUMN customers.fico_credit_score IS 'FICO credit score, 3 digits (300-850)';

-- ============================================================================
-- Table: accounts
-- Purpose: Store customer account information
-- Requirement: 11.2
-- ============================================================================
CREATE TABLE accounts (
    account_id NUMERIC(11) PRIMARY KEY,
    customer_id NUMERIC(9) NOT NULL,
    active_status CHAR(1) NOT NULL CHECK (active_status IN ('Y', 'N')),
    current_balance NUMERIC(12, 2) NOT NULL DEFAULT 0,
    credit_limit NUMERIC(12, 2) NOT NULL,
    cash_credit_limit NUMERIC(12, 2) NOT NULL,
    open_date VARCHAR(10) NOT NULL,
    expiration_date VARCHAR(10),
    reissue_date VARCHAR(10),
    current_cycle_credit NUMERIC(12, 2) DEFAULT 0,
    current_cycle_debit NUMERIC(12, 2) DEFAULT 0,
    address_zip VARCHAR(10),
    group_id VARCHAR(10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT fk_accounts_customer FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);

-- Indexes for common account queries
CREATE INDEX idx_accounts_customer ON accounts(customer_id);
CREATE INDEX idx_accounts_status ON accounts(active_status);
CREATE INDEX idx_accounts_zip ON accounts(address_zip);

COMMENT ON TABLE accounts IS 'Customer account information and balances';
COMMENT ON COLUMN accounts.account_id IS '11-digit unique account identifier';
COMMENT ON COLUMN accounts.active_status IS 'Account status: Y=Active, N=Inactive';
COMMENT ON COLUMN accounts.current_balance IS 'Current account balance, signed decimal(12,2)';
COMMENT ON COLUMN accounts.credit_limit IS 'Credit limit for the account';
COMMENT ON COLUMN accounts.cash_credit_limit IS 'Cash advance credit limit';
COMMENT ON COLUMN accounts.open_date IS 'Account open date in YYYY-MM-DD format';
COMMENT ON COLUMN accounts.current_cycle_credit IS 'Credits in current billing cycle';
COMMENT ON COLUMN accounts.current_cycle_debit IS 'Debits in current billing cycle';

-- ============================================================================
-- Table: cards
-- Purpose: Store credit card information
-- Requirement: 11.4
-- ============================================================================
CREATE TABLE cards (
    card_number NUMERIC(16) PRIMARY KEY,
    account_id NUMERIC(11) NOT NULL,
    customer_id NUMERIC(9) NOT NULL,
    card_status VARCHAR(10) NOT NULL,
    expiration_date VARCHAR(10),
    issue_date VARCHAR(10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT fk_cards_account FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    CONSTRAINT fk_cards_customer FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);

-- Indexes for common card queries
CREATE INDEX idx_cards_account ON cards(account_id);
CREATE INDEX idx_cards_customer ON cards(customer_id);
CREATE INDEX idx_cards_status ON cards(card_status);

COMMENT ON TABLE cards IS 'Credit card information and status';
COMMENT ON COLUMN cards.card_number IS '16-digit unique card number';
COMMENT ON COLUMN cards.card_status IS 'Card status code (e.g., ACTIVE, EXPIRED, BLOCKED)';
COMMENT ON COLUMN cards.expiration_date IS 'Card expiration date in YYYY-MM-DD format';
COMMENT ON COLUMN cards.issue_date IS 'Card issue date in YYYY-MM-DD format';

-- ============================================================================
-- Table: transactions
-- Purpose: Store financial transaction records
-- Requirement: 11.5
-- ============================================================================
CREATE TABLE transactions (
    transaction_id BIGSERIAL PRIMARY KEY,
    account_id NUMERIC(11) NOT NULL,
    card_number NUMERIC(16),
    transaction_type VARCHAR(10) NOT NULL,
    transaction_category VARCHAR(20),
    amount NUMERIC(12, 2) NOT NULL,
    transaction_date VARCHAR(10) NOT NULL,
    transaction_time VARCHAR(8),
    description VARCHAR(100),
    merchant_name VARCHAR(50),
    merchant_city VARCHAR(30),
    merchant_zip VARCHAR(10),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT fk_transactions_account FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    CONSTRAINT fk_transactions_card FOREIGN KEY (card_number) REFERENCES cards(card_number)
);

-- Indexes for common transaction queries
CREATE INDEX idx_transactions_account ON transactions(account_id);
CREATE INDEX idx_transactions_card ON transactions(card_number);
CREATE INDEX idx_transactions_date ON transactions(transaction_date);
CREATE INDEX idx_transactions_type ON transactions(transaction_type);
CREATE INDEX idx_transactions_account_date ON transactions(account_id, transaction_date);

COMMENT ON TABLE transactions IS 'Financial transaction records';
COMMENT ON COLUMN transactions.transaction_id IS 'Auto-generated unique transaction identifier';
COMMENT ON COLUMN transactions.amount IS 'Transaction amount, signed decimal(12,2)';
COMMENT ON COLUMN transactions.transaction_date IS 'Transaction date in YYYY-MM-DD format';
COMMENT ON COLUMN transactions.transaction_time IS 'Transaction time in HH:MM:SS format';
COMMENT ON COLUMN transactions.transaction_type IS 'Transaction type code';
COMMENT ON COLUMN transactions.transaction_category IS 'Transaction category for reporting';

-- ============================================================================
-- Table: card_xref
-- Purpose: Cross-reference table for card-customer-account relationships
-- Requirement: 11.6
-- ============================================================================
CREATE TABLE card_xref (
    card_number NUMERIC(16) NOT NULL,
    customer_id NUMERIC(9) NOT NULL,
    account_id NUMERIC(11) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (card_number, customer_id, account_id),
    CONSTRAINT fk_card_xref_card FOREIGN KEY (card_number) REFERENCES cards(card_number),
    CONSTRAINT fk_card_xref_customer FOREIGN KEY (customer_id) REFERENCES customers(customer_id),
    CONSTRAINT fk_card_xref_account FOREIGN KEY (account_id) REFERENCES accounts(account_id)
);

-- Indexes for cross-reference lookups
CREATE INDEX idx_card_xref_account ON card_xref(account_id);
CREATE INDEX idx_card_xref_customer ON card_xref(customer_id);

COMMENT ON TABLE card_xref IS 'Cross-reference table linking cards, customers, and accounts';
COMMENT ON COLUMN card_xref.card_number IS 'Reference to cards table';
COMMENT ON COLUMN card_xref.customer_id IS 'Reference to customers table';
COMMENT ON COLUMN card_xref.account_id IS 'Reference to accounts table';

-- ============================================================================
-- Schema Version Tracking
-- ============================================================================
COMMENT ON SCHEMA public IS 'CardDemo schema version 1.0 - Initial migration complete';
