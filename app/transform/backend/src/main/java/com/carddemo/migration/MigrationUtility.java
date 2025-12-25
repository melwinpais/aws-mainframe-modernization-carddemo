package com.carddemo.migration;

import com.carddemo.entity.*;
import com.carddemo.migration.model.*;
import com.carddemo.repository.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * Utility for migrating data from VSAM files to PostgreSQL database.
 * Handles extraction, conversion, loading, error logging, and summary reporting.
 */
@Component
public class MigrationUtility {

    private static final Logger logger = LoggerFactory.getLogger(MigrationUtility.class);

    @Autowired
    private VsamConverter vsamConverter;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private CustomerRepository customerRepository;

    @Autowired
    private CardRepository cardRepository;

    @Autowired
    private TransactionRepository transactionRepository;

    /**
     * Migration summary report containing statistics about the migration process.
     */
    public static class MigrationSummary {
        private int totalRecordsProcessed;
        private int successfulRecords;
        private int failedRecords;
        private List<String> errors;
        private long startTime;
        private long endTime;

        public MigrationSummary() {
            this.errors = new ArrayList<>();
            this.startTime = System.currentTimeMillis();
        }

        public void recordSuccess() {
            totalRecordsProcessed++;
            successfulRecords++;
        }

        public void recordFailure(String error) {
            totalRecordsProcessed++;
            failedRecords++;
            errors.add(error);
        }

        public void complete() {
            this.endTime = System.currentTimeMillis();
        }

        public int getTotalRecordsProcessed() {
            return totalRecordsProcessed;
        }

        public int getSuccessfulRecords() {
            return successfulRecords;
        }

        public int getFailedRecords() {
            return failedRecords;
        }

        public List<String> getErrors() {
            return errors;
        }

        public long getDurationMs() {
            return endTime - startTime;
        }

        @Override
        public String toString() {
            return String.format(
                "Migration Summary:\n" +
                "  Total Records Processed: %d\n" +
                "  Successful: %d\n" +
                "  Failed: %d\n" +
                "  Duration: %d ms\n" +
                "  Errors: %d",
                totalRecordsProcessed, successfulRecords, failedRecords,
                getDurationMs(), errors.size()
            );
        }
    }

    /**
     * Extract VSAM records from source files.
     * This is a placeholder method - actual implementation would read from VSAM files.
     * 
     * @param vsamFilePath Path to VSAM file
     * @param recordType Type of record to extract
     * @return List of VSAM records
     */
    public <T> List<T> extractFromVsam(String vsamFilePath, Class<T> recordType) {
        logger.info("Extracting records from VSAM file: {} for type: {}", 
                    vsamFilePath, recordType.getSimpleName());
        
        // Placeholder implementation
        // In a real implementation, this would:
        // 1. Open the VSAM file
        // 2. Read records sequentially
        // 3. Parse COBOL copybook structure
        // 4. Convert to Java objects
        // 5. Return list of records
        
        List<T> records = new ArrayList<>();
        logger.info("Extracted {} records from VSAM file", records.size());
        return records;
    }

    /**
     * Load user records from VSAM to PostgreSQL.
     * 
     * @param vsamRecords List of VSAM user records
     * @return Migration summary
     */
    @Transactional
    public MigrationSummary loadUsersToPostgres(List<VsamUserRecord> vsamRecords) {
        logger.info("Starting user migration. Total records: {}", vsamRecords.size());
        MigrationSummary summary = new MigrationSummary();

        for (VsamUserRecord vsamRecord : vsamRecords) {
            try {
                User user = vsamConverter.convertUserRecord(vsamRecord);
                userRepository.save(user);
                summary.recordSuccess();
                logger.debug("Successfully migrated user: {}", user.getUserId());
            } catch (Exception e) {
                String errorMsg = String.format("Failed to migrate user %s: %s", 
                                               vsamRecord.getUserId(), e.getMessage());
                logger.error(errorMsg, e);
                summary.recordFailure(errorMsg);
                // Continue processing remaining records
            }
        }

        summary.complete();
        logger.info("User migration completed. {}", summary);
        return summary;
    }

    /**
     * Load account records from VSAM to PostgreSQL.
     * 
     * @param vsamRecords List of VSAM account records
     * @return Migration summary
     */
    @Transactional
    public MigrationSummary loadAccountsToPostgres(List<VsamAccountRecord> vsamRecords) {
        logger.info("Starting account migration. Total records: {}", vsamRecords.size());
        MigrationSummary summary = new MigrationSummary();

        for (VsamAccountRecord vsamRecord : vsamRecords) {
            try {
                Account account = vsamConverter.convertAccountRecord(vsamRecord);
                accountRepository.save(account);
                summary.recordSuccess();
                logger.debug("Successfully migrated account: {}", account.getAccountId());
            } catch (Exception e) {
                String errorMsg = String.format("Failed to migrate account %s: %s", 
                                               vsamRecord.getAccountId(), e.getMessage());
                logger.error(errorMsg, e);
                summary.recordFailure(errorMsg);
                // Continue processing remaining records
            }
        }

        summary.complete();
        logger.info("Account migration completed. {}", summary);
        return summary;
    }

    /**
     * Load customer records from VSAM to PostgreSQL.
     * 
     * @param vsamRecords List of VSAM customer records
     * @return Migration summary
     */
    @Transactional
    public MigrationSummary loadCustomersToPostgres(List<VsamCustomerRecord> vsamRecords) {
        logger.info("Starting customer migration. Total records: {}", vsamRecords.size());
        MigrationSummary summary = new MigrationSummary();

        for (VsamCustomerRecord vsamRecord : vsamRecords) {
            try {
                Customer customer = vsamConverter.convertCustomerRecord(vsamRecord);
                customerRepository.save(customer);
                summary.recordSuccess();
                logger.debug("Successfully migrated customer: {}", customer.getCustomerId());
            } catch (Exception e) {
                String errorMsg = String.format("Failed to migrate customer %s: %s", 
                                               vsamRecord.getCustomerId(), e.getMessage());
                logger.error(errorMsg, e);
                summary.recordFailure(errorMsg);
                // Continue processing remaining records
            }
        }

        summary.complete();
        logger.info("Customer migration completed. {}", summary);
        return summary;
    }

    /**
     * Load card records from VSAM to PostgreSQL.
     * 
     * @param vsamRecords List of VSAM card records
     * @return Migration summary
     */
    @Transactional
    public MigrationSummary loadCardsToPostgres(List<VsamCardRecord> vsamRecords) {
        logger.info("Starting card migration. Total records: {}", vsamRecords.size());
        MigrationSummary summary = new MigrationSummary();

        for (VsamCardRecord vsamRecord : vsamRecords) {
            try {
                Card card = vsamConverter.convertCardRecord(vsamRecord);
                cardRepository.save(card);
                summary.recordSuccess();
                logger.debug("Successfully migrated card: {}", card.getCardNumber());
            } catch (Exception e) {
                String errorMsg = String.format("Failed to migrate card %s: %s", 
                                               vsamRecord.getCardNumber(), e.getMessage());
                logger.error(errorMsg, e);
                summary.recordFailure(errorMsg);
                // Continue processing remaining records
            }
        }

        summary.complete();
        logger.info("Card migration completed. {}", summary);
        return summary;
    }

    /**
     * Load transaction records from VSAM to PostgreSQL.
     * 
     * @param vsamRecords List of VSAM transaction records
     * @return Migration summary
     */
    @Transactional
    public MigrationSummary loadTransactionsToPostgres(List<VsamTransactionRecord> vsamRecords) {
        logger.info("Starting transaction migration. Total records: {}", vsamRecords.size());
        MigrationSummary summary = new MigrationSummary();

        for (VsamTransactionRecord vsamRecord : vsamRecords) {
            try {
                Transaction transaction = vsamConverter.convertTransactionRecord(vsamRecord);
                transactionRepository.save(transaction);
                summary.recordSuccess();
                logger.debug("Successfully migrated transaction for account: {}", 
                           transaction.getAccountId());
            } catch (Exception e) {
                String errorMsg = String.format("Failed to migrate transaction for account %s: %s", 
                                               vsamRecord.getAccountId(), e.getMessage());
                logger.error(errorMsg, e);
                summary.recordFailure(errorMsg);
                // Continue processing remaining records
            }
        }

        summary.complete();
        logger.info("Transaction migration completed. {}", summary);
        return summary;
    }

    /**
     * Perform complete migration from VSAM to PostgreSQL.
     * Migrates all record types in the correct order (respecting foreign key dependencies).
     * 
     * @param vsamBasePath Base path to VSAM files
     * @return Overall migration summary
     */
    public MigrationSummary performFullMigration(String vsamBasePath) {
        logger.info("Starting full migration from VSAM to PostgreSQL");
        MigrationSummary overallSummary = new MigrationSummary();

        try {
            // Step 1: Migrate users (no dependencies)
            List<VsamUserRecord> userRecords = extractFromVsam(
                vsamBasePath + "/USRSEC", VsamUserRecord.class);
            MigrationSummary userSummary = loadUsersToPostgres(userRecords);
            mergeSummaries(overallSummary, userSummary);

            // Step 2: Migrate customers (no dependencies)
            List<VsamCustomerRecord> customerRecords = extractFromVsam(
                vsamBasePath + "/CUSTDAT", VsamCustomerRecord.class);
            MigrationSummary customerSummary = loadCustomersToPostgres(customerRecords);
            mergeSummaries(overallSummary, customerSummary);

            // Step 3: Migrate accounts (no dependencies)
            List<VsamAccountRecord> accountRecords = extractFromVsam(
                vsamBasePath + "/ACCTDAT", VsamAccountRecord.class);
            MigrationSummary accountSummary = loadAccountsToPostgres(accountRecords);
            mergeSummaries(overallSummary, accountSummary);

            // Step 4: Migrate cards (depends on accounts and customers)
            List<VsamCardRecord> cardRecords = extractFromVsam(
                vsamBasePath + "/CARDDAT", VsamCardRecord.class);
            MigrationSummary cardSummary = loadCardsToPostgres(cardRecords);
            mergeSummaries(overallSummary, cardSummary);

            // Step 5: Migrate transactions (depends on accounts and cards)
            List<VsamTransactionRecord> transactionRecords = extractFromVsam(
                vsamBasePath + "/TRANSACT", VsamTransactionRecord.class);
            MigrationSummary transactionSummary = loadTransactionsToPostgres(transactionRecords);
            mergeSummaries(overallSummary, transactionSummary);

        } catch (Exception e) {
            logger.error("Full migration failed with error: {}", e.getMessage(), e);
            overallSummary.recordFailure("Full migration failed: " + e.getMessage());
        }

        overallSummary.complete();
        logger.info("Full migration completed. {}", overallSummary);
        return overallSummary;
    }

    /**
     * Merge statistics from individual migration summaries into overall summary.
     */
    private void mergeSummaries(MigrationSummary overall, MigrationSummary individual) {
        overall.totalRecordsProcessed += individual.totalRecordsProcessed;
        overall.successfulRecords += individual.successfulRecords;
        overall.failedRecords += individual.failedRecords;
        overall.errors.addAll(individual.errors);
    }
}
