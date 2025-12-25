package com.carddemo.migration;

import com.carddemo.entity.*;
import com.carddemo.migration.model.*;
import com.carddemo.repository.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * Unit tests for MigrationUtility.
 * Tests error handling, logging, and summary report generation.
 */
@ExtendWith(MockitoExtension.class)
public class MigrationUtilityTest {

    @Mock
    private VsamConverter vsamConverter;

    @Mock
    private UserRepository userRepository;

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private CustomerRepository customerRepository;

    @Mock
    private CardRepository cardRepository;

    @Mock
    private TransactionRepository transactionRepository;

    @InjectMocks
    private MigrationUtility migrationUtility;

    @Test
    public void testLoadUsersToPostgres_Success() {
        // Given
        VsamUserRecord vsamRecord = new VsamUserRecord("USER0001", "John", "Doe", "password", "U");
        User user = new User();
        user.setUserId("USER0001");
        user.setFirstName("John");
        user.setLastName("Doe");

        when(vsamConverter.convertUserRecord(vsamRecord)).thenReturn(user);
        when(userRepository.save(any(User.class))).thenReturn(user);

        // When
        MigrationUtility.MigrationSummary summary = migrationUtility.loadUsersToPostgres(
            Arrays.asList(vsamRecord));

        // Then
        assertEquals(1, summary.getTotalRecordsProcessed());
        assertEquals(1, summary.getSuccessfulRecords());
        assertEquals(0, summary.getFailedRecords());
        verify(vsamConverter, times(1)).convertUserRecord(vsamRecord);
        verify(userRepository, times(1)).save(user);
    }

    @Test
    public void testLoadUsersToPostgres_WithErrors() {
        // Given
        VsamUserRecord validRecord = new VsamUserRecord("USER0001", "John", "Doe", "password", "U");
        VsamUserRecord invalidRecord = new VsamUserRecord("USER0002", "Jane", "Smith", "password", "U");

        User validUser = new User();
        validUser.setUserId("USER0001");

        when(vsamConverter.convertUserRecord(validRecord)).thenReturn(validUser);
        when(vsamConverter.convertUserRecord(invalidRecord))
            .thenThrow(new RuntimeException("Conversion error"));
        when(userRepository.save(validUser)).thenReturn(validUser);

        // When
        MigrationUtility.MigrationSummary summary = migrationUtility.loadUsersToPostgres(
            Arrays.asList(validRecord, invalidRecord));

        // Then
        assertEquals(2, summary.getTotalRecordsProcessed());
        assertEquals(1, summary.getSuccessfulRecords());
        assertEquals(1, summary.getFailedRecords());
        assertEquals(1, summary.getErrors().size());
        assertTrue(summary.getErrors().get(0).contains("USER0002"));
        assertTrue(summary.getErrors().get(0).contains("Conversion error"));
    }

    @Test
    public void testLoadAccountsToPostgres_Success() {
        // Given
        VsamAccountRecord vsamRecord = new VsamAccountRecord(
            12345678901L, "Y", new BigDecimal("1000.00"), new BigDecimal("5000.00"),
            new BigDecimal("1000.00"), "2024-01-01", "2026-01-01", "2024-01-01",
            new BigDecimal("0.00"), new BigDecimal("0.00"), "12345", "GROUP001"
        );
        Account account = new Account();
        account.setAccountId(12345678901L);

        when(vsamConverter.convertAccountRecord(vsamRecord)).thenReturn(account);
        when(accountRepository.save(any(Account.class))).thenReturn(account);

        // When
        MigrationUtility.MigrationSummary summary = migrationUtility.loadAccountsToPostgres(
            Arrays.asList(vsamRecord));

        // Then
        assertEquals(1, summary.getTotalRecordsProcessed());
        assertEquals(1, summary.getSuccessfulRecords());
        assertEquals(0, summary.getFailedRecords());
    }

    @Test
    public void testLoadCustomersToPostgres_Success() {
        // Given
        VsamCustomerRecord vsamRecord = new VsamCustomerRecord(
            123456789L, "John", "M", "Doe", "123 Main St", "", "",
            "CA", "USA", "12345", "(555)123-4567", "", 123456789L,
            "DL123456", "1980-01-01", "EFT001", "Y", 750
        );
        Customer customer = new Customer();
        customer.setCustomerId(123456789L);

        when(vsamConverter.convertCustomerRecord(vsamRecord)).thenReturn(customer);
        when(customerRepository.save(any(Customer.class))).thenReturn(customer);

        // When
        MigrationUtility.MigrationSummary summary = migrationUtility.loadCustomersToPostgres(
            Arrays.asList(vsamRecord));

        // Then
        assertEquals(1, summary.getTotalRecordsProcessed());
        assertEquals(1, summary.getSuccessfulRecords());
        assertEquals(0, summary.getFailedRecords());
    }

    @Test
    public void testLoadCardsToPostgres_Success() {
        // Given
        VsamCardRecord vsamRecord = new VsamCardRecord(
            1234567890123456L, 12345678901L, 123456789L,
            "ACTIVE", "2026-12-31", "2024-01-01"
        );
        Card card = new Card();
        card.setCardNumber(1234567890123456L);

        when(vsamConverter.convertCardRecord(vsamRecord)).thenReturn(card);
        when(cardRepository.save(any(Card.class))).thenReturn(card);

        // When
        MigrationUtility.MigrationSummary summary = migrationUtility.loadCardsToPostgres(
            Arrays.asList(vsamRecord));

        // Then
        assertEquals(1, summary.getTotalRecordsProcessed());
        assertEquals(1, summary.getSuccessfulRecords());
        assertEquals(0, summary.getFailedRecords());
    }

    @Test
    public void testLoadTransactionsToPostgres_Success() {
        // Given
        VsamTransactionRecord vsamRecord = new VsamTransactionRecord(
            12345678901L, 1234567890123456L, "PURCHASE", "RETAIL",
            new BigDecimal("50.00"), "2024-12-24", "10:30:00",
            "Test purchase", "Test Merchant", "Test City", "12345"
        );
        Transaction transaction = new Transaction();
        transaction.setAccountId(12345678901L);

        when(vsamConverter.convertTransactionRecord(vsamRecord)).thenReturn(transaction);
        when(transactionRepository.save(any(Transaction.class))).thenReturn(transaction);

        // When
        MigrationUtility.MigrationSummary summary = migrationUtility.loadTransactionsToPostgres(
            Arrays.asList(vsamRecord));

        // Then
        assertEquals(1, summary.getTotalRecordsProcessed());
        assertEquals(1, summary.getSuccessfulRecords());
        assertEquals(0, summary.getFailedRecords());
    }

    @Test
    public void testMigrationSummary_ToString() {
        // Given
        MigrationUtility.MigrationSummary summary = new MigrationUtility.MigrationSummary();
        summary.recordSuccess();
        summary.recordSuccess();
        summary.recordFailure("Error 1");
        summary.complete();

        // When
        String summaryString = summary.toString();

        // Then
        assertTrue(summaryString.contains("Total Records Processed: 3"));
        assertTrue(summaryString.contains("Successful: 2"));
        assertTrue(summaryString.contains("Failed: 1"));
        assertTrue(summaryString.contains("Errors: 1"));
    }

    @Test
    public void testMigrationSummary_ErrorContinuesProcessing() {
        // Given - Mix of valid and invalid records
        VsamUserRecord validRecord1 = new VsamUserRecord("USER0001", "John", "Doe", "password", "U");
        VsamUserRecord invalidRecord = new VsamUserRecord("USER0002", "Jane", "Smith", "password", "U");
        VsamUserRecord validRecord2 = new VsamUserRecord("USER0003", "Bob", "Jones", "password", "U");

        User user1 = new User();
        user1.setUserId("USER0001");
        User user3 = new User();
        user3.setUserId("USER0003");

        when(vsamConverter.convertUserRecord(validRecord1)).thenReturn(user1);
        when(vsamConverter.convertUserRecord(invalidRecord))
            .thenThrow(new RuntimeException("Database constraint violation"));
        when(vsamConverter.convertUserRecord(validRecord2)).thenReturn(user3);
        when(userRepository.save(any(User.class))).thenAnswer(invocation -> invocation.getArgument(0));

        // When
        MigrationUtility.MigrationSummary summary = migrationUtility.loadUsersToPostgres(
            Arrays.asList(validRecord1, invalidRecord, validRecord2));

        // Then - Should process all records despite error in the middle
        assertEquals(3, summary.getTotalRecordsProcessed());
        assertEquals(2, summary.getSuccessfulRecords());
        assertEquals(1, summary.getFailedRecords());
        verify(vsamConverter, times(3)).convertUserRecord(any());
        verify(userRepository, times(2)).save(any(User.class));
    }
}
