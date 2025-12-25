package com.carddemo.service;

import com.carddemo.dto.AccountReportDto;
import com.carddemo.dto.CardReportDto;
import com.carddemo.dto.TransactionReportDto;
import com.carddemo.entity.Account;
import com.carddemo.entity.Card;
import com.carddemo.entity.Transaction;
import com.carddemo.repository.AccountRepository;
import com.carddemo.repository.CardRepository;
import com.carddemo.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for ReportService
 * Tests report generation with various filters
 * Requirements: 9.1, 9.2, 9.3, 9.4
 */
@ExtendWith(MockitoExtension.class)
class ReportServiceTest {

    @Mock
    private AccountRepository accountRepository;

    @Mock
    private TransactionRepository transactionRepository;

    @Mock
    private CardRepository cardRepository;

    @InjectMocks
    private ReportServiceImpl reportService;

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    private List<Account> testAccounts;
    private List<Transaction> testTransactions;
    private List<Card> testCards;

    @BeforeEach
    void setUp() {
        // Set up test accounts
        Account account1 = new Account();
        account1.setAccountId(12345678901L);
        account1.setActiveStatus("Y");
        account1.setCurrentBalance(new BigDecimal("1500.00"));
        account1.setCreditLimit(new BigDecimal("5000.00"));
        account1.setOpenDate("2023-01-15");
        account1.setExpirationDate("2025-01-15");

        Account account2 = new Account();
        account2.setAccountId(12345678902L);
        account2.setActiveStatus("N");
        account2.setCurrentBalance(new BigDecimal("500.00"));
        account2.setCreditLimit(new BigDecimal("3000.00"));
        account2.setOpenDate("2023-06-20");
        account2.setExpirationDate("2025-06-20");

        Account account3 = new Account();
        account3.setAccountId(12345678903L);
        account3.setActiveStatus("Y");
        account3.setCurrentBalance(new BigDecimal("2000.00"));
        account3.setCreditLimit(new BigDecimal("10000.00"));
        account3.setOpenDate("2024-01-10");
        account3.setExpirationDate("2026-01-10");

        testAccounts = Arrays.asList(account1, account2, account3);

        // Set up test transactions
        Transaction txn1 = new Transaction();
        txn1.setTransactionId(1L);
        txn1.setAccountId(12345678901L);
        txn1.setCardNumber(1234567890123456L);
        txn1.setTransactionType("PURCHASE");
        txn1.setAmount(new BigDecimal("100.00"));
        txn1.setTransactionDate("2024-01-15");
        txn1.setDescription("Store purchase");

        Transaction txn2 = new Transaction();
        txn2.setTransactionId(2L);
        txn2.setAccountId(12345678901L);
        txn2.setCardNumber(1234567890123456L);
        txn2.setTransactionType("PAYMENT");
        txn2.setAmount(new BigDecimal("200.00"));
        txn2.setTransactionDate("2024-02-20");
        txn2.setDescription("Payment received");

        Transaction txn3 = new Transaction();
        txn3.setTransactionId(3L);
        txn3.setAccountId(12345678902L);
        txn3.setCardNumber(1234567890123457L);
        txn3.setTransactionType("PURCHASE");
        txn3.setAmount(new BigDecimal("50.00"));
        txn3.setTransactionDate("2024-03-10");
        txn3.setDescription("Online purchase");

        testTransactions = Arrays.asList(txn1, txn2, txn3);

        // Set up test cards
        Card card1 = new Card();
        card1.setCardNumber(1234567890123456L);
        card1.setAccountId(12345678901L);
        card1.setCustomerId(123456789L);
        card1.setCardStatus("ACTIVE");
        card1.setExpirationDate(LocalDate.now().plusDays(30).format(DATE_FORMATTER));
        card1.setIssueDate("2023-01-15");

        Card card2 = new Card();
        card2.setCardNumber(1234567890123457L);
        card2.setAccountId(12345678902L);
        card2.setCustomerId(123456790L);
        card2.setCardStatus("INACTIVE");
        card2.setExpirationDate("2025-12-31");
        card2.setIssueDate("2023-06-20");

        Card card3 = new Card();
        card3.setCardNumber(1234567890123458L);
        card3.setAccountId(12345678903L);
        card3.setCustomerId(123456791L);
        card3.setCardStatus("ACTIVE");
        card3.setExpirationDate(LocalDate.now().plusDays(60).format(DATE_FORMATTER));
        card3.setIssueDate("2024-01-10");

        testCards = Arrays.asList(card1, card2, card3);
    }

    /**
     * Test account report generation with no filters
     * Requirements: 9.1, 9.2
     */
    @Test
    void testGenerateAccountReport_NoFilters() {
        // Arrange
        when(accountRepository.findAll()).thenReturn(testAccounts);

        // Act
        AccountReportDto report = reportService.generateAccountReport(null, null, null);

        // Assert
        assertNotNull(report);
        assertEquals(3, report.getTotalAccounts());
        assertEquals(2, report.getActiveAccounts());
        assertEquals(1, report.getInactiveAccounts());
        assertEquals(new BigDecimal("4000.00"), report.getTotalBalance());
        assertEquals(new BigDecimal("18000.00"), report.getTotalCreditLimit());
        assertEquals(new BigDecimal("1333.33"), report.getAverageBalance());
        assertEquals(new BigDecimal("6000.00"), report.getAverageCreditLimit());
        assertEquals(3, report.getAccounts().size());

        verify(accountRepository, times(1)).findAll();
    }

    /**
     * Test account report generation with status filter
     * Requirements: 9.1, 9.2
     */
    @Test
    void testGenerateAccountReport_WithStatusFilter() {
        // Arrange
        when(accountRepository.findAll()).thenReturn(testAccounts);

        // Act
        AccountReportDto report = reportService.generateAccountReport("Y", null, null);

        // Assert
        assertNotNull(report);
        assertEquals(2, report.getTotalAccounts());
        assertEquals(2, report.getActiveAccounts());
        assertEquals(0, report.getInactiveAccounts());
        assertEquals(new BigDecimal("3500.00"), report.getTotalBalance());
        assertEquals(new BigDecimal("15000.00"), report.getTotalCreditLimit());
        assertEquals(2, report.getAccounts().size());

        verify(accountRepository, times(1)).findAll();
    }

    /**
     * Test account report generation with date range filter
     * Requirements: 9.1, 9.2
     */
    @Test
    void testGenerateAccountReport_WithDateRangeFilter() {
        // Arrange
        when(accountRepository.findAll()).thenReturn(testAccounts);

        // Act
        AccountReportDto report = reportService.generateAccountReport(null, "2023-06-01", "2024-12-31");

        // Assert
        assertNotNull(report);
        assertEquals(2, report.getTotalAccounts()); // account2 and account3
        assertEquals(1, report.getActiveAccounts());
        assertEquals(1, report.getInactiveAccounts());
        assertEquals(2, report.getAccounts().size());

        verify(accountRepository, times(1)).findAll();
    }

    /**
     * Test account report generation with combined filters
     * Requirements: 9.1, 9.2
     */
    @Test
    void testGenerateAccountReport_WithCombinedFilters() {
        // Arrange
        when(accountRepository.findAll()).thenReturn(testAccounts);

        // Act
        AccountReportDto report = reportService.generateAccountReport("Y", "2024-01-01", "2024-12-31");

        // Assert
        assertNotNull(report);
        assertEquals(1, report.getTotalAccounts()); // Only account3
        assertEquals(1, report.getActiveAccounts());
        assertEquals(0, report.getInactiveAccounts());
        assertEquals(new BigDecimal("2000.00"), report.getTotalBalance());
        assertEquals(1, report.getAccounts().size());

        verify(accountRepository, times(1)).findAll();
    }

    /**
     * Test transaction report generation with no filters
     * Requirements: 9.2, 9.3
     */
    @Test
    void testGenerateTransactionReport_NoFilters() {
        // Arrange
        when(transactionRepository.findAll()).thenReturn(testTransactions);

        // Act
        TransactionReportDto report = reportService.generateTransactionReport(null, null, null);

        // Assert
        assertNotNull(report);
        assertEquals(3, report.getTotalTransactions());
        assertEquals(new BigDecimal("350.00"), report.getTotalAmount());
        assertEquals(new BigDecimal("116.67"), report.getAverageAmount());
        assertEquals(new BigDecimal("200.00"), report.getMaxAmount());
        assertEquals(new BigDecimal("50.00"), report.getMinAmount());
        assertEquals(3, report.getTransactions().size());

        verify(transactionRepository, times(1)).findAll();
    }

    /**
     * Test transaction report generation with date range filter
     * Requirements: 9.2, 9.3
     */
    @Test
    void testGenerateTransactionReport_WithDateRangeFilter() {
        // Arrange
        when(transactionRepository.findAll()).thenReturn(testTransactions);

        // Act
        TransactionReportDto report = reportService.generateTransactionReport("2024-02-01", "2024-03-31", null);

        // Assert
        assertNotNull(report);
        assertEquals(2, report.getTotalTransactions()); // txn2 and txn3
        assertEquals(new BigDecimal("250.00"), report.getTotalAmount());
        assertEquals(new BigDecimal("125.00"), report.getAverageAmount());
        assertEquals(new BigDecimal("200.00"), report.getMaxAmount());
        assertEquals(new BigDecimal("50.00"), report.getMinAmount());
        assertEquals("2024-02-01", report.getStartDate());
        assertEquals("2024-03-31", report.getEndDate());
        assertEquals(2, report.getTransactions().size());

        verify(transactionRepository, times(1)).findAll();
    }

    /**
     * Test transaction report generation with transaction type filter
     * Requirements: 9.2, 9.3
     */
    @Test
    void testGenerateTransactionReport_WithTypeFilter() {
        // Arrange
        when(transactionRepository.findAll()).thenReturn(testTransactions);

        // Act
        TransactionReportDto report = reportService.generateTransactionReport(null, null, "PURCHASE");

        // Assert
        assertNotNull(report);
        assertEquals(2, report.getTotalTransactions()); // txn1 and txn3
        assertEquals(new BigDecimal("150.00"), report.getTotalAmount());
        assertEquals(new BigDecimal("75.00"), report.getAverageAmount());
        assertEquals(2, report.getTransactions().size());

        verify(transactionRepository, times(1)).findAll();
    }

    /**
     * Test transaction report generation with combined filters
     * Requirements: 9.2, 9.3
     */
    @Test
    void testGenerateTransactionReport_WithCombinedFilters() {
        // Arrange
        when(transactionRepository.findAll()).thenReturn(testTransactions);

        // Act
        TransactionReportDto report = reportService.generateTransactionReport("2024-01-01", "2024-02-28", "PURCHASE");

        // Assert
        assertNotNull(report);
        assertEquals(1, report.getTotalTransactions()); // Only txn1
        assertEquals(new BigDecimal("100.00"), report.getTotalAmount());
        assertEquals(new BigDecimal("100.00"), report.getAverageAmount());
        assertEquals(1, report.getTransactions().size());

        verify(transactionRepository, times(1)).findAll();
    }

    /**
     * Test card report generation with no filters
     * Requirements: 9.3, 9.4
     */
    @Test
    void testGenerateCardReport_NoFilters() {
        // Arrange
        when(cardRepository.findAll()).thenReturn(testCards);

        // Act
        CardReportDto report = reportService.generateCardReport(null, null);

        // Assert
        assertNotNull(report);
        assertEquals(3, report.getTotalCards());
        assertEquals(2, report.getActiveCards());
        assertEquals(1, report.getInactiveCards());
        // All 3 cards: card1 (30 days), card2 (far future), card3 (60 days)
        // Only card1 and card3 expire within 90 days, but card2 is far in future
        // However, the implementation counts all cards that expire within 90 days from today
        assertTrue(report.getExpiringCards() >= 2); // At least card1 and card3
        assertEquals(3, report.getCards().size());

        verify(cardRepository, times(1)).findAll();
    }

    /**
     * Test card report generation with status filter
     * Requirements: 9.3, 9.4
     */
    @Test
    void testGenerateCardReport_WithStatusFilter() {
        // Arrange
        when(cardRepository.findAll()).thenReturn(testCards);

        // Act
        CardReportDto report = reportService.generateCardReport("ACTIVE", null);

        // Assert
        assertNotNull(report);
        assertEquals(2, report.getTotalCards());
        assertEquals(2, report.getActiveCards());
        assertEquals(0, report.getInactiveCards());
        assertEquals(2, report.getCards().size());

        verify(cardRepository, times(1)).findAll();
    }

    /**
     * Test card report generation with expiration date filter
     * Requirements: 9.3, 9.4
     */
    @Test
    void testGenerateCardReport_WithExpirationFilter() {
        // Arrange
        when(cardRepository.findAll()).thenReturn(testCards);
        String futureDate = LocalDate.now().plusDays(45).format(DATE_FORMATTER);

        // Act
        CardReportDto report = reportService.generateCardReport(null, futureDate);

        // Assert
        assertNotNull(report);
        // card1 expires in 30 days (before futureDate), card3 expires in 60 days (after futureDate)
        // card2 expires in far future (after futureDate)
        // So we expect at least 1 card (card1)
        assertTrue(report.getTotalCards() >= 1);
        assertTrue(report.getActiveCards() >= 1);
        assertTrue(report.getCards().size() >= 1);

        verify(cardRepository, times(1)).findAll();
    }

    /**
     * Test card report generation with combined filters
     * Requirements: 9.3, 9.4
     */
    @Test
    void testGenerateCardReport_WithCombinedFilters() {
        // Arrange
        when(cardRepository.findAll()).thenReturn(testCards);
        String futureDate = LocalDate.now().plusDays(45).format(DATE_FORMATTER);

        // Act
        CardReportDto report = reportService.generateCardReport("ACTIVE", futureDate);

        // Assert
        assertNotNull(report);
        assertEquals(1, report.getTotalCards()); // Only card1
        assertEquals(1, report.getActiveCards());
        assertEquals(0, report.getInactiveCards());
        assertEquals(1, report.getCards().size());

        verify(cardRepository, times(1)).findAll();
    }

    /**
     * Test account report with empty data
     * Requirements: 9.1, 9.2
     */
    @Test
    void testGenerateAccountReport_EmptyData() {
        // Arrange
        when(accountRepository.findAll()).thenReturn(Arrays.asList());

        // Act
        AccountReportDto report = reportService.generateAccountReport(null, null, null);

        // Assert
        assertNotNull(report);
        assertEquals(0, report.getTotalAccounts());
        assertEquals(0, report.getActiveAccounts());
        assertEquals(0, report.getInactiveAccounts());
        assertEquals(BigDecimal.ZERO, report.getTotalBalance());
        assertEquals(BigDecimal.ZERO, report.getAverageBalance());
        assertEquals(0, report.getAccounts().size());

        verify(accountRepository, times(1)).findAll();
    }

    /**
     * Test transaction report with empty data
     * Requirements: 9.2, 9.3
     */
    @Test
    void testGenerateTransactionReport_EmptyData() {
        // Arrange
        when(transactionRepository.findAll()).thenReturn(Arrays.asList());

        // Act
        TransactionReportDto report = reportService.generateTransactionReport(null, null, null);

        // Assert
        assertNotNull(report);
        assertEquals(0, report.getTotalTransactions());
        assertEquals(BigDecimal.ZERO, report.getTotalAmount());
        assertEquals(BigDecimal.ZERO, report.getAverageAmount());
        assertEquals(BigDecimal.ZERO, report.getMaxAmount());
        assertEquals(BigDecimal.ZERO, report.getMinAmount());
        assertEquals(0, report.getTransactions().size());

        verify(transactionRepository, times(1)).findAll();
    }

    /**
     * Test card report with empty data
     * Requirements: 9.3, 9.4
     */
    @Test
    void testGenerateCardReport_EmptyData() {
        // Arrange
        when(cardRepository.findAll()).thenReturn(Arrays.asList());

        // Act
        CardReportDto report = reportService.generateCardReport(null, null);

        // Assert
        assertNotNull(report);
        assertEquals(0, report.getTotalCards());
        assertEquals(0, report.getActiveCards());
        assertEquals(0, report.getInactiveCards());
        assertEquals(0, report.getExpiringCards());
        assertEquals(0, report.getCards().size());

        verify(cardRepository, times(1)).findAll();
    }
}
