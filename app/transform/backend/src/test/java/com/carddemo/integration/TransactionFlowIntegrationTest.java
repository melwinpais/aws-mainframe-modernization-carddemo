package com.carddemo.integration;

import com.carddemo.CardDemoApplication;
import com.carddemo.dto.*;
import com.carddemo.entity.*;
import com.carddemo.repository.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Integration tests for transaction flow
 * Tests transaction list with pagination
 * Tests transaction creation with balance update
 * Validates Requirements: 6.1-6.6
 */
@SpringBootTest(
    classes = CardDemoApplication.class,
    webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT
)
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
class TransactionFlowIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

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

    @Autowired
    private PasswordEncoder passwordEncoder;

    private static final String TEST_USER_ID = "TESTUSER";
    private static final String PASSWORD = "password123";
    private static final Long TEST_ACCOUNT_ID = 12345678901L;
    private static final Long TEST_CUSTOMER_ID = 123456789L;
    private static final Long TEST_CARD_NUMBER = 4111111111111111L;

    private String authToken;

    @BeforeEach
    void setUp() throws Exception {
        // Clean up existing test data
        transactionRepository.deleteAll();
        cardRepository.deleteAll();
        accountRepository.deleteAll();
        customerRepository.deleteAll();
        userRepository.deleteAll();

        // Create test user
        User testUser = new User();
        testUser.setUserId(TEST_USER_ID);
        testUser.setFirstName("Test");
        testUser.setLastName("User");
        testUser.setPassword(passwordEncoder.encode(PASSWORD));
        testUser.setUserType("U");
        userRepository.save(testUser);

        // Create test customer
        Customer testCustomer = new Customer();
        testCustomer.setCustomerId(TEST_CUSTOMER_ID);
        testCustomer.setFirstName("John");
        testCustomer.setLastName("Doe");
        testCustomer.setAddressLine1("123 Main St");
        testCustomer.setStateCode("CA");
        testCustomer.setZipCode("90210");
        testCustomer.setPhoneNumber1("(555)123-4567");
        testCustomer.setSsn(123456789L);
        testCustomer.setFicoCreditScore(750);
        customerRepository.save(testCustomer);

        // Create test account
        Account testAccount = new Account();
        testAccount.setAccountId(TEST_ACCOUNT_ID);
        testAccount.setCustomerId(TEST_CUSTOMER_ID);
        testAccount.setActiveStatus("Y");
        testAccount.setCurrentBalance(new BigDecimal("1500.00"));
        testAccount.setCreditLimit(new BigDecimal("10000.00"));
        testAccount.setCashCreditLimit(new BigDecimal("2000.00"));
        testAccount.setOpenDate("2020-01-15");
        testAccount.setExpirationDate("2025-01-15");
        testAccount.setCurrentCycleCredit(new BigDecimal("500.00"));
        testAccount.setCurrentCycleDebit(new BigDecimal("300.00"));
        testAccount.setAddressZip("90210");
        testAccount.setGroupId("GRP001");
        accountRepository.save(testAccount);

        // Create test card
        Card testCard = new Card();
        testCard.setCardNumber(TEST_CARD_NUMBER);
        testCard.setAccountId(TEST_ACCOUNT_ID);
        testCard.setCustomerId(TEST_CUSTOMER_ID);
        testCard.setCardStatus("A");
        testCard.setExpirationDate("2025-12-31");
        cardRepository.save(testCard);

        // Login and get auth token
        LoginRequestDto loginRequest = new LoginRequestDto();
        loginRequest.setUserId(TEST_USER_ID);
        loginRequest.setPassword(PASSWORD);

        MvcResult loginResult = mockMvc.perform(post("/api/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(loginRequest)))
                .andExpect(status().isOk())
                .andReturn();

        String responseBody = loginResult.getResponse().getContentAsString();
        LoginResponseDto loginResponse = objectMapper.readValue(responseBody, LoginResponseDto.class);
        authToken = loginResponse.getToken();
    }

    /**
     * Test transaction list with pagination
     * 1. Create multiple transactions
     * 2. Retrieve transactions with pagination
     * 3. Verify pagination works correctly
     * Validates Requirements: 6.1, 6.2
     */
    @Test
    void testTransactionListWithPagination() throws Exception {
        // Step 1: Create 25 test transactions
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        for (int i = 1; i <= 25; i++) {
            Transaction transaction = new Transaction();
            transaction.setAccountId(TEST_ACCOUNT_ID);
            transaction.setCardNumber(TEST_CARD_NUMBER);
            transaction.setTransactionType("PURCHASE");
            transaction.setTransactionCategory("RETAIL");
            transaction.setAmount(new BigDecimal("50.00"));
            transaction.setDescription("Test Transaction " + i);
            transaction.setTransactionDate(LocalDateTime.now().minusDays(i).format(formatter));
            transactionRepository.save(transaction);
        }

        // Step 2: Retrieve first page (default size 20)
        MvcResult firstPageResult = mockMvc.perform(get("/api/transactions/account/" + TEST_ACCOUNT_ID)
                        .param("page", "0")
                        .param("size", "20")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content").isArray())
                .andExpect(jsonPath("$.content.length()").value(20))
                .andExpect(jsonPath("$.totalElements").value(25))
                .andExpect(jsonPath("$.totalPages").value(2))
                .andExpect(jsonPath("$.number").value(0))
                .andExpect(jsonPath("$.size").value(20))
                .andExpect(jsonPath("$.first").value(true))
                .andExpect(jsonPath("$.last").value(false))
                .andReturn();

        // Step 3: Retrieve second page
        MvcResult secondPageResult = mockMvc.perform(get("/api/transactions/account/" + TEST_ACCOUNT_ID)
                        .param("page", "1")
                        .param("size", "20")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content").isArray())
                .andExpect(jsonPath("$.content.length()").value(5))
                .andExpect(jsonPath("$.totalElements").value(25))
                .andExpect(jsonPath("$.totalPages").value(2))
                .andExpect(jsonPath("$.number").value(1))
                .andExpect(jsonPath("$.size").value(20))
                .andExpect(jsonPath("$.first").value(false))
                .andExpect(jsonPath("$.last").value(true))
                .andReturn();

        // Step 4: Test with smaller page size
        MvcResult smallPageResult = mockMvc.perform(get("/api/transactions/account/" + TEST_ACCOUNT_ID)
                        .param("page", "0")
                        .param("size", "10")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content.length()").value(10))
                .andExpect(jsonPath("$.totalElements").value(25))
                .andExpect(jsonPath("$.totalPages").value(3))
                .andReturn();
    }

    /**
     * Test transaction creation with balance update
     * 1. Get initial account balance
     * 2. Create a new transaction
     * 3. Verify transaction was created
     * 4. Verify account balance was updated
     * Validates Requirements: 6.4, 6.5, 6.6
     */
    @Test
    void testTransactionCreationWithBalanceUpdate() throws Exception {
        // Step 1: Get initial account balance
        MvcResult initialAccountResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andReturn();

        String initialAccountResponseBody = initialAccountResult.getResponse().getContentAsString();
        AccountDto initialAccount = objectMapper.readValue(initialAccountResponseBody, AccountDto.class);
        BigDecimal initialBalance = initialAccount.getCurrentBalance();

        assertThat(initialBalance).isEqualByComparingTo(new BigDecimal("1500.00"));

        // Step 2: Create a new purchase transaction
        String currentDate = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
        TransactionCreateDto transactionCreate = new TransactionCreateDto();
        transactionCreate.setAccountId(TEST_ACCOUNT_ID);
        transactionCreate.setCardNumber(TEST_CARD_NUMBER);
        transactionCreate.setTransactionType("PURCHASE");
        transactionCreate.setTransactionCategory("RETAIL");
        transactionCreate.setAmount(new BigDecimal("250.00"));
        transactionCreate.setDescription("Test Purchase");
        transactionCreate.setTransactionDate(currentDate);

        MvcResult createResult = mockMvc.perform(post("/api/transactions")
                        .header("Authorization", "Bearer " + authToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(transactionCreate)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.transactionId").exists())
                .andExpect(jsonPath("$.accountId").value(TEST_ACCOUNT_ID))
                .andExpect(jsonPath("$.cardNumber").value(TEST_CARD_NUMBER))
                .andExpect(jsonPath("$.transactionType").value("PURCHASE"))
                .andExpect(jsonPath("$.amount").value(250.00))
                .andExpect(jsonPath("$.description").value("Test Purchase"))
                .andReturn();

        String createResponseBody = createResult.getResponse().getContentAsString();
        TransactionDto createdTransaction = objectMapper.readValue(createResponseBody, TransactionDto.class);

        assertThat(createdTransaction.getTransactionId()).isNotNull();

        // Step 3: Verify transaction was created by retrieving it
        mockMvc.perform(get("/api/transactions/" + createdTransaction.getTransactionId())
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.transactionId").value(createdTransaction.getTransactionId()))
                .andExpect(jsonPath("$.accountId").value(TEST_ACCOUNT_ID))
                .andExpect(jsonPath("$.amount").value(250.00));

        // Step 4: Verify account balance was updated (increased by purchase amount)
        MvcResult updatedAccountResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andReturn();

        String updatedAccountResponseBody = updatedAccountResult.getResponse().getContentAsString();
        AccountDto updatedAccount = objectMapper.readValue(updatedAccountResponseBody, AccountDto.class);
        BigDecimal updatedBalance = updatedAccount.getCurrentBalance();

        // Balance should increase by transaction amount for purchases
        BigDecimal expectedBalance = initialBalance.add(new BigDecimal("250.00"));
        assertThat(updatedBalance).isEqualByComparingTo(expectedBalance);
    }

    /**
     * Test transaction creation with payment (balance decrease)
     * Validates Requirements: 6.4, 6.5, 6.6
     */
    @Test
    void testPaymentTransactionWithBalanceDecrease() throws Exception {
        // Step 1: Get initial account balance
        MvcResult initialAccountResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andReturn();

        String initialAccountResponseBody = initialAccountResult.getResponse().getContentAsString();
        AccountDto initialAccount = objectMapper.readValue(initialAccountResponseBody, AccountDto.class);
        BigDecimal initialBalance = initialAccount.getCurrentBalance();

        // Step 2: Create a payment transaction (use negative amount for payment)
        String currentDate = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
        TransactionCreateDto paymentCreate = new TransactionCreateDto();
        paymentCreate.setAccountId(TEST_ACCOUNT_ID);
        paymentCreate.setCardNumber(TEST_CARD_NUMBER);
        paymentCreate.setTransactionType("PAYMENT");
        paymentCreate.setTransactionCategory("PAYMENT");
        paymentCreate.setAmount(new BigDecimal("-500.00")); // Negative amount for payment
        paymentCreate.setDescription("Payment");
        paymentCreate.setTransactionDate(currentDate);

        MvcResult createResult = mockMvc.perform(post("/api/transactions")
                        .header("Authorization", "Bearer " + authToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(paymentCreate)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.transactionId").exists())
                .andExpect(jsonPath("$.transactionType").value("PAYMENT"))
                .andExpect(jsonPath("$.amount").value(-500.00))
                .andReturn();

        // Step 3: Verify account balance was decreased
        MvcResult updatedAccountResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andReturn();

        String updatedAccountResponseBody = updatedAccountResult.getResponse().getContentAsString();
        AccountDto updatedAccount = objectMapper.readValue(updatedAccountResponseBody, AccountDto.class);
        BigDecimal updatedBalance = updatedAccount.getCurrentBalance();

        // Balance should decrease by payment amount
        BigDecimal expectedBalance = initialBalance.subtract(new BigDecimal("500.00"));
        assertThat(updatedBalance).isEqualByComparingTo(expectedBalance);
    }

    /**
     * Test transaction retrieval by card number with pagination
     * Validates Requirements: 6.1, 6.2, 6.3
     */
    @Test
    void testTransactionRetrievalByCardWithPagination() throws Exception {
        // Create transactions for the test card
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        for (int i = 1; i <= 15; i++) {
            Transaction transaction = new Transaction();
            transaction.setAccountId(TEST_ACCOUNT_ID);
            transaction.setCardNumber(TEST_CARD_NUMBER);
            transaction.setTransactionType("PURCHASE");
            transaction.setTransactionCategory("RETAIL");
            transaction.setAmount(new BigDecimal("25.00"));
            transaction.setDescription("Card Transaction " + i);
            transaction.setTransactionDate(LocalDateTime.now().minusDays(i).format(formatter));
            transactionRepository.save(transaction);
        }

        // Retrieve transactions by card number with pagination
        MvcResult result = mockMvc.perform(get("/api/transactions/card/" + TEST_CARD_NUMBER)
                        .param("page", "0")
                        .param("size", "10")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content").isArray())
                .andExpect(jsonPath("$.content.length()").value(10))
                .andExpect(jsonPath("$.totalElements").value(15))
                .andExpect(jsonPath("$.totalPages").value(2))
                .andExpect(jsonPath("$.content[0].cardNumber").value(TEST_CARD_NUMBER))
                .andReturn();
    }

    /**
     * Test transaction retrieval with date range filter
     * Validates Requirements: 6.2, 6.3
     */
    @Test
    void testTransactionRetrievalWithDateRangeFilter() throws Exception {
        // Create transactions with different dates
        LocalDateTime now = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        // Transaction 1: 10 days ago
        Transaction txn1 = new Transaction();
        txn1.setAccountId(TEST_ACCOUNT_ID);
        txn1.setCardNumber(TEST_CARD_NUMBER);
        txn1.setTransactionType("PURCHASE");
        txn1.setTransactionCategory("RETAIL");
        txn1.setAmount(new BigDecimal("100.00"));
        txn1.setDescription("Old Transaction");
        txn1.setTransactionDate(now.minusDays(10).format(formatter));
        transactionRepository.save(txn1);

        // Transaction 2: 5 days ago
        Transaction txn2 = new Transaction();
        txn2.setAccountId(TEST_ACCOUNT_ID);
        txn2.setCardNumber(TEST_CARD_NUMBER);
        txn2.setTransactionType("PURCHASE");
        txn2.setTransactionCategory("RETAIL");
        txn2.setAmount(new BigDecimal("200.00"));
        txn2.setDescription("Recent Transaction");
        txn2.setTransactionDate(now.minusDays(5).format(formatter));
        transactionRepository.save(txn2);

        // Transaction 3: Today
        Transaction txn3 = new Transaction();
        txn3.setAccountId(TEST_ACCOUNT_ID);
        txn3.setCardNumber(TEST_CARD_NUMBER);
        txn3.setTransactionType("PURCHASE");
        txn3.setTransactionCategory("RETAIL");
        txn3.setAmount(new BigDecimal("300.00"));
        txn3.setDescription("Today Transaction");
        txn3.setTransactionDate(now.format(formatter));
        transactionRepository.save(txn3);

        // Filter transactions from 7 days ago to today
        String startDate = now.minusDays(7).format(formatter);
        String endDate = now.format(formatter);

        MvcResult result = mockMvc.perform(get("/api/transactions/account/" + TEST_ACCOUNT_ID)
                        .param("startDate", startDate)
                        .param("endDate", endDate)
                        .param("page", "0")
                        .param("size", "20")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content").isArray())
                .andExpect(jsonPath("$.content.length()").value(2)) // Should only return txn2 and txn3
                .andReturn();
    }

    /**
     * Test transaction retrieval by ID
     * Validates Requirements: 6.1
     */
    @Test
    void testTransactionRetrievalById() throws Exception {
        // Create a transaction
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        Transaction transaction = new Transaction();
        transaction.setAccountId(TEST_ACCOUNT_ID);
        transaction.setCardNumber(TEST_CARD_NUMBER);
        transaction.setTransactionType("PURCHASE");
        transaction.setTransactionCategory("RETAIL");
        transaction.setAmount(new BigDecimal("150.00"));
        transaction.setDescription("Test Transaction");
        transaction.setTransactionDate(LocalDateTime.now().format(formatter));
        Transaction savedTransaction = transactionRepository.save(transaction);

        // Retrieve transaction by ID
        mockMvc.perform(get("/api/transactions/" + savedTransaction.getTransactionId())
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.transactionId").value(savedTransaction.getTransactionId()))
                .andExpect(jsonPath("$.accountId").value(TEST_ACCOUNT_ID))
                .andExpect(jsonPath("$.cardNumber").value(TEST_CARD_NUMBER))
                .andExpect(jsonPath("$.transactionType").value("PURCHASE"))
                .andExpect(jsonPath("$.amount").value(150.00))
                .andExpect(jsonPath("$.description").value("Test Transaction"));
    }

    /**
     * Test transaction creation with validation
     * Validates Requirements: 6.4
     */
    @Test
    void testTransactionCreationWithValidation() throws Exception {
        // Test with valid transaction data
        String currentDate = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
        TransactionCreateDto validTransaction = new TransactionCreateDto();
        validTransaction.setAccountId(TEST_ACCOUNT_ID);
        validTransaction.setCardNumber(TEST_CARD_NUMBER);
        validTransaction.setTransactionType("PURCHASE");
        validTransaction.setTransactionCategory("RETAIL");
        validTransaction.setAmount(new BigDecimal("100.00"));
        validTransaction.setDescription("Valid Transaction");
        validTransaction.setTransactionDate(currentDate);

        mockMvc.perform(post("/api/transactions")
                        .header("Authorization", "Bearer " + authToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(validTransaction)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.transactionId").exists());
    }

    /**
     * Test transaction creation with invalid account
     * Validates Requirements: 6.4
     */
    @Test
    void testTransactionCreationWithInvalidAccount() throws Exception {
        // Test with non-existent account
        String currentDate = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
        TransactionCreateDto invalidTransaction = new TransactionCreateDto();
        invalidTransaction.setAccountId(99999999999L);
        invalidTransaction.setCardNumber(TEST_CARD_NUMBER);
        invalidTransaction.setTransactionType("PURCHASE");
        invalidTransaction.setTransactionCategory("RETAIL");
        invalidTransaction.setAmount(new BigDecimal("100.00"));
        invalidTransaction.setDescription("Invalid Account Transaction");
        invalidTransaction.setTransactionDate(currentDate);

        mockMvc.perform(post("/api/transactions")
                        .header("Authorization", "Bearer " + authToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(invalidTransaction)))
                .andExpect(status().isNotFound());
    }

    /**
     * Test complete transaction flow
     * 1. Create multiple transactions
     * 2. List transactions with pagination
     * 3. Filter by date range
     * 4. Verify balance updates
     * Validates Requirements: 6.1, 6.2, 6.3, 6.4, 6.5, 6.6
     */
    @Test
    void testCompleteTransactionFlow() throws Exception {
        // Step 1: Get initial balance
        MvcResult initialAccountResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andReturn();

        String initialAccountResponseBody = initialAccountResult.getResponse().getContentAsString();
        AccountDto initialAccount = objectMapper.readValue(initialAccountResponseBody, AccountDto.class);
        BigDecimal initialBalance = initialAccount.getCurrentBalance();

        // Step 2: Create multiple transactions
        BigDecimal totalPurchases = BigDecimal.ZERO;
        BigDecimal totalPayments = BigDecimal.ZERO;

        // Create 3 purchase transactions
        String currentDate = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
        for (int i = 1; i <= 3; i++) {
            TransactionCreateDto purchase = new TransactionCreateDto();
            purchase.setAccountId(TEST_ACCOUNT_ID);
            purchase.setCardNumber(TEST_CARD_NUMBER);
            purchase.setTransactionType("PURCHASE");
            purchase.setTransactionCategory("RETAIL");
            purchase.setAmount(new BigDecimal("100.00"));
            purchase.setDescription("Purchase " + i);
            purchase.setTransactionDate(currentDate);

            mockMvc.perform(post("/api/transactions")
                            .header("Authorization", "Bearer " + authToken)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(purchase)))
                    .andExpect(status().isCreated());

            totalPurchases = totalPurchases.add(new BigDecimal("100.00"));
        }

        // Create 1 payment transaction (use negative amount for payment)
        TransactionCreateDto payment = new TransactionCreateDto();
        payment.setAccountId(TEST_ACCOUNT_ID);
        payment.setCardNumber(TEST_CARD_NUMBER);
        payment.setTransactionType("PAYMENT");
        payment.setTransactionCategory("PAYMENT");
        payment.setAmount(new BigDecimal("-200.00")); // Negative amount for payment
        payment.setDescription("Payment");
        payment.setTransactionDate(currentDate);

        mockMvc.perform(post("/api/transactions")
                        .header("Authorization", "Bearer " + authToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(payment)))
                .andExpect(status().isCreated());

        totalPayments = totalPayments.add(new BigDecimal("200.00"));

        // Step 3: List all transactions for the account
        MvcResult listResult = mockMvc.perform(get("/api/transactions/account/" + TEST_ACCOUNT_ID)
                        .param("page", "0")
                        .param("size", "20")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content").isArray())
                .andExpect(jsonPath("$.content.length()").value(4))
                .andExpect(jsonPath("$.totalElements").value(4))
                .andReturn();

        // Step 4: Verify final balance
        MvcResult finalAccountResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andReturn();

        String finalAccountResponseBody = finalAccountResult.getResponse().getContentAsString();
        AccountDto finalAccount = objectMapper.readValue(finalAccountResponseBody, AccountDto.class);
        BigDecimal finalBalance = finalAccount.getCurrentBalance();

        // Calculate expected balance: initial + purchases - payments
        BigDecimal expectedBalance = initialBalance.add(totalPurchases).subtract(totalPayments);
        assertThat(finalBalance).isEqualByComparingTo(expectedBalance);
    }

    /**
     * Test unauthorized access without token
     * Validates Requirements: 13.3
     */
    @Test
    void testUnauthorizedAccessWithoutToken() throws Exception {
        mockMvc.perform(get("/api/transactions/account/" + TEST_ACCOUNT_ID))
                .andExpect(status().isUnauthorized());

        mockMvc.perform(post("/api/transactions")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{}"))
                .andExpect(status().isUnauthorized());
    }
}
