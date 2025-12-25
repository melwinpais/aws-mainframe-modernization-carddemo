package com.carddemo.integration;

import com.carddemo.CardDemoApplication;
import com.carddemo.dto.*;
import com.carddemo.entity.Account;
import com.carddemo.entity.Card;
import com.carddemo.entity.Customer;
import com.carddemo.entity.User;
import com.carddemo.repository.AccountRepository;
import com.carddemo.repository.CardRepository;
import com.carddemo.repository.CustomerRepository;
import com.carddemo.repository.UserRepository;
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Integration tests for account management flow
 * Tests the complete flow: account view → account update → verification
 * Tests account search with various inputs
 * Validates Requirements: 3.1-3.11, 4.1-4.6
 */
@SpringBootTest(
    classes = CardDemoApplication.class,
    webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT
)
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
class AccountManagementFlowIntegrationTest {

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
     * Test complete account view flow
     * 1. Search for account by ID
     * 2. Verify account details are returned
     * 3. Verify customer details are included
     * 4. Verify cards are included
     * Validates Requirements: 3.1, 3.5, 3.6, 3.7, 3.10
     */
    @Test
    void testAccountViewFlow() throws Exception {
        // Step 1: Get account details
        MvcResult accountResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.accountId").value(TEST_ACCOUNT_ID))
                .andExpect(jsonPath("$.activeStatus").value("Y"))
                .andExpect(jsonPath("$.currentBalance").value(1500.00))
                .andExpect(jsonPath("$.creditLimit").value(10000.00))
                .andExpect(jsonPath("$.cashCreditLimit").value(2000.00))
                .andExpect(jsonPath("$.openDate").value("2020-01-15"))
                .andExpect(jsonPath("$.expirationDate").value("2025-01-15"))
                .andExpect(jsonPath("$.currentCycleCredit").value(500.00))
                .andExpect(jsonPath("$.currentCycleDebit").value(300.00))
                .andExpect(jsonPath("$.addressZip").value("90210"))
                .andExpect(jsonPath("$.groupId").value("GRP001"))
                .andReturn();

        String accountResponseBody = accountResult.getResponse().getContentAsString();
        AccountDto accountDto = objectMapper.readValue(accountResponseBody, AccountDto.class);

        assertThat(accountDto).isNotNull();
        assertThat(accountDto.getAccountId()).isEqualTo(TEST_ACCOUNT_ID);

        // Step 2: Get customer for account
        MvcResult customerResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID + "/customer")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.customerId").value(TEST_CUSTOMER_ID))
                .andExpect(jsonPath("$.firstName").value("John"))
                .andExpect(jsonPath("$.lastName").value("Doe"))
                .andExpect(jsonPath("$.addressLine1").value("123 Main St"))
                .andExpect(jsonPath("$.stateCode").value("CA"))
                .andExpect(jsonPath("$.zipCode").value("90210"))
                .andExpect(jsonPath("$.phoneNumber1").value("(555)123-4567"))
                .andExpect(jsonPath("$.ficoCreditScore").value(750))
                .andReturn();

        String customerResponseBody = customerResult.getResponse().getContentAsString();
        CustomerDto customerDto = objectMapper.readValue(customerResponseBody, CustomerDto.class);

        assertThat(customerDto).isNotNull();
        assertThat(customerDto.getCustomerId()).isEqualTo(TEST_CUSTOMER_ID);

        // Step 3: Get cards for account
        MvcResult cardsResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID + "/cards")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andExpect(jsonPath("$[0].cardNumber").value(TEST_CARD_NUMBER))
                .andExpect(jsonPath("$[0].accountId").value(TEST_ACCOUNT_ID))
                .andExpect(jsonPath("$[0].cardStatus").value("A"))
                .andExpect(jsonPath("$[0].expirationDate").value("2025-12-31"))
                .andReturn();

        String cardsResponseBody = cardsResult.getResponse().getContentAsString();
        CardDto[] cards = objectMapper.readValue(cardsResponseBody, CardDto[].class);

        assertThat(cards).isNotEmpty();
        assertThat(cards[0].getCardNumber()).isEqualTo(TEST_CARD_NUMBER);
    }

    /**
     * Test complete account update flow
     * 1. Get account details
     * 2. Update account information
     * 3. Verify updates were persisted
     * 4. Verify customer updates were persisted
     * Validates Requirements: 4.1, 4.2, 4.3, 4.4, 4.5
     */
    @Test
    void testAccountUpdateFlow() throws Exception {
        // Step 1: Get original account details
        MvcResult originalResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andReturn();

        String originalResponseBody = originalResult.getResponse().getContentAsString();
        AccountDto originalAccount = objectMapper.readValue(originalResponseBody, AccountDto.class);

        assertThat(originalAccount.getCurrentBalance()).isEqualByComparingTo(new BigDecimal("1500.00"));
        assertThat(originalAccount.getCreditLimit()).isEqualByComparingTo(new BigDecimal("10000.00"));

        // Step 2: Update account information
        AccountUpdateDto updateDto = new AccountUpdateDto();
        updateDto.setActiveStatus("Y");
        updateDto.setCreditLimit(new BigDecimal("15000.00"));
        updateDto.setCashCreditLimit(new BigDecimal("3000.00"));

        // Also update customer information
        CustomerUpdateDto customerUpdate = new CustomerUpdateDto();
        customerUpdate.setFirstName("John");
        customerUpdate.setLastName("Doe");
        customerUpdate.setAddressLine1("456 Oak Ave");
        customerUpdate.setStateCode("CA");
        customerUpdate.setZipCode("90211");
        customerUpdate.setPhoneNumber1("(555)987-6543");
        customerUpdate.setFicoCreditScore(780);

        updateDto.setCustomer(customerUpdate);

        MvcResult updateResult = mockMvc.perform(put("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(updateDto)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.accountId").value(TEST_ACCOUNT_ID))
                .andExpect(jsonPath("$.creditLimit").value(15000.00))
                .andExpect(jsonPath("$.cashCreditLimit").value(3000.00))
                .andReturn();

        String updateResponseBody = updateResult.getResponse().getContentAsString();
        AccountDto updatedAccount = objectMapper.readValue(updateResponseBody, AccountDto.class);

        assertThat(updatedAccount.getCreditLimit()).isEqualByComparingTo(new BigDecimal("15000.00"));
        assertThat(updatedAccount.getCashCreditLimit()).isEqualByComparingTo(new BigDecimal("3000.00"));

        // Step 3: Verify updates were persisted by fetching account again
        MvcResult verifyResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.creditLimit").value(15000.00))
                .andExpect(jsonPath("$.cashCreditLimit").value(3000.00))
                .andReturn();

        // Step 4: Verify customer updates were persisted
        MvcResult customerVerifyResult = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID + "/customer")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.addressLine1").value("456 Oak Ave"))
                .andExpect(jsonPath("$.zipCode").value("90211"))
                .andExpect(jsonPath("$.phoneNumber1").value("(555)987-6543"))
                .andExpect(jsonPath("$.ficoCreditScore").value(780))
                .andReturn();

        String customerVerifyResponseBody = customerVerifyResult.getResponse().getContentAsString();
        CustomerDto verifiedCustomer = objectMapper.readValue(customerVerifyResponseBody, CustomerDto.class);

        assertThat(verifiedCustomer.getAddressLine1()).isEqualTo("456 Oak Ave");
        assertThat(verifiedCustomer.getFicoCreditScore()).isEqualTo(780);
    }

    /**
     * Test account search with valid account ID
     * Validates Requirements: 3.1, 3.5
     */
    @Test
    void testAccountSearchWithValidAccountId() throws Exception {
        mockMvc.perform(get("/api/accounts/search")
                        .param("accountId", TEST_ACCOUNT_ID.toString())
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andExpect(jsonPath("$[0].accountId").value(TEST_ACCOUNT_ID))
                .andExpect(jsonPath("$[0].activeStatus").value("Y"))
                .andExpect(jsonPath("$[0].currentBalance").value(1500.00));
    }

    /**
     * Test account search with valid customer ID
     * Validates Requirements: 3.1, 3.5
     */
    @Test
    void testAccountSearchWithValidCustomerId() throws Exception {
        mockMvc.perform(get("/api/accounts/search")
                        .param("customerId", TEST_CUSTOMER_ID.toString())
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andExpect(jsonPath("$[0].accountId").value(TEST_ACCOUNT_ID));
    }

    /**
     * Test account search with non-existent account ID
     * Validates Requirements: 3.8
     */
    @Test
    void testAccountSearchWithNonExistentAccountId() throws Exception {
        Long nonExistentAccountId = 99999999999L;

        mockMvc.perform(get("/api/accounts/" + nonExistentAccountId)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Did not find this account in account master file"));
    }

    /**
     * Test account search with invalid account ID format
     * Validates Requirements: 3.1, 3.2, 3.3, 3.4
     */
    @Test
    void testAccountSearchWithInvalidAccountIdFormat() throws Exception {
        // Test with all zeroes
        mockMvc.perform(get("/api/accounts/0")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Account number must be a non zero 11 digit number"));
    }

    /**
     * Test account update with invalid data
     * Validates Requirements: 4.6
     */
    @Test
    void testAccountUpdateWithInvalidData() throws Exception {
        // Create update with invalid active status (not Y or N)
        AccountUpdateDto invalidUpdate = new AccountUpdateDto();
        invalidUpdate.setActiveStatus("X"); // Invalid: must be Y or N
        invalidUpdate.setCreditLimit(new BigDecimal("10000.00"));
        invalidUpdate.setCashCreditLimit(new BigDecimal("2000.00"));

        mockMvc.perform(put("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(invalidUpdate)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").exists());
    }

    /**
     * Test account update with invalid active status
     * Validates Requirements: 4.2, 4.6
     */
    @Test
    void testAccountUpdateWithInvalidActiveStatus() throws Exception {
        AccountUpdateDto invalidUpdate = new AccountUpdateDto();
        invalidUpdate.setActiveStatus("X"); // Invalid: must be Y or N
        invalidUpdate.setCreditLimit(new BigDecimal("10000.00"));
        invalidUpdate.setCashCreditLimit(new BigDecimal("2000.00"));

        mockMvc.perform(put("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(invalidUpdate)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").exists());
    }

    /**
     * Test customer not found error when account references non-existent customer
     * Validates Requirements: 3.9
     */
    @Test
    void testCustomerNotFoundError() throws Exception {
        // Create account with non-existent customer ID
        Long nonExistentCustomerId = 999999999L;
        Long newAccountId = 98765432101L;

        Account orphanAccount = new Account();
        orphanAccount.setAccountId(newAccountId);
        orphanAccount.setCustomerId(nonExistentCustomerId);
        orphanAccount.setActiveStatus("Y");
        orphanAccount.setCurrentBalance(new BigDecimal("0.00"));
        orphanAccount.setCreditLimit(new BigDecimal("5000.00"));
        orphanAccount.setCashCreditLimit(new BigDecimal("1000.00"));
        orphanAccount.setOpenDate("2024-01-01");
        accountRepository.save(orphanAccount);

        // Try to get customer for this account
        mockMvc.perform(get("/api/accounts/" + newAccountId + "/customer")
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Did not find associated customer in master file"));
    }

    /**
     * Test complete flow with multiple accounts for same customer
     * Validates Requirements: 3.5, 3.6, 3.7
     */
    @Test
    void testMultipleAccountsForSameCustomer() throws Exception {
        // Create second account for same customer
        Long secondAccountId = 98765432102L;
        Account secondAccount = new Account();
        secondAccount.setAccountId(secondAccountId);
        secondAccount.setCustomerId(TEST_CUSTOMER_ID);
        secondAccount.setActiveStatus("Y");
        secondAccount.setCurrentBalance(new BigDecimal("2500.00"));
        secondAccount.setCreditLimit(new BigDecimal("20000.00"));
        secondAccount.setCashCreditLimit(new BigDecimal("4000.00"));
        secondAccount.setOpenDate("2021-06-01");
        secondAccount.setExpirationDate("2026-06-01");
        accountRepository.save(secondAccount);

        // Search by customer ID should return both accounts
        MvcResult searchResult = mockMvc.perform(get("/api/accounts/search")
                        .param("customerId", TEST_CUSTOMER_ID.toString())
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andExpect(jsonPath("$.length()").value(2))
                .andReturn();

        String searchResponseBody = searchResult.getResponse().getContentAsString();
        AccountDto[] accounts = objectMapper.readValue(searchResponseBody, AccountDto[].class);

        assertThat(accounts).hasSize(2);
        assertThat(accounts).extracting(AccountDto::getAccountId)
                .containsExactlyInAnyOrder(TEST_ACCOUNT_ID, secondAccountId);
    }

    /**
     * Test account view returns complete data
     * Validates Requirements: 3.10
     */
    @Test
    void testAccountViewReturnsCompleteData() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID)
                        .header("Authorization", "Bearer " + authToken))
                .andExpect(status().isOk())
                .andReturn();

        String responseBody = result.getResponse().getContentAsString();
        AccountDto account = objectMapper.readValue(responseBody, AccountDto.class);

        // Verify all required fields are present
        assertThat(account.getAccountId()).isNotNull();
        assertThat(account.getActiveStatus()).isNotNull();
        assertThat(account.getCurrentBalance()).isNotNull();
        assertThat(account.getCreditLimit()).isNotNull();
        assertThat(account.getCashCreditLimit()).isNotNull();
        assertThat(account.getOpenDate()).isNotNull();
        assertThat(account.getExpirationDate()).isNotNull();
        assertThat(account.getCurrentCycleCredit()).isNotNull();
        assertThat(account.getCurrentCycleDebit()).isNotNull();
        assertThat(account.getAddressZip()).isNotNull();
        assertThat(account.getGroupId()).isNotNull();
    }

    /**
     * Test unauthorized access without token
     * Validates Requirements: 13.3
     */
    @Test
    void testUnauthorizedAccessWithoutToken() throws Exception {
        mockMvc.perform(get("/api/accounts/" + TEST_ACCOUNT_ID))
                .andExpect(status().isUnauthorized());

        mockMvc.perform(put("/api/accounts/" + TEST_ACCOUNT_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{}"))
                .andExpect(status().isUnauthorized());
    }
}
